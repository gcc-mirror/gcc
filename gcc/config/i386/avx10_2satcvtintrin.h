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
#error "Never use <avx10_2satcvtintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2SATCVTINTRIN_H_INCLUDED
#define _AVX10_2SATCVTINTRIN_H_INCLUDED

#if !defined (__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_bf16_epi8 (__m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtbf162ibs128_mask ((__v8bf) __A,
						       (__v8hi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_bf16_epi8 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtbf162ibs128_mask ((__v8bf) __A,
						       (__v8hi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_bf16_epi8 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtbf162ibs128_mask ((__v8bf) __A,
						       (__v8hi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_bf16_epi8 (__m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtbf162ibs256_mask ((__v16bf) __A,
						  (__v16hi)
						  _mm256_undefined_si256 (),
						  (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_bf16_epi8 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvtbf162ibs256_mask ((__v16bf) __A,
						       (__v16hi) __W,
						       (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_bf16_epi8 (__mmask16 __U, __m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtbf162ibs256_mask ((__v16bf) __A,
						  (__v16hi)
						  _mm256_setzero_si256 (),
						  (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_bf16_epu8 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvtbf162iubs128_mask ((__v8bf) __A,
						   (__v8hi)
						   _mm_undefined_si128 (),
						   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_bf16_epu8 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtbf162iubs128_mask ((__v8bf) __A,
							(__v8hi) __W,
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_bf16_epu8 (__mmask8 __U, __m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvtbf162iubs128_mask ((__v8bf) __A,
						   (__v8hi)
						   _mm_setzero_si128 (),
						   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_bf16_epu8 (__m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtbf162iubs256_mask ((__v16bf) __A,
						   (__v16hi)
						   _mm256_undefined_si256 (),
						   (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_bf16_epu8 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvtbf162iubs256_mask ((__v16bf) __A,
							(__v16hi) __W,
							(__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_bf16_epu8 (__mmask16 __U, __m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtbf162iubs256_mask ((__v16bf) __A,
						   (__v16hi)
						   _mm256_setzero_si256 (),
						   (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_ph_epi8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_ph_epi8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_ph_epi8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_ph_epu8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_ph_epu8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_ph_epu8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_ps_epi8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_ps_epi8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_ps_epi8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvts_ps_epu8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvts_ps_epu8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvts_ps_epu8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_bf16_epi8 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvttbf162ibs128_mask ((__v8bf) __A,
						   (__v8hi)
						   _mm_undefined_si128 (),
						   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_bf16_epi8 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttbf162ibs128_mask ((__v8bf) __A,
							(__v8hi) __W,
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_bf16_epi8 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttbf162ibs128_mask ((__v8bf) __A,
							(__v8hi)
							_mm_setzero_si128 (),
							(__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_bf16_epu8 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvttbf162iubs128_mask ((__v8bf) __A,
						    (__v8hi)
						    _mm_undefined_si128 (),
						    (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_bf16_epu8 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttbf162iubs128_mask ((__v8bf) __A,
							 (__v8hi) __W,
							 (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_bf16_epu8 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttbf162iubs128_mask ((__v8bf) __A,
							 (__v8hi)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_bf16_epi8 (__m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttbf162ibs256_mask ((__v16bf) __A,
					 (__v16hi) _mm256_undefined_si256 (),
					 (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_bf16_epi8 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvttbf162ibs256_mask ((__v16bf) __A,
							(__v16hi) __W,
							(__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_bf16_epi8 (__mmask16 __U, __m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttbf162ibs256_mask ((__v16bf) __A,
					 (__v16hi) _mm256_setzero_si256 (),
					 (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_bf16_epu8 (__m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttbf162iubs256_mask ((__v16bf) __A,
					  (__v16hi) _mm256_undefined_si256 (),
					  (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_bf16_epu8 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvttbf162iubs256_mask ((__v16bf) __A,
							 (__v16hi) __W,
							 (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_bf16_epu8 (__mmask16 __U, __m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttbf162iubs256_mask ((__v16bf) __A,
					  (__v16hi) _mm256_setzero_si256 (),
					  (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_ph_epi8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_ph_epi8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_ph_epi8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_ph_epu8 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_ph_epu8 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_ph_epu8 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_ps_epi8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_ps_epi8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_ps_epi8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtts_ps_epu8 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtts_ps_epu8 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtts_ps_epu8 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_pd_epi32 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_pd_epi32 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_pd_epi32 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_pd_epi64 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_pd_epi64 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_pd_epi64 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_pd_epu32 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_pd_epu32 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_pd_epu32 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_pd_epu64 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_pd_epu64 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_pd_epu64 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ps_epi32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_ps_epi32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_ps_epi32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ps_epi64 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_ps_epi64 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_ps_epi64 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ps_epu32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_ps_epu32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_ps_epu32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ps_epu64 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtts_ps_epu64 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtts_ps_epu64 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_ph_epi8 (__m256h __A)
{
  return
    (__m256i) __builtin_ia32_cvtph2ibs256_mask ((__v16hf) __A,
						(__v16hi)
						_mm256_undefined_si256 (),
						(__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_ph_epi8 (__m256i __W, __mmask16 __U, __m256h __A)
{
  return (__m256i) __builtin_ia32_cvtph2ibs256_mask ((__v16hf) __A,
						     (__v16hi) __W,
						     (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_ph_epi8 (__mmask16 __U, __m256h __A)
{
  return
    (__m256i) __builtin_ia32_cvtph2ibs256_mask ((__v16hf) __A,
						(__v16hi)
						_mm256_setzero_si256 (),
						(__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_ph_epu8 (__m256h __A)
{
  return (__m256i)
    __builtin_ia32_cvtph2iubs256_mask ((__v16hf) __A,
				       (__v16hi)
				       _mm256_undefined_si256 (),
				       (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_ph_epu8 (__m256i __W, __mmask16 __U, __m256h __A)
{
  return (__m256i) __builtin_ia32_cvtph2iubs256_mask ((__v16hf) __A,
						      (__v16hi) __W,
						      (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_ph_epu8 (__mmask16 __U, __m256h __A)
{
  return
    (__m256i) __builtin_ia32_cvtph2iubs256_mask ((__v16hf) __A,
						 (__v16hi)
						 _mm256_setzero_si256 (),
						 (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_ps_epi8 (__m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvtps2ibs256_mask ((__v8sf) __A,
						(__v8si)
						_mm256_undefined_si256 (),
						(__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_ps_epi8 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvtps2ibs256_mask ((__v8sf) __A,
						     (__v8si) __W,
						     (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_ps_epi8 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvtps2ibs256_mask ((__v8sf) __A,
						(__v8si)
						_mm256_setzero_si256 (),
						(__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvts_ps_epu8 (__m256 __A)
{
  return (__m256i)
    __builtin_ia32_cvtps2iubs256_mask ((__v8sf) __A,
				       (__v8si)
				       _mm256_undefined_si256 (),
				       (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvts_ps_epu8 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvtps2iubs256_mask ((__v8sf) __A,
						      (__v8si) __W,
						      (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvts_ps_epu8 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvtps2iubs256_mask ((__v8sf) __A,
						 (__v8si)
						 _mm256_setzero_si256 (),
						 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_ph_epi8 (__m256h __A)
{
  return (__m256i)
    __builtin_ia32_cvttph2ibs256_mask ((__v16hf) __A,
				       (__v16hi)
				       _mm256_undefined_si256 (),
				       (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_ph_epi8 (__m256i __W, __mmask16 __U, __m256h __A)
{
  return (__m256i) __builtin_ia32_cvttph2ibs256_mask ((__v16hf) __A,
						      (__v16hi) __W,
						      (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_ph_epi8 (__mmask16 __U, __m256h __A)
{
  return
    (__m256i) __builtin_ia32_cvttph2ibs256_mask ((__v16hf) __A,
						 (__v16hi)
						 _mm256_setzero_si256 (),
						 (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_ph_epu8 (__m256h __A)
{
  return (__m256i)
    __builtin_ia32_cvttph2iubs256_mask ((__v16hf) __A,
					(__v16hi)
					_mm256_undefined_si256 (),
					(__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_ph_epu8 (__m256i __W, __mmask16 __U, __m256h __A)
{
  return (__m256i) __builtin_ia32_cvttph2iubs256_mask ((__v16hf) __A,
						       (__v16hi) __W,
						       (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_ph_epu8 (__mmask16 __U, __m256h __A)
{
  return
    (__m256i) __builtin_ia32_cvttph2iubs256_mask ((__v16hf) __A,
						  (__v16hi)
						  _mm256_setzero_si256 (),
						  (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_ps_epi8 (__m256 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2ibs256_mask ((__v8sf) __A,
				       (__v8si)
				       _mm256_undefined_si256 (),
				       (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_ps_epi8 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvttps2ibs256_mask ((__v8sf) __A,
						      (__v8si) __W,
						      (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_ps_epi8 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2ibs256_mask ((__v8sf) __A,
						 (__v8si)
						 _mm256_setzero_si256 (),
						 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtts_ps_epu8 (__m256 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2iubs256_mask ((__v8sf) __A,
					(__v8si)
					_mm256_undefined_si256 (),
					(__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtts_ps_epu8 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvttps2iubs256_mask ((__v8sf) __A,
						       (__v8si) __W,
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtts_ps_epu8 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2iubs256_mask ((__v8sf) __A,
						  (__v8si)
						  _mm256_setzero_si256 (),
						  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_pd_epi32 (__m256d __A)
{
  return
    (__m128i) __builtin_ia32_cvttpd2dqs256_mask ((__v4df) __A,
						 (__v4si)
						 _mm_undefined_si128 (),
						 (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_pd_epi32 (__m128i __W, __mmask8 __U, __m256d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs256_mask ((__v4df) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_pd_epi32 (__mmask8 __U, __m256d __A)
{
  return
    (__m128i) __builtin_ia32_cvttpd2dqs256_mask ((__v4df) __A,
						 (__v4si)
						 _mm_setzero_si128 (),
						 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_pd_epi64 (__m256d __A)
{
  return (__m256i)
    __builtin_ia32_cvttpd2qqs256_mask ((__v4df) __A,
				       (__v4di)
				       _mm256_undefined_si256 (),
				       (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_pd_epi64 (__m256i __W, __mmask8 __U, __m256d __A)
{
  return (__m256i) __builtin_ia32_cvttpd2qqs256_mask ((__v4df) __A,
						      (__v4di) __W,
						      (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_pd_epi64 (__mmask8 __U, __m256d __A)
{
  return
    (__m256i) __builtin_ia32_cvttpd2qqs256_mask ((__v4df) __A,
						 (__v4di)
						 _mm256_setzero_si256 (),
						 (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_pd_epu32 (__m256d __A)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udqs256_mask ((__v4df) __A,
						  (__v4si)
						  _mm_undefined_si128 (),
						  (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_pd_epu32 (__m128i __W, __mmask8 __U, __m256d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs256_mask ((__v4df) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_pd_epu32 (__mmask8 __U, __m256d __A)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udqs256_mask ((__v4df) __A,
						  (__v4si)
						  _mm_setzero_si128 (),
						  (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_pd_epu64 (__m256d __A)
{
  return (__m256i)
    __builtin_ia32_cvttpd2uqqs256_mask ((__v4df) __A,
					(__v4di)
					_mm256_undefined_si256 (),
					(__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_pd_epu64 (__m256i __W, __mmask8 __U, __m256d __A)
{
  return (__m256i) __builtin_ia32_cvttpd2uqqs256_mask ((__v4df) __A,
						       (__v4di) __W,
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_pd_epu64 (__mmask8 __U, __m256d __A)
{
  return
    (__m256i) __builtin_ia32_cvttpd2uqqs256_mask ((__v4df) __A,
						  (__v4di)
						  _mm256_setzero_si256 (),
						  (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_ps_epi32 (__m256 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2dqs256_mask ((__v8sf) __A,
				       (__v8si)
				       _mm256_undefined_si256 (),
				       (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_ps_epi32 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvttps2dqs256_mask ((__v8sf) __A,
						      (__v8si) __W,
						      (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_ps_epi32 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2dqs256_mask ((__v8sf) __A,
						 (__v8si)
						 _mm256_setzero_si256 (),
						 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_ps_epi64 (__m128 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2qqs256_mask ((__v4sf) __A,
				       (__v4di)
				       _mm256_undefined_si256 (),
				       (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_ps_epi64 (__m256i __W, __mmask8 __U, __m128 __A)
{
  return (__m256i) __builtin_ia32_cvttps2qqs256_mask ((__v4sf) __A,
						      (__v4di) __W,
						      (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_ps_epi64 (__mmask8 __U, __m128 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2qqs256_mask ((__v4sf) __A,
						 (__v4di)
						 _mm256_setzero_si256 (),
						 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_ps_epu32 (__m256 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2udqs256_mask ((__v8sf) __A,
					(__v8si)
					_mm256_undefined_si256 (),
					(__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_ps_epu32 (__m256i __W, __mmask8 __U, __m256 __A)
{
  return (__m256i) __builtin_ia32_cvttps2udqs256_mask ((__v8sf) __A,
						       (__v8si) __W,
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_ps_epu32 (__mmask8 __U, __m256 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2udqs256_mask ((__v8sf) __A,
						  (__v8si)
						  _mm256_setzero_si256 (),
						  (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_ps_epu64 (__m128 __A)
{
  return (__m256i)
    __builtin_ia32_cvttps2uqqs256_mask ((__v4sf) __A,
					(__v4di)
					_mm256_undefined_si256 (),
					(__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_ps_epu64 (__m256i __W, __mmask8 __U, __m128 __A)
{
  return (__m256i) __builtin_ia32_cvttps2uqqs256_mask ((__v4sf) __A,
						       (__v4di) __W,
						       (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_ps_epu64 (__mmask8 __U, __m128 __A)
{
  return
    (__m256i) __builtin_ia32_cvttps2uqqs256_mask ((__v4sf) __A,
						  (__v4di)
						  _mm256_setzero_si256 (),
						  (__mmask8) __U);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_sd_epi32 (__m128d __A)
{
  return (int) __builtin_ia32_cvttsd2sis32_round ((__v2df) __A,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_sd_epu32 (__m128d __A)
{
  return (unsigned int) __builtin_ia32_cvttsd2usis32_round ((__v2df) __A,
							    _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ss_epi32 (__m128 __A)
{
  return (int) __builtin_ia32_cvttss2sis32_round ((__v4sf) __A,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ss_epu32 (__m128 __A)
{
  return (unsigned int) __builtin_ia32_cvttss2usis32_round ((__v4sf) __A,
							    _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundsd_epi32 (__m128d __A, const int __R)
{
  return (int) __builtin_ia32_cvttsd2sis32_round ((__v2df) __A,
						  __R);
}

extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundsd_epu32 (__m128d __A, const int __R)
{
  return (unsigned int) __builtin_ia32_cvttsd2usis32_round ((__v2df) __A,
							    __R);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundss_epi32 (__m128 __A, const int __R)
{
  return (int) __builtin_ia32_cvttss2sis32_round ((__v4sf) __A,
						  __R);
}

extern __inline unsigned int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundss_epu32 (__m128 __A, const int __R)
{
  return (unsigned int) __builtin_ia32_cvttss2usis32_round ((__v4sf) __A,
							    __R);
}
#else
#define _mm_cvtts_roundsd_epi32(A, R) \
  ((int) __builtin_ia32_cvttsd2sis32_round ((__v2df) (A), \
					    (R)))

#define _mm_cvtts_roundsd_epu32(A, R) \
  ((unsigned int) __builtin_ia32_cvttsd2usis32_round ((__v2df) (A), \
						      (R)))

#define _mm_cvtts_roundss_epi32(A, R) \
  ((int) __builtin_ia32_cvttss2sis32_round ((__v4sf) (A), \
					    (R)))

#define _mm_cvtts_roundss_epu32(A, R) \
  ((unsigned int) __builtin_ia32_cvttss2usis32_round ((__v4sf) (A), \
						      (R)))
#endif

#ifdef __x86_64__
extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_sd_epi64 (__m128d __A)
{
  return (long long) __builtin_ia32_cvttsd2sis64_round ((__v2df) __A,
							_MM_FROUND_CUR_DIRECTION);
}

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_sd_epu64 (__m128d __A)
{
  return (unsigned long long) __builtin_ia32_cvttsd2usis64_round ((__v2df) __A,
								  _MM_FROUND_CUR_DIRECTION);
}

extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ss_epi64 (__m128 __A)
{
  return (long long) __builtin_ia32_cvttss2sis64_round ((__v4sf) __A,
							_MM_FROUND_CUR_DIRECTION);
}


extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_ss_epu64 (__m128 __A)
{
  return (unsigned long long) __builtin_ia32_cvttss2usis64_round ((__v4sf) __A,
								  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundsd_epi64 (__m128d __A, const int __R)
{
  return (long long) __builtin_ia32_cvttsd2sis64_round ((__v2df) __A,
							__R);
}

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundsd_epu64 (__m128d __A, const int __R)
{
  return (unsigned long long) __builtin_ia32_cvttsd2usis64_round ((__v2df) __A,
								  __R);
}

extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundss_epi64 (__m128 __A, const int __R)
{
  return (long long) __builtin_ia32_cvttss2sis64_round ((__v4sf) __A,
							__R);
}

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtts_roundss_epu64 (__m128 __A, const int __R)
{
  return (unsigned long long) __builtin_ia32_cvttss2usis64_round ((__v4sf) __A,
								  __R);
}
#else
#define _mm_cvtts_roundsd_epi64(A, R) \
  ((long long) __builtin_ia32_cvttsd2sis64_round ((__v2df) (A), \
						  (R)))

#define _mm_cvtts_roundsd_epu64(A, R) \
  ((unsigned long long) __builtin_ia32_cvttsd2usis64_round ((__v2df) (A), \
							    (R)))

#define _mm_cvtts_roundss_epi64(A, R) \
  ((long long) __builtin_ia32_cvttss2sis64_round ((__v4sf) (A), \
						  (R)))

#define _mm_cvtts_roundss_epu64(A, R) \
  ((unsigned long long) __builtin_ia32_cvttss2usis64_round ((__v4sf) (A), \
							    (R)))
#endif
#endif /* __x86_64__ */

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* _AVX10_2SATCVTINTRIN_H_INCLUDED */
