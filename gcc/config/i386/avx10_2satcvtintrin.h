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

#if !defined (__AVX10_2_256__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2_256__
#endif /* __AVX10_2_256__ */

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtnebf16_epi16 (__m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtnebf162ibs128_mask ((__v8bf) __A,
							 (__v8hi)
							 _mm_undefined_si128 (),
							 (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtnebf16_epi16 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtnebf162ibs128_mask ((__v8bf) __A,
							 (__v8hi) __W,
							 (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtnebf16_epi16 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtnebf162ibs128_mask ((__v8bf) __A,
							 (__v8hi)
							 _mm_setzero_si128 (),
							 (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtnebf16_epi16 (__m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtnebf162ibs256_mask ((__v16bf) __A,
						    (__v16hi)
						    _mm256_undefined_si256 (),
						    (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtnebf16_epi16 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvtnebf162ibs256_mask ((__v16bf) __A,
							 (__v16hi) __W,
							 (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtnebf16_epi16 (__mmask16 __U, __m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtnebf162ibs256_mask ((__v16bf) __A,
						    (__v16hi)
						    _mm256_setzero_si256 (),
						    (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtnebf16_epu16 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvtnebf162iubs128_mask ((__v8bf) __A,
						     (__v8hi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtnebf16_epu16 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvtnebf162iubs128_mask ((__v8bf) __A,
							  (__v8hi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtnebf16_epu16 (__mmask8 __U, __m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvtnebf162iubs128_mask ((__v8bf) __A,
						     (__v8hi)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtnebf16_epu16 (__m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtnebf162iubs256_mask ((__v16bf) __A,
						     (__v16hi)
						     _mm256_undefined_si256 (),
						     (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtnebf16_epu16 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvtnebf162iubs256_mask ((__v16bf) __A,
							  (__v16hi) __W,
							  (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtnebf16_epu16 (__mmask16 __U, __m256bh __A)
{
  return
    (__m256i) __builtin_ia32_cvtnebf162iubs256_mask ((__v16bf) __A,
						     (__v16hi)
						     _mm256_setzero_si256 (),
						     (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtph_epi16 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtph_epi16 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtph_epi16 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2ibs128_mask ((__v8hf) __A,
						     (__v8hi)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtph_epu16 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtph_epu16 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtph_epu16 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvtph2iubs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtps_epi32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtps_epi32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si) __W,
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtps_epi32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2ibs128_mask ((__v4sf) __A,
						     (__v4si)
						     _mm_setzero_si128 (),
						     (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvtps_epu32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvtps_epu32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvtps_epu32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvtps2iubs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttnebf16_epi16 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvttnebf162ibs128_mask ((__v8bf) __A,
						     (__v8hi)
						     _mm_undefined_si128 (),
						     (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttnebf16_epi16 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttnebf162ibs128_mask ((__v8bf) __A,
							  (__v8hi) __W,
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttnebf16_epi16 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttnebf162ibs128_mask ((__v8bf) __A,
							  (__v8hi)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttnebf16_epu16 (__m128bh __A)
{
  return
    (__m128i) __builtin_ia32_cvttnebf162iubs128_mask ((__v8bf) __A,
						      (__v8hi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttnebf16_epu16 (__m128i __W, __mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttnebf162iubs128_mask ((__v8bf) __A,
							   (__v8hi) __W,
							   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttnebf16_epu16 (__mmask8 __U, __m128bh __A)
{
  return (__m128i) __builtin_ia32_cvttnebf162iubs128_mask ((__v8bf) __A,
							   (__v8hi)
							   _mm_setzero_si128 (),
							   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvttnebf16_epi16 (__m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttnebf162ibs256_mask ((__v16bf) __A,
					   (__v16hi)
					   _mm256_undefined_si256 (),
					   (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvttnebf16_epi16 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvttnebf162ibs256_mask ((__v16bf) __A,
							  (__v16hi) __W,
							  (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvttnebf16_epi16 (__mmask16 __U, __m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttnebf162ibs256_mask ((__v16bf) __A,
					   (__v16hi)
					   _mm256_setzero_si256 (),
					   (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvttnebf16_epu16 (__m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttnebf162iubs256_mask ((__v16bf) __A,
					    (__v16hi)
					    _mm256_undefined_si256 (),
					    (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvttnebf16_epu16 (__m256i __W, __mmask16 __U, __m256bh __A)
{
  return (__m256i) __builtin_ia32_cvttnebf162iubs256_mask ((__v16bf) __A,
							   (__v16hi) __W,
							   (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvttnebf16_epu16 (__mmask16 __U, __m256bh __A)
{
  return (__m256i)
    __builtin_ia32_cvttnebf162iubs256_mask ((__v16bf) __A,
					    (__v16hi)
					    _mm256_setzero_si256 (),
					    (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttph_epi16 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttph_epi16 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttph_epi16 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2ibs128_mask ((__v8hf) __A,
						      (__v8hi)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttph_epu16 (__m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttph_epu16 (__m128i __W, __mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttph_epu16 (__mmask8 __U, __m128h __A)
{
  return (__m128i) __builtin_ia32_cvttph2iubs128_mask ((__v8hf) __A,
						       (__v8hi)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttps_epi32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttps_epi32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttps_epi32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2ibs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ipcvttps_epu32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_ipcvttps_epu32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_ipcvttps_epu32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2iubs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttspd_epi32 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttspd_epi32 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttspd_epi32 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs128_mask ((__v2df) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttspd_epi64 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttspd_epi64 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttspd_epi64 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2qqs128_mask ((__v2df) __A,
						      (__v2di)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttspd_epu32 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttspd_epu32 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttspd_epu32 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs128_mask ((__v2df) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttspd_epu64 (__m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttspd_epu64 (__m128i __W, __mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttspd_epu64 (__mmask8 __U, __m128d __A)
{
  return (__m128i) __builtin_ia32_cvttpd2uqqs128_mask ((__v2df) __A,
						       (__v2di)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttsps_epi32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttsps_epi32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttsps_epi32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2dqs128_mask ((__v4sf) __A,
						      (__v4si)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttsps_epi64 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di)
						      _mm_undefined_si128 (),
						      (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttsps_epi64 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di) __W,
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttsps_epi64 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2qqs128_mask ((__v4sf) __A,
						      (__v2di)
						      _mm_setzero_si128 (),
						      (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttsps_epu32 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttsps_epu32 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttsps_epu32 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2udqs128_mask ((__v4sf) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvttsps_epu64 (__m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di)
						       _mm_undefined_si128 (),
						       (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvttsps_epu64 (__m128i __W, __mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di) __W,
						       (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvttsps_epu64 (__mmask8 __U, __m128 __A)
{
  return (__m128i) __builtin_ia32_cvttps2uqqs128_mask ((__v4sf) __A,
						       (__v2di)
						       _mm_setzero_si128 (),
						       (__mmask8) __U);
}

#ifdef __OPTIMIZE__
extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvt_roundph_epi16 (__m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_undefined_si256 (),
						      (__mmask16) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvt_roundph_epi16 (__m256i __W, __mmask16 __U, __m256h __A,
			     const int __R)
{
  return (__m256i) __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) __A,
							   (__v16hi) __W,
							   (__mmask16) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvt_roundph_epi16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) __A,
						      (__v16hi)
						      _mm256_setzero_si256 (),
						      (__mmask16) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvt_roundph_epu16 (__m256h __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) __A,
					     (__v16hi)
					     _mm256_undefined_si256 (),
					     (__mmask16) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvt_roundph_epu16 (__m256i __W, __mmask16 __U, __m256h __A,
			      const int __R)
{
  return (__m256i) __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) __A,
							    (__v16hi) __W,
							    (__mmask16) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvt_roundph_epu16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) __A,
						       (__v16hi)
						       _mm256_setzero_si256 (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvt_roundps_epi32 (__m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_undefined_si256 (),
						      (__mmask8) -1,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvt_roundps_epi32 (__m256i __W, __mmask8 __U, __m256 __A,
			     const int __R)
{
  return (__m256i) __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) __A,
							   (__v8si) __W,
							   (__mmask8) __U,
							   __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvt_roundps_epi32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) __A,
						      (__v8si)
						      _mm256_setzero_si256 (),
						      (__mmask8) __U,
						      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvt_roundps_epu32 (__m256 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) __A,
					     (__v8si)
					     _mm256_undefined_si256 (),
					     (__mmask8) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvt_roundps_epu32 (__m256i __W, __mmask8 __U, __m256 __A,
			      const int __R)
{
  return (__m256i) __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvt_roundps_epu32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtt_roundph_epi16 (__m256h __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) __A,
					     (__v16hi)
					     _mm256_undefined_si256 (),
					     (__mmask16) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtt_roundph_epi16 (__m256i __W, __mmask16 __U, __m256h __A,
			      const int __R)
{
  return (__m256i) __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) __A,
							    (__v16hi) __W,
							    (__mmask16) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtt_roundph_epi16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) __A,
						       (__v16hi)
						       _mm256_setzero_si256 (),
						       (__mmask16) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtt_roundph_epu16 (__m256h __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) __A,
					      (__v16hi)
					      _mm256_undefined_si256 (),
					      (__mmask16) -1,
					      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtt_roundph_epu16 (__m256i __W, __mmask16 __U, __m256h __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) __A,
							     (__v16hi) __W,
							     (__mmask16) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtt_roundph_epu16 (__mmask16 __U, __m256h __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) __A,
							(__v16hi)
							_mm256_setzero_si256 (),
							(__mmask16) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtt_roundps_epi32 (__m256 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) __A,
					     (__v8si)
					     _mm256_undefined_si256 (),
					     (__mmask8) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtt_roundps_epi32 (__m256i __W, __mmask8 __U, __m256 __A,
			      const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtt_roundps_epi32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_ipcvtt_roundps_epu32 (__m256 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) __A,
					      (__v8si)
					      _mm256_undefined_si256 (),
					      (__mmask8) -1,
					      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_ipcvtt_roundps_epu32 (__m256i __W, __mmask8 __U, __m256 __A,
			       const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) __A,
							     (__v8si) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_ipcvtt_roundps_epu32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) __A,
							(__v8si)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundpd_epi32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) __A,
						       (__v4si)
						       _mm_undefined_si128 (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundpd_epi32 (__m128i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) __A,
							    (__v4si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundpd_epi32 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) __A,
						       (__v4si)
						       _mm_setzero_si128 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundpd_epi64 (__m256d __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) __A,
					     (__v4di)
					     _mm256_undefined_si256 (),
					     (__mmask8) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundpd_epi64 (__m256i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundpd_epi64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundpd_epu32 (__m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) __A,
							(__v4si)
							_mm_undefined_si128 (),
							(__mmask8) -1,
							__R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundpd_epu32 (__m128i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m128i) __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) __A,
							     (__v4si) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundpd_epu32 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m128i) __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) __A,
							(__v4si)
							_mm_setzero_si128 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundpd_epu64 (__m256d __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) __A,
					      (__v4di)
					      _mm256_undefined_si256 (),
					      (__mmask8) -1,
					      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundpd_epu64 (__m256i __W, __mmask8 __U, __m256d __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) __A,
							     (__v4di) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundpd_epu64 (__mmask8 __U, __m256d __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) __A,
							(__v4di)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundps_epi32 (__m256 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) __A,
					     (__v8si)
					     _mm256_undefined_si256 (),
					     (__mmask8) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundps_epi32 (__m256i __W, __mmask8 __U, __m256 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) __A,
							    (__v8si) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundps_epi32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) __A,
						       (__v8si)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundps_epi64 (__m128 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) __A,
					     (__v4di)
					     _mm256_undefined_si256 (),
					     (__mmask8) -1,
					     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundps_epi64 (__m256i __W, __mmask8 __U, __m128 __A,
				    const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) __A,
							    (__v4di) __W,
							    (__mmask8) __U,
							    __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundps_epi64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) __A,
						       (__v4di)
						       _mm256_setzero_si256 (),
						       (__mmask8) __U,
						       __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundps_epu32 (__m256 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) __A,
					      (__v8si)
					      _mm256_undefined_si256 (),
					      (__mmask8) -1,
					      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundps_epu32 (__m256i __W, __mmask8 __U, __m256 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) __A,
							     (__v8si) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundps_epu32 (__mmask8 __U, __m256 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) __A,
							(__v8si)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtts_roundps_epu64 (__m128 __A, const int __R)
{
  return (__m256i)
    __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) __A,
					      (__v4di)
					      _mm256_undefined_si256 (),
					      (__mmask8) -1,
					      __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtts_roundps_epu64 (__m256i __W, __mmask8 __U, __m128 __A,
				const int __R)
{
  return (__m256i) __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) __A,
							     (__v4di) __W,
							     (__mmask8) __U,
							     __R);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtts_roundps_epu64 (__mmask8 __U, __m128 __A, const int __R)
{
  return
    (__m256i) __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) __A,
							(__v4di)
							_mm256_setzero_si256 (),
							(__mmask8) __U,
							__R);
}

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

#define _mm256_ipcvt_roundph_epi16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) (A), \
					   (__v16hi) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask16) (-1), \
					   (R)))

#define _mm256_mask_ipcvt_roundph_epi16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) (A), \
						     (__v16hi) (W), \
						     (__mmask16) (U), \
						     (R)))

#define _mm256_maskz_ipcvt_roundph_epi16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtph2ibs256_mask_round ((__v16hf) (A), \
					   (__v16hi) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask16) (U), \
					   (R)))

#define _mm256_ipcvt_roundph_epu16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask16) (-1), \
					    (R)))

#define _mm256_mask_ipcvt_roundph_epu16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) (A), \
						      (__v16hi) (W), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_maskz_ipcvt_roundph_epu16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtph2iubs256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (U), \
					    (R)))

#define _mm256_ipcvt_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_undefined_si256 ()), \
					   (__mmask8) (-1), \
					   (R)))

#define _mm256_mask_ipcvt_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) (A), \
						     (__v8si) (W), \
						     (__mmask8) (U), \
						     (R)))

#define _mm256_maskz_ipcvt_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2ibs256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define _mm256_ipcvt_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_ipcvt_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_ipcvt_roundps_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvtps2iubs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))


#define _mm256_ipcvttne_roundbf16_epi16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttnebf162ibs256_mask_round ((__v16bf) (A), \
						(__v16hi) \
						(_mm256_undefined_si256 ()), \
						(__mmask16) (-1), \
						(R)))

#define _mm256_mask_ipcvttne_roundbf16_epi16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttnebf162ibs256_mask_round ((__v16bf) (A), \
							  (__v16hi) (W), \
							  (__mmask16) (U), \
							  (R)))

#define _mm256_maskz_ipcvttne_roundbf16_epi16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttnebf162ibs256_mask_round ((__v16bf) (A), \
						(__v16hi) \
						(_mm256_setzero_si256 ()), \
						(__mmask16) (U), \
						(R)))

#define _mm256_ipcvttne_roundbf16_epu16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttnebf162iubs256_mask_round ((__v16bf) (A), \
						 (__v16hi) \
						 (_mm256_undefined_si256 ()), \
						 (__mmask16) (-1), \
						 (R)))

#define _mm256_mask_ipcvttne_roundbf16_epu16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttnebf162iubs256_mask_round ((__v16bf) (A), \
							   (__v16hi) (W), \
							   (__mmask16) (U), \
							   (R)))

#define _mm256_maskz_ipcvttne_roundbf16_epu16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttnebf162iubs256_mask_round ((__v16bf) (A), \
						 (__v16hi) \
						 (_mm256_setzero_si256 ()), \
						 (__mmask16) (U), \
						 (R)))

#define _mm256_ipcvtt_roundph_epi16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask16) (-1), \
					    (R)))

#define _mm256_mask_ipcvtt_roundph_epi16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) (A), \
						      (__v16hi) (W), \
						      (__mmask16) (U), \
						      (R)))

#define _mm256_maskz_ipcvtt_roundph_epi16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttph2ibs256_mask_round ((__v16hf) (A), \
					    (__v16hi) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask16) (U), \
					    (R)))

#define _mm256_ipcvtt_roundph_epu16(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) (A), \
					     (__v16hi) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask16) (-1), \
					     (R)))

#define _mm256_mask_ipcvtt_roundph_epu16(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) (A), \
						       (__v16hi) (W), \
						       (__mmask16) (U), \
						       (R)))

#define _mm256_maskz_ipcvtt_roundph_epu16(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttph2iubs256_mask_round ((__v16hf) (A), \
					     (__v16hi) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask16) (U), \
					     (R)))

#define _mm256_ipcvtt_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_ipcvtt_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_ipcvtt_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2ibs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_ipcvtt_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) (A), \
					     (__v8si) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_ipcvtt_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) (A), \
						       (__v8si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_ipcvtt_roundps_epu32(U, A, R) \
((__m256i) \
 __builtin_ia32_cvttps2iubs256_mask_round ((__v8sf) (A), \
					   (__v8si) \
					   (_mm256_setzero_si256 ()), \
					   (__mmask8) (U), \
					   (R)))

#define _mm256_cvtts_roundpd_epi32(A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
					    (__v4si) \
					    (_mm_undefined_si128 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundpd_epi32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
						      (__v4si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundpd_epi32(U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
						      (__v4si) \
						      (_mm_setzero_si128 ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvtts_roundpd_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundpd_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundpd_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundpd_epu32(A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
					     (__v4si) \
					     (_mm_undefined_si128 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundpd_epu32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
						       (__v4si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundpd_epu32(U, A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
					     (__v4si) (_mm_setzero_si128 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundpd_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
					     (__v4di) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundpd_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
						       (__v4di) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundpd_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundps_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundps_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundps_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
					     (__v8si) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
						       (__v8si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundps_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
					     (__v8si) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundps_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
					     (__v4di) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundps_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
						       (__v4di) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundps_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

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
#define _mm256_cvtts_roundpd_epi32(A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
					    (__v4si) \
					    (_mm_undefined_si128 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundpd_epi32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
						      (__v4si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundpd_epi32(U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2dqs256_mask_round ((__v4df) (A), \
						      (__v4si) \
						      (_mm_setzero_si128 ()), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_cvtts_roundpd_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundpd_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundpd_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2qqs256_mask_round ((__v4df) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundpd_epu32(A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
					     (__v4si) \
					     (_mm_undefined_si128 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundpd_epu32(W, U, A, R) \
  ((__m128i) __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
						       (__v4si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundpd_epu32(U, A, R) \
  ((__m128i) \
   __builtin_ia32_cvttpd2udqs256_mask_round ((__v4df) (A), \
					     (__v4si) (_mm_setzero_si128 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundpd_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
					     (__v4di) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundpd_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
						       (__v4di) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundpd_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttpd2uqqs256_mask_round ((__v4df) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundps_epi32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundps_epi32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
						      (__v8si) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundps_epi32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2dqs256_mask_round ((__v8sf) (A), \
					    (__v8si) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundps_epi64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_undefined_si256 ()), \
					    (__mmask8) (-1), \
					    (R)))

#define _mm256_mask_cvtts_roundps_epi64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
						      (__v4di) (W), \
						      (__mmask8) (U), \
						      (R)))

#define _mm256_maskz_cvtts_roundps_epi64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2qqs256_mask_round ((__v4sf) (A), \
					    (__v4di) \
					    (_mm256_setzero_si256 ()), \
					    (__mmask8) (U), \
					    (R)))

#define _mm256_cvtts_roundps_epu32(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
					     (__v8si) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundps_epu32(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
						       (__v8si) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundps_epu32(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2udqs256_mask_round ((__v8sf) (A), \
					     (__v8si) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

#define _mm256_cvtts_roundps_epu64(A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
					     (__v4di) \
					     (_mm256_undefined_si256 ()), \
					     (__mmask8) (-1), \
					     (R)))

#define _mm256_mask_cvtts_roundps_epu64(W, U, A, R) \
  ((__m256i) __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
						       (__v4di) (W), \
						       (__mmask8) (U), \
						       (R)))

#define _mm256_maskz_cvtts_roundps_epu64(U, A, R) \
  ((__m256i) \
   __builtin_ia32_cvttps2uqqs256_mask_round ((__v4sf) (A), \
					     (__v4di) \
					     (_mm256_setzero_si256 ()), \
					     (__mmask8) (U), \
					     (R)))

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

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* _AVX10_2SATCVTINTRIN_H_INCLUDED */
