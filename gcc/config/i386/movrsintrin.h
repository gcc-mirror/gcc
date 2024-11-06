/* Copyright (C) 2024 Free Software Foundation, Inc.

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
# error "Never use <movrsintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _MOVRSINTRIN_H_INCLUDED
#define _MOVRSINTRIN_H_INCLUDED

#ifndef __MOVRS__
#pragma GCC push_options
#pragma GCC target("movrs")
#define __DISABLE_MOVRS__
#endif /* __MOVRS__ */

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_m_prefetchrs (void* __P)
{
  __builtin_ia32_prefetch (__P, 2, 1, 0 /* _MM_HINT_RST2 */);
}

#ifdef __x86_64__

extern __inline char
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_movrs_i8 (void const * __P)
{
  return (char) __builtin_ia32_movrsqi ((const char *) __P);
}

extern __inline short
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_movrs_i16 (void const * __P)
{
  return (short) __builtin_ia32_movrshi ((const short *) __P);
}

extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_movrs_i32 (void const  * __P)
{
  return (int) __builtin_ia32_movrssi ((const int *) __P);
}

extern __inline long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_movrs_i64 (void const * __P)
{
  return (long long) __builtin_ia32_movrsdi ((const long long *) __P);
}

#endif /* __x86_64__ */

#ifdef __DISABLE_MOVRS__
#undef __DISABLE_MOVRS__
#pragma GCC pop_options
#endif /* __DISABLE_MOVRS__ */

#ifdef __x86_64__

#if !defined (__AVX10_2_256__) || !defined (__MOVRS__)
#pragma GCC push_options
#pragma GCC target("avx10.2,movrs")
#define __DISABLE_MOVRS_AVX10_2__
#endif /* __MOVRS_AVX10_2__ */

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_loadrs_epi8 (void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsb256_mask ((const __v32qi *) __A,
						   (__v32qi)
						   _mm256_setzero_si256 (),
						   (__mmask32) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_loadrs_epi8 (__m256i __D, __mmask32 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsb256_mask ((const __v32qi *) __A,
						   (__v32qi) __D,
						   (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_loadrs_epi8 (__mmask32 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsb256_mask ((const __v32qi *) __A,
						   (__v32qi)
						   _mm256_setzero_si256 (),
						   (__mmask32) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_loadrs_epi32 (void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsd256_mask ((const __v8si *) __A,
						   (__v8si)
						   _mm256_setzero_si256 (),
						   (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_loadrs_epi32 (__m256i __D, __mmask8 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsd256_mask ((const __v8si *) __A,
						   (__v8si) __D,
						   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_loadrs_epi32 (__mmask8 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsd256_mask ((const __v8si *) __A,
						   (__v8si)
						   _mm256_setzero_si256 (),
						   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_loadrs_epi64 (void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsq256_mask ((const __v4di *) __A,
						   (__v4di)
						   _mm256_setzero_si256 (),
						   (__mmask8) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_loadrs_epi64 (__m256i __D, __mmask8 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsq256_mask ((const __v4di *) __A,
						   (__v4di) __D,
						   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_loadrs_epi64 (__mmask8 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsq256_mask ((const __v4di *) __A,
						   (__v4di)
						   _mm256_setzero_si256 (),
						   (__mmask8) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_loadrs_epi16 (void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsw256_mask ((const __v16hi *) __A,
						   (__v16hi)
						   _mm256_setzero_si256 (),
						   (__mmask16) -1);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_loadrs_epi16 (__m256i __D, __mmask16 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsw256_mask ((const __v16hi *) __A,
						   (__v16hi) __D,
						   (__mmask16) __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_loadrs_epi16 (__mmask16 __U, void const *__A)
{
  return (__m256i) __builtin_ia32_vmovrsw256_mask ((const __v16hi *) __A,
						   (__v16hi)
						   _mm256_setzero_si256 (),
						   (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_loadrs_epi8 (void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsb128_mask ((const __v16qi *) __A,
						   (__v16qi)
						   _mm_setzero_si128 (),
						   (__mmask16) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_loadrs_epi8 (__m128i __D, __mmask16 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsb128_mask ((const __v16qi *) __A,
						   (__v16qi) __D,
						   (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_loadrs_epi8 (__mmask16 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsb128_mask ((const __v16qi *) __A,
						   (__v16qi)
						   _mm_setzero_si128 (),
						   (__mmask16) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_loadrs_epi32 (void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsd128_mask ((const __v4si *) __A,
						   (__v4si)
						   _mm_setzero_si128 (),
						   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_loadrs_epi32 (__m128i __D, __mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsd128_mask ((const __v4si *) __A,
						   (__v4si) __D,
						   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_loadrs_epi32 (__mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsd128_mask ((const __v4si *) __A,
						   (__v4si)
						   _mm_setzero_si128 (),
						   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_loadrs_epi64 (void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsq128_mask ((const __v2di *) __A,
						   (__v2di)
						   _mm_setzero_si128 (),
						   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_loadrs_epi64 (__m128i __D, __mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsq128_mask ((const __v2di *) __A,
						   (__v2di) __D,
						   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_loadrs_epi64 (__mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsq128_mask ((const __v2di *) __A,
						   (__v2di)
						   _mm_setzero_si128 (),
						   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_loadrs_epi16 (void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsw128_mask ((const __v8hi *) __A,
						   (__v8hi)
						   _mm_setzero_si128 (),
						   (__mmask8) -1);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_loadrs_epi16 (__m128i __D, __mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsw128_mask ((const __v8hi *) __A,
						   (__v8hi) __D,
						   (__mmask8) __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_loadrs_epi16 (__mmask8 __U, void const *__A)
{
  return (__m128i) __builtin_ia32_vmovrsw128_mask ((const __v8hi *) __A,
						   (__v8hi)
						   _mm_setzero_si128 (),
						   (__mmask8) __U);
}

#ifdef __DISABLE_MOVRS_AVX10_2__
#undef __DISABLE_MOVRS_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_MOVRS_AVX10_2__ */

#if !defined (__AVX10_2_512__) || !defined (__MOVRS__)
#pragma GCC push_options
#pragma GCC target("avx10.2-512,movrs")
#define __DISABLE_MOVRS_AVX10_2_512__
#endif /* __MOVRS_AVX10_2_512__ */

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_loadrs_epi8 (void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsb512_mask ((const __v64qi *) __A,
						   (__v64qi)
						   _mm512_setzero_si512 (),
						   (__mmask64) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_loadrs_epi8 (__m512i __D, __mmask64 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsb512_mask ((const __v64qi *) __A,
						   (__v64qi) __D,
						   (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_loadrs_epi8 (__mmask64 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsb512_mask ((const __v64qi *) __A,
						   (__v64qi)
						   _mm512_setzero_si512 (),
						   (__mmask64) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_loadrs_epi32 (void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsd512_mask ((const __v16si *) __A,
						   (__v16si) _mm512_setzero_si512 (),
						   (__mmask16) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_loadrs_epi32 (__m512i __D, __mmask16 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsd512_mask ((const __v16si *) __A,
						   (__v16si) __D,
						   (__mmask16) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_loadrs_epi32 (__mmask16 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsd512_mask ((const __v16si *) __A,
						   (__v16si)
						   _mm512_setzero_si512 (),
						   (__mmask16) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_loadrs_epi64 (void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsq512_mask ((const __v8di *) __A,
						   (__v8di)
						   _mm512_setzero_si512 (),
						   (__mmask8) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_loadrs_epi64 (__m512i __D, __mmask8 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsq512_mask ((const __v8di *) __A,
						   (__v8di) __D,
						   (__mmask8) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_loadrs_epi64 (__mmask8 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsq512_mask ((const __v8di *) __A,
						   (__v8di)
						   _mm512_setzero_si512 (),
						   (__mmask8) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_loadrs_epi16 (void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsw512_mask ((const __v32hi *) __A,
						   (__v32hi)
						   _mm512_setzero_si512 (),
						   (__mmask32) -1);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_loadrs_epi16 (__m512i __D, __mmask32 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsw512_mask ((const __v32hi *) __A,
						   (__v32hi) __D,
						   (__mmask32) __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_loadrs_epi16 (__mmask32 __U, void const *__A)
{
  return (__m512i) __builtin_ia32_vmovrsw512_mask ((const __v32hi *) __A,
						   (__v32hi)
						   _mm512_setzero_si512 (),
						   (__mmask32) __U);
}

#ifdef __DISABLE_MOVRS_AVX10_2_512__
#undef __DISABLE_MOVRS_AVX10_2_512__
#pragma GCC pop_options
#endif /* __DISABLE_MOVRS_AVX10_2_512__ */

#endif /* __x86_64__ */

#endif /* _MOVRSINTRIN_H_INCLUDED */
