/* Copyright (C) 2024-2026 Free Software Foundation, Inc.

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
#error "Never use <avx10_2mediaintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2MEDIAINTRIN_H_INCLUDED
#define _AVX10_2MEDIAINTRIN_H_INCLUDED

#if !defined(__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpph_ps (__m512 __W, __m512h __A, __m512h __B)
{
  return (__m512)
    __builtin_ia32_vdpphps512_mask ((__v16sf) __W,
				    (__v16sf) __A,
				    (__v16sf) __B,
				    (__mmask16) -1);
}

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpph_ps (__m512 __W, __mmask16 __U, __m512h __A,
		     __m512h __B)
{
  return (__m512)
    __builtin_ia32_vdpphps512_mask ((__v16sf) __W,
				    (__v16sf) __A,
				    (__v16sf) __B,
				    (__mmask16) __U);
}

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpph_ps (__mmask16 __U, __m512 __W, __m512h __A,
		      __m512h __B)
{
  return (__m512)
    __builtin_ia32_vdpphps512_maskz ((__v16sf) __W,
				     (__v16sf) __A,
				     (__v16sf) __B,
				     (__mmask16) __U);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpph_ps (__m256 __W, __m256h __A, __m256h __B)
{
  return (__m256)
    __builtin_ia32_vdpphps256_mask ((__v8sf) __W,
				    (__v8sf) __A,
				    (__v8sf) __B,
				    (__mmask8) -1);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpph_ps (__m256 __W, __mmask8 __U, __m256h __A,
		     __m256h __B)
{
  return (__m256)
    __builtin_ia32_vdpphps256_mask ((__v8sf) __W,
				    (__v8sf) __A,
				    (__v8sf) __B,
				    (__mmask8) __U);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpph_ps (__mmask8 __U, __m256 __W, __m256h __A,
		      __m256h __B)
{
  return (__m256)
    __builtin_ia32_vdpphps256_maskz ((__v8sf) __W,
				     (__v8sf) __A,
				     (__v8sf) __B,
				     (__mmask8) __U);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpph_ps (__m128 __W, __m128h __A, __m128h __B)
{
  return (__m128)
    __builtin_ia32_vdpphps128_mask ((__v4sf) __W,
				    (__v4sf) __A,
				    (__v4sf) __B,
				    (__mmask8) -1);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpph_ps (__m128 __W, __mmask8 __U, __m128h __A,
		  __m128h __B)
{
  return (__m128)
    __builtin_ia32_vdpphps128_mask ((__v4sf) __W,
				    (__v4sf) __A,
				    (__v4sf) __B,
				    (__mmask8) __U);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpph_ps (__mmask8 __U, __m128 __W, __m128h __A,
		   __m128h __B)
{
  return (__m128)
    __builtin_ia32_vdpphps128_maskz ((__v4sf) __W,
				     (__v4sf) __A,
				     (__v4sf) __B,
				     (__mmask8) __U);
}

#ifdef __OPTIMIZE__
extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mpsadbw_epu8 (__m128i __W, __mmask8 __U, __m128i __X,
		       __m128i __Y, const int __M)
{
  return (__m128i) __builtin_ia32_mpsadbw128_mask ((__v16qi) __X,
						   (__v16qi) __Y,
						   __M,
						   (__v8hi) __W,
						   __U);
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mpsadbw_epu8 (__mmask8 __U, __m128i __X,
			__m128i __Y, const int __M)
{
  return (__m128i) __builtin_ia32_mpsadbw128_mask ((__v16qi) __X,
						   (__v16qi) __Y,
						   __M,
						   (__v8hi) _mm_setzero_si128 (),
						   __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mpsadbw_epu8 (__m256i __W, __mmask16 __U, __m256i __X,
			  __m256i __Y, const int __M)
{
  return (__m256i) __builtin_ia32_mpsadbw256_mask ((__v32qi) __X,
						   (__v32qi) __Y,
						   __M,
						   (__v16hi) __W,
						   __U);
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mpsadbw_epu8 (__mmask16 __U, __m256i __X,
			   __m256i __Y, const int __M)
{
  return (__m256i) __builtin_ia32_mpsadbw256_mask ((__v32qi) __X,
						   (__v32qi) __Y,
						   __M,
						   (__v16hi) _mm256_setzero_si256 (),
						   __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mpsadbw_epu8 (__m512i __X, __m512i __Y, const int __M)
{
  return (__m512i) __builtin_ia32_mpsadbw512 ((__v64qi) __X,
					      (__v64qi) __Y,
					      __M);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mpsadbw_epu8 (__m512i __W, __mmask32 __U, __m512i __X,
			  __m512i __Y, const int __M)
{
  return (__m512i) __builtin_ia32_mpsadbw512_mask ((__v64qi) __X,
						   (__v64qi) __Y,
						   __M,
						   (__v32hi) __W,
						   __U);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mpsadbw_epu8 (__mmask32 __U, __m512i __X,
			   __m512i __Y, const int __M)
{
  return (__m512i) __builtin_ia32_mpsadbw512_mask ((__v64qi) __X,
						   (__v64qi) __Y,
						   __M,
						   (__v32hi) _mm512_setzero_epi32 (),
						   __U);
}
#else
#define _mm_mask_mpsadbw_epu8(W, U, X, Y, M)				\
  (__m128i) __builtin_ia32_mpsadbw128_mask ((__v16qi)(__m128i)(X),	\
					    (__v16qi)(__m128i)(Y),	\
					    (int)(M),			\
					    (__v8hi)(__m128i)(W),	\
					    (__mmask8)(U))

#define _mm_maskz_mpsadbw_epu8(U, X, Y, M)				\
  (__m128i) __builtin_ia32_mpsadbw128_mask ((__v16qi)(__m128i)(X),	\
					    (__v16qi)(__m128i)(Y),	\
					    (int)(M),			\
					    (__v8hi) _mm_setzero_si128 (),  \
					    (__mmask8)(U))

#define _mm256_mask_mpsadbw_epu8(W, U, X, Y, M)				\
  (__m256i) __builtin_ia32_mpsadbw256_mask ((__v32qi)(__m256i)(X),	\
					    (__v32qi)(__m256i)(Y),	\
					    (int)(M),			\
					    (__v16hi)(__m256i)(W),	\
					    (__mmask16)(U))

#define _mm256_maskz_mpsadbw_epu8(U, X, Y, M)				\
  (__m256i) __builtin_ia32_mpsadbw256_mask ((__v32qi)(__m256i)(X),	\
					    (__v32qi)(__m256i)(Y),	\
					    (int)(M),			\
					    (__v16hi) _mm256_setzero_si256 (),	\
					    (__mmask16)(U))

#define _mm512_mpsadbw_epu8(X, Y, M)					\
  (__m512i) __builtin_ia32_mpsadbw512 ((__v64qi)(__m512i)(X),		\
				       (__v64qi)(__m512i)(Y), (int)(M))

#define _mm512_mask_mpsadbw_epu8(W, U, X, Y, M)				\
  (__m512i) __builtin_ia32_mpsadbw512_mask ((__v64qi)(__m512i)(X),	\
					    (__v64qi)(__m512i)(Y),	\
					    (int)(M),			\
					    (__v32hi)(__m512i)(W),	\
					    (__mmask32)(U))

#define _mm512_maskz_mpsadbw_epu8(U, X, Y, M)				\
  (__m512i) __builtin_ia32_mpsadbw512_mask ((__v64qi)(__m512i)(X),	\
					    (__v64qi)(__m512i)(Y),	\
					    (int)(M),			\
					    (__v32hi) _mm512_setzero_epi32 (),	\
					    (__mmask32)(U))
#endif

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* __AVX10_2MEDIAINTRIN_H_INCLUDED */
