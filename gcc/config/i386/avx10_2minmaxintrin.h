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
#error "Never use <avx10_2minmaxintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2MINMAXINTRIN_H_INCLUDED
#define _AVX10_2MINMAXINTRIN_H_INCLUDED

#if !defined(__AVX10_2_256__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2_256__
#endif /* __AVX10_2_256__ */

#ifdef __OPTIMIZE__
extern __inline __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_nepbh (__m128bh __A, __m128bh __B, const int __C)
{
  return (__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) __A,
							  (__v8bf) __B,
							  __C,
							  (__v8bf)(__m128bh)
							  _mm_setzero_si128 (),
							  (__mmask8) -1);
}

extern __inline __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_nepbh (__m128bh __W, __mmask8 __U, __m128bh __A,
		       __m128bh __B, const int __C)
{
  return (__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) __A,
							  (__v8bf) __B,
							  __C,
							  (__v8bf) __W,
							  (__mmask8) __U);
}

extern __inline __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_nepbh (__mmask8 __U, __m128bh __A, __m128bh __B, const int __C)
{
  return (__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) __A,
							  (__v8bf) __B,
							  __C,
							  (__v8bf)(__m128bh)
							  _mm_setzero_si128 (),
							  (__mmask8) __U);
}

extern __inline __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_nepbh (__m256bh __A, __m256bh __B, const int __C)
{
  return (__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) __A,
							  (__v16bf) __B,
							  __C,
							  (__v16bf)(__m256bh)
							  _mm256_setzero_si256 (),
							  (__mmask16) -1);
}

extern __inline __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_nepbh (__m256bh __W, __mmask16 __U, __m256bh __A, __m256bh __B,
			    const int __C)
{
  return (__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) __A,
							  (__v16bf) __B,
							  __C,
							  (__v16bf) __W,
							  (__mmask16) __U);
}

extern __inline __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_nepbh (__mmask16 __U, __m256bh __A, __m256bh __B, const int __C)
{
  return (__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) __A,
							  (__v16bf) __B,
							  __C,
							  (__v16bf)(__m256bh)
							  _mm256_setzero_si256 (),
							  (__mmask16) __U);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_pd (__m128d __A, __m128d __B, const int __C)
{
  return (__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) __A,
						    (__v2df) __B,
						    __C,
						    (__v2df)(__m128d)
						    _mm_undefined_pd (),
						    (__mmask8) -1);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_pd (__m128d __W, __mmask8 __U, __m128d __A, __m128d __B,
		    const int __C)
{
  return (__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) __A,
						    (__v2df) __B,
						    __C,
						    (__v2df) __W,
						    (__mmask8) __U);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_pd (__mmask8 __U, __m128d __A, __m128d __B, const int __C)
{
  return (__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) __A,
						    (__v2df) __B,
						    __C,
						    (__v2df)(__m128d)
						    _mm_setzero_pd (),
						    (__mmask8) __U);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_pd (__m256d __A, __m256d __B, const int __C)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C,
		   (__v4df) (__m256d) _mm256_undefined_pd (),
		   (__mmask8) -1, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_pd (__m256d __W, __mmask8 __U, __m256d __A, __m256d __B,
		       const int __C)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C, (__v4df) __W,
		   (__mmask8) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_pd (__mmask8 __U, __m256d __A, __m256d __B, const int __C)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C,
		   (__v4df) (__m256d) _mm256_setzero_pd (),
		   (__mmask8) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_round_pd (__m256d __A, __m256d __B, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C,
		   (__v4df) (__m256d) _mm256_undefined_pd (),
		   (__mmask8) -1, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_round_pd (__m256d __W, __mmask8 __U, __m256d __A,
			     __m256d __B, const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C, (__v4df) __W,
		   (__mmask8) __U, __R);
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_round_pd (__mmask8 __U, __m256d __A, __m256d __B,
			      const int __C, const int __R)
{
  return (__m256d) __builtin_ia32_minmaxpd256_mask_round (
		   (__v4df) __A, (__v4df) __B, __C,
		   (__v4df) (__m256d) _mm256_setzero_pd (),
		   (__mmask8) __U, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_ph (__m128h __A, __m128h __B, const int __C)
{
  return (__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) __A,
						    (__v8hf) __B,
						    __C,
						    (__v8hf)(__m128h)
						    _mm_undefined_ph (),
						    (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_ph (__m128h __W, __mmask8 __U, __m128h __A, __m128h __B,
		    const int __C)
{
  return (__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) __A,
						    (__v8hf) __B,
						    __C,
						    (__v8hf) __W,
						    (__mmask8) __U);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_ph (__mmask8 __U, __m128h __A, __m128h __B, const int __C)
{
  return (__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) __A,
						    (__v8hf) __B,
						    __C,
						    (__v8hf)(__m128h)
						    _mm_setzero_ph (),
						    (__mmask8) __U);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_ph (__m256h __A, __m256h __B, const int __C)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C,
		  (__v16hf) (__m256h) _mm256_undefined_ph (),
		  (__mmask16) -1, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_ph (__m256h __W, __mmask16 __U, __m256h __A, __m256h __B,
		       const int __C)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C, (__v16hf) __W,
		  (__mmask16) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_ph (__mmask16 __U, __m256h __A, __m256h __B, const int __C)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C,
		  (__v16hf) (__m256h) _mm256_setzero_ph (),
		  (__mmask16) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_round_ph (__m256h __A, __m256h __B, const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C,
		  (__v16hf) (__m256h) _mm256_undefined_ph (),
		  (__mmask16) -1, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_round_ph (__m256h __W, __mmask16 __U, __m256h __A,
			     __m256h __B, const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C, (__v16hf) __W,
		  (__mmask16) __U, __R);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_round_ph (__mmask16 __U, __m256h __A, __m256h __B,
			      const int __C, const int __R)
{
  return (__m256h) __builtin_ia32_minmaxph256_mask_round (
		  (__v16hf) __A, (__v16hf) __B, __C,
		  (__v16hf) (__m256h) _mm256_setzero_ph (),
		  (__mmask16) __U, __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_ps (__m128 __A, __m128 __B, const int __C)
{
  return (__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) __A,
						   (__v4sf) __B,
						   __C,
						   (__v4sf)(__m128)
						   _mm_undefined_ps (),
						   (__mmask8) -1);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_ps (__m128 __W, __mmask8 __U, __m128 __A, __m128 __B,
		    const int __C)
{
  return (__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) __A,
						   (__v4sf) __B,
						   __C,
						   (__v4sf) __W,
						   (__mmask8) __U);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_ps (__mmask8 __U, __m128 __A, __m128 __B, const int __C)
{
  return (__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) __A,
						   (__v4sf) __B,
						   __C,
						   (__v4sf)(__m128)
						   _mm_setzero_ps (),
						   (__mmask8) __U);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_ps (__m256 __A, __m256 __B, const int __C)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C,
		  (__v8sf) (__m256) _mm256_undefined_ps (),
		  (__mmask8) -1, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
		       const int __C)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C, (__v8sf) __W,
		  (__mmask8) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_ps (__mmask8 __U, __m256 __A, __m256 __B, const int __C)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C,
		  (__v8sf) (__m256) _mm256_setzero_ps (),
		  (__mmask8) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_minmax_round_ps (__m256 __A, __m256 __B, const int __C, const int __R)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C,
		  (__v8sf) (__m256) _mm256_undefined_ps (),
		  (__mmask8) -1, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_minmax_round_ps (__m256 __W, __mmask8 __U, __m256 __A, __m256 __B,
			     const int __C, const int __R)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C, (__v8sf) __W,
		  (__mmask8) __U, __R);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_minmax_round_ps (__mmask8 __U, __m256 __A, __m256 __B,
			      const int __C, const int __R)
{
  return (__m256) __builtin_ia32_minmaxps256_mask_round (
		  (__v8sf) __A, (__v8sf) __B, __C,
		  (__v8sf) (__m256) _mm256_setzero_ps (),
		  (__mmask8) __U, __R);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_sd (__m128d __A, __m128d __B, const int __C)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df)
						       _mm_undefined_pd (),
						       (__mmask8) -1,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_sd (__m128d __W, __mmask8 __U, __m128d __A,
		    __m128d __B, const int __C)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df) __W,
						       (__mmask8) __U,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_sd (__mmask8 __U, __m128d __A, __m128d __B,
		     const int __C)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df)
						       _mm_setzero_pd (),
						       (__mmask8) __U,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_round_sd (__m128d __A, __m128d __B, const int __C, const int __R)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df)
						       _mm_undefined_pd (),
						       (__mmask8) -1, __R);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_round_sd (__m128d __W, __mmask8 __U, __m128d __A,
			  __m128d __B, const int __C, const int __R)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df) __W,
						       (__mmask8) __U, __R);
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_round_sd (__mmask8 __U, __m128d __A, __m128d __B,
			   const int __C, const int __R)
{
  return (__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) __A,
						       (__v2df) __B,
						       __C,
						       (__v2df)
						       _mm_setzero_pd (),
						       (__mmask8) __U, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_sh (__m128h __A, __m128h __B, const int __C)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf)
						       _mm_undefined_ph (),
						       (__mmask8) -1,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_sh (__m128h __W, __mmask8 __U, __m128h __A, __m128h __B,
		    const int __C)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf) __W,
						       (__mmask8) __U,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_sh (__mmask8 __U, __m128h __A, __m128h __B,
		     const int __C)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf)
						       _mm_setzero_ph (),
						       (__mmask8) __U,
						       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_round_sh (__m128h __A, __m128h __B, const int __C, const int __R)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf)
						       _mm_undefined_ph (),
						       (__mmask8) -1, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_round_sh (__m128h __W, __mmask8 __U, __m128h __A, __m128h __B,
			  const int __C, const int __R)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf) __W,
						       (__mmask8) __U, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_round_sh (__mmask8 __U, __m128h __A, __m128h __B,
			   const int __C, const int __R)
{
  return (__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       __C,
						       (__v8hf)
						       _mm_setzero_ph (),
						       (__mmask8) __U, __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_ss (__m128 __A, __m128 __B, const int __C)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf)
						      _mm_undefined_ps (),
						      (__mmask8) -1,
						      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_ss (__m128 __W, __mmask8 __U, __m128 __A, __m128 __B,
		    const int __C)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf) __W,
						      (__mmask8) __U,
						      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_ss (__mmask8 __U, __m128 __A, __m128 __B,
		     const int __C)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf)
						      _mm_setzero_ps (),
						      (__mmask8) __U,
						      _MM_FROUND_CUR_DIRECTION);
}


extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_minmax_round_ss (__m128 __A, __m128 __B, const int __C, const int __R)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf)
						      _mm_undefined_ps (),
						      (__mmask8) -1, __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_minmax_round_ss (__m128 __W, __mmask8 __U, __m128 __A, __m128 __B,
			  const int __C, const int __R)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf) __W,
						      (__mmask8) __U, __R);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_minmax_round_ss (__mmask8 __U, __m128 __A, __m128 __B,
			   const int __C, const int __R)
{
  return (__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) __A,
						      (__v4sf) __B,
						      __C,
						      (__v4sf)
						      _mm_setzero_ps (),
						      (__mmask8) __U, __R);
}

#else
#define _mm_minmax_nepbh(A, B, C)					      \
  ((__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) (A),	      \
						    (__v8bf) (B), 	      \
						    (int) (C),		      \
						    (__v8bf) (__m128bh)	      \
						    _mm_setzero_si128 (),     \
						    (__mmask8) (-1)))

#define _mm_mask_minmax_nepbh(W, U, A, B, C)				      \
  ((__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) (A),	      \
						    (__v8bf) (B),	      \
						    (int) (C),		      \
						    (__v8bf) (__m128bh) (W),  \
						    (__mmask8) (U)))

#define _mm_maskz_minmax_nepbh(U, A, B, C)				      \
  ((__m128bh) __builtin_ia32_minmaxnepbf16128_mask ((__v8bf) (A),	      \
						    (__v8bf) (B),	      \
						    (int) (C),		      \
						    (__v8bf) (__m128bh)	      \
						    _mm_setzero_si128 (),     \
						    (__mmask8) (U)))

#define _mm256_minmax_nepbh(A, B, C)					      \
  ((__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) (A),	      \
						    (__v16bf) (B),	      \
						    (int) (C),		      \
						    (__v16bf) (__m256bh)      \
						    _mm256_setzero_si256 (),  \
						    (__mmask16) (-1)))

#define _mm256_mask_minmax_nepbh(W, U, A, B, C)				      \
  ((__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) (A),	      \
  						    (__v16bf) (B),	      \
						    (int) (C),		      \
						    (__v16bf) (__m256bh) (W), \
						    (__mmask16) (U)))

#define _mm256_maskz_minmax_nepbh(U, A, B, C)				      \
  ((__m256bh) __builtin_ia32_minmaxnepbf16256_mask ((__v16bf) (A),	      \
						    (__v16bf) (B),	      \
						    (int) (C),		      \
						    (__v16bf) (__m256bh)      \
						    _mm256_setzero_si256 (),  \
						    (__mmask16) (U)))

#define _mm_minmax_pd(A, B, C)						      \
  ((__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) (A),		      \
					      (__v2df) (B),		      \
  					      (int) (C),		      \
					      (__v2df) (__m128d)	      \
					      _mm_undefined_pd (),	      \
					      (__mmask8) (-1)))

#define _mm_mask_minmax_pd(W, U, A, B, C)				      \
  ((__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) (A),		      \
					      (__v2df) (B),		      \
					      (int) (C),		      \
					      (__v2df) (__m128d) (W),	      \
					      (__mmask8) (U)))

#define _mm_maskz_minmax_pd(U, A, B, C)					      \
  ((__m128d) __builtin_ia32_minmaxpd128_mask ((__v2df) (A),		      \
					      (__v2df) (B),		      \
  					      (int) (C),		      \
					      (__v2df) (__m128d)	      \
					      _mm_setzero_pd (),	      \
	  				      (__mmask8) (U)))

#define _mm256_minmax_pd(A, B, C)					      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d)	      \
						    _mm256_undefined_pd (),   \
						    (__mmask8) (-1),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_mask_minmax_pd(W, U, A, B, C)				      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d) (W),   \
						    (__mmask8) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_maskz_minmax_pd(U, A, B, C)				      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d)	      \
						    _mm256_setzero_pd (),     \
						    (__mmask8) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_minmax_round_pd(A, B, C, R)				      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d)	      \
						    _mm256_undefined_pd (),   \
						    (__mmask8) (-1),	      \
						    (int) (R)))

#define _mm256_mask_minmax_round_pd(W, U, A, B, C, R)			      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d) (W),   \
						    (__mmask8) (U),	      \
						    (int) (R)))

#define _mm256_maskz_minmax_round_pd(U, A, B, C, R)			      \
  ((__m256d) __builtin_ia32_minmaxpd256_mask_round ((__v4df) (A),	      \
						    (__v4df) (B),	      \
						    (int) (C),		      \
						    (__v4df) (__m256d)	      \
						    _mm256_setzero_pd (),     \
						    (__mmask8) (U),	      \
						    (int) (R)))

#define _mm_minmax_ph(A, B, C)						      \
  ((__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) (A),		      \
					      (__v8hf) (B),		      \
  					      (int) (C),		      \
					      (__v8hf) (__m128h)	      \
					      _mm_undefined_ph (),	      \
					      (__mmask8) (-1)))

#define _mm_mask_minmax_ph(W, U, A, B, C)				      \
  ((__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) (A),		      \
					      (__v8hf) (B),		      \
					      (int) (C),		      \
					      (__v8hf) (__m128h) (W),	      \
					      (__mmask8) (U)))

#define _mm_maskz_minmax_ph(U, A, B, C)					      \
  ((__m128h) __builtin_ia32_minmaxph128_mask ((__v8hf) (A),		      \
					      (__v8hf) (B),		      \
  					      (int) (C),		      \
					      (__v8hf) (__m128h)	      \
					      _mm_setzero_ph (),	      \
					      (__mmask8) (U)))

#define _mm256_minmax_ph(A, B, C)					      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h)	      \
						    _mm256_undefined_ph (),   \
						    (__mmask16) (-1),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_mask_minmax_ph(W, U, A, B, C)				      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h) (W),  \
						    (__mmask16) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_maskz_minmax_ph(U, A, B, C)				      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h)	      \
						    _mm256_setzero_ph (),     \
						    (__mmask16) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm256_minmax_round_ph(A, B, C, R)				      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h)	      \
						    _mm256_undefined_ph (),   \
						    (__mmask16) (-1),	      \
						    (int) (R)))

#define _mm256_mask_minmax_round_ph(W, U, A, B, C, R)			      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h) (W),  \
						    (__mmask16) (U),	      \
						    (int) (R)))

#define _mm256_maskz_minmax_round_ph(U, A, B, C, R)			      \
  ((__m256h) __builtin_ia32_minmaxph256_mask_round ((__v16hf) (A),	      \
						    (__v16hf) (B),	      \
						    (int) (C),		      \
						    (__v16hf) (__m256h)	      \
						    _mm256_setzero_ph (),     \
						    (__mmask16) (U),	      \
						    (int) (R)))

#define _mm_minmax_ps(A, B, C)						      \
  ((__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) (A),		      \
					     (__v4sf) (B),		      \
  					     (int) (C),			      \
					     (__v4sf) (__m128)		      \
					     _mm_undefined_ps (),	      \
	  				     (__mmask8) (-1)))

#define _mm_mask_minmax_ps(W, U, A, B, C)				      \
  ((__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) (A),		      \
					     (__v4sf) (B),		      \
  					     (int) (C),			      \
					     (__v4sf) (__m128) (W),	      \
					     (__mmask8) (U)))

#define _mm_maskz_minmax_ps(U, A, B, C)					      \
  ((__m128) __builtin_ia32_minmaxps128_mask ((__v4sf) (A),		      \
					     (__v4sf) (B),		      \
  					     (int) (C),			      \
					     (__v4sf) (__m128)		      \
					     _mm_setzero_ps (),		      \
					     (__mmask8) (U)))

#define _mm256_minmax_ps(A, B, C)					      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256)	      \
						   _mm256_undefined_ps (),    \
						   (__mmask8) (-1),	      \
						   _MM_FROUND_CUR_DIRECTION))

#define _mm256_mask_minmax_ps(W, U, A, B, C)				      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256) (W),     \
						   (__mmask8) (U),	      \
						   _MM_FROUND_CUR_DIRECTION))

#define _mm256_maskz_minmax_ps(U, A, B, C)				      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256)	      \
						   _mm256_setzero_ps (),      \
						   (__mmask8) (U),	      \
						   _MM_FROUND_CUR_DIRECTION))

#define _mm256_minmax_round_ps(A, B, C, R)				      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256)	      \
						   _mm256_undefined_ps (),    \
						   (__mmask8) (-1),	      \
						   (int) (R)))

#define _mm256_mask_minmax_round_ps(W, U, A, B, C, R)			      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256) (W),     \
						   (__mmask8) (U),	      \
						   (int) (R)))

#define _mm256_maskz_minmax_round_ps(U, A, B, C, R)			      \
  ((__m256) __builtin_ia32_minmaxps256_mask_round ((__v8sf) (A),	      \
						   (__v8sf) (B),	      \
						   (int) (C),		      \
						   (__v8sf) (__m256)	      \
						   _mm256_setzero_ps (),      \
						   (__mmask8) (U),	      \
						   (int) (R)))

#define _mm_minmax_round_sd(A, B, C, R)					      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df) (B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d)	      \
						 _mm_undefined_pd (),	      \
						 (__mmask8) (-1),	      \
						 (int) (R)))

#define _mm_mask_minmax_round_sd(W, U, A, B, C, R)			      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df) (B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d) (W),      \
						 (__mmask8) (U),	      \
						 (int) (R)))

#define _mm_maskz_minmax_round_sd(U, A, B, C, R)			      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df)(B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d)	      \
						 _mm_setzero_pd (),	      \
						 (__mmask8) (U),	      \
						 (int) (R)))

#define _mm_minmax_round_sh(A, B, C, R)					      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h)	      \
						 _mm_undefined_ph (),	      \
						 (__mmask8) (-1),	      \
						 (int) (R)))

#define _mm_mask_minmax_round_sh(W, U, A, B, C, R)			      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h) (W),      \
						 (__mmask8) (U),	      \
						 (int) (R)))

#define _mm_maskz_minmax_round_sh(U, A, B, C, R)			      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h)	      \
						 _mm_setzero_ph (),	      \
						 (__mmask8) (U),	      \
						 (int) (R)))

#define _mm_minmax_round_ss(A, B, C, R)					      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf) (__m128)	      \
						_mm_undefined_ps (),	      \
						(__mmask8) (-1),	      \
						(int) (R)))

#define _mm_mask_minmax_round_ss(W, U, A, B, C, R)			      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf) (__m128) (W),	      \
						(__mmask8) (U),		      \
						(int) (R)))

#define _mm_maskz_minmax_round_ss(U, A, B, C, R)			      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf)(__m128)	      \
						_mm_setzero_ps (),	      \
						(__mmask8) (U),		      \
						(int) (R)))

#define _mm_minmax_sd(A, B, C)						      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df) (B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d)	      \
						 _mm_undefined_pd (),	      \
						 (__mmask8) (-1),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_minmax_sd(W, U, A, B, C)				      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df) (B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d) (W),      \
						 (__mmask8) (U),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_minmax_sd(U, A, B, C)					      \
  ((__m128d) __builtin_ia32_minmaxsd_mask_round ((__v2df) (A),		      \
						 (__v2df) (B),		      \
						 (int) (C),		      \
						 (__v2df) (__m128d)	      \
						 _mm_setzero_pd (),	      \
						 (__mmask8) (U),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_minmax_sh(A, B, C)						      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h)	      \
						 _mm_undefined_ph (),	      \
						 (__mmask8) (-1),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_minmax_sh(W, U, A, B, C)				      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h) (W),      \
						 (__mmask8) (U),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_minmax_sh(U, A, B, C)					      \
  ((__m128h) __builtin_ia32_minmaxsh_mask_round ((__v8hf) (A),		      \
						 (__v8hf) (B),		      \
						 (int) (C),		      \
						 (__v8hf) (__m128h)	      \
						 _mm_setzero_ph (),	      \
						 (__mmask8) (U),	      \
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_minmax_ss(A, B, C)						      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf) (__m128)	      \
						_mm_undefined_ps (),	      \
						(__mmask8) (-1),	      \
						_MM_FROUND_CUR_DIRECTION))

#define _mm_mask_minmax_ss(W, U, A, B, C)				      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf) (__m128) (W),	      \
						(__mmask8) (U),		      \
						_MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_minmax_ss(U, A, B, C)					      \
  ((__m128) __builtin_ia32_minmaxss_mask_round ((__v4sf) (A),		      \
						(__v4sf) (B),		      \
						(int) (C),		      \
						(__v4sf) (__m128)	      \
						_mm_setzero_ps (),	      \
						(__mmask8) (U),		      \
						_MM_FROUND_CUR_DIRECTION))

#endif

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* _AVX10_2MINMAXINTRIN_H_INCLUDED */
