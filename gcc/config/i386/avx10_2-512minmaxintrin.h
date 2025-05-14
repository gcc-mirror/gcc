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
#error "Never use <avx10_2-512minmaxintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2_512MINMAXINTRIN_H_INCLUDED
#define _AVX10_2_512MINMAXINTRIN_H_INCLUDED

#if !defined (__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

#ifdef __OPTIMIZE__
extern __inline __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_pbh (__m512bh __A, __m512bh __B, const int __C)
{
  return (__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) __A,
						       (__v32bf) __B,
						       __C,
						       (__v32bf)(__m512bh)
						       _mm512_setzero_si512 (),
						       (__mmask32) -1);
}

extern __inline __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_pbh (__m512bh __W, __mmask32 __U,
			__m512bh __A, __m512bh __B, const int __C)
{
  return (__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) __A,
						       (__v32bf) __B,
						       __C,
						       (__v32bf) __W,
						       (__mmask32) __U);
}

extern __inline __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_pbh (__mmask32 __U, __m512bh __A,
			 __m512bh __B, const int __C)
{
  return (__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) __A,
						       (__v32bf) __B,
						       __C,
						       (__v32bf)(__m512bh)
						       _mm512_setzero_si512 (),
						       (__mmask32) __U);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_pd (__m512d __A, __m512d __B, const int __C)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df)
							  _mm512_undefined_pd (),
							  (__mmask8) -1,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_pd (__m512d __W, __mmask8 __U, __m512d __A,
		       __m512d __B, const int __C)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df) __W,
							  (__mmask8) __U,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_pd (__mmask8 __U, __m512d __A, __m512d __B,
			const int __C)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df)
							  _mm512_setzero_pd (),
							  (__mmask8) __U,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_round_pd (__m512d __A, __m512d __B, const int __C,
			const int __R)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df)
							  _mm512_undefined_pd (),
							  (__mmask8) -1, __R);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_round_pd (__m512d __W, __mmask8 __U, __m512d __A,
			     __m512d __B, const int __C, const int __R)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df) __W,
							  (__mmask8) __U, __R);
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_round_pd (__mmask8 __U, __m512d __A, __m512d __B,
			      const int __C, const int __R)
{
  return (__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) __A,
							  (__v8df) __B,
							  __C,
							  (__v8df)
							  _mm512_setzero_pd (),
							  (__mmask8) __U, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_ph (__m512h __A, __m512h __B, const int __C)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf)
							  _mm512_undefined_ph (),
							  (__mmask32) -1,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_ph (__m512h __W, __mmask32 __U, __m512h __A,
		       __m512h __B, const int __C)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf) __W,
							  (__mmask32) __U,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_ph (__mmask32 __U, __m512h __A, __m512h __B,
			const int __C)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf)
							  _mm512_setzero_ph (),
							  (__mmask32) __U,
							  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_round_ph (__m512h __A, __m512h __B, const int __C, const int __R)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf)
							  _mm512_undefined_ph (),
							  (__mmask32) -1, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_round_ph (__m512h __W, __mmask32 __U, __m512h __A,
			     __m512h __B, const int __C, const int __R)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf) __W,
							  (__mmask32) __U, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_round_ph (__mmask32 __U, __m512h __A, __m512h __B,
			      const int __C, const int __R)
{
  return (__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) __A,
							  (__v32hf) __B,
							  __C,
							  (__v32hf)
							  _mm512_setzero_ph (),
							  (__mmask32) __U, __R);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_ps (__m512 __A, __m512 __B, const int __C)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf)
							 _mm512_undefined_ps (),
							 (__mmask16) -1,
							 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_ps (__m512 __W, __mmask16 __U, __m512 __A,
		       __m512 __B, const int __C)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf) __W,
							 (__mmask16) __U,
							 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_ps (__mmask16 __U, __m512 __A, __m512 __B,
			const int __C)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf)
							 _mm512_setzero_ps (),
							 (__mmask16) __U,
							 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_minmax_round_ps (__m512 __A, __m512 __B, const int __C, const int __R)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf)
							 _mm512_undefined_ps (),
							 (__mmask16) -1, __R);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_minmax_round_ps (__m512 __W, __mmask16 __U, __m512 __A,
			     __m512 __B, const int __C, const int __R)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf) __W,
							 (__mmask16) __U, __R);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_minmax_round_ps (__mmask16 __U, __m512 __A, __m512 __B,
			      const int __C, const int __R)
{
  return (__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) __A,
							 (__v16sf) __B,
							 __C,
							 (__v16sf)
							 _mm512_setzero_ps (),
							 (__mmask16) __U, __R);
}

#else
#define _mm512_minmax_pbh(A, B, C)					      \
  ((__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) (A),		      \
						 (__v32bf) (B),		      \
						 (int) (C),		      \
						 (__v32bf) (__m512bh)	      \
						 _mm512_setzero_si512 (),     \
						 (__mmask32) (-1)))

#define _mm512_mask_minmax_pbh(W, U, A, B, C)				      \
  ((__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) (A),		      \
						 (__v32bf) (B), 	      \
						 (int) (C),		      \
						 (__v32bf) (__m512bh) (W),    \
						 (__mmask32) (U)))

#define _mm512_maskz_minmax_pbh(U, A, B, C)				      \
  ((__m512bh) __builtin_ia32_minmaxbf16512_mask ((__v32bf) (A),		      \
						 (__v32bf) (B),		      \
						 (int) (C),		      \
						 (__v32bf) (__m512bh)	      \
						 _mm512_setzero_si512 (),     \
						 (__mmask32) (U)))

#define _mm512_minmax_round_pd(A, B, C, R)				      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d)	      \
						    _mm512_undefined_pd (),   \
						    (__mmask8) (-1),	      \
						    (int) (R)))

#define _mm512_mask_minmax_round_pd(W, U, A, B, C, R)			      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d) (W),   \
						    (__mmask8) (U),	      \
						    (int) (R)))

#define _mm512_maskz_minmax_round_pd(U, A, B, C, R)			      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d)	      \
						    _mm512_setzero_pd (),     \
						    (__mmask8) (U), 	      \
						    (int) (R)))

#define _mm512_minmax_round_ph(A, B, C, R)				      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h)	      \
						    _mm512_undefined_ph (),   \
						    (__mmask32) (-1),	      \
						    (int) (R)))

#define _mm512_mask_minmax_round_ph(W, U, A, B, C, R)			      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h) (W),  \
						    (__mmask32) (U),	      \
						    (int) (R)))

#define _mm512_maskz_minmax_round_ph(U, A, B, C, R)			      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h)	      \
						    _mm512_setzero_ph (),     \
						    (__mmask32) (U),	      \
						    (int) (R)))

#define _mm512_minmax_round_ps(A, B, C, R)				      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A),	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512)	      \
						   _mm512_undefined_ps (),    \
						   (__mmask16) (-1),	      \
						   (int) (R)))

#define _mm512_mask_minmax_round_ps(W, U, A, B, C, R)			      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A),	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512) (W),    \
						   (__mmask16) (U),	      \
						   (int) (R)))

#define _mm512_maskz_minmax_round_ps(U, A, B, C, R)			      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A), 	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512)	      \
						   _mm512_setzero_ps (),      \
						   (__mmask16) (U),	      \
						   (int) (R)))

#define _mm512_minmax_pd(A, B, C)					      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d)	      \
						    _mm512_undefined_pd (),   \
						    (__mmask8) (-1),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_minmax_pd(W, U, A, B, C)				      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d) (W),   \
						    (__mmask8) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_minmax_pd(U, A, B, C)				      \
  ((__m512d) __builtin_ia32_minmaxpd512_mask_round ((__v8df) (A),	      \
						    (__v8df) (B),	      \
						    (int) (C),		      \
						    (__v8df) (__m512d)	      \
						    _mm512_setzero_pd (),     \
						    (__mmask8) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_minmax_ph(A, B, C)					      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h)	      \
						    _mm512_undefined_ph (),   \
						    (__mmask32) (-1),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_minmax_ph(W, U, A, B, C)				      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h) (W),  \
						    (__mmask32) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_minmax_ph(U, A, B, C)				      \
  ((__m512h) __builtin_ia32_minmaxph512_mask_round ((__v32hf) (A),	      \
						    (__v32hf) (B),	      \
						    (int) (C),		      \
						    (__v32hf) (__m512h)	      \
						    _mm512_setzero_ph (),     \
						    (__mmask32) (U),	      \
						    _MM_FROUND_CUR_DIRECTION))

#define _mm512_minmax_ps(A, B, C)					      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A),	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512)	      \
						   _mm512_undefined_ps (),    \
						   (__mmask16) (-1),	      \
						   _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_minmax_ps(W, U, A, B, C)				      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A),	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512) (W),    \
						   (__mmask16) (U),	      \
						   _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_minmax_ps(U, A, B, C)				      \
  ((__m512) __builtin_ia32_minmaxps512_mask_round ((__v16sf) (A),	      \
						   (__v16sf) (B),	      \
						   (int) (C),		      \
						   (__v16sf) (__m512)	      \
						   _mm512_setzero_ps (),      \
						   (__mmask16) (U),	      \
						   _MM_FROUND_CUR_DIRECTION))

#endif

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* _AVX10_2_512MINMAXINTRIN_H_INCLUDED */
