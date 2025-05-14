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
#error "Never use <avx10_2-512bf16intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2_512BF16INTRIN_H_INCLUDED
#define _AVX10_2_512BF16INTRIN_H_INCLUDED

#if !defined (__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_add_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_addbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_add_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_addbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_add_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_addbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sub_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_subbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sub_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_subbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sub_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_subbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mul_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_mulbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mul_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_mulbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mul_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_mulbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_div_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_divbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_div_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_divbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_div_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_divbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_maxbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_maxbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_maxbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_minbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_pbh (__m512bh __W, __mmask32 __U,
		     __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_minbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_minbf16512_mask (__A, __B,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_scalefbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_pbh (__m512bh __W, __mmask32 __U,
			__m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_scalefbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_scalefbf16512_mask (__A, __B,
				       (__v32bf) _mm512_setzero_si512 (),
				       __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fmadd_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fmadd_pbh (__m512bh __A, __mmask32 __U,
		       __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fmadd_pbh (__m512bh __A, __m512bh __B,
			__m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fmaddbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fmadd_pbh (__mmask32 __U, __m512bh __A,
			  __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fmsub_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fmsub_pbh (__m512bh __A, __mmask32 __U,
		       __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fmsub_pbh (__m512bh __A, __m512bh __B,
			__m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fmsubbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fmsub_pbh (__mmask32 __U, __m512bh __A,
			__m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fnmadd_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fnmadd_pbh (__m512bh __A, __mmask32 __U,
			__m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fnmadd_pbh (__m512bh __A, __m512bh __B,
			 __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fnmaddbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fnmadd_pbh (__mmask32 __U, __m512bh __A,
			 __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fnmsub_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fnmsub_pbh (__m512bh __A, __mmask32 __U,
			__m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fnmsub_pbh (__m512bh __A, __m512bh __B,
			 __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fnmsubbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fnmsub_pbh (__mmask32 __U, __m512bh __A,
			 __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rsqrt_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtbf16512_mask (__A,
				      (__v32bf) _mm512_setzero_si512 (),
				      (__mmask32) -1);

}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rsqrt_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rsqrt_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtbf16512_mask (__A,
				      (__v32bf) _mm512_setzero_si512 (),
				      __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrt_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtbf16512_mask (__A,
				     (__v32bf) _mm512_setzero_si512 (),
				     (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrt_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrt_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtbf16512_mask (__A,
				     (__v32bf) _mm512_setzero_si512 (),
				     __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rcp_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcpbf16512_mask (__A,
				    (__v32bf) _mm512_setzero_si512 (),
				    (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rcp_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcpbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rcp_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcpbf16512_mask (__A,
				    (__v32bf) _mm512_setzero_si512 (),
				    __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getexp_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_getexpbf16512_mask (__A,
				       (__v32bf) _mm512_setzero_si512 (),
				       (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getexp_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh) __builtin_ia32_getexpbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getexp_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_getexpbf16512_mask (__A,
				       (__v32bf) _mm512_setzero_si512 (),
				       __U);
}

/* Intrinsics vrndscalebf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscale_pbh (__m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalebf16512_mask (__A, B,
					 (__v32bf) _mm512_setzero_si512 (),
					 (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscale_pbh (__m512bh __W, __mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalebf16512_mask (__A, B, __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscale_pbh (__mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalebf16512_mask (__A, B,
					 (__v32bf) _mm512_setzero_si512 (),
					 __U);
}

#else
#define _mm512_roundscale_pbh(A, B)					      \
  (__builtin_ia32_rndscalebf16512_mask ((A), (B),			      \
					(__v32bf) _mm512_setzero_si512 (),    \
					(__mmask32) -1))

#define _mm512_mask_roundscale_pbh(A, B, C, D)	    		      \
  (__builtin_ia32_rndscalebf16512_mask ((C), (D), (A), (B)))

#define _mm512_maskz_roundscale_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalebf16512_mask ((B), (C),			      \
					(__v32bf) _mm512_setzero_si512 (),    \
					(A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducebf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reduce_pbh (__m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducebf16512_mask (__A, B,
				       (__v32bf) _mm512_setzero_si512 (),
				       (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reduce_pbh (__m512bh __W, __mmask32 __U,
			__m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducebf16512_mask (__A, B, __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reduce_pbh (__mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducebf16512_mask (__A, B,
					  (__v32bf) _mm512_setzero_si512 (),
					  __U);
}

#else
#define _mm512_reduce_pbh(A, B)					      \
  (__builtin_ia32_reducebf16512_mask ((A), (B),			      \
				      (__v32bf) _mm512_setzero_si512 (),   \
				      (__mmask32) -1))

#define _mm512_mask_reduce_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducebf16512_mask ((C), (D), (A), (B)))

#define _mm512_maskz_reduce_pbh(A, B, C)				      \
  (__builtin_ia32_reducebf16512_mask ((B), (C),			      \
				      (__v32bf) _mm512_setzero_si512 (),      \
				      (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vgetmantbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getmant_pbh (__m512bh __A, _MM_MANTISSA_NORM_ENUM __B,
		    _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512bh)
    __builtin_ia32_getmantbf16512_mask (__A, (int) (__C << 2) | __B,
					(__v32bf) _mm512_setzero_si512 (),
					(__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getmant_pbh (__m512bh __W, __mmask32 __U, __m512bh __A,
			 _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512bh)
    __builtin_ia32_getmantbf16512_mask (__A, (int) (__C << 2) | __B,
					__W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getmant_pbh (__mmask32 __U, __m512bh __A,
			  _MM_MANTISSA_NORM_ENUM __B,
			  _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512bh)
    __builtin_ia32_getmantbf16512_mask (__A, (int) (__C << 2) | __B,
					(__v32bf) _mm512_setzero_si512 (),
					__U);
}

#else
#define _mm512_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantbf16512_mask ((A), (int)(((C)<<2) | (B)),	      \
				       (__v32bf) _mm512_setzero_si512 (),     \
				       (__mmask32) -1))

#define _mm512_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantbf16512_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm512_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantbf16512_mask ((B), (int)(((C)<<2) | (D)),	      \
				       (__v32bf) _mm512_setzero_si512 (),     \
					  (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclassbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fpclass_pbh_mask (__mmask32 __U, __m512bh __A,
			      const int __imm)
{
  return (__mmask32)
    __builtin_ia32_fpclassbf16512_mask (__A, __imm, __U);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fpclass_pbh_mask (__m512bh __A, const int __imm)
{
  return (__mmask32)
    __builtin_ia32_fpclassbf16512_mask (__A, __imm,
					(__mmask32) -1);
}

#else
#define _mm512_mask_fpclass_pbh_mask(U, X, C)				   \
  ((__mmask32) __builtin_ia32_fpclassbf16512_mask (			   \
      (__v32bf) (__m512bh) (X), (int) (C), (__mmask32) (U)))

#define _mm512_fpclass_pbh_mask(X, C)					   \
  ((__mmask32) __builtin_ia32_fpclassbf16512_mask (			   \
      (__v32bf) (__m512bh) (X), (int) (C), (__mmask32) (-1)))
#endif /* __OPIMTIZE__ */


/* Intrinsics vcmpbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_pbh_mask (__mmask32 __U, __m512bh __A, __m512bh __B,
			  const int __imm)
{
  return (__mmask32)
    __builtin_ia32_cmpbf16512_mask (__A, __B, __imm, __U);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_pbh_mask (__m512bh __A, __m512bh __B, const int __imm)
{
  return (__mmask32)
    __builtin_ia32_cmpbf16512_mask (__A, __B, __imm,
				    (__mmask32) -1);
}

#else
#define _mm512_mask_cmp_pbh_mask(A, B, C, D)				\
  ((__mmask32) __builtin_ia32_cmpbf16512_mask ((B), (C), (D), (A)))

#define _mm512_cmp_pbh_mask(A, B, C)					\
  ((__mmask32) __builtin_ia32_cmpbf16512_mask ((A), (B), (C), (-1)))

#endif /* __OPIMTIZE__ */

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* _AVX10_2_512BF16INTRIN_H_INCLUDED */
