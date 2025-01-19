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

#if !defined (__AVX10_2_512__)
#pragma GCC push_options
#pragma GCC target("avx10.2-512")
#define __DISABLE_AVX10_2_512__
#endif /* __AVX10_2_512__ */

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_addne_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_addnepbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_addne_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_addnepbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_addne_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_addnepbf16512_mask (__A, __B,
				       (__v32bf) _mm512_setzero_si512 (),
				        __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_subne_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_subnepbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_subne_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_subnepbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_subne_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_subnepbf16512_mask (__A, __B,
				       (__v32bf) _mm512_setzero_si512 (),
				        __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mulne_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_mulnepbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mulne_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_mulnepbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mulne_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_mulnepbf16512_mask (__A, __B,
				       (__v32bf) _mm512_setzero_si512 (),
				        __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_divne_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_divnepbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_divne_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_divnepbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_divne_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_divnepbf16512_mask (__A, __B,
				       (__v32bf) _mm512_setzero_si512 (),
				        __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_maxpbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_maxpbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_maxpbf16512_mask (__A, __B,
				     (__v32bf) _mm512_setzero_si512 (),
				     __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_minpbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_pbh (__m512bh __W, __mmask32 __U,
		       __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_minpbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_minpbf16512_mask (__A, __B,
				     (__v32bf) _mm512_setzero_si512 (),
				     __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_pbh (__m512bh __A, __m512bh __B)
{
  return (__m512bh) __builtin_ia32_scalefpbf16512 (__A, __B);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_pbh (__m512bh __W, __mmask32 __U,
			  __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_scalefpbf16512_mask (__A, __B, __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_pbh (__mmask32 __U, __m512bh __A, __m512bh __B)
{
  return (__m512bh)
    __builtin_ia32_scalefpbf16512_mask (__A, __B,
					(__v32bf) _mm512_setzero_si512 (),
					__U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fmaddne_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddnepbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fmaddne_pbh (__m512bh __A, __mmask32 __U,
			 __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddnepbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fmaddne_pbh (__m512bh __A, __m512bh __B,
			  __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fmaddnepbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fmaddne_pbh (__mmask32 __U, __m512bh __A,
			  __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmaddnepbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fmsubne_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubnepbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fmsubne_pbh (__m512bh __A, __mmask32 __U,
			 __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubnepbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fmsubne_pbh (__m512bh __A, __m512bh __B,
			  __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fmsubnepbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fmsubne_pbh (__mmask32 __U, __m512bh __A,
			  __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fmsubnepbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fnmaddne_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddnepbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fnmaddne_pbh (__m512bh __A, __mmask32 __U,
			  __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddnepbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fnmaddne_pbh (__m512bh __A, __m512bh __B,
			   __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fnmaddnepbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fnmaddne_pbh (__mmask32 __U, __m512bh __A,
			   __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmaddnepbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fnmsubne_pbh (__m512bh __A, __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubnepbf16512_mask (__A, __B, __C, (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fnmsubne_pbh (__m512bh __A, __mmask32 __U,
			  __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubnepbf16512_mask (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask3_fnmsubne_pbh (__m512bh __A, __m512bh __B,
			   __m512bh __C, __mmask32 __U)
{
  return (__m512bh)
    __builtin_ia32_fnmsubnepbf16512_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_fnmsubne_pbh (__mmask32 __U, __m512bh __A,
			   __m512bh __B, __m512bh __C)
{
  return (__m512bh)
    __builtin_ia32_fnmsubnepbf16512_maskz (__A, __B, __C, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rsqrt_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtpbf16512_mask (__A,
				       (__v32bf) _mm512_setzero_si512 (),
				       (__mmask32) -1);

}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rsqrt_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtpbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rsqrt_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rsqrtpbf16512_mask (__A,
				       (__v32bf) _mm512_setzero_si512 (),
				       __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrtne_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtnepbf16512_mask (__A,
				        (__v32bf) _mm512_setzero_si512 (),
					(__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrtne_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtnepbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrtne_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_sqrtnepbf16512_mask (__A,
					(__v32bf) _mm512_setzero_si512 (),
					__U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rcp_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcppbf16512_mask (__A,
				     (__v32bf) _mm512_setzero_si512 (),
				     (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rcp_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcppbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rcp_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_rcppbf16512_mask (__A,
				     (__v32bf) _mm512_setzero_si512 (),
				     __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getexp_pbh (__m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_getexppbf16512_mask (__A,
					(__v32bf) _mm512_setzero_si512 (),
					(__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getexp_pbh (__m512bh __W, __mmask32 __U, __m512bh __A)
{
  return (__m512bh) __builtin_ia32_getexppbf16512_mask (__A,  __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getexp_pbh (__mmask32 __U, __m512bh __A)
{
  return (__m512bh)
    __builtin_ia32_getexppbf16512_mask (__A,
					(__v32bf) _mm512_setzero_si512 (),
					__U);
}

/* Intrinsics vrndscalepbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscalene_pbh (__m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalenepbf16512_mask (__A, B,
					    (__v32bf) _mm512_setzero_si512 (),
					    (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscalene_pbh (__m512bh __W, __mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalenepbf16512_mask (__A, B, __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscalene_pbh (__mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_rndscalenepbf16512_mask (__A, B,
					    (__v32bf) _mm512_setzero_si512 (),
					    __U);
}

#else
#define _mm512_roundscalene_pbh(A, B)					      \
  (__builtin_ia32_rndscalenepbf16512_mask ((A), (B),			      \
					   (__v32bf) _mm512_setzero_si512 (), \
					   (__mmask32) -1))

#define _mm512_mask_roundscalene_pbh(A, B, C, D)	    		      \
  (__builtin_ia32_rndscalenepbf16512_mask ((C), (D), (A), (B)))

#define _mm512_maskz_roundscalene_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalenepbf16512_mask ((B), (C),			      \
					   (__v32bf) _mm512_setzero_si512 (), \
					   (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducepbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reducene_pbh (__m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducenepbf16512_mask (__A, B,
					  (__v32bf) _mm512_setzero_si512 (),
					  (__mmask32) -1);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reducene_pbh (__m512bh __W, __mmask32 __U,
			  __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducenepbf16512_mask (__A, B, __W,  __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reducene_pbh (__mmask32 __U, __m512bh __A, int B)
{
  return (__m512bh)
    __builtin_ia32_reducenepbf16512_mask (__A, B,
					  (__v32bf) _mm512_setzero_si512 (),
					  __U);
}

#else
#define _mm512_reducene_pbh(A, B)					      \
  (__builtin_ia32_reducenepbf16512_mask ((A), (B),			      \
					 (__v32bf) _mm512_setzero_si512 (),   \
					 (__mmask32) -1))

#define _mm512_mask_reducene_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducenepbf16512_mask ((C), (D), (A), (B)))

#define _mm512_maskz_reducene_pbh(A, B, C)				      \
  (__builtin_ia32_reducenepbf16512_mask ((B), (C),			      \
					 (__v32bf) _mm512_setzero_si512 (),   \
					 (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vgetmantpbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getmant_pbh (__m512bh __A, _MM_MANTISSA_NORM_ENUM __B,
		    _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512bh)
    __builtin_ia32_getmantpbf16512_mask (__A, (int) (__C << 2) | __B,
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
    __builtin_ia32_getmantpbf16512_mask (__A, (int) (__C << 2) | __B,
					 __W, __U);
}

extern __inline__ __m512bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getmant_pbh (__mmask32 __U, __m512bh __A,
			  _MM_MANTISSA_NORM_ENUM __B,
			  _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512bh)
    __builtin_ia32_getmantpbf16512_mask (__A, (int) (__C << 2) | __B,
					 (__v32bf) _mm512_setzero_si512 (),
					 __U);
}

#else
#define _mm512_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantpbf16512_mask ((A), (int)(((C)<<2) | (B)),	      \
					  (__v32bf) _mm512_setzero_si512 (),  \
					  (__mmask32) -1))

#define _mm512_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantpbf16512_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm512_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantpbf16512_mask ((B), (int)(((C)<<2) | (D)),	      \
					  (__v32bf) _mm512_setzero_si512 (),  \
					  (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclasspbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fpclass_pbh_mask (__mmask32 __U, __m512bh __A,
			      const int __imm)
{
  return (__mmask32)
    __builtin_ia32_fpclasspbf16512_mask (__A, __imm, __U);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fpclass_pbh_mask (__m512bh __A, const int __imm)
{
  return (__mmask32)
    __builtin_ia32_fpclasspbf16512_mask (__A, __imm,
					 (__mmask32) -1);
}

#else
#define _mm512_mask_fpclass_pbh_mask(U, X, C)				   \
  ((__mmask32) __builtin_ia32_fpclasspbf16512_mask (			   \
      (__v32bf) (__m512bh) (X), (int) (C), (__mmask32) (U)))

#define _mm512_fpclass_pbh_mask(X, C)					   \
  ((__mmask32) __builtin_ia32_fpclasspbf16512_mask (			   \
      (__v32bf) (__m512bh) (X), (int) (C), (__mmask32) (-1)))
#endif /* __OPIMTIZE__ */


/* Intrinsics vcmppbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_pbh_mask (__mmask32 __U, __m512bh __A, __m512bh __B,
			  const int __imm)
{
  return (__mmask32)
    __builtin_ia32_cmppbf16512_mask (__A, __B, __imm, __U);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_pbh_mask (__m512bh __A, __m512bh __B, const int __imm)
{
  return (__mmask32)
    __builtin_ia32_cmppbf16512_mask (__A, __B, __imm,
				     (__mmask32) -1);
}

#else
#define _mm512_mask_cmp_pbh_mask(A, B, C, D)				\
  ((__mmask32) __builtin_ia32_cmppbf16512_mask ((B), (C), (D), (A)))

#define _mm512_cmp_pbh_mask(A, B, C)					\
  ((__mmask32) __builtin_ia32_cmppbf16512_mask ((A), (B), (C), (-1)))

#endif /* __OPIMTIZE__ */

#ifdef __DISABLE_AVX10_2_512__
#undef __DISABLE_AVX10_2_512__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_512__ */

#endif /* _AVX10_2_512BF16INTRIN_H_INCLUDED */
