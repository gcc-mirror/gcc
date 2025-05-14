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
#error "Never use <avx10_2-512mediaintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2_512MEDIAINTRIN_H_INCLUDED
#define _AVX10_2_512MEDIAINTRIN_H_INCLUDED

#if !defined(__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbssd_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbssd_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbssd_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssd_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbssds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbssds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbssds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbssds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbsud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbsud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbsud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbsuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbsuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbsuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbsuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbuud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbuud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbuud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbuuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbuuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbuuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpbuuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwsud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwsud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwsud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwsuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwsuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwsuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwsuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwusd_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwusd_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwusd_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusd_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwusds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwusds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwusds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwusds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwuud_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwuud_epi32 (__m512i __W, __mmask16 __U,
			  __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud_v16si_mask ((__v16si) __W,
					(__v16si) __A,
					(__v16si) __B,
					(__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwuud_epi32 (__mmask16 __U, __m512i __W,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuud_v16si_maskz ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpwuuds_epi32 (__m512i __W, __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds512 ((__v16si) __W, (__v16si) __A, (__v16si) __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpwuuds_epi32 (__m512i __W, __mmask16 __U,
			   __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds_v16si_mask ((__v16si) __W,
					 (__v16si) __A,
					 (__v16si) __B,
					 (__mmask16) __U);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpwuuds_epi32 (__mmask16 __U, __m512i __W,
			    __m512i __A, __m512i __B)
{
  return (__m512i)
    __builtin_ia32_vpdpwuuds_v16si_maskz ((__v16si) __W,
					  (__v16si) __A,
					  (__v16si) __B,
					  (__mmask16) __U);
}

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

#ifdef __OPTIMIZE__
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

#endif /* __AVX10_2_512MEDIAINTRIN_H_INCLUDED */
