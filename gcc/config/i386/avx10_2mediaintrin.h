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
#error "Never use <avx10_2mediaintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2MEDIAINTRIN_H_INCLUDED
#define _AVX10_2MEDIAINTRIN_H_INCLUDED

#if !defined(__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

#define _mm_dpbssd_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbssd128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbssds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbssds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbsud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbsud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbsuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbsuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbuud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbuud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpbuuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpbuuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm256_dpbssd_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbssd256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbssds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbssds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbsud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbsud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbsuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbsuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbuud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbuud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpbuuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpbuuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm_dpwsud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwsud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwsuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwsuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwusd_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwusd128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwusds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwusds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwuud_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwuud128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm_dpwuuds_epi32(W, A, B) \
  (__m128i) __builtin_ia32_vpdpwuuds128 ((__v4si) (W), (__v4si) (A), (__v4si) (B))

#define _mm256_dpwsud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwsud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwsuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwsuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwusd_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwusd256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwusds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwusds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwuud_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwuud256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

#define _mm256_dpwuuds_epi32(W, A, B) \
  (__m256i) __builtin_ia32_vpdpwuuds256 ((__v8si) (W), (__v8si) (A), (__v8si) (B))

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbssd_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssd_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbssd_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssd_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbssds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbssds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbssds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbsud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbsud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbsuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbsuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbsuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbuud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbuud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbuuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbuuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpbuuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbssd_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssd_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbssd_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssd_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbssds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbssds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbssds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbsud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbsud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbsuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbsuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbsuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbuud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbuud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbuuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbuuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpbuuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwsud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwsud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwsuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwsuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwsuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwusd_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusd_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwusd_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusd_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwusds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwusds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwusds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwuud_epi32 (__m128i __W, __mmask8 __U,
		       __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuud_v4si_mask ((__v4si) __W,
				       (__v4si) __A,
				       (__v4si) __B,
				       (__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwuud_epi32 (__mmask8 __U, __m128i __W,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuud_v4si_maskz ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpwuuds_epi32 (__m128i __W, __mmask8 __U,
			__m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuuds_v4si_mask ((__v4si) __W,
					(__v4si) __A,
					(__v4si) __B,
					(__mmask8) __U);
}

extern __inline __m128i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpwuuds_epi32 (__mmask8 __U, __m128i __W,
			 __m128i __A, __m128i __B)
{
  return (__m128i)
    __builtin_ia32_vpdpwuuds_v4si_maskz ((__v4si) __W,
					 (__v4si) __A,
					 (__v4si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwsud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwsud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwsuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwsuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwsuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwusd_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusd_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwusd_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusd_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwusds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwusds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwusds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwuud_epi32 (__m256i __W, __mmask8 __U,
			  __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuud_v8si_mask ((__v8si) __W,
				       (__v8si) __A,
				       (__v8si) __B,
				       (__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwuud_epi32 (__mmask8 __U, __m256i __W,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuud_v8si_maskz ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpwuuds_epi32 (__m256i __W, __mmask8 __U,
			   __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuuds_v8si_mask ((__v8si) __W,
					(__v8si) __A,
					(__v8si) __B,
					(__mmask8) __U);
}

extern __inline __m256i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpwuuds_epi32 (__mmask8 __U, __m256i __W,
			    __m256i __A, __m256i __B)
{
  return (__m256i)
    __builtin_ia32_vpdpwuuds_v8si_maskz ((__v8si) __W,
					 (__v8si) __A,
					 (__v8si) __B,
					 (__mmask8) __U);
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

#endif

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* __AVX10_2MEDIAINTRIN_H_INCLUDED */
