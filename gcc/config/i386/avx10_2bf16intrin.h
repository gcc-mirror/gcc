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
#error "Never use <avx10_2bf16intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX10_2BF16INTRIN_H_INCLUDED
#define _AVX10_2BF16INTRIN_H_INCLUDED

#if !defined(__AVX10_2__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2__
#endif /* __AVX10_2__ */

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_add_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_addbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_add_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_addbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_add_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_addbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_addbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_add_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_addbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_addbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sub_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_subbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sub_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_subbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sub_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_subbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sub_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_subbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sub_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_subbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_subbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mul_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_mulbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mul_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_mulbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mul_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_mulbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mul_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_mulbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mul_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_mulbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_mulbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_div_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_divbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_div_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_divbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_div_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_divbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_div_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_divbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_div_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_divbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_divbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_maxbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_maxbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_maxbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_maxbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_maxbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_maxbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_minbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_minbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_minbf16256_mask (__A, __B,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_minbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_minbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_minbf16128_mask (__A, __B,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_scalef_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_scalefbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_scalef_pbh (__m256bh __W, __mmask16 __U,
			__m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_scalefbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_scalef_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_scalefbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_scalefbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_pbh (__m128bh __W, __mmask8 __U,
		     __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_scalefbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_scalefbf16128_mask (__A, __B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmadd_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmadd_pbh (__m256bh __A, __mmask16 __U,
		       __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmadd_pbh (__m256bh __A, __m256bh __B,
			__m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fmaddbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmadd_pbh (__mmask16 __U, __m256bh __A,
			__m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fmadd_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fmadd_pbh (__m128bh __A, __mmask8 __U,
		    __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fmadd_pbh (__m128bh __A, __m128bh __B,
		     __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fmaddbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fmadd_pbh (__mmask8 __U, __m128bh __A,
		     __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsub_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmsubbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsub_pbh (__m256bh __A, __mmask16 __U,
		       __m256bh __B, __m256bh __C)
{
  return (__m256bh) __builtin_ia32_fmsubbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsub_pbh (__m256bh __A, __m256bh __B,
			__m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fmsubbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsub_pbh (__mmask16 __U, __m256bh __A,
			__m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmsubbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fmsub_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fmsub_pbh (__m128bh __A, __mmask8 __U,
		    __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fmsub_pbh (__m128bh __A, __m128bh __B,
		     __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fmsubbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fmsub_pbh (__mmask8 __U, __m128bh __A,
		     __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmadd_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmadd_pbh (__m256bh __A, __mmask16 __U,
			__m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmadd_pbh (__m256bh __A, __m256bh __B,
			 __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fnmaddbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmadd_pbh (__mmask16 __U, __m256bh __A,
			 __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fnmadd_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fnmadd_pbh (__m128bh __A, __mmask8 __U,
		     __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fnmadd_pbh (__m128bh __A, __m128bh __B,
		      __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fnmaddbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fnmadd_pbh (__mmask8 __U, __m128bh __A,
		      __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmsub_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmsub_pbh (__m256bh __A, __mmask16 __U,
			__m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmsub_pbh (__m256bh __A, __m256bh __B,
			 __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fnmsubbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmsub_pbh (__mmask16 __U, __m256bh __A,
			 __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fnmsub_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fnmsub_pbh (__m128bh __A, __mmask8 __U,
		     __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fnmsub_pbh (__m128bh __A, __m128bh __B,
		      __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fnmsubbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fnmsub_pbh (__mmask8 __U, __m128bh __A,
		      __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_rsqrt_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtbf16256_mask (__A,
				      (__v16bf) _mm256_setzero_si256 (),
				      (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_rsqrt_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_rsqrt_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtbf16256_mask (__A,
				      (__v16bf) _mm256_setzero_si256 (),
				      __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rsqrt_pbh (__m128bh __A)
{
  return (__m128bh)
	__builtin_ia32_rsqrtbf16128_mask (__A,
					  (__v8bf) _mm_setzero_si128 (),
					  (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rsqrt_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rsqrtbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rsqrt_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rsqrtbf16128_mask (__A,
				      (__v8bf) _mm_setzero_si128 (),
				      __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sqrt_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtbf16256_mask (__A,
				     (__v16bf) _mm256_setzero_si256 (),
				     (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sqrt_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sqrt_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtbf16256_mask (__A,
				     (__v16bf) _mm256_setzero_si256 (),
				     __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrt_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtbf16128_mask (__A,
				     (__v8bf) _mm_setzero_si128 (),
				     (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrt_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrt_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtbf16128_mask (__A,
				     (__v8bf) _mm_setzero_si128 (),
				     __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_rcp_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcpbf16256_mask (__A,
				    (__v16bf) _mm256_setzero_si256 (),
				    (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_rcp_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcpbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_rcp_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcpbf16256_mask (__A,
				    (__v16bf) _mm256_setzero_si256 (),
				    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rcp_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcpbf16128_mask (__A,
				    (__v8bf) _mm_setzero_si128 (),
				    (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rcp_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcpbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rcp_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcpbf16128_mask (__A,
				    (__v8bf) _mm_setzero_si128 (),
				    __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getexp_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexpbf16256_mask (__A,
				       (__v16bf) _mm256_setzero_si256 (),
				       (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getexp_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexpbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getexp_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexpbf16256_mask (__A,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getexp_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexpbf16128_mask (__A,
				       (__v8bf) _mm_setzero_si128 (),
				       (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getexp_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexpbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getexp_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexpbf16128_mask (__A,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

/* Intrinsics vrndscalebf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_roundscale_pbh (__m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalebf16256_mask (__A, B,
					 (__v16bf) _mm256_setzero_si256 (),
					 (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_roundscale_pbh (__m256bh __W, __mmask16 __U,
			    __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalebf16256_mask (__A, B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_roundscale_pbh (__mmask16 __U, __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalebf16256_mask (__A, B,
					 (__v16bf) _mm256_setzero_si256 (),
					 __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscale_pbh (__m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalebf16128_mask (__A, B,
					 (__v8bf) _mm_setzero_si128 (),
					 (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscale_pbh (__m128bh __W, __mmask8 __U,
			 __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalebf16128_mask (__A, B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscale_pbh (__mmask8 __U, __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalebf16128_mask (__A, B,
					 (__v8bf) _mm_setzero_si128 (),
					 __U);
}

#else
#define _mm256_roundscale_pbh(A, B)					      \
  (__builtin_ia32_rndscalebf16256_mask ((A), (B),			      \
					(__v16bf) _mm256_setzero_si256 (),    \
					(__mmask16) -1))

#define _mm256_mask_roundscale_pbh(A, B, C, D)	    		      \
  (__builtin_ia32_rndscalebf16256_mask ((C), (D), (A), (B)))

#define _mm256_maskz_roundscale_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalebf16256_mask ((B), (C),			      \
					(__v16bf) _mm256_setzero_si256 (),    \
					(A)))

#define _mm_roundscale_pbh(A, B)					      \
  (__builtin_ia32_rndscalebf16128_mask ((A), (B),			      \
					(__v8bf) _mm_setzero_si128 (),	      \
					(__mmask8) -1))

#define _mm_mask_roundscale_pbh(A, B, C, D)				      \
  (__builtin_ia32_rndscalebf16128_mask ((C), (D), (A), (B)))

#define _mm_maskz_roundscale_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalebf16128_mask ((B), (C),			      \
					(__v8bf) _mm_setzero_si128 (),	      \
					(A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducebf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_reduce_pbh (__m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducebf16256_mask (__A, B,
				       (__v16bf) _mm256_setzero_si256 (),
				       (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_reduce_pbh (__m256bh __W, __mmask16 __U,
			__m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducebf16256_mask (__A, B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_reduce_pbh (__mmask16 __U, __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducebf16256_mask (__A, B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reduce_pbh (__m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducebf16128_mask (__A, B,
				       (__v8bf) _mm_setzero_si128 (),
				       (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reduce_pbh (__m128bh __W, __mmask8 __U,
		     __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducebf16128_mask (__A, B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reduce_pbh (__mmask8 __U, __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducebf16128_mask (__A, B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

#else
#define _mm256_reduce_pbh(A, B)					      \
  (__builtin_ia32_reducebf16256_mask ((A), (B),			      \
				      (__v16bf) _mm256_setzero_si256 (),      \
				      (__mmask16) -1))

#define _mm256_mask_reduce_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducebf16256_mask ((C), (D), (A), (B)))

#define _mm256_maskz_reduce_pbh(A, B, C)				      \
  (__builtin_ia32_reducebf16256_mask ((B), (C),			      \
				      (__v16bf) _mm256_setzero_si256 (),      \
				      (A)))

#define _mm_reduce_pbh(A, B)						      \
  (__builtin_ia32_reducebf16128_mask ((A), (B),			      \
				      (__v8bf) _mm_setzero_si128 (),	      \
				      (__mmask8) -1))

#define _mm_mask_reduce_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducebf16128_mask ((C), (D), (A), (B)))

#define _mm_maskz_reduce_pbh(A, B, C)					      \
  (__builtin_ia32_reducebf16128_mask ((B), (C),			      \
				      (__v8bf) _mm_setzero_si128 (),	      \
				      (A)))

#endif /* __OPTIMIZE__ */


/* Intrinsics vgetmantbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getmant_pbh (__m256bh __A, _MM_MANTISSA_NORM_ENUM __B,
		    _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m256bh)
    __builtin_ia32_getmantbf16256_mask (__A, (int) (__C << 2) | __B,
					(__v16bf) _mm256_setzero_si256 (),
					(__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getmant_pbh (__m256bh __W, __mmask16 __U, __m256bh __A,
			 _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m256bh)
    __builtin_ia32_getmantbf16256_mask (__A, (int) (__C << 2) | __B,
					__W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getmant_pbh (__mmask16 __U, __m256bh __A,
			  _MM_MANTISSA_NORM_ENUM __B,
			  _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m256bh)
    __builtin_ia32_getmantbf16256_mask (__A, (int) (__C << 2) | __B,
					(__v16bf) _mm256_setzero_si256 (),
					__U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getmant_pbh (__m128bh __A, _MM_MANTISSA_NORM_ENUM __B,
		 _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m128bh)
    __builtin_ia32_getmantbf16128_mask (__A, (int) (__C << 2) | __B,
					(__v8bf) _mm_setzero_si128 (),
					(__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getmant_pbh (__m128bh __W, __mmask8 __U, __m128bh __A,
		      _MM_MANTISSA_NORM_ENUM __B,
		      _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m128bh)
    __builtin_ia32_getmantbf16128_mask (__A, (int) (__C << 2) | __B,
					__W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getmant_pbh (__mmask8 __U, __m128bh __A,
		       _MM_MANTISSA_NORM_ENUM __B,
		       _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m128bh)
    __builtin_ia32_getmantbf16128_mask (__A, (int) (__C << 2) | __B,
					(__v8bf) _mm_setzero_si128 (),
					__U);
}

#else
#define _mm256_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantbf16256_mask ((A), (int)(((C)<<2) | (B)),	      \
				       (__v16bf) _mm256_setzero_si256 (),     \
				       (__mmask16) (-1)))

#define _mm256_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantbf16256_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm256_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantbf16256_mask ((B), (int)(((C)<<2) | (D)),	      \
				       (__v16bf) _mm256_setzero_si256 (),     \
				       (A)))

#define _mm_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantbf16128_mask ((A), (int)(((C)<<2) | (B)),	      \
				       (__v8bf) _mm_setzero_si128 (),	      \
				       (__mmask8) (-1)))

#define _mm_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantbf16128_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantbf16128_mask ((B), (int)(((C)<<2) | (D)),	      \
				       (__v8bf) _mm_setzero_si128 (), (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclassbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fpclass_pbh_mask (__mmask16 __U, __m256bh __A,
			      const int __imm)
{
  return (__mmask16)
    __builtin_ia32_fpclassbf16256_mask (__A, __imm, __U);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fpclass_pbh_mask (__m256bh __A, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_fpclassbf16256_mask (__A, __imm, (__mmask16) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fpclass_pbh_mask (__mmask8 __U, __m128bh __A, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_fpclassbf16128_mask (__A, __imm, __U);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fpclass_pbh_mask (__m128bh __A, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_fpclassbf16128_mask (__A, __imm, (__mmask8) -1);
}

#else
#define _mm256_mask_fpclass_pbh_mask(U, A, B)			      \
  ((__mmask16) __builtin_ia32_fpclassbf16256_mask ((A), (B), (U)))

#define _mm256_fpclass_pbh_mask(A, B)				      \
  ((__mmask16) __builtin_ia32_fpclassbf16256_mask ((A), (B),	      \
						   (__mmask16) (-1)))

#define _mm_mask_fpclass_pbh_mask(U, A, B)			      \
  ((__mmask8) __builtin_ia32_fpclassbf16128_mask ((A), (B), (U)))

#define _mm_fpclass_pbh_mask(A, B)				      \
  ((__mmask8) __builtin_ia32_fpclassbf16128_mask ((A), (B),	      \
						  (__mmask8) (-1)))

#endif /* __OPIMTIZE__ */


/* Intrinsics vcmpbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cmp_pbh_mask (__mmask16 __U, __m256bh __A,
			  __m256bh __B, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_cmpbf16256_mask (__A, __B, __imm, __U);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cmp_pbh_mask (__m256bh __A, __m256bh __B, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_cmpbf16256_mask (__A, __B, __imm, (__mmask16) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_pbh_mask (__mmask8 __U, __m128bh __A,
		       __m128bh __B, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_cmpbf16128_mask (__A, __B, __imm, __U);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_pbh_mask (__m128bh __A, __m128bh __B, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_cmpbf16128_mask (__A, __B, __imm, (__mmask8) -1);
}

#else
#define _mm256_mask_cmp_pbh_mask(A, B, C, D)			      \
  ((__mmask16) __builtin_ia32_cmpbf16256_mask ((B), (C), (D), (A)))

#define _mm256_cmp_pbh_mask(A, B, C)				      \
  ((__mmask16) __builtin_ia32_cmpbf16256_mask ((A), (B), (C),	      \
					       (__mmask16) (-1)))

#define _mm_mask_cmp_pbh_mask(A, B, C, D)			      \
  ((__mmask8) __builtin_ia32_cmpbf16128_mask ((B), (C), (D), (A)))

#define _mm_cmp_pbh_mask(A, B, C)				      \
  ((__mmask8) __builtin_ia32_cmpbf16128_mask ((A), (B), (C),	      \
					      (__mmask8) (-1)))

#endif /* __OPIMTIZE__ */

/* Intrinsics vcomisbf16.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comieq_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16eq (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comilt_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16lt (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comile_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16le (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comigt_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16gt (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comige_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16ge (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comineq_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomisbf16neq (__A, __B);
}

#ifdef __DISABLE_AVX10_2__
#undef __DISABLE_AVX10_2__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2__ */

#endif /* __AVX10_2BF16INTRIN_H_INCLUDED */
