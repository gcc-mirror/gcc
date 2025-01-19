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

#if !defined(__AVX10_2_256__)
#pragma GCC push_options
#pragma GCC target("avx10.2")
#define __DISABLE_AVX10_2_256__
#endif /* __AVX10_2_256__ */

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_addne_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_addnepbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_addne_pbh (__m256bh __W, __mmask16 __U,
		       __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_addnepbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_addne_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_addnepbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_addne_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_addnepbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_addne_pbh (__m128bh __W, __mmask8 __U,
		    __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_addnepbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_addne_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_addnepbf16128_mask (__A, __B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_subne_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_subnepbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_subne_pbh (__m256bh __W, __mmask16 __U,
		       __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_subnepbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_subne_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_subnepbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_subne_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_subnepbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_subne_pbh (__m128bh __W, __mmask8 __U,
		    __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_subnepbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_subne_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_subnepbf16128_mask (__A, __B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mulne_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_mulnepbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mulne_pbh (__m256bh __W, __mmask16 __U,
		       __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_mulnepbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mulne_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_mulnepbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mulne_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_mulnepbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mulne_pbh (__m128bh __W, __mmask8 __U,
		    __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_mulnepbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mulne_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_mulnepbf16128_mask (__A, __B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_divne_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_divnepbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_divne_pbh (__m256bh __W, __mmask16 __U,
		       __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_divnepbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_divne_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_divnepbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_divne_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_divnepbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_divne_pbh (__m128bh __W, __mmask8 __U,
		    __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_divnepbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_divne_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_divnepbf16128_mask (__A, __B,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_maxpbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_pbh (__m256bh __W, __mmask16 __U,
		       __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_maxpbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_maxpbf16256_mask (__A, __B,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_maxpbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_pbh (__m128bh __W, __mmask8 __U,
		    __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_maxpbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_maxpbf16128_mask (__A, __B,
				     (__v8bf) _mm_setzero_si128 (),
				     __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_minpbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_pbh (__m256bh __W, __mmask16 __U,
		     __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_minpbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_minpbf16256_mask (__A, __B,
				     (__v16bf) _mm256_setzero_si256 (),
				     __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_minpbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_pbh (__m128bh __W, __mmask8 __U,
		  __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_minpbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_minpbf16128_mask (__A, __B,
				     (__v8bf) _mm_setzero_si128 (),
				     __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_scalef_pbh (__m256bh __A, __m256bh __B)
{
  return (__m256bh) __builtin_ia32_scalefpbf16256 (__A, __B);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_scalef_pbh (__m256bh __W, __mmask16 __U,
			__m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_scalefpbf16256_mask (__A, __B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_scalef_pbh (__mmask16 __U, __m256bh __A, __m256bh __B)
{
  return (__m256bh)
    __builtin_ia32_scalefpbf16256_mask (__A, __B,
					(__v16bf) _mm256_setzero_si256 (),
					__U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_pbh (__m128bh __A, __m128bh __B)
{
  return (__m128bh) __builtin_ia32_scalefpbf16128 (__A, __B);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_pbh (__m128bh __W, __mmask8 __U,
		     __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_scalefpbf16128_mask (__A, __B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_pbh (__mmask8 __U, __m128bh __A, __m128bh __B)
{
  return (__m128bh)
    __builtin_ia32_scalefpbf16128_mask (__A, __B,
					(__v8bf) _mm_setzero_si128 (),
					__U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmaddne_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddnepbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmaddne_pbh (__m256bh __A, __mmask16 __U,
			 __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddnepbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmaddne_pbh (__m256bh __A, __m256bh __B,
			  __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fmaddnepbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmaddne_pbh (__mmask16 __U, __m256bh __A,
			  __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmaddnepbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fmaddne_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddnepbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fmaddne_pbh (__m128bh __A, __mmask8 __U,
		      __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddnepbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fmaddne_pbh (__m128bh __A, __m128bh __B,
		       __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fmaddnepbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fmaddne_pbh (__mmask8 __U, __m128bh __A,
		       __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmaddnepbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fmsubne_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmsubnepbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fmsubne_pbh (__m256bh __A, __mmask16 __U,
			 __m256bh __B, __m256bh __C)
{
  return (__m256bh) __builtin_ia32_fmsubnepbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fmsubne_pbh (__m256bh __A, __m256bh __B,
			  __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fmsubnepbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fmsubne_pbh (__mmask16 __U, __m256bh __A,
			  __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fmsubnepbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fmsubne_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubnepbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fmsubne_pbh (__m128bh __A, __mmask8 __U,
		      __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubnepbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fmsubne_pbh (__m128bh __A, __m128bh __B,
		       __m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fmsubnepbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fmsubne_pbh (__mmask8 __U, __m128bh __A,
		       __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fmsubnepbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmaddne_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddnepbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmaddne_pbh (__m256bh __A, __mmask16 __U,
			  __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddnepbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmaddne_pbh (__m256bh __A, __m256bh __B,
			   __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fnmaddnepbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmaddne_pbh (__mmask16 __U, __m256bh __A,
			   __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmaddnepbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fnmaddne_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddnepbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fnmaddne_pbh (__m128bh __A, __mmask8 __U,
		       __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddnepbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fnmaddne_pbh (__m128bh __A, __m128bh __B,
			__m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fnmaddnepbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fnmaddne_pbh (__mmask8 __U, __m128bh __A,
			__m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmaddnepbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fnmsubne_pbh (__m256bh __A, __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubnepbf16256_mask (__A, __B, __C, (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fnmsubne_pbh (__m256bh __A, __mmask16 __U,
			  __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubnepbf16256_mask (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask3_fnmsubne_pbh (__m256bh __A, __m256bh __B,
			   __m256bh __C, __mmask16 __U)
{
  return (__m256bh)
    __builtin_ia32_fnmsubnepbf16256_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_fnmsubne_pbh (__mmask16 __U, __m256bh __A,
			   __m256bh __B, __m256bh __C)
{
  return (__m256bh)
    __builtin_ia32_fnmsubnepbf16256_maskz (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fnmsubne_pbh (__m128bh __A, __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubnepbf16128_mask (__A, __B, __C, (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fnmsubne_pbh (__m128bh __A, __mmask8 __U,
		       __m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubnepbf16128_mask (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask3_fnmsubne_pbh (__m128bh __A, __m128bh __B,
			__m128bh __C, __mmask8 __U)
{
  return (__m128bh)
    __builtin_ia32_fnmsubnepbf16128_mask3 (__A, __B, __C, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_fnmsubne_pbh (__mmask8 __U, __m128bh __A,
			__m128bh __B, __m128bh __C)
{
  return (__m128bh)
    __builtin_ia32_fnmsubnepbf16128_maskz (__A, __B, __C, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_rsqrt_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtpbf16256_mask (__A,
				       (__v16bf) _mm256_setzero_si256 (),
				       (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_rsqrt_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtpbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_rsqrt_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rsqrtpbf16256_mask (__A,
				       (__v16bf) _mm256_setzero_si256 (),
				       __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rsqrt_pbh (__m128bh __A)
{
  return (__m128bh)
	__builtin_ia32_rsqrtpbf16128_mask (__A,
				       	   (__v8bf) _mm_setzero_si128 (),
					   (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rsqrt_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rsqrtpbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rsqrt_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rsqrtpbf16128_mask (__A,
				       (__v8bf) _mm_setzero_si128 (),
				       __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sqrtne_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtnepbf16256_mask (__A,
				       	(__v16bf) _mm256_setzero_si256 (),
					(__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sqrtne_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtnepbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sqrtne_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_sqrtnepbf16256_mask (__A,
					(__v16bf) _mm256_setzero_si256 (),
					__U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrtne_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtnepbf16128_mask (__A,
				       	(__v8bf) _mm_setzero_si128 (),
					(__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrtne_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtnepbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrtne_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_sqrtnepbf16128_mask (__A,
					(__v8bf) _mm_setzero_si128 (),
					__U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_rcp_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcppbf16256_mask (__A,
				     (__v16bf) _mm256_setzero_si256 (),
				     (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_rcp_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcppbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_rcp_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_rcppbf16256_mask (__A,
				     (__v16bf) _mm256_setzero_si256 (),
				     __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rcp_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcppbf16128_mask (__A,
				     (__v8bf) _mm_setzero_si128 (),
				     (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rcp_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcppbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rcp_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_rcppbf16128_mask (__A,
				     (__v8bf) _mm_setzero_si128 (),
				     __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getexp_pbh (__m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexppbf16256_mask (__A,
					(__v16bf) _mm256_setzero_si256 (),
					(__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_getexp_pbh (__m256bh __W, __mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexppbf16256_mask (__A, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getexp_pbh (__mmask16 __U, __m256bh __A)
{
  return (__m256bh)
    __builtin_ia32_getexppbf16256_mask (__A,
					(__v16bf) _mm256_setzero_si256 (),
					__U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getexp_pbh (__m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexppbf16128_mask (__A,
				       	(__v8bf) _mm_setzero_si128 (),
					(__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getexp_pbh (__m128bh __W, __mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexppbf16128_mask (__A, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getexp_pbh (__mmask8 __U, __m128bh __A)
{
  return (__m128bh)
    __builtin_ia32_getexppbf16128_mask (__A,
					(__v8bf) _mm_setzero_si128 (),
					__U);
}

/* Intrinsics vrndscalepbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_roundscalene_pbh (__m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalenepbf16256_mask (__A, B,
					    (__v16bf) _mm256_setzero_si256 (),
					    (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_roundscalene_pbh (__m256bh __W, __mmask16 __U,
			      __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalenepbf16256_mask (__A, B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_roundscalene_pbh (__mmask16 __U, __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_rndscalenepbf16256_mask (__A, B,
					    (__v16bf) _mm256_setzero_si256 (),
					    __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscalene_pbh (__m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalenepbf16128_mask (__A, B,
					    (__v8bf) _mm_setzero_si128 (),
					    (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscalene_pbh (__m128bh __W, __mmask8 __U,
			   __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalenepbf16128_mask (__A, B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscalene_pbh (__mmask8 __U, __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_rndscalenepbf16128_mask (__A, B,
					    (__v8bf) _mm_setzero_si128 (),
					    __U);
}

#else
#define _mm256_roundscalene_pbh(A, B)					      \
  (__builtin_ia32_rndscalenepbf16256_mask ((A), (B),			      \
					   (__v16bf) _mm256_setzero_si256 (), \
					   (__mmask16) -1))

#define _mm256_mask_roundscalene_pbh(A, B, C, D)	    		      \
  (__builtin_ia32_rndscalenepbf16256_mask ((C), (D), (A), (B)))

#define _mm256_maskz_roundscalene_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalenepbf16256_mask ((B), (C),			      \
					   (__v16bf) _mm256_setzero_si256 (), \
					   (A)))

#define _mm_roundscalene_pbh(A, B)					      \
  (__builtin_ia32_rndscalenepbf16128_mask ((A), (B),			      \
					   (__v8bf) _mm_setzero_si128 (),     \
					   (__mmask8) -1))

#define _mm_mask_roundscalene_pbh(A, B, C, D)				      \
  (__builtin_ia32_rndscalenepbf16128_mask ((C), (D), (A), (B)))

#define _mm_maskz_roundscalene_pbh(A, B, C)				      \
  (__builtin_ia32_rndscalenepbf16128_mask ((B), (C),			      \
					   (__v8bf) _mm_setzero_si128 (),     \
					   (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducepbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_reducene_pbh (__m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducenepbf16256_mask (__A, B,
					  (__v16bf) _mm256_setzero_si256 (),
					  (__mmask16) -1);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_reducene_pbh (__m256bh __W, __mmask16 __U,
			  __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducenepbf16256_mask (__A, B, __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_reducene_pbh (__mmask16 __U, __m256bh __A, int B)
{
  return (__m256bh)
    __builtin_ia32_reducenepbf16256_mask (__A, B,
					  (__v16bf) _mm256_setzero_si256 (),
					  __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reducene_pbh (__m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducenepbf16128_mask (__A, B,
					  (__v8bf) _mm_setzero_si128 (),
					  (__mmask8) -1);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reducene_pbh (__m128bh __W, __mmask8 __U,
		       __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducenepbf16128_mask (__A, B, __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reducene_pbh (__mmask8 __U, __m128bh __A, int B)
{
  return (__m128bh)
    __builtin_ia32_reducenepbf16128_mask (__A, B,
					  (__v8bf) _mm_setzero_si128 (),
					  __U);
}

#else
#define _mm256_reducene_pbh(A, B)					      \
  (__builtin_ia32_reducenepbf16256_mask ((A), (B),			      \
					 (__v16bf) _mm256_setzero_si256 (),   \
					 (__mmask16) -1))

#define _mm256_mask_reducene_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducenepbf16256_mask ((C), (D), (A), (B)))

#define _mm256_maskz_reducene_pbh(A, B, C)				      \
  (__builtin_ia32_reducenepbf16256_mask ((B), (C),			      \
					 (__v16bf) _mm256_setzero_si256 (),   \
					 (A)))

#define _mm_reducene_pbh(A, B)						      \
  (__builtin_ia32_reducenepbf16128_mask ((A), (B),			      \
					 (__v8bf) _mm_setzero_si128 (),       \
					 (__mmask8) -1))

#define _mm_mask_reducene_pbh(A, B, C, D)				      \
  (__builtin_ia32_reducenepbf16128_mask ((C), (D), (A), (B)))

#define _mm_maskz_reducene_pbh(A, B, C)					      \
  (__builtin_ia32_reducenepbf16128_mask ((B), (C),			      \
					 (__v8bf) _mm_setzero_si128 (),       \
					 (A)))

#endif /* __OPTIMIZE__ */


/* Intrinsics vgetmantpbf16.  */
#ifdef __OPTIMIZE__
extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_getmant_pbh (__m256bh __A, _MM_MANTISSA_NORM_ENUM __B,
		    _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m256bh)
    __builtin_ia32_getmantpbf16256_mask (__A, (int) (__C << 2) | __B,
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
    __builtin_ia32_getmantpbf16256_mask (__A, (int) (__C << 2) | __B,
					 __W, __U);
}

extern __inline__ __m256bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_getmant_pbh (__mmask16 __U, __m256bh __A,
			  _MM_MANTISSA_NORM_ENUM __B,
			  _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m256bh)
    __builtin_ia32_getmantpbf16256_mask (__A, (int) (__C << 2) | __B,
					 (__v16bf) _mm256_setzero_si256 (),
					 __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getmant_pbh (__m128bh __A, _MM_MANTISSA_NORM_ENUM __B,
		 _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m128bh)
    __builtin_ia32_getmantpbf16128_mask (__A, (int) (__C << 2) | __B,
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
    __builtin_ia32_getmantpbf16128_mask (__A, (int) (__C << 2) | __B,
					 __W, __U);
}

extern __inline__ __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getmant_pbh (__mmask8 __U, __m128bh __A,
		       _MM_MANTISSA_NORM_ENUM __B,
		       _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m128bh)
    __builtin_ia32_getmantpbf16128_mask (__A, (int) (__C << 2) | __B,
					 (__v8bf) _mm_setzero_si128 (),
					 __U);
}

#else
#define _mm256_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantpbf16256_mask ((A), (int)(((C)<<2) | (B)),	      \
					   (__v16bf) _mm256_setzero_si256 (), \
					   (__mmask16) (-1)))

#define _mm256_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantpbf16256_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm256_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantpbf16256_mask ((B), (int)(((C)<<2) | (D)),	      \
					   (__v16bf) _mm256_setzero_si256 (), \
					   (A)))

#define _mm_getmant_pbh(A, B, C)					      \
  (__builtin_ia32_getmantpbf16128_mask ((A), (int)(((C)<<2) | (B)),	      \
					(__v8bf) _mm_setzero_si128 (),	      \
					(__mmask8) (-1)))

#define _mm_mask_getmant_pbh(A, B, C, D, E)				      \
  (__builtin_ia32_getmantpbf16128_mask ((C), (int)(((D)<<2) | (E)), (A), (B)))

#define _mm_maskz_getmant_pbh(A, B, C, D)				      \
  (__builtin_ia32_getmantpbf16128_mask ((B), (int)(((C)<<2) | (D)),	      \
					(__v8bf) _mm_setzero_si128 (), (A)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclasspbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_fpclass_pbh_mask (__mmask16 __U, __m256bh __A,
				const int __imm)
{
  return (__mmask16)
    __builtin_ia32_fpclasspbf16256_mask (__A, __imm, __U);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_fpclass_pbh_mask (__m256bh __A, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_fpclasspbf16256_mask (__A, __imm, (__mmask16) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fpclass_pbh_mask (__mmask8 __U, __m128bh __A, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_fpclasspbf16128_mask (__A, __imm, __U);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fpclass_pbh_mask (__m128bh __A, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_fpclasspbf16128_mask (__A, __imm, (__mmask8) -1);
}

#else
#define _mm256_mask_fpclass_pbh_mask(U, A, B)			      \
  ((__mmask16) __builtin_ia32_fpclasspbf16256_mask ((A), (B), (U)))

#define _mm256_fpclass_pbh_mask(A, B)				      \
  ((__mmask16) __builtin_ia32_fpclasspbf16256_mask ((A), (B),	      \
						    (__mmask16) (-1)))

#define _mm_mask_fpclass_pbh_mask(U, A, B)			      \
  ((__mmask8) __builtin_ia32_fpclasspbf16128_mask ((A), (B), (U)))

#define _mm_fpclass_pbh_mask(A, B)				      \
  ((__mmask8) __builtin_ia32_fpclasspbf16128_mask ((A), (B),	      \
						   (__mmask8) (-1)))

#endif /* __OPIMTIZE__ */


/* Intrinsics vcmppbf16.  */
#ifdef __OPTIMIZE__
extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cmp_pbh_mask (__mmask16 __U, __m256bh __A,
			    __m256bh __B, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_cmppbf16256_mask (__A, __B, __imm, __U);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cmp_pbh_mask (__m256bh __A, __m256bh __B, const int __imm)
{
  return (__mmask16)
    __builtin_ia32_cmppbf16256_mask (__A, __B, __imm, (__mmask16) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_pbh_mask (__mmask8 __U, __m128bh __A,
			 __m128bh __B, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_cmppbf16128_mask (__A, __B, __imm, __U);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_pbh_mask (__m128bh __A, __m128bh __B, const int __imm)
{
  return (__mmask8)
    __builtin_ia32_cmppbf16128_mask (__A, __B, __imm, (__mmask8) -1);
}

#else
#define _mm256_mask_cmp_pbh_mask(A, B, C, D)			      \
  ((__mmask16) __builtin_ia32_cmppbf16256_mask ((B), (C), (D), (A)))

#define _mm256_cmp_pbh_mask(A, B, C)				      \
  ((__mmask16) __builtin_ia32_cmppbf16256_mask ((A), (B), (C),	      \
						(__mmask16) (-1)))

#define _mm_mask_cmp_pbh_mask(A, B, C, D)			      \
  ((__mmask8) __builtin_ia32_cmppbf16128_mask ((B), (C), (D), (A)))

#define _mm_cmp_pbh_mask(A, B, C)				      \
  ((__mmask8) __builtin_ia32_cmppbf16128_mask ((A), (B), (C),	      \
					       (__mmask8) (-1)))

#endif /* __OPIMTIZE__ */

/* Intrinsics vcomsbf16.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16eq (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16lt (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16le (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16gt (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16ge (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_sbh (__m128bh __A, __m128bh __B)
{
  return __builtin_ia32_vcomsbf16neq (__A, __B);
}

#ifdef __DISABLE_AVX10_2_256__
#undef __DISABLE_AVX10_2_256__
#pragma GCC pop_options
#endif /* __DISABLE_AVX10_2_256__ */

#endif /* __AVX10_2BF16INTRIN_H_INCLUDED */
