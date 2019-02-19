/* Copyright (C) 2019 Free Software Foundation, Inc.

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
#error "Never use <avx512fp16vlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef __AVX512FP16VLINTRIN_H_INCLUDED
#define __AVX512FP16VLINTRIN_H_INCLUDED

#if !defined(__AVX512VL__) || !defined(__AVX512FP16__)
#pragma GCC push_options
#pragma GCC target("avx512fp16,avx512vl")
#define __DISABLE_AVX512FP16VL__
#endif /* __AVX512FP16VL__ */

/* Intrinsics v[add,sub,mul,div]ph.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_ph (__m128h __A, __m128h __B)
{
  return (__m128h) ((__v8hf) __A + (__v8hf) __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_add_ph (__m256h __A, __m256h __B)
{
  return (__m256h) ((__v16hf) __A + (__v16hf) __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_add_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vaddph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_add_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vaddph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vaddph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_add_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vaddph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sub_ph (__m128h __A, __m128h __B)
{
  return (__m128h) ((__v8hf) __A - (__v8hf) __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_sub_ph (__m256h __A, __m256h __B)
{
  return (__m256h) ((__v16hf) __A - (__v16hf) __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sub_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vsubph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_sub_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vsubph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vsubph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_sub_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vsubph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mul_ph (__m128h __A, __m128h __B)
{
  return (__m128h) ((__v8hf) __A * (__v8hf) __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mul_ph (__m256h __A, __m256h __B)
{
  return (__m256h) ((__v16hf) __A * (__v16hf) __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mul_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vmulph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_mul_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vmulph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vmulph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_mul_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vmulph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_div_ph (__m128h __A, __m128h __B)
{
  return (__m128h) ((__v8hf) __A / (__v8hf) __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_div_ph (__m256h __A, __m256h __B)
{
  return (__m256h) ((__v16hf) __A / (__v16hf) __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_div_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vdivph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_div_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vdivph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vdivph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_div_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vdivph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

/* Intrinsics v[max,min]ph.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_ph (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vmaxph_v8hf_mask (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_max_ph (__m256h __A, __m256h __B)
{
  return __builtin_ia32_vmaxph_v16hf_mask (__A, __B,
					  _mm256_setzero_ph (),
					  (__mmask16) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vmaxph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_max_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vmaxph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vmaxph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_max_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vmaxph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_ph (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vminph_v8hf_mask (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_min_ph (__m256h __A, __m256h __B)
{
  return __builtin_ia32_vminph_v16hf_mask (__A, __B,
					  _mm256_setzero_ph (),
					  (__mmask16) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_ph (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vminph_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_min_ph (__m256h __A, __mmask16 __B, __m256h __C, __m256h __D)
{
  return __builtin_ia32_vminph_v16hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_ph (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vminph_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_min_ph (__mmask16 __A, __m256h __B, __m256h __C)
{
  return __builtin_ia32_vminph_v16hf_mask (__B, __C,
					   _mm256_setzero_ph (), __A);
}

/* vcmpph */
#ifdef __OPTIMIZE
extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_ph_mask (__m128h __A, __m128h __B, const int __C)
{
  return (__mmask8) __builtin_ia32_vcmpph_v8hf_mask (__A, __B, __C,
						     (__mmask8) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_ph_mask (__mmask8 __A, __m128h __B, __m128h __C,
		      const int __D)
{
  return (__mmask8) __builtin_ia32_vcmpph_v8hf_mask (__B, __C, __D, __A);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_ph_mask (__m256h __A, __m256h __B, const int __C)
{
  return (__mmask16) __builtin_ia32_vcmpph_v16hf_mask (__A, __B, __C,
						       (__mmask16) -1);
}

extern __inline __mmask16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_ph_mask (__mmask16 __A, __m256h __B, __m256h __C,
		      const int __D)
{
  return (__mmask16) __builtin_ia32_vcmpph_v16hf_mask (__B, __C, __D,
						       __A);
}

#else
#define _mm_cmp_ph_mask(A, B, C)		\
  (__builtin_ia32_vcmpph_v8hf_mask ((A), (B), (C), (-1)))

#define _mm_mask_cmp_ph_mask(A, B, C, D)	\
  (__builtin_ia32_vcmpph_v8hf_mask ((B), (C), (D), (A)))

#define _mm256_cmp_ph_mask(A, B, C)		\
  (__builtin_ia32_vcmpph_v16hf_mask ((A), (B), (C), (-1)))

#define _mm256_mask_cmp_ph_mask(A, B, C, D)	\
  (__builtin_ia32_vcmpph_v16hf_mask ((B), (C), (D), (A)))

#endif /* __OPTIMIZE__ */

#ifdef __DISABLE_AVX512FP16VL__
#undef __DISABLE_AVX512FP16VL__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512FP16VL__ */

#endif /* __AVX512FP16VLINTRIN_H_INCLUDED */
