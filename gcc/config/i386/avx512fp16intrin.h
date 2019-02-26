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
#error "Never use <avx512fp16intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef __AVX512FP16INTRIN_H_INCLUDED
#define __AVX512FP16INTRIN_H_INCLUDED

#ifndef __AVX512FP16__
#pragma GCC push_options
#pragma GCC target("avx512fp16")
#define __DISABLE_AVX512FP16__
#endif /* __AVX512FP16__ */

/* Internal data types for implementing the intrinsics.  */
typedef _Float16 __v8hf __attribute__ ((__vector_size__ (16)));
typedef _Float16 __v16hf __attribute__ ((__vector_size__ (32)));
typedef _Float16 __v32hf __attribute__ ((__vector_size__ (64)));

/* The Intel API is flexible enough that we must allow aliasing with other
   vector types, and their scalar components.  */
typedef _Float16 __m128h __attribute__ ((__vector_size__ (16), __may_alias__));
typedef _Float16 __m256h __attribute__ ((__vector_size__ (32), __may_alias__));
typedef _Float16 __m512h __attribute__ ((__vector_size__ (64), __may_alias__));

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_ph (_Float16 __A7, _Float16 __A6, _Float16 __A5,
	    _Float16 __A4, _Float16 __A3, _Float16 __A2,
	    _Float16 __A1, _Float16 __A0)
{
  return __extension__ (__m128h)(__v8hf){ __A0, __A1, __A2, __A3,
					  __A4, __A5, __A6, __A7 };
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_set_ph (_Float16 __A15, _Float16 __A14, _Float16 __A13,
	       _Float16 __A12, _Float16 __A11, _Float16 __A10,
	       _Float16 __A9, _Float16 __A8, _Float16 __A7,
	       _Float16 __A6, _Float16 __A5, _Float16 __A4,
	       _Float16 __A3, _Float16 __A2, _Float16 __A1,
	       _Float16 __A0)
{
  return __extension__ (__m256h)(__v16hf){ __A0, __A1, __A2, __A3,
					   __A4, __A5, __A6, __A7,
					   __A8, __A9, __A10, __A11,
					   __A12, __A13, __A14, __A15 };
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_set_ph (_Float16 __A31, _Float16 __A30, _Float16 __A29,
	       _Float16 __A28, _Float16 __A27, _Float16 __A26,
	       _Float16 __A25, _Float16 __A24, _Float16 __A23,
	       _Float16 __A22, _Float16 __A21, _Float16 __A20,
	       _Float16 __A19, _Float16 __A18, _Float16 __A17,
	       _Float16 __A16, _Float16 __A15, _Float16 __A14,
	       _Float16 __A13, _Float16 __A12, _Float16 __A11,
	       _Float16 __A10, _Float16 __A9, _Float16 __A8,
	       _Float16 __A7, _Float16 __A6, _Float16 __A5,
	       _Float16 __A4, _Float16 __A3, _Float16 __A2,
	       _Float16 __A1, _Float16 __A0)
{
  return __extension__ (__m512h)(__v32hf){ __A0, __A1, __A2, __A3,
					   __A4, __A5, __A6, __A7,
					   __A8, __A9, __A10, __A11,
					   __A12, __A13, __A14, __A15,
					   __A16, __A17, __A18, __A19,
					   __A20, __A21, __A22, __A23,
					   __A24, __A25, __A26, __A27,
					   __A28, __A29, __A30, __A31 };
}

/* Create vectors of elements in the reversed order from _mm_set_ph,
   _mm256_set_ph and _mm512_set_ph functions.  */

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_setr_ph (_Float16 __A0, _Float16 __A1, _Float16 __A2,
	     _Float16 __A3, _Float16 __A4, _Float16 __A5,
	     _Float16 __A6, _Float16 __A7)
{
  return _mm_set_ph (__A7, __A6, __A5, __A4, __A3, __A2, __A1, __A0);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_setr_ph (_Float16 __A0, _Float16 __A1, _Float16 __A2,
		_Float16 __A3, _Float16 __A4, _Float16 __A5,
		_Float16 __A6, _Float16 __A7, _Float16 __A8,
		_Float16 __A9, _Float16 __A10, _Float16 __A11,
		_Float16 __A12, _Float16 __A13, _Float16 __A14,
		_Float16 __A15)
{
  return _mm256_set_ph (__A15, __A14, __A13, __A12, __A11, __A10, __A9,
			__A8, __A7, __A6, __A5, __A4, __A3, __A2, __A1,
			__A0);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_setr_ph (_Float16 __A0, _Float16 __A1, _Float16 __A2,
		_Float16 __A3, _Float16 __A4, _Float16 __A5,
		_Float16 __A6, _Float16 __A7, _Float16 __A8,
		_Float16 __A9, _Float16 __A10, _Float16 __A11,
		_Float16 __A12, _Float16 __A13, _Float16 __A14,
		_Float16 __A15, _Float16 __A16, _Float16 __A17,
		_Float16 __A18, _Float16 __A19, _Float16 __A20,
		_Float16 __A21, _Float16 __A22, _Float16 __A23,
		_Float16 __A24, _Float16 __A25, _Float16 __A26,
		_Float16 __A27, _Float16 __A28, _Float16 __A29,
		_Float16 __A30, _Float16 __A31)

{
  return _mm512_set_ph (__A31, __A30, __A29, __A28, __A27, __A26, __A25,
			__A24, __A23, __A22, __A21, __A20, __A19, __A18,
			__A17, __A16, __A15, __A14, __A13, __A12, __A11,
			__A10, __A9, __A8, __A7, __A6, __A5, __A4, __A3,
			__A2, __A1, __A0);
}

/* Broadcast _Float16 to vector.  */

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set1_ph (_Float16 __A)
{
  return _mm_set_ph (__A, __A, __A, __A, __A, __A, __A, __A);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_set1_ph (_Float16 __A)
{
  return _mm256_set_ph (__A, __A, __A, __A, __A, __A, __A, __A,
			__A, __A, __A, __A, __A, __A, __A, __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_set1_ph (_Float16 __A)
{
  return _mm512_set_ph (__A, __A, __A, __A, __A, __A, __A, __A,
			__A, __A, __A, __A, __A, __A, __A, __A,
			__A, __A, __A, __A, __A, __A, __A, __A,
			__A, __A, __A, __A, __A, __A, __A, __A);
}

/* Create a vector with all zeros.  */

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_setzero_ph (void)
{
  return _mm_set1_ph (0.0f);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_setzero_ph (void)
{
  return _mm256_set1_ph (0.0f);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_setzero_ph (void)
{
  return _mm512_set1_ph (0.0f);
}

/* Create a vector with element 0 as F and the rest zero.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_sh (_Float16 __F)
{
  return _mm_set_ph (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, __F);
}

/* Create a vector with element 0 as *P and the rest zero.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_load_sh (void const *__P)
{
  return _mm_set_ph (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		     *(_Float16 const *) __P);
}

/* Stores the lower _Float16 value.  */
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_store_sh (void *__P, __m128h __A)
{
  *(_Float16 *) __P = ((__v8hf)__A)[0];
}

/* Intrinsics v[add,sub,mul,div]ph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_add_ph (__m512h __A, __m512h __B)
{
  return (__m512h) ((__v32hf) __A + (__v32hf) __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_add_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vaddph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_add_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vaddph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sub_ph (__m512h __A, __m512h __B)
{
  return (__m512h) ((__v32hf) __A - (__v32hf) __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sub_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vsubph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sub_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vsubph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mul_ph (__m512h __A, __m512h __B)
{
  return (__m512h) ((__v32hf) __A * (__v32hf) __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mul_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vmulph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mul_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vmulph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_div_ph (__m512h __A, __m512h __B)
{
  return (__m512h) ((__v32hf) __A / (__v32hf) __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_div_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vdivph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_div_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vdivph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_add_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vaddph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_add_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vaddph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_add_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vaddph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sub_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vsubph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sub_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vsubph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sub_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vsubph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mul_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vmulph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mul_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vmulph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mul_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vmulph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_div_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vdivph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_div_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vdivph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_div_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vdivph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}
#else
#define _mm512_add_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_vaddph_v32hf_mask_round((A), (B),		\
						   _mm512_setzero_ph (),\
						   (__mmask32)-1, (C)))

#define _mm512_mask_add_round_ph(A, B, C, D, E)			\
  ((__m512h)__builtin_ia32_vaddph_v32hf_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_add_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_vaddph_v32hf_mask_round((B), (C),		\
						   _mm512_setzero_ph (),\
						   (A), (D)))

#define _mm512_sub_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_vsubph_v32hf_mask_round((A), (B),		\
						   _mm512_setzero_ph (),\
						   (__mmask32)-1, (C)))

#define _mm512_mask_sub_round_ph(A, B, C, D, E)			\
  ((__m512h)__builtin_ia32_vsubph_v32hf_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_sub_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_vsubph_v32hf_mask_round((B), (C),		\
						   _mm512_setzero_ph (),\
						   (A), (D)))

#define _mm512_mul_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_vmulph_v32hf_mask_round((A), (B),		\
						   _mm512_setzero_ph (),\
						   (__mmask32)-1, (C)))

#define _mm512_mask_mul_round_ph(A, B, C, D, E)			\
  ((__m512h)__builtin_ia32_vmulph_v32hf_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_mul_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_vmulph_v32hf_mask_round((B), (C),		\
						   _mm512_setzero_ph (),\
						   (A), (D)))

#define _mm512_div_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_vdivph_v32hf_mask_round((A), (B),		\
						   _mm512_setzero_ph (),\
						   (__mmask32)-1, (C)))

#define _mm512_mask_div_round_ph(A, B, C, D, E)			\
  ((__m512h)__builtin_ia32_vdivph_v32hf_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_div_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_vdivph_v32hf_mask_round((B), (C),		\
						   _mm512_setzero_ph (),\
						   (A), (D)))
#endif  /* __OPTIMIZE__  */

/* Intrinsics of v[add,sub,mul,div]sh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_sh (__m128h __A, __m128h __B)
{
  __A[0] += __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_add_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vaddsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vaddsh_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sub_sh (__m128h __A, __m128h __B)
{
  __A[0] -= __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sub_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vsubsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vsubsh_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mul_sh (__m128h __A, __m128h __B)
{
  __A[0] *= __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mul_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vmulsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vmulsh_v8hf_mask (__B, __C, _mm_setzero_ph (), __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_div_sh (__m128h __A, __m128h __B)
{
  __A[0] /= __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_div_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vdivsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vdivsh_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vaddsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_add_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vaddsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vaddsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sub_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vsubsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sub_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vsubsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vsubsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mul_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vmulsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mul_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vmulsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vmulsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_div_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vdivsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_div_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vdivsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vdivsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}
#else
#define _mm_add_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_vaddsh_v8hf_mask_round ((A), (B),		\
						   _mm_setzero_ph (),	\
						   (__mmask8)-1, (C)))

#define _mm_mask_add_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_vaddsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_add_round_sh(A, B, C, D)				\
  ((__m128h)__builtin_ia32_vaddsh_v8hf_mask_round ((B), (C),		\
						   _mm_setzero_ph (),	\
						   (A), (D)))

#define _mm_sub_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_vsubsh_v8hf_mask_round ((A), (B),		\
						   _mm_setzero_ph (),	\
						   (__mmask8)-1, (C)))

#define _mm_mask_sub_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_vsubsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_sub_round_sh(A, B, C, D)				\
  ((__m128h)__builtin_ia32_vsubsh_v8hf_mask_round ((B), (C),		\
						   _mm_setzero_ph (),	\
						   (A), (D)))

#define _mm_mul_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_vmulsh_v8hf_mask_round ((A), (B),		\
						   _mm_setzero_ph (),	\
						   (__mmask8)-1, (C)))

#define _mm_mask_mul_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_vmulsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_mul_round_sh(A, B, C, D)				\
  ((__m128h)__builtin_ia32_vmulsh_v8hf_mask_round ((B), (C),		\
						   _mm_setzero_ph (),	\
						   (A), (D)))

#define _mm_div_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_vdivsh_v8hf_mask_round ((A), (B),		\
						   _mm_setzero_ph (),	\
						   (__mmask8)-1, (C)))

#define _mm_mask_div_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_vdivsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_div_round_sh(A, B, C, D)				\
  ((__m128h)__builtin_ia32_vdivsh_v8hf_mask_round ((B), (C),		\
						   _mm_setzero_ph (),	\
						   (A), (D)))
#endif /* __OPTIMIZE__ */

/* Intrinsic vmaxph vminph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_vmaxph_v32hf_mask (__A, __B,
					   _mm512_setzero_ph (),
					   (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vmaxph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vmaxph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_vminph_v32hf_mask (__A, __B,
					   _mm512_setzero_ph (),
					   (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vminph_v32hf_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vminph_v32hf_mask (__B, __C,
					   _mm512_setzero_ph (), __A);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vmaxph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vmaxph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vmaxph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vminph_v32hf_mask_round (__A, __B,
						 _mm512_setzero_ph (),
						 (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_vminph_v32hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vminph_v32hf_mask_round (__B, __C,
						 _mm512_setzero_ph (),
						 __A, __D);
}

#else
#define _mm512_max_round_ph(A, B, C)					\
  (__builtin_ia32_vmaxph_v32hf_mask_round ((A), (B),			\
					   _mm512_setzero_ph (),	\
					   (__mmask32)-1, (C)))

#define _mm512_mask_max_round_ph(A, B, C, D, E)				\
  (__builtin_ia32_vmaxph_v32hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_max_round_ph(A, B, C, D)				\
  (__builtin_ia32_vmaxph_v32hf_mask_round ((B), (C),			\
					   _mm512_setzero_ph (),	\
					   (A), (D)))

#define _mm512_min_round_ph(A, B, C)					\
  (__builtin_ia32_vminph_v32hf_mask_round ((A), (B),			\
					   _mm512_setzero_ph (),	\
					   (__mmask32)-1, (C)))

#define _mm512_mask_min_round_ph(A, B, C, D, E)				\
  (__builtin_ia32_vminph_v32hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_min_round_ph(A, B, C, D)				\
  (__builtin_ia32_vminph_v32hf_mask_round ((B), (C),			\
					   _mm512_setzero_ph (),	\
					   (A), (D)))
#endif /* __OPTIMIZE__ */

/* Intrinsic vmaxsh vminsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_sh (__m128h __A, __m128h __B)
{
  __A[0] = __A[0] > __B[0] ? __A[0] : __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vmaxsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vmaxsh_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_sh (__m128h __A, __m128h __B)
{
  __A[0] = __A[0] < __B[0] ? __A[0] : __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vminsh_v8hf_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vminsh_v8hf_mask (__B, __C, _mm_setzero_ph (),
					  __A);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vmaxsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vmaxsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vmaxsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vminsh_v8hf_mask_round (__A, __B,
						_mm_setzero_ph (),
						(__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_vminsh_v8hf_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_vminsh_v8hf_mask_round (__B, __C,
						_mm_setzero_ph (),
						__A, __D);
}

#else
#define _mm_max_round_sh(A, B, C)					\
  (__builtin_ia32_vmaxsh_v8hf_mask_round ((A), (B),			\
					  _mm_setzero_ph (),		\
					  (__mmask8)-1, (C)))

#define _mm_mask_max_round_sh(A, B, C, D, E)				\
  (__builtin_ia32_vmaxsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_max_round_sh(A, B, C, D)				\
  (__builtin_ia32_vmaxsh_v8hf_mask_round ((B), (C),			\
					  _mm_setzero_ph (),		\
					  (A), (D)))

#define _mm_min_round_sh(A, B, C)					\
  (__builtin_ia32_vminsh_v8hf_mask_round ((A), (B),			\
					  _mm_setzero_ph (),		\
					  (__mmask8)-1, (C)))

#define _mm_mask_min_round_sh(A, B, C, D, E)				\
  (__builtin_ia32_vminsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_min_round_sh(A, B, C, D)				\
  (__builtin_ia32_vminsh_v8hf_mask_round ((B), (C),			\
					  _mm_setzero_ph (),		\
					  (A), (D)))

#endif /* __OPTIMIZE__ */

/* vcmpph */
#ifdef __OPTIMIZE
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_ph_mask (__m512h __A, __m512h __B, const int __C)
{
  return (__mmask32) __builtin_ia32_vcmpph_v32hf_mask (__A, __B, __C,
						       (__mmask32) -1);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_ph_mask (__mmask32 __A, __m512h __B, __m512h __C,
			 const int __D)
{
  return (__mmask32) __builtin_ia32_vcmpph_v32hf_mask (__B, __C, __D,
						       __A);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_round_ph_mask (__m512h __A, __m512h __B, const int __C,
			  const int __D)
{
  return (__mmask32) __builtin_ia32_vcmpph_v32hf_mask_round (__A, __B,
							     __C, (__mmask32) -1,
							     __D);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_round_ph_mask (__mmask32 __A, __m512h __B, __m512h __C,
			       const int __D, const int __E)
{
  return (__mmask32) __builtin_ia32_vcmpph_v32hf_mask_round (__B, __C,
							     __D, __A,
							     __E);
}

#else
#define _mm512_cmp_ph_mask(A, B, C)			\
  (__builtin_ia32_vcmpph_v32hf_mask ((A), (B), (C), (-1)))

#define _mm512_mask_cmp_ph_mask(A, B, C, D)		\
  (__builtin_ia32_vcmpph_v32hf_mask ((B), (C), (D), (A)))

#define _mm512_cmp_round_ph_mask(A, B, C, D)		\
  (__builtin_ia32_vcmpph_v32hf_mask_round ((A), (B), (C), (-1), (D)))

#define _mm512_mask_cmp_round_ph_mask(A, B, C, D, E)	\
  (__builtin_ia32_vcmpph_v32hf_mask_round ((B), (C), (D), (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcmpsh.  */
#ifdef __OPTIMIZE__
extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_sh_mask (__m128h __A, __m128h __B, const int __C)
{
  return (__mmask8)
    __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B,
					   __C, (__mmask8) -1,
					   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_sh_mask (__mmask8 __A, __m128h __B, __m128h __C,
		      const int __D)
{
  return (__mmask8)
    __builtin_ia32_vcmpsh_v8hf_mask_round (__B, __C,
					   __D, __A,
					   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_round_sh_mask (__m128h __A, __m128h __B, const int __C,
		       const int __D)
{
  return (__mmask8) __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B,
							   __C, (__mmask8) -1,
							   __D);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_round_sh_mask (__mmask8 __A, __m128h __B, __m128h __C,
			    const int __D, const int __E)
{
  return (__mmask8) __builtin_ia32_vcmpsh_v8hf_mask_round (__B, __C,
							   __D, __A,
							   __E);
}

#else
#define _mm_cmp_sh_mask(A, B, C)		\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((A), (B), (C), (-1), \
					  (_MM_FROUND_CUR_DIRECTION)))

#define _mm_mask_cmp_sh_mask(A, B, C, D)	\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((B), (C), (D), (A),		\
					  (_MM_FROUND_CUR_DIRECTION)))

#define _mm_cmp_round_sh_mask(A, B, C, D)				\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((A), (B), (C), (-1), (D)))

#define _mm_mask_cmp_round_sh_mask(A, B, C, D, E)	\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((B), (C), (D), (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcomish.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comieq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_EQ_OS,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comilt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_LT_OS,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comile_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_LE_OS,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comigt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_GT_OS,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comige_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_GE_OS,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comineq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_NEQ_US,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomieq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_EQ_OQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomilt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_LT_OQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomile_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_LE_OQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomigt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_GT_OQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomige_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_GE_OQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomineq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, _CMP_NEQ_UQ,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
  _mm_comi_sh (__m128h __A, __m128h __B, const int __P)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, __P,
						(__mmask8) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comi_round_sh (__m128h __A, __m128h __B, const int __P, const int __R)
{
  return __builtin_ia32_vcmpsh_v8hf_mask_round (__A, __B, __P,
						(__mmask8) -1,__R);
}

#else
#define _mm_comi_round_sh(A, B, P, R)		\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((A), (B), (P), (__mmask8) (-1), (R)))
#define _mm_comi_sh(A, B, P)		\
  (__builtin_ia32_vcmpsh_v8hf_mask_round ((A), (B), (P), (__mmask8) (-1), \
					  _MM_FROUND_CUR_DIRECTION))

#endif /* __OPTIMIZE__  */

/* Intrinsics vsqrtph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrt_ph (__m512h __A)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__A,
						  _mm512_setzero_ph(),
						  (__mmask32) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrt_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__C, __A, __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrt_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__B,
						  _mm512_setzero_ph (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrt_round_ph (__m512h __A, const int __B)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__A,
						  _mm512_setzero_ph(),
						  (__mmask32) -1, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrt_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__C, __A, __B, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrt_round_ph (__mmask32 __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vsqrtph_v32hf_mask_round (__B,
						  _mm512_setzero_ph (),
						  __A, __C);
}

#else
#define _mm512_sqrt_round_ph(A, B)					\
  (__builtin_ia32_vsqrtph_v32hf_mask_round ((A),			\
					    _mm512_setzero_ph (),	\
					    (__mmask32)-1, (B)))

#define _mm512_mask_sqrt_round_ph(A, B, C, D)				\
  (__builtin_ia32_vsqrtph_v32hf_mask_round ((C), (A), (B), (D)))

#define _mm512_maskz_sqrt_round_ph(A, B, C)				\
  (__builtin_ia32_vsqrtph_v32hf_mask_round ((B),			\
					    _mm512_setzero_ph (),	\
					    (A), (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrsqrtph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rsqrt_ph (__m512h __A)
{
  return __builtin_ia32_vrsqrtph_v32hf_mask (__A, _mm512_setzero_ph (),
					     (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rsqrt_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_vrsqrtph_v32hf_mask (__C, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rsqrt_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_vrsqrtph_v32hf_mask (__B, _mm512_setzero_ph (),
					     __A);
}

/* Intrinsics vrsqrtsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rsqrt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vrsqrtsh_v8hf_mask (__B, __A, _mm_setzero_ph (),
					    (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rsqrt_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vrsqrtsh_v8hf_mask (__D, __C, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rsqrt_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vrsqrtsh_v8hf_mask (__C, __B, _mm_setzero_ph (),
					    __A);
}

/* Intrinsics vsqrtsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__B, __A,
						 _mm_setzero_ph (),
						 (__mmask8) -1,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrt_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__D, __C, __A, __B,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrt_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__C, __B,
						 _mm_setzero_ph (),
						 __A, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrt_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__B, __A,
						 _mm_setzero_ph (),
						 (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrt_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			__m128h __D, const int __E)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__D, __C, __A, __B,
						 __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrt_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			 const int __D)
{
  return __builtin_ia32_vsqrtsh_v8hf_mask_round (__C, __B,
						 _mm_setzero_ph (),
						 __A, __D);
}

#else
#define _mm_sqrt_round_sh(A, B, C)				\
  (__builtin_ia32_vsqrtsh_v8hf_mask_round ((B), (A),		\
					   _mm_setzero_ph (),	\
					   (__mmask8)-1, (C)))

#define _mm_mask_sqrt_round_sh(A, B, C, D, E)			\
  (__builtin_ia32_vsqrtsh_v8hf_mask_round ((D), (C), (A), (B), (E)))

#define _mm_maskz_sqrt_round_sh(A, B, C, D)			\
  (__builtin_ia32_vsqrtsh_v8hf_mask_round ((C), (B),		\
					   _mm_setzero_ph (),	\
					   (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrcpph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rcp_ph (__m512h __A)
{
  return __builtin_ia32_vrcpph_v32hf_mask (__A, _mm512_setzero_ph (),
					   (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rcp_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_vrcpph_v32hf_mask (__C, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rcp_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_vrcpph_v32hf_mask (__B, _mm512_setzero_ph (),
					   __A);
}

/* Intrinsics vrcpsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rcp_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vrcpsh_v8hf_mask (__B, __A, _mm_setzero_ph (),
					  (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rcp_sh (__m128h __A, __mmask32 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vrcpsh_v8hf_mask (__D, __C, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rcp_sh (__mmask32 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vrcpsh_v8hf_mask (__C, __B, _mm_setzero_ph (),
					  __A);
}

/* Intrinsics vscalefph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__A, __B,
						    _mm512_setzero_ph (),
						    (__mmask32) -1,
						    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__C, __D, __A, __B,
						    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__B, __C,
						    _mm512_setzero_ph (),
						    __A,
						    _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__A, __B,
						    _mm512_setzero_ph (),
						    (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			     __m512h __D, const int __E)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__C, __D, __A, __B,
						    __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			      const int __D)
{
  return __builtin_ia32_vscalefph_v32hf_mask_round (__B, __C,
						    _mm512_setzero_ph (),
						    __A, __D);
}

#else
#define _mm512_scalef_round_ph(A, B, C)					\
  (__builtin_ia32_vscalefph_v32hf_mask_round ((A), (B),			\
					      _mm512_setzero_ph (),	\
					      (__mmask32)-1, (C)))

#define _mm512_mask_scalef_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_vscalefph_v32hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_scalef_round_ph(A, B, C, D)			\
  (__builtin_ia32_vscalefph_v32hf_mask_round ((B), (C),			\
					      _mm512_setzero_ph (),	\
					      (A), (D)))

#endif  /* __OPTIMIZE__ */

/* Intrinsics vscalefsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__A, __B,
						   _mm_setzero_ph (),
						   (__mmask8) -1,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__C, __D, __A, __B,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__B, __C,
						   _mm_setzero_ph (),
						   __A,
						   _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__A, __B,
						   _mm_setzero_ph (),
						   (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			  __m128h __D, const int __E)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__C, __D, __A, __B,
						   __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			   const int __D)
{
  return __builtin_ia32_vscalefsh_v8hf_mask_round (__B, __C,
						   _mm_setzero_ph (),
						   __A, __D);
}

#else
#define _mm_scalef_round_sh(A, B, C)					  \
  (__builtin_ia32_vscalefsh_v8hf_mask_round ((A), (B),			  \
					     _mm_setzero_ph (),		  \
					     (__mmask8)-1, (C)))

#define _mm_mask_scalef_round_sh(A, B, C, D, E)				  \
  (__builtin_ia32_vscalefsh_v8hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_scalef_round_sh(A, B, C, D)				  \
  (__builtin_ia32_vscalefsh_v8hf_mask_round ((B), (C), _mm_setzero_ph (), \
					     (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreduceph.  */
#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reduce_ph (__m512h __A, int __B)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__A, __B,
						    _mm512_setzero_ph (),
						    (__mmask32) -1,
						    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reduce_ph (__m512h __A, __mmask32 __B, __m512h __C, int __D)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__C, __D, __A, __B,
						    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reduce_ph (__mmask32 __A, __m512h __B, int __C)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__B, __C,
						    _mm512_setzero_ph (),
						    __A,
						    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reduce_round_ph (__m512h __A, int __B, const int __C)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__A, __B,
						    _mm512_setzero_ph (),
						    (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reduce_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			     int __D, const int __E)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__C, __D, __A, __B,
						    __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reduce_round_ph (__mmask32 __A, __m512h __B, int __C,
			      const int __D)
{
  return __builtin_ia32_vreduceph_v32hf_mask_round (__B, __C,
						    _mm512_setzero_ph (),
						    __A, __D);
}

#else
#define _mm512_reduce_ph(A, B)						\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((A), (B),			\
					      _mm512_setzero_ph (),	\
					      (__mmask32)-1,		\
					      _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_reduce_ph(A, B, C, D)				\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((C), (D), (A), (B),	\
					      _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_reduce_ph(A, B, C)					\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((B), (C),			\
					      _mm512_setzero_ph (),	\
					      (A), _MM_FROUND_CUR_DIRECTION))

#define _mm512_reduce_round_ph(A, B, C)					\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((A), (B),			\
					      _mm512_setzero_ph (),	\
					      (__mmask32)-1, (C)))

#define _mm512_mask_reduce_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_reduce_round_ph(A, B, C, D)			\
  (__builtin_ia32_vreduceph_v32hf_mask_round ((B), (C),			\
					      _mm512_setzero_ph (),	\
					      (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducesh.  */
#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reduce_sh (__m128h __A, __m128h __B, int __C)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__A, __B, __C,
						   _mm_setzero_ph (),
						   (__mmask8) -1,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reduce_sh (__m128h __A, __mmask8 __B, __m128h __C,
		    __m128h __D, int __E)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__C, __D, __E, __A, __B,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reduce_sh (__mmask8 __A, __m128h __B, __m128h __C, int __D)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__B, __C, __D,
						   _mm_setzero_ph (), __A,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reduce_round_sh (__m128h __A, __m128h __B, int __C, const int __D)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__A, __B, __C,
						   _mm_setzero_ph (),
						   (__mmask8) -1, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reduce_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			  __m128h __D, int __E, const int __F)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__C, __D, __E, __A,
						   __B, __F);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reduce_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			   int __D, const int __E)
{
  return __builtin_ia32_vreducesh_v8hf_mask_round (__B, __C, __D,
						   _mm_setzero_ph (),
						   __A, __E);
}

#else
#define _mm_reduce_sh(A, B, C)						\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((A), (B), (C),		\
					     _mm_setzero_ph (),	\
					     (__mmask8)-1,		\
					     _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_reduce_sh(A, B, C, D, E)				\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((C), (D), (E), (A), (B),	\
					     _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_reduce_sh(A, B, C, D)					\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((B), (C), (D),		\
					     _mm_setzero_ph (),	\
					     (A), _MM_FROUND_CUR_DIRECTION))

#define _mm_reduce_round_sh(A, B, C, D)				\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((A), (B), (C),	\
					     _mm_setzero_ph (),	\
					     (__mmask8)-1, (D)))

#define _mm_mask_reduce_round_sh(A, B, C, D, E, F)			\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((C), (D), (E), (A), (B), (F)))

#define _mm_maskz_reduce_round_sh(A, B, C, D, E)		\
  (__builtin_ia32_vreducesh_v8hf_mask_round ((B), (C), (D),	\
					     _mm_setzero_ph (),	\
					     (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrndscaleph.  */
#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscale_ph (__m512h __A, int __B)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__A, __B,
						      _mm512_setzero_ph (),
						      (__mmask32) -1,
						      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscale_ph (__m512h __A, __mmask32 __B,
				 __m512h __C, int __D)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__C, __D, __A, __B,
						      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscale_ph (__mmask32 __A, __m512h __B, int __C)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__B, __C,
						      _mm512_setzero_ph (),
						      __A,
						      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscale_round_ph (__m512h __A, int __B, const int __C)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__A, __B,
						      _mm512_setzero_ph (),
						      (__mmask32) -1,
						      __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscale_round_ph (__m512h __A, __mmask32 __B,
				 __m512h __C, int __D, const int __E)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__C, __D, __A,
						      __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscale_round_ph (__mmask32 __A, __m512h __B, int __C,
				  const int __D)
{
  return __builtin_ia32_vrndscaleph_v32hf_mask_round (__B, __C,
						      _mm512_setzero_ph (),
						      __A, __D);
}

#else
#define _mm512_roundscale_ph(A, B) \
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((A), (B),		\
						_mm512_setzero_ph (),	\
						(__mmask32)-1,		\
						_MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_roundscale_ph(A, B, C, D) \
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((C), (D), (A), (B),	\
						_MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_roundscale_ph(A, B, C) \
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((B), (C),		\
						_mm512_setzero_ph (),	\
						(A),			\
						_MM_FROUND_CUR_DIRECTION))
#define _mm512_roundscale_round_ph(A, B, C) \
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((A), (B),		\
						_mm512_setzero_ph (),	\
						(__mmask32)-1, (C)))

#define _mm512_mask_roundscale_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_roundscale_round_ph(A, B, C, D) \
  (__builtin_ia32_vrndscaleph_v32hf_mask_round ((B), (C),		\
						_mm512_setzero_ph (),	\
						(A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrndscalesh.  */
#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscale_sh (__m128h __A, __m128h __B, int __C)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__A, __B, __C,
						     _mm_setzero_ph (),
						     (__mmask8) -1,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscale_sh (__m128h __A, __mmask8 __B, __m128h __C,
			__m128h __D, int __E)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__C, __D, __E, __A, __B,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscale_sh (__mmask8 __A, __m128h __B, __m128h __C, int __D)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__B, __C, __D,
						     _mm_setzero_ph (), __A,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscale_round_sh (__m128h __A, __m128h __B, int __C, const int __D)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__A, __B, __C,
						     _mm_setzero_ph (),
						     (__mmask8) -1,
						     __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscale_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			      __m128h __D, int __E, const int __F)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__C, __D, __E,
						     __A, __B, __F);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscale_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			       int __D, const int __E)
{
  return __builtin_ia32_vrndscalesh_v8hf_mask_round (__B, __C, __D,
						     _mm_setzero_ph (),
						     __A, __E);
}

#else
#define _mm_roundscale_sh(A, B, C)					\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((A), (B), (C),		\
					       _mm_setzero_ph (),	\
					       (__mmask8)-1, \
					       _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_roundscale_sh(A, B, C, D, E)				\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((C), (D), (E), (A), (B), \
					       _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_roundscale_sh(A, B, C, D)				\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((B), (C), (D),		\
					       _mm_setzero_ph (),	\
					       (A), _MM_FROUND_CUR_DIRECTION))

#define _mm_roundscale_round_sh(A, B, C, D)				\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((A), (B), (C),		\
					       _mm_setzero_ph (),	\
					       (__mmask8)-1, (D)))

#define _mm_mask_roundscale_round_sh(A, B, C, D, E, F)			\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((C), (D), (E), (A), (B), (F)))

#define _mm_maskz_roundscale_round_sh(A, B, C, D, E)			\
  (__builtin_ia32_vrndscalesh_v8hf_mask_round ((B), (C), (D),		\
					       _mm_setzero_ph (),	\
					       (A), (E)))

#endif /* __OPTIMIZE__ */

#ifdef __DISABLE_AVX512FP16__
#undef __DISABLE_AVX512FP16__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512FP16__ */

#endif /* __AVX512FP16INTRIN_H_INCLUDED */
