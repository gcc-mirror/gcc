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
  return __builtin_ia32_addph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_add_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_addph512_mask (__B, __C,
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
  return __builtin_ia32_subph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sub_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_subph512_mask (__B, __C,
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
  return __builtin_ia32_mulph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mul_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_mulph512_mask (__B, __C,
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
  return __builtin_ia32_divph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_div_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_divph512_mask (__B, __C,
				       _mm512_setzero_ph (), __A);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_add_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_addph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_add_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_addph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_add_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_addph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sub_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_subph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sub_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_subph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sub_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_subph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mul_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_mulph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_mul_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_mulph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_mul_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_mulph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_div_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_divph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_div_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_divph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_div_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_divph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}
#else
#define _mm512_add_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_addph512_mask_round((A), (B),		\
					       _mm512_setzero_ph (),	\
					       (__mmask32)-1, (C)))

#define _mm512_mask_add_round_ph(A, B, C, D, E)				\
  ((__m512h)__builtin_ia32_addph512_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_add_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_addph512_mask_round((B), (C),		\
					       _mm512_setzero_ph (),	\
					       (A), (D)))

#define _mm512_sub_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_subph512_mask_round((A), (B),		\
					       _mm512_setzero_ph (),	\
					       (__mmask32)-1, (C)))

#define _mm512_mask_sub_round_ph(A, B, C, D, E)				\
  ((__m512h)__builtin_ia32_subph512_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_sub_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_subph512_mask_round((B), (C),		\
					       _mm512_setzero_ph (),	\
					       (A), (D)))

#define _mm512_mul_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_mulph512_mask_round((A), (B),		\
					       _mm512_setzero_ph (),	\
					       (__mmask32)-1, (C)))

#define _mm512_mask_mul_round_ph(A, B, C, D, E)				\
  ((__m512h)__builtin_ia32_mulph512_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_mul_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_mulph512_mask_round((B), (C),		\
					       _mm512_setzero_ph (),	\
					       (A), (D)))

#define _mm512_div_round_ph(A, B, C)					\
  ((__m512h)__builtin_ia32_divph512_mask_round((A), (B),		\
					       _mm512_setzero_ph (),	\
					       (__mmask32)-1, (C)))

#define _mm512_mask_div_round_ph(A, B, C, D, E)				\
  ((__m512h)__builtin_ia32_divph512_mask_round((C), (D), (A), (B), (E)))

#define _mm512_maskz_div_round_ph(A, B, C, D)				\
  ((__m512h)__builtin_ia32_divph512_mask_round((B), (C),		\
					       _mm512_setzero_ph (),	\
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
  return __builtin_ia32_addsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_addsh_mask (__B, __C, _mm_setzero_ph (),
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
  return __builtin_ia32_subsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_subsh_mask (__B, __C, _mm_setzero_ph (),
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
  return __builtin_ia32_mulsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_mulsh_mask (__B, __C, _mm_setzero_ph (), __A);
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
  return __builtin_ia32_divsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_divsh_mask (__B, __C, _mm_setzero_ph (),
				    __A);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_add_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_addsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_add_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_addsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_add_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_addsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sub_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_subsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sub_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_subsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sub_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_subsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mul_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_mulsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_mul_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_mulsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_mul_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_mulsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_div_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_divsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_div_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_divsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_div_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_divsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}
#else
#define _mm_add_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_addsh_mask_round ((A), (B),			\
					     _mm_setzero_ph (),		\
					     (__mmask8)-1, (C)))

#define _mm_mask_add_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_addsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_add_round_sh(A, B, C, D)			\
  ((__m128h)__builtin_ia32_addsh_mask_round ((B), (C),		\
					     _mm_setzero_ph (),	\
					     (A), (D)))

#define _mm_sub_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_subsh_mask_round ((A), (B),			\
					     _mm_setzero_ph (),		\
					     (__mmask8)-1, (C)))

#define _mm_mask_sub_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_subsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_sub_round_sh(A, B, C, D)			\
  ((__m128h)__builtin_ia32_subsh_mask_round ((B), (C),		\
					     _mm_setzero_ph (),	\
					     (A), (D)))

#define _mm_mul_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_mulsh_mask_round ((A), (B),			\
					     _mm_setzero_ph (),		\
					     (__mmask8)-1, (C)))

#define _mm_mask_mul_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_mulsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_mul_round_sh(A, B, C, D)			\
  ((__m128h)__builtin_ia32_mulsh_mask_round ((B), (C),		\
					     _mm_setzero_ph (),	\
					     (A), (D)))

#define _mm_div_round_sh(A, B, C)					\
  ((__m128h)__builtin_ia32_divsh_mask_round ((A), (B),			\
					     _mm_setzero_ph (),		\
					     (__mmask8)-1, (C)))

#define _mm_mask_div_round_sh(A, B, C, D, E)				\
  ((__m128h)__builtin_ia32_divsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_div_round_sh(A, B, C, D)			\
  ((__m128h)__builtin_ia32_divsh_mask_round ((B), (C),		\
					     _mm_setzero_ph (),	\
					     (A), (D)))
#endif /* __OPTIMIZE__ */

/* Intrinsic vmaxph vminph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_maxph512_mask (__A, __B,
				       _mm512_setzero_ph (),
				       (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_maxph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_maxph512_mask (__B, __C,
				       _mm512_setzero_ph (), __A);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_minph512_mask (__A, __B,
				       _mm512_setzero_ph (),
				       (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_minph512_mask (__C, __D, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_minph512_mask (__B, __C,
				       _mm512_setzero_ph (), __A);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_max_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_maxph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_max_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_maxph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_max_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_maxph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_min_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_minph512_mask_round (__A, __B,
					     _mm512_setzero_ph (),
					     (__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_min_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			  __m512h __D, const int __E)
{
  return __builtin_ia32_minph512_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_min_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_minph512_mask_round (__B, __C,
					     _mm512_setzero_ph (),
					     __A, __D);
}

#else
#define _mm512_max_round_ph(A, B, C)				\
  (__builtin_ia32_maxph512_mask_round ((A), (B),		\
				       _mm512_setzero_ph (),	\
				       (__mmask32)-1, (C)))

#define _mm512_mask_max_round_ph(A, B, C, D, E)				\
  (__builtin_ia32_maxph512_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_max_round_ph(A, B, C, D)			\
  (__builtin_ia32_maxph512_mask_round ((B), (C),		\
				       _mm512_setzero_ph (),	\
				       (A), (D)))

#define _mm512_min_round_ph(A, B, C)				\
  (__builtin_ia32_minph512_mask_round ((A), (B),		\
				       _mm512_setzero_ph (),	\
				       (__mmask32)-1, (C)))

#define _mm512_mask_min_round_ph(A, B, C, D, E)				\
  (__builtin_ia32_minph512_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_min_round_ph(A, B, C, D)			\
  (__builtin_ia32_minph512_mask_round ((B), (C),		\
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
  return __builtin_ia32_maxsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_maxsh_mask (__B, __C, _mm_setzero_ph (),
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
  return __builtin_ia32_minsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_minsh_mask (__B, __C, _mm_setzero_ph (),
				    __A);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_max_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_maxsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_max_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_maxsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_max_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_maxsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_min_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_minsh_mask_round (__A, __B,
					  _mm_setzero_ph (),
					  (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_min_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
		       __m128h __D, const int __E)
{
  return __builtin_ia32_minsh_mask_round (__C, __D, __A, __B, __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_min_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			const int __D)
{
  return __builtin_ia32_minsh_mask_round (__B, __C,
					  _mm_setzero_ph (),
					  __A, __D);
}

#else
#define _mm_max_round_sh(A, B, C)			\
  (__builtin_ia32_maxsh_mask_round ((A), (B),		\
				    _mm_setzero_ph (),	\
				    (__mmask8)-1, (C)))

#define _mm_mask_max_round_sh(A, B, C, D, E)			\
  (__builtin_ia32_maxsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_max_round_sh(A, B, C, D)		\
  (__builtin_ia32_maxsh_mask_round ((B), (C),		\
				    _mm_setzero_ph (),	\
				    (A), (D)))

#define _mm_min_round_sh(A, B, C)			\
  (__builtin_ia32_minsh_mask_round ((A), (B),		\
				    _mm_setzero_ph (),	\
				    (__mmask8)-1, (C)))

#define _mm_mask_min_round_sh(A, B, C, D, E)			\
  (__builtin_ia32_minsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_min_round_sh(A, B, C, D)		\
  (__builtin_ia32_minsh_mask_round ((B), (C),		\
				    _mm_setzero_ph (),	\
				    (A), (D)))

#endif /* __OPTIMIZE__ */

/* vcmpph */
#ifdef __OPTIMIZE
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_ph_mask (__m512h __A, __m512h __B, const int __C)
{
  return (__mmask32) __builtin_ia32_cmpph512_mask (__A, __B, __C,
						   (__mmask32) -1);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_ph_mask (__mmask32 __A, __m512h __B, __m512h __C,
			 const int __D)
{
  return (__mmask32) __builtin_ia32_cmpph512_mask (__B, __C, __D,
						   __A);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cmp_round_ph_mask (__m512h __A, __m512h __B, const int __C,
			  const int __D)
{
  return (__mmask32) __builtin_ia32_cmpph512_mask_round (__A, __B,
							 __C, (__mmask32) -1,
							 __D);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cmp_round_ph_mask (__mmask32 __A, __m512h __B, __m512h __C,
			       const int __D, const int __E)
{
  return (__mmask32) __builtin_ia32_cmpph512_mask_round (__B, __C,
							 __D, __A,
							 __E);
}

#else
#define _mm512_cmp_ph_mask(A, B, C)			\
  (__builtin_ia32_cmpph512_mask ((A), (B), (C), (-1)))

#define _mm512_mask_cmp_ph_mask(A, B, C, D)		\
  (__builtin_ia32_cmpph512_mask ((B), (C), (D), (A)))

#define _mm512_cmp_round_ph_mask(A, B, C, D)				\
  (__builtin_ia32_cmpph512_mask_round ((A), (B), (C), (-1), (D)))

#define _mm512_mask_cmp_round_ph_mask(A, B, C, D, E)			\
  (__builtin_ia32_cmpph512_mask_round ((B), (C), (D), (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcmpsh.  */
#ifdef __OPTIMIZE__
extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_sh_mask (__m128h __A, __m128h __B, const int __C)
{
  return (__mmask8)
    __builtin_ia32_cmpsh_mask_round (__A, __B,
				     __C, (__mmask8) -1,
				     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_sh_mask (__mmask8 __A, __m128h __B, __m128h __C,
		      const int __D)
{
  return (__mmask8)
    __builtin_ia32_cmpsh_mask_round (__B, __C,
				     __D, __A,
				     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmp_round_sh_mask (__m128h __A, __m128h __B, const int __C,
		       const int __D)
{
  return (__mmask8) __builtin_ia32_cmpsh_mask_round (__A, __B,
						     __C, (__mmask8) -1,
						     __D);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cmp_round_sh_mask (__mmask8 __A, __m128h __B, __m128h __C,
			    const int __D, const int __E)
{
  return (__mmask8) __builtin_ia32_cmpsh_mask_round (__B, __C,
						     __D, __A,
						     __E);
}

#else
#define _mm_cmp_sh_mask(A, B, C)					\
  (__builtin_ia32_cmpsh_mask_round ((A), (B), (C), (-1),		\
				    (_MM_FROUND_CUR_DIRECTION)))

#define _mm_mask_cmp_sh_mask(A, B, C, D)				\
  (__builtin_ia32_cmpsh_mask_round ((B), (C), (D), (A),			\
				    (_MM_FROUND_CUR_DIRECTION)))

#define _mm_cmp_round_sh_mask(A, B, C, D)			\
  (__builtin_ia32_cmpsh_mask_round ((A), (B), (C), (-1), (D)))

#define _mm_mask_cmp_round_sh_mask(A, B, C, D, E)		\
  (__builtin_ia32_cmpsh_mask_round ((B), (C), (D), (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcomish.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comieq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_EQ_OS,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comilt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_LT_OS,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comile_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_LE_OS,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comigt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_GT_OS,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comige_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_GE_OS,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comineq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_NEQ_US,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomieq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_EQ_OQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomilt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_LT_OQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomile_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_LE_OQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomigt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_GT_OQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomige_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_GE_OQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_ucomineq_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, _CMP_NEQ_UQ,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comi_sh (__m128h __A, __m128h __B, const int __P)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, __P,
					  (__mmask8) -1,
					  _MM_FROUND_CUR_DIRECTION);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_comi_round_sh (__m128h __A, __m128h __B, const int __P, const int __R)
{
  return __builtin_ia32_cmpsh_mask_round (__A, __B, __P,
					  (__mmask8) -1,__R);
}

#else
#define _mm_comi_round_sh(A, B, P, R)					\
  (__builtin_ia32_cmpsh_mask_round ((A), (B), (P), (__mmask8) (-1), (R)))
#define _mm_comi_sh(A, B, P)						\
  (__builtin_ia32_cmpsh_mask_round ((A), (B), (P), (__mmask8) (-1),	\
				    _MM_FROUND_CUR_DIRECTION))

#endif /* __OPTIMIZE__  */

/* Intrinsics vsqrtph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrt_ph (__m512h __A)
{
  return __builtin_ia32_sqrtph512_mask_round (__A,
					      _mm512_setzero_ph(),
					      (__mmask32) -1,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrt_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_sqrtph512_mask_round (__C, __A, __B,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrt_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_sqrtph512_mask_round (__B,
					      _mm512_setzero_ph (),
					      __A,
					      _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_sqrt_round_ph (__m512h __A, const int __B)
{
  return __builtin_ia32_sqrtph512_mask_round (__A,
					      _mm512_setzero_ph(),
					      (__mmask32) -1, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_sqrt_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			   const int __D)
{
  return __builtin_ia32_sqrtph512_mask_round (__C, __A, __B, __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_sqrt_round_ph (__mmask32 __A, __m512h __B, const int __C)
{
  return __builtin_ia32_sqrtph512_mask_round (__B,
					      _mm512_setzero_ph (),
					      __A, __C);
}

#else
#define _mm512_sqrt_round_ph(A, B)				\
  (__builtin_ia32_sqrtph512_mask_round ((A),			\
					_mm512_setzero_ph (),	\
					(__mmask32)-1, (B)))

#define _mm512_mask_sqrt_round_ph(A, B, C, D)			\
  (__builtin_ia32_sqrtph512_mask_round ((C), (A), (B), (D)))

#define _mm512_maskz_sqrt_round_ph(A, B, C)			\
  (__builtin_ia32_sqrtph512_mask_round ((B),			\
					_mm512_setzero_ph (),	\
					(A), (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrsqrtph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rsqrt_ph (__m512h __A)
{
  return __builtin_ia32_rsqrtph512_mask (__A, _mm512_setzero_ph (),
					 (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rsqrt_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_rsqrtph512_mask (__C, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rsqrt_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_rsqrtph512_mask (__B, _mm512_setzero_ph (),
					 __A);
}

/* Intrinsics vrsqrtsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rsqrt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_rsqrtsh_mask (__B, __A, _mm_setzero_ph (),
				      (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rsqrt_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_rsqrtsh_mask (__D, __C, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rsqrt_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_rsqrtsh_mask (__C, __B, _mm_setzero_ph (),
				      __A);
}

/* Intrinsics vsqrtsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrt_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_sqrtsh_mask_round (__B, __A,
					   _mm_setzero_ph (),
					   (__mmask8) -1,
					   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrt_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_sqrtsh_mask_round (__D, __C, __A, __B,
					   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrt_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_sqrtsh_mask_round (__C, __B,
					   _mm_setzero_ph (),
					   __A, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_sqrt_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_sqrtsh_mask_round (__B, __A,
					   _mm_setzero_ph (),
					   (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_sqrt_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			__m128h __D, const int __E)
{
  return __builtin_ia32_sqrtsh_mask_round (__D, __C, __A, __B,
					   __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_sqrt_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			 const int __D)
{
  return __builtin_ia32_sqrtsh_mask_round (__C, __B,
					   _mm_setzero_ph (),
					   __A, __D);
}

#else
#define _mm_sqrt_round_sh(A, B, C)				\
  (__builtin_ia32_sqrtsh_mask_round ((B), (A),			\
				     _mm_setzero_ph (),		\
				     (__mmask8)-1, (C)))

#define _mm_mask_sqrt_round_sh(A, B, C, D, E)			\
  (__builtin_ia32_sqrtsh_mask_round ((D), (C), (A), (B), (E)))

#define _mm_maskz_sqrt_round_sh(A, B, C, D)		\
  (__builtin_ia32_sqrtsh_mask_round ((C), (B),		\
				     _mm_setzero_ph (),	\
				     (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrcpph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_rcp_ph (__m512h __A)
{
  return __builtin_ia32_rcpph512_mask (__A, _mm512_setzero_ph (),
				       (__mmask32) -1);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_rcp_ph (__m512h __A, __mmask32 __B, __m512h __C)
{
  return __builtin_ia32_rcpph512_mask (__C, __A, __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_rcp_ph (__mmask32 __A, __m512h __B)
{
  return __builtin_ia32_rcpph512_mask (__B, _mm512_setzero_ph (),
				       __A);
}

/* Intrinsics vrcpsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_rcp_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_rcpsh_mask (__B, __A, _mm_setzero_ph (),
				    (__mmask8) -1);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_rcp_sh (__m128h __A, __mmask32 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_rcpsh_mask (__D, __C, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_rcp_sh (__mmask32 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_rcpsh_mask (__C, __B, _mm_setzero_ph (),
				    __A);
}

/* Intrinsics vscalefph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_ph (__m512h __A, __m512h __B)
{
  return __builtin_ia32_scalefph512_mask_round (__A, __B,
						_mm512_setzero_ph (),
						(__mmask32) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_ph (__m512h __A, __mmask32 __B, __m512h __C, __m512h __D)
{
  return __builtin_ia32_scalefph512_mask_round (__C, __D, __A, __B,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_ph (__mmask32 __A, __m512h __B, __m512h __C)
{
  return __builtin_ia32_scalefph512_mask_round (__B, __C,
						_mm512_setzero_ph (),
						__A,
						_MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_scalef_round_ph (__m512h __A, __m512h __B, const int __C)
{
  return __builtin_ia32_scalefph512_mask_round (__A, __B,
						_mm512_setzero_ph (),
						(__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_scalef_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			     __m512h __D, const int __E)
{
  return __builtin_ia32_scalefph512_mask_round (__C, __D, __A, __B,
						__E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_scalef_round_ph (__mmask32 __A, __m512h __B, __m512h __C,
			      const int __D)
{
  return __builtin_ia32_scalefph512_mask_round (__B, __C,
						_mm512_setzero_ph (),
						__A, __D);
}

#else
#define _mm512_scalef_round_ph(A, B, C)				\
  (__builtin_ia32_scalefph512_mask_round ((A), (B),		\
					  _mm512_setzero_ph (),	\
					  (__mmask32)-1, (C)))

#define _mm512_mask_scalef_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_scalefph512_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_scalef_round_ph(A, B, C, D)		\
  (__builtin_ia32_scalefph512_mask_round ((B), (C),		\
					  _mm512_setzero_ph (),	\
					  (A), (D)))

#endif  /* __OPTIMIZE__ */

/* Intrinsics vscalefsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_sh (__m128h __A, __m128h __B)
{
  return __builtin_ia32_scalefsh_mask_round (__A, __B,
					     _mm_setzero_ph (),
					     (__mmask8) -1,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_sh (__m128h __A, __mmask8 __B, __m128h __C, __m128h __D)
{
  return __builtin_ia32_scalefsh_mask_round (__C, __D, __A, __B,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_sh (__mmask8 __A, __m128h __B, __m128h __C)
{
  return __builtin_ia32_scalefsh_mask_round (__B, __C,
					     _mm_setzero_ph (),
					     __A,
					     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_scalef_round_sh (__m128h __A, __m128h __B, const int __C)
{
  return __builtin_ia32_scalefsh_mask_round (__A, __B,
					     _mm_setzero_ph (),
					     (__mmask8) -1, __C);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_scalef_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			  __m128h __D, const int __E)
{
  return __builtin_ia32_scalefsh_mask_round (__C, __D, __A, __B,
					     __E);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_scalef_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			   const int __D)
{
  return __builtin_ia32_scalefsh_mask_round (__B, __C,
					     _mm_setzero_ph (),
					     __A, __D);
}

#else
#define _mm_scalef_round_sh(A, B, C)				\
  (__builtin_ia32_scalefsh_mask_round ((A), (B),		\
				       _mm_setzero_ph (),	\
				       (__mmask8)-1, (C)))

#define _mm_mask_scalef_round_sh(A, B, C, D, E)				\
  (__builtin_ia32_scalefsh_mask_round ((C), (D), (A), (B), (E)))

#define _mm_maskz_scalef_round_sh(A, B, C, D)				\
  (__builtin_ia32_scalefsh_mask_round ((B), (C), _mm_setzero_ph (),	\
				       (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreduceph.  */
#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reduce_ph (__m512h __A, int __B)
{
  return __builtin_ia32_reduceph512_mask_round (__A, __B,
						_mm512_setzero_ph (),
						(__mmask32) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reduce_ph (__m512h __A, __mmask32 __B, __m512h __C, int __D)
{
  return __builtin_ia32_reduceph512_mask_round (__C, __D, __A, __B,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reduce_ph (__mmask32 __A, __m512h __B, int __C)
{
  return __builtin_ia32_reduceph512_mask_round (__B, __C,
						_mm512_setzero_ph (),
						__A,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_reduce_round_ph (__m512h __A, int __B, const int __C)
{
  return __builtin_ia32_reduceph512_mask_round (__A, __B,
						_mm512_setzero_ph (),
						(__mmask32) -1, __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_reduce_round_ph (__m512h __A, __mmask32 __B, __m512h __C,
			     int __D, const int __E)
{
  return __builtin_ia32_reduceph512_mask_round (__C, __D, __A, __B,
						__E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_reduce_round_ph (__mmask32 __A, __m512h __B, int __C,
			      const int __D)
{
  return __builtin_ia32_reduceph512_mask_round (__B, __C,
						_mm512_setzero_ph (),
						__A, __D);
}

#else
#define _mm512_reduce_ph(A, B)						\
  (__builtin_ia32_reduceph512_mask_round ((A), (B),			\
					  _mm512_setzero_ph (),		\
					  (__mmask32)-1,		\
					  _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_reduce_ph(A, B, C, D)				\
  (__builtin_ia32_reduceph512_mask_round ((C), (D), (A), (B),		\
					  _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_reduce_ph(A, B, C)					\
  (__builtin_ia32_reduceph512_mask_round ((B), (C),			\
					  _mm512_setzero_ph (),		\
					  (A), _MM_FROUND_CUR_DIRECTION))

#define _mm512_reduce_round_ph(A, B, C)				\
  (__builtin_ia32_reduceph512_mask_round ((A), (B),		\
					  _mm512_setzero_ph (),	\
					  (__mmask32)-1, (C)))

#define _mm512_mask_reduce_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_reduceph512_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_reduce_round_ph(A, B, C, D)		\
  (__builtin_ia32_reduceph512_mask_round ((B), (C),		\
					  _mm512_setzero_ph (),	\
					  (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vreducesh.  */
#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reduce_sh (__m128h __A, __m128h __B, int __C)
{
  return __builtin_ia32_reducesh_mask_round (__A, __B, __C,
					     _mm_setzero_ph (),
					     (__mmask8) -1,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reduce_sh (__m128h __A, __mmask8 __B, __m128h __C,
		    __m128h __D, int __E)
{
  return __builtin_ia32_reducesh_mask_round (__C, __D, __E, __A, __B,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reduce_sh (__mmask8 __A, __m128h __B, __m128h __C, int __D)
{
  return __builtin_ia32_reducesh_mask_round (__B, __C, __D,
					     _mm_setzero_ph (), __A,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_reduce_round_sh (__m128h __A, __m128h __B, int __C, const int __D)
{
  return __builtin_ia32_reducesh_mask_round (__A, __B, __C,
					     _mm_setzero_ph (),
					     (__mmask8) -1, __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_reduce_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			  __m128h __D, int __E, const int __F)
{
  return __builtin_ia32_reducesh_mask_round (__C, __D, __E, __A,
					     __B, __F);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_reduce_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			   int __D, const int __E)
{
  return __builtin_ia32_reducesh_mask_round (__B, __C, __D,
					     _mm_setzero_ph (),
					     __A, __E);
}

#else
#define _mm_reduce_sh(A, B, C)						\
  (__builtin_ia32_reducesh_mask_round ((A), (B), (C),			\
				       _mm_setzero_ph (),		\
				       (__mmask8)-1,			\
				       _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_reduce_sh(A, B, C, D, E)				\
  (__builtin_ia32_reducesh_mask_round ((C), (D), (E), (A), (B),		\
				       _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_reduce_sh(A, B, C, D)					\
  (__builtin_ia32_reducesh_mask_round ((B), (C), (D),			\
				       _mm_setzero_ph (),		\
				       (A), _MM_FROUND_CUR_DIRECTION))

#define _mm_reduce_round_sh(A, B, C, D)				\
  (__builtin_ia32_reducesh_mask_round ((A), (B), (C),		\
				       _mm_setzero_ph (),	\
				       (__mmask8)-1, (D)))

#define _mm_mask_reduce_round_sh(A, B, C, D, E, F)			\
  (__builtin_ia32_reducesh_mask_round ((C), (D), (E), (A), (B), (F)))

#define _mm_maskz_reduce_round_sh(A, B, C, D, E)		\
  (__builtin_ia32_reducesh_mask_round ((B), (C), (D),		\
				       _mm_setzero_ph (),	\
				       (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrndscaleph.  */
#ifdef __OPTIMIZE__
extern __inline __m512h
  __attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscale_ph (__m512h __A, int __B)
{
  return __builtin_ia32_rndscaleph512_mask_round (__A, __B,
						  _mm512_setzero_ph (),
						  (__mmask32) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscale_ph (__m512h __A, __mmask32 __B,
			   __m512h __C, int __D)
{
  return __builtin_ia32_rndscaleph512_mask_round (__C, __D, __A, __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscale_ph (__mmask32 __A, __m512h __B, int __C)
{
  return __builtin_ia32_rndscaleph512_mask_round (__B, __C,
						  _mm512_setzero_ph (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_roundscale_round_ph (__m512h __A, int __B, const int __C)
{
  return __builtin_ia32_rndscaleph512_mask_round (__A, __B,
						  _mm512_setzero_ph (),
						  (__mmask32) -1,
						  __C);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_roundscale_round_ph (__m512h __A, __mmask32 __B,
				 __m512h __C, int __D, const int __E)
{
  return __builtin_ia32_rndscaleph512_mask_round (__C, __D, __A,
						  __B, __E);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_roundscale_round_ph (__mmask32 __A, __m512h __B, int __C,
				  const int __D)
{
  return __builtin_ia32_rndscaleph512_mask_round (__B, __C,
						  _mm512_setzero_ph (),
						  __A, __D);
}

#else
#define _mm512_roundscale_ph(A, B)					\
  (__builtin_ia32_rndscaleph512_mask_round ((A), (B),			\
					    _mm512_setzero_ph (),	\
					    (__mmask32)-1,		\
					    _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_roundscale_ph(A, B, C, D)				\
  (__builtin_ia32_rndscaleph512_mask_round ((C), (D), (A), (B),		\
					    _MM_FROUND_CUR_DIRECTION))

#define _mm512_maskz_roundscale_ph(A, B, C)				\
  (__builtin_ia32_rndscaleph512_mask_round ((B), (C),			\
					    _mm512_setzero_ph (),	\
					    (A),			\
					    _MM_FROUND_CUR_DIRECTION))
#define _mm512_roundscale_round_ph(A, B, C)				\
  (__builtin_ia32_rndscaleph512_mask_round ((A), (B),			\
					    _mm512_setzero_ph (),	\
					    (__mmask32)-1, (C)))

#define _mm512_mask_roundscale_round_ph(A, B, C, D, E)			\
  (__builtin_ia32_rndscaleph512_mask_round ((C), (D), (A), (B), (E)))

#define _mm512_maskz_roundscale_round_ph(A, B, C, D)			\
  (__builtin_ia32_rndscaleph512_mask_round ((B), (C),			\
					    _mm512_setzero_ph (),	\
					    (A), (D)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vrndscalesh.  */
#ifdef __OPTIMIZE__
extern __inline __m128h
  __attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscale_sh (__m128h __A, __m128h __B, int __C)
{
  return __builtin_ia32_rndscalesh_mask_round (__A, __B, __C,
					       _mm_setzero_ph (),
					       (__mmask8) -1,
					       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscale_sh (__m128h __A, __mmask8 __B, __m128h __C,
			__m128h __D, int __E)
{
  return __builtin_ia32_rndscalesh_mask_round (__C, __D, __E, __A, __B,
					       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscale_sh (__mmask8 __A, __m128h __B, __m128h __C, int __D)
{
  return __builtin_ia32_rndscalesh_mask_round (__B, __C, __D,
					       _mm_setzero_ph (), __A,
					       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_roundscale_round_sh (__m128h __A, __m128h __B, int __C, const int __D)
{
  return __builtin_ia32_rndscalesh_mask_round (__A, __B, __C,
					       _mm_setzero_ph (),
					       (__mmask8) -1,
					       __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_roundscale_round_sh (__m128h __A, __mmask8 __B, __m128h __C,
			      __m128h __D, int __E, const int __F)
{
  return __builtin_ia32_rndscalesh_mask_round (__C, __D, __E,
					       __A, __B, __F);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_roundscale_round_sh (__mmask8 __A, __m128h __B, __m128h __C,
			       int __D, const int __E)
{
  return __builtin_ia32_rndscalesh_mask_round (__B, __C, __D,
					       _mm_setzero_ph (),
					       __A, __E);
}

#else
#define _mm_roundscale_sh(A, B, C)					\
  (__builtin_ia32_rndscalesh_mask_round ((A), (B), (C),			\
					 _mm_setzero_ph (),		\
					 (__mmask8)-1,			\
					 _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_roundscale_sh(A, B, C, D, E)				\
  (__builtin_ia32_rndscalesh_mask_round ((C), (D), (E), (A), (B),	\
					 _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_roundscale_sh(A, B, C, D)				\
  (__builtin_ia32_rndscalesh_mask_round ((B), (C), (D),			\
					 _mm_setzero_ph (),		\
					 (A), _MM_FROUND_CUR_DIRECTION))

#define _mm_roundscale_round_sh(A, B, C, D)			\
  (__builtin_ia32_rndscalesh_mask_round ((A), (B), (C),		\
					 _mm_setzero_ph (),	\
					 (__mmask8)-1, (D)))

#define _mm_mask_roundscale_round_sh(A, B, C, D, E, F)			\
  (__builtin_ia32_rndscalesh_mask_round ((C), (D), (E), (A), (B), (F)))

#define _mm_maskz_roundscale_round_sh(A, B, C, D, E)		\
  (__builtin_ia32_rndscalesh_mask_round ((B), (C), (D),		\
					 _mm_setzero_ph (),	\
					 (A), (E)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclasssh.  */
#ifdef __OPTIMIZE__
extern __inline __mmask8
  __attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_fpclass_sh_mask (__m128h __A, const int __imm)
{
  return (__mmask8) __builtin_ia32_fpclasssh_mask ((__v8hf) __A, __imm,
						   (__mmask8) -1);
}

extern __inline __mmask8
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_fpclass_sh_mask (__mmask8 __U, __m128h __A, const int __imm)
{
  return (__mmask8) __builtin_ia32_fpclasssh_mask ((__v8hf) __A, __imm, __U);
}

#else
#define _mm_fpclass_sh_mask(X, C)					\
  ((__mmask8) __builtin_ia32_fpclasssh_mask ((__v8hf) (__m128h) (X),	\
					     (int) (C), (__mmask8) (-1))) \

#define _mm_mask_fpclass_sh_mask(U, X, C)				\
  ((__mmask8) __builtin_ia32_fpclasssh_mask ((__v8hf) (__m128h) (X),	\
					     (int) (C), (__mmask8) (U)))
#endif /* __OPTIMIZE__ */

/* Intrinsics vfpclassph.  */
#ifdef __OPTIMIZE__
extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_fpclass_ph_mask (__mmask32 __U, __m512h __A,
			     const int __imm)
{
  return (__mmask32) __builtin_ia32_fpclassph512_mask ((__v32hf) __A,
						       __imm, __U);
}

extern __inline __mmask32
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_fpclass_ph_mask (__m512h __A, const int __imm)
{
  return (__mmask32) __builtin_ia32_fpclassph512_mask ((__v32hf) __A,
						       __imm,
						       (__mmask32) -1);
}

#else
#define _mm512_mask_fpclass_ph_mask(u, x, c)				\
  ((__mmask32) __builtin_ia32_fpclassph512_mask ((__v32hf) (__m512h) (x), \
						 (int) (c),(__mmask8)(u)))

#define _mm512_fpclass_ph_mask(x, c)                                    \
  ((__mmask32) __builtin_ia32_fpclassph512_mask ((__v32hf) (__m512h) (x), \
						 (int) (c),(__mmask8)-1))
#endif /* __OPIMTIZE__ */

/* Intrinsics vgetexpph, vgetexpsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getexp_sh (__m128h __A, __m128h __B)
{
  return (__m128h)
    __builtin_ia32_getexpsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					(__v8hf) _mm_setzero_ph (),
					(__mmask8) -1,
					_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getexp_sh (__m128h __W, __mmask8 __U, __m128h __A, __m128h __B)
{
  return (__m128h)
    __builtin_ia32_getexpsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					(__v8hf) __W, (__mmask8) __U,
					_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getexp_sh (__mmask8 __U, __m128h __A, __m128h __B)
{
  return (__m128h)
    __builtin_ia32_getexpsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					(__v8hf) _mm_setzero_ph (),
					(__mmask8) __U,
					_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getexp_ph (__m512h __A)
{
  return (__m512h)
    __builtin_ia32_getexpph512_mask ((__v32hf) __A,
				     (__v32hf) _mm512_setzero_ph (),
				     (__mmask32) -1, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getexp_ph (__m512h __W, __mmask32 __U, __m512h __A)
{
  return (__m512h)
    __builtin_ia32_getexpph512_mask ((__v32hf) __A, (__v32hf) __W,
				     (__mmask32) __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getexp_ph (__mmask32 __U, __m512h __A)
{
  return (__m512h)
    __builtin_ia32_getexpph512_mask ((__v32hf) __A,
				     (__v32hf) _mm512_setzero_ph (),
				     (__mmask32) __U, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getexp_round_sh (__m128h __A, __m128h __B, const int __R)
{
  return (__m128h) __builtin_ia32_getexpsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       _mm_setzero_ph (),
						       (__mmask8) -1,
						       __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getexp_round_sh (__m128h __W, __mmask8 __U, __m128h __A,
			  __m128h __B, const int __R)
{
  return (__m128h) __builtin_ia32_getexpsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v8hf) __W,
						       (__mmask8) __U, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getexp_round_sh (__mmask8 __U, __m128h __A, __m128h __B,
			   const int __R)
{
  return (__m128h) __builtin_ia32_getexpsh_mask_round ((__v8hf) __A,
						       (__v8hf) __B,
						       (__v8hf)
						       _mm_setzero_ph (),
						       (__mmask8) __U, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getexp_round_ph (__m512h __A, const int __R)
{
  return (__m512h) __builtin_ia32_getexpph512_mask ((__v32hf) __A,
						    (__v32hf)
						    _mm512_setzero_ph (),
						    (__mmask32) -1, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getexp_round_ph (__m512h __W, __mmask32 __U, __m512h __A,
			     const int __R)
{
  return (__m512h) __builtin_ia32_getexpph512_mask ((__v32hf) __A,
						    (__v32hf) __W,
						    (__mmask32) __U, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getexp_round_ph (__mmask32 __U, __m512h __A, const int __R)
{
  return (__m512h) __builtin_ia32_getexpph512_mask ((__v32hf) __A,
						    (__v32hf)
						    _mm512_setzero_ph (),
						    (__mmask32) __U, __R);
}

#else
#define _mm_getexp_round_sh(A, B, R)					\
  ((__m128h)__builtin_ia32_getexpsh_mask_round((__v8hf)(__m128h)(A),	\
					       (__v8hf)(__m128h)(B),	\
					       (__v8hf)_mm_setzero_ph(), \
					       (__mmask8)-1, R))

#define _mm_mask_getexp_round_sh(W, U, A, B, C)			\
  (__m128h)__builtin_ia32_getexpsh_mask_round(A, B, W, U, C)

#define _mm_maskz_getexp_round_sh(U, A, B, C)				\
  (__m128h)__builtin_ia32_getexpsh_mask_round(A, B,			\
					      (__v8hf)_mm_setzero_ph(),	\
					      U, C)

#define _mm512_getexp_round_ph(A, R)					\
  ((__m512h)__builtin_ia32_getexpph512_mask((__v32hf)(__m512h)(A),	\
					    (__v32hf)_mm512_setzero_ph(), (__mmask32)-1, R))

#define _mm512_mask_getexp_round_ph(W, U, A, R)				\
  ((__m512h)__builtin_ia32_getexpph512_mask((__v32hf)(__m512h)(A),	\
					    (__v32hf)(__m512h)(W), (__mmask32)(U), R))

#define _mm512_maskz_getexp_round_ph(U, A, R)				\
  ((__m512h)__builtin_ia32_getexpph512_mask((__v32hf)(__m512h)(A),	\
					    (__v32hf)_mm512_setzero_ph(), (__mmask32)(U), R))

#endif /* __OPTIMIZE__ */

/* Intrinsics vgetmantph, vgetmantsh.  */
#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getmant_sh (__m128h __A, __m128h __B,
		_MM_MANTISSA_NORM_ENUM __C,
		_MM_MANTISSA_SIGN_ENUM __D)
{
  return (__m128h)
    __builtin_ia32_getmantsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					 (__D << 2) | __C, _mm_setzero_ph (),
					 (__mmask8) -1,
					 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getmant_sh (__m128h __W, __mmask8 __U, __m128h __A,
		     __m128h __B, _MM_MANTISSA_NORM_ENUM __C,
		     _MM_MANTISSA_SIGN_ENUM __D)
{
  return (__m128h)
    __builtin_ia32_getmantsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					 (__D << 2) | __C, (__v8hf) __W,
					 __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getmant_sh (__mmask8 __U, __m128h __A, __m128h __B,
		      _MM_MANTISSA_NORM_ENUM __C,
		      _MM_MANTISSA_SIGN_ENUM __D)
{
  return (__m128h)
    __builtin_ia32_getmantsh_mask_round ((__v8hf) __A, (__v8hf) __B,
					 (__D << 2) | __C,
					 (__v8hf) _mm_setzero_ph(),
					 __U, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getmant_ph (__m512h __A, _MM_MANTISSA_NORM_ENUM __B,
		   _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     _mm512_setzero_ph (),
						     (__mmask32) -1,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getmant_ph (__m512h __W, __mmask32 __U, __m512h __A,
			_MM_MANTISSA_NORM_ENUM __B,
			_MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     (__v32hf) __W, __U,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getmant_ph (__mmask32 __U, __m512h __A,
			 _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     (__v32hf)
						     _mm512_setzero_ph (),
						     __U,
						     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_getmant_round_sh (__m128h __A, __m128h __B,
		      _MM_MANTISSA_NORM_ENUM __C,
		      _MM_MANTISSA_SIGN_ENUM __D, const int __R)
{
  return (__m128h) __builtin_ia32_getmantsh_mask_round ((__v8hf) __A,
							(__v8hf) __B,
							(__D << 2) | __C,
							_mm_setzero_ph (),
							(__mmask8) -1,
							__R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_getmant_round_sh (__m128h __W, __mmask8 __U, __m128h __A,
			   __m128h __B, _MM_MANTISSA_NORM_ENUM __C,
			   _MM_MANTISSA_SIGN_ENUM __D, const int __R)
{
  return (__m128h) __builtin_ia32_getmantsh_mask_round ((__v8hf) __A,
							(__v8hf) __B,
							(__D << 2) | __C,
							(__v8hf) __W,
							__U, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_getmant_round_sh (__mmask8 __U, __m128h __A, __m128h __B,
			    _MM_MANTISSA_NORM_ENUM __C,
			    _MM_MANTISSA_SIGN_ENUM __D, const int __R)
{
  return (__m128h) __builtin_ia32_getmantsh_mask_round ((__v8hf) __A,
							(__v8hf) __B,
							(__D << 2) | __C,
							(__v8hf)
							_mm_setzero_ph(),
							__U, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_getmant_round_ph (__m512h __A, _MM_MANTISSA_NORM_ENUM __B,
			 _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     _mm512_setzero_ph (),
						     (__mmask32) -1, __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_getmant_round_ph (__m512h __W, __mmask32 __U, __m512h __A,
			      _MM_MANTISSA_NORM_ENUM __B,
			      _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     (__v32hf) __W, __U,
						     __R);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_getmant_round_ph (__mmask32 __U, __m512h __A,
			       _MM_MANTISSA_NORM_ENUM __B,
			       _MM_MANTISSA_SIGN_ENUM __C, const int __R)
{
  return (__m512h) __builtin_ia32_getmantph512_mask ((__v32hf) __A,
						     (__C << 2) | __B,
						     (__v32hf)
						     _mm512_setzero_ph (),
						     __U, __R);
}

#else
#define _mm512_getmant_ph(X, B, C)					\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)	\
					      _mm512_setzero_ph(),	\
					      (__mmask32)-1,		\
					      _MM_FROUND_CUR_DIRECTION))

#define _mm512_mask_getmant_ph(W, U, X, B, C)				\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)(W),	\
					      (__mmask32)(U),		\
					      _MM_FROUND_CUR_DIRECTION))


#define _mm512_maskz_getmant_ph(U, X, B, C)				\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)	\
					      _mm512_setzero_ph(),	\
					      (__mmask32)(U),		\
					      _MM_FROUND_CUR_DIRECTION))

#define _mm_getmant_sh(X, Y, C, D)					\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)	\
						 _mm_setzero_ph (),	\
						 (__mmask8)-1,		\
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_mask_getmant_sh(W, U, X, Y, C, D)				\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)(W),	\
						 (__mmask8)(U),		\
						 _MM_FROUND_CUR_DIRECTION))

#define _mm_maskz_getmant_sh(U, X, Y, C, D)				\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)	\
						 _mm_setzero_ph(),	\
						 (__mmask8)(U),		\
						 _MM_FROUND_CUR_DIRECTION))

#define _mm512_getmant_round_ph(X, B, C, R)				\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)	\
					      _mm512_setzero_ph(),	\
					      (__mmask32)-1,		\
					      (R)))

#define _mm512_mask_getmant_round_ph(W, U, X, B, C, R)			\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)(W),	\
					      (__mmask32)(U),		\
					      (R)))


#define _mm512_maskz_getmant_round_ph(U, X, B, C, R)			\
  ((__m512h)__builtin_ia32_getmantph512_mask ((__v32hf)(__m512h)(X),	\
					      (int)(((C)<<2) | (B)),	\
					      (__v32hf)(__m512h)	\
					      _mm512_setzero_ph(),	\
					      (__mmask32)(U),		\
					      (R)))

#define _mm_getmant_round_sh(X, Y, C, D, R)				\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)	\
						 _mm_setzero_ph (),	\
						 (__mmask8)-1,		\
						 (R)))

#define _mm_mask_getmant_round_sh(W, U, X, Y, C, D, R)			\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)(W),	\
						 (__mmask8)(U),		\
						 (R)))

#define _mm_maskz_getmant_round_sh(U, X, Y, C, D, R)			\
  ((__m128h)__builtin_ia32_getmantsh_mask_round ((__v8hf)(__m128h)(X),	\
						 (__v8hf)(__m128h)(Y),	\
						 (int)(((D)<<2) | (C)),	\
						 (__v8hf)(__m128h)	\
						 _mm_setzero_ph(),	\
						 (__mmask8)(U),		\
						 (R)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vmovw.  */
extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsi16_si128 (short __A)
{
  return _mm_set_epi16 (0, 0, 0, 0, 0, 0, 0, __A);
}

extern __inline short
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsi128_si16 (__m128i __A)
{
  return __builtin_ia32_vec_ext_v8hi ((__v8hi)__A, 0);
}

/* Intrinsics vmovsh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_load_sh (__m128h __A, __mmask8 __B, _Float16 const* __C)
{
  return __builtin_ia32_loadsh_mask (__C, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_load_sh (__mmask8 __A, _Float16 const* __B)
{
  return __builtin_ia32_loadsh_mask (__B, _mm_setzero_ph (), __A);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_store_sh (_Float16 const* __A, __mmask8 __B, __m128h __C)
{
  __builtin_ia32_storesh_mask (__A,  __C, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_move_sh (__m128h __A, __m128h  __B)
{
  __A[0] = __B[0];
  return __A;
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_move_sh (__m128h __A, __mmask8 __B, __m128h  __C, __m128h __D)
{
  return __builtin_ia32_vmovsh_mask (__C, __D, __A, __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_move_sh (__mmask8 __A, __m128h  __B, __m128h __C)
{
  return __builtin_ia32_vmovsh_mask (__B, __C, _mm_setzero_ph (), __A);
}

/* Intrinsics vcvtph2dq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epi32 (__m256h __A)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__A,
					    (__v16si)
					    _mm512_setzero_si512 (),
					    (__mmask16) -1,
					    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epi32 (__m512i __A, __mmask16 __B, __m256h __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__C,
					    (__v16si) __A,
					    __B,
					    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epi32 (__mmask16 __A, __m256h __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__B,
					    (__v16si)
					    _mm512_setzero_si512 (),
					    __A,
					    _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epi32 (__m256h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__A,
					    (__v16si)
					    _mm512_setzero_si512 (),
					    (__mmask16) -1,
					    __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epi32 (__m512i __A, __mmask16 __B, __m256h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__C,
					    (__v16si) __A,
					    __B,
					    __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epi32 (__mmask16 __A, __m256h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2dq512_mask_round (__B,
					    (__v16si)
					    _mm512_setzero_si512 (),
					    __A,
					    __C);
}

#else
#define _mm512_cvt_roundph_epi32(A, B)					\
  ((__m512i)								\
   __builtin_ia32_vcvtph2dq512_mask_round ((A),				\
					   (__v16si)			\
					   _mm512_setzero_si512 (),	\
					   (__mmask16)-1,		\
					   (B)))

#define _mm512_mask_cvt_roundph_epi32(A, B, C, D)			\
  ((__m512i)								\
   __builtin_ia32_vcvtph2dq512_mask_round ((C), (__v16si)(A), (B), (D)))

#define _mm512_maskz_cvt_roundph_epi32(A, B, C)				\
  ((__m512i)								\
   __builtin_ia32_vcvtph2dq512_mask_round ((B),				\
					   (__v16si)			\
					   _mm512_setzero_si512 (),	\
					   (A),				\
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtph2udq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epu32 (__m256h __A)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__A,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     (__mmask16) -1,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epu32 (__m512i __A, __mmask16 __B, __m256h __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__C,
					     (__v16si) __A,
					     __B,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epu32 (__mmask16 __A, __m256h __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__B,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     __A,
					     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epu32 (__m256h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__A,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     (__mmask16) -1,
					     __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epu32 (__m512i __A, __mmask16 __B, __m256h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__C,
					     (__v16si) __A,
					     __B,
					     __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epu32 (__mmask16 __A, __m256h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2udq512_mask_round (__B,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     __A,
					     __C);
}

#else
#define _mm512_cvt_roundph_epu32(A, B)					\
  ((__m512i)								\
   __builtin_ia32_vcvtph2udq512_mask_round ((A),			\
					    (__v16si)			\
					    _mm512_setzero_si512 (),	\
					    (__mmask16)-1,		\
					    (B)))

#define _mm512_mask_cvt_roundph_epu32(A, B, C, D)			\
  ((__m512i)								\
   __builtin_ia32_vcvtph2udq512_mask_round ((C), (__v16si)(A), (B), (D)))

#define _mm512_maskz_cvt_roundph_epu32(A, B, C)				\
  ((__m512i)								\
   __builtin_ia32_vcvtph2udq512_mask_round ((B),			\
					    (__v16si)			\
					    _mm512_setzero_si512 (),	\
					    (A),			\
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2dq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epi32 (__m256h __A)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__A,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     (__mmask16) -1,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epi32 (__m512i __A, __mmask16 __B, __m256h __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__C,
					     (__v16si) __A,
					     __B,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epi32 (__mmask16 __A, __m256h __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__B,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     __A,
					     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epi32 (__m256h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__A,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     (__mmask16) -1,
					     __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epi32 (__m512i __A, __mmask16 __B,
				__m256h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__C,
					     (__v16si) __A,
					     __B,
					     __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epi32 (__mmask16 __A, __m256h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2dq512_mask_round (__B,
					     (__v16si)
					     _mm512_setzero_si512 (),
					     __A,
					     __C);
}

#else
#define _mm512_cvtt_roundph_epi32(A, B)					\
  ((__m512i)								\
   __builtin_ia32_vcvttph2dq512_mask_round ((A),			\
					    (__v16si)			\
					    (_mm512_setzero_si512 ()),	\
					    (__mmask16)(-1), (B)))

#define _mm512_mask_cvtt_roundph_epi32(A, B, C, D)		\
  ((__m512i)							\
   __builtin_ia32_vcvttph2dq512_mask_round ((C),		\
					    (__v16si)(A),	\
					    (B),		\
					    (D)))

#define _mm512_maskz_cvtt_roundph_epi32(A, B, C)			\
  ((__m512i)								\
   __builtin_ia32_vcvttph2dq512_mask_round ((B),			\
					    (__v16si)			\
					    _mm512_setzero_si512 (),	\
					    (A),			\
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2udq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epu32 (__m256h __A)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__A,
					      (__v16si)
					      _mm512_setzero_si512 (),
					      (__mmask16) -1,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epu32 (__m512i __A, __mmask16 __B, __m256h __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__C,
					      (__v16si) __A,
					      __B,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epu32 (__mmask16 __A, __m256h __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__B,
					      (__v16si)
					      _mm512_setzero_si512 (),
					      __A,
					      _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epu32 (__m256h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__A,
					      (__v16si)
					      _mm512_setzero_si512 (),
					      (__mmask16) -1,
					      __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epu32 (__m512i __A, __mmask16 __B,
				__m256h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__C,
					      (__v16si) __A,
					      __B,
					      __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epu32 (__mmask16 __A, __m256h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2udq512_mask_round (__B,
					      (__v16si)
					      _mm512_setzero_si512 (),
					      __A,
					      __C);
}

#else
#define _mm512_cvtt_roundph_epu32(A, B)					\
  ((__m512i)								\
   __builtin_ia32_vcvttph2udq512_mask_round ((A),			\
					     (__v16si)			\
					     _mm512_setzero_si512 (),	\
					     (__mmask16)-1,		\
					     (B)))

#define _mm512_mask_cvtt_roundph_epu32(A, B, C, D)		\
  ((__m512i)							\
   __builtin_ia32_vcvttph2udq512_mask_round ((C),		\
					     (__v16si)(A),	\
					     (B),		\
					     (D)))

#define _mm512_maskz_cvtt_roundph_epu32(A, B, C)			\
  ((__m512i)								\
   __builtin_ia32_vcvttph2udq512_mask_round ((B),			\
					     (__v16si)			\
					     _mm512_setzero_si512 (),	\
					     (A),			\
					     (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtdq2ph.  */
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtepi32_ph (__m512i __A)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __A,
						 _mm256_setzero_ph (),
						 (__mmask16) -1,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepi32_ph (__m256h __A, __mmask16 __B, __m512i __C)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __C,
						 __A,
						 __B,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepi32_ph (__mmask16 __A, __m512i __B)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __B,
						 _mm256_setzero_ph (),
						 __A,
						 _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepi32_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __A,
						 _mm256_setzero_ph (),
						 (__mmask16) -1,
						 __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepi32_ph (__m256h __A, __mmask16 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __C,
						 __A,
						 __B,
						 __D);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepi32_ph (__mmask16 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtdq2ph512_mask_round ((__v16si) __B,
						 _mm256_setzero_ph (),
						 __A,
						 __C);
}

#else
#define _mm512_cvt_roundepi32_ph(A, B)					\
  (__builtin_ia32_vcvtdq2ph512_mask_round ((__v16si)(A),		\
					   _mm256_setzero_ph (),	\
					   (__mmask16)-1,		\
					   (B)))

#define _mm512_mask_cvt_roundepi32_ph(A, B, C, D)		\
  (__builtin_ia32_vcvtdq2ph512_mask_round ((__v16si)(C),	\
					   (A),			\
					   (B),			\
					   (D)))

#define _mm512_maskz_cvt_roundepi32_ph(A, B, C)				\
  (__builtin_ia32_vcvtdq2ph512_mask_round ((__v16si)(B),		\
					   _mm256_setzero_ph (),	\
					   (A),				\
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtudq2ph.  */
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtepu32_ph (__m512i __A)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __A,
						  _mm256_setzero_ph (),
						  (__mmask16) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepu32_ph (__m256h __A, __mmask16 __B, __m512i __C)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __C,
						  __A,
						  __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepu32_ph (__mmask16 __A, __m512i __B)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __B,
						  _mm256_setzero_ph (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepu32_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __A,
						  _mm256_setzero_ph (),
						  (__mmask16) -1,
						  __B);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepu32_ph (__m256h __A, __mmask16 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __C,
						  __A,
						  __B,
						  __D);
}

extern __inline __m256h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepu32_ph (__mmask16 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtudq2ph512_mask_round ((__v16si) __B,
						  _mm256_setzero_ph (),
						  __A,
						  __C);
}

#else
#define _mm512_cvt_roundepu32_ph(A, B)					\
  (__builtin_ia32_vcvtudq2ph512_mask_round ((__v16si)(A),		\
					    _mm256_setzero_ph (),	\
					    (__mmask16)-1,		\
					    B))

#define _mm512_mask_cvt_roundepu32_ph(A, B, C, D)	\
  (__builtin_ia32_vcvtudq2ph512_mask_round ((__v16si)C,	\
					    A,		\
					    B,		\
					    D))

#define _mm512_maskz_cvt_roundepu32_ph(A, B, C)				\
  (__builtin_ia32_vcvtudq2ph512_mask_round ((__v16si)B,			\
					    _mm256_setzero_ph (),	\
					    A,				\
					    C))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtph2qq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epi64 (__m128h __A)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__A,
						 _mm512_setzero_si512 (),
						 (__mmask8) -1,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epi64 (__m512i __A, __mmask8 __B, __m128h __C)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__C, __A, __B,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epi64 (__mmask8 __A, __m128h __B)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__B,
						 _mm512_setzero_si512 (),
						 __A,
						 _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epi64 (__m128h __A, int __B)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__A,
						 _mm512_setzero_si512 (),
						 (__mmask8) -1,
						 __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epi64 (__m512i __A, __mmask8 __B, __m128h __C, int __D)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__C, __A, __B, __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epi64 (__mmask8 __A, __m128h __B, int __C)
{
  return __builtin_ia32_vcvtph2qq512_mask_round (__B,
						 _mm512_setzero_si512 (),
						 __A,
						 __C);
}

#else
#define _mm512_cvt_roundph_epi64(A, B)					\
  (__builtin_ia32_vcvtph2qq512_mask_round ((A),				\
					   _mm512_setzero_si512 (),	\
					   (__mmask8)-1,		\
					   (B)))

#define _mm512_mask_cvt_roundph_epi64(A, B, C, D)		\
  (__builtin_ia32_vcvtph2qq512_mask_round ((C), (A), (B), (D)))

#define _mm512_maskz_cvt_roundph_epi64(A, B, C)				\
  (__builtin_ia32_vcvtph2qq512_mask_round ((B),				\
					   _mm512_setzero_si512 (),	\
					   (A),				\
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtph2uqq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epu64 (__m128h __A)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__A,
						  _mm512_setzero_si512 (),
						  (__mmask8) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epu64 (__m512i __A, __mmask8 __B, __m128h __C)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__C, __A, __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epu64 (__mmask8 __A, __m128h __B)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__B,
						  _mm512_setzero_si512 (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epu64 (__m128h __A, int __B)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__A,
						  _mm512_setzero_si512 (),
						  (__mmask8) -1,
						  __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epu64 (__m512i __A, __mmask8 __B, __m128h __C, int __D)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__C, __A, __B, __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epu64 (__mmask8 __A, __m128h __B, int __C)
{
  return __builtin_ia32_vcvtph2uqq512_mask_round (__B,
						  _mm512_setzero_si512 (),
						  __A,
						  __C);
}

#else
#define _mm512_cvt_roundph_epu64(A, B)					\
  (__builtin_ia32_vcvtph2uqq512_mask_round ((A),			\
					    _mm512_setzero_si512 (),	\
					    (__mmask8)-1,		\
					    (B)))

#define _mm512_mask_cvt_roundph_epu64(A, B, C, D)			\
  (__builtin_ia32_vcvtph2uqq512_mask_round ((C), (A), (B), (D)))

#define _mm512_maskz_cvt_roundph_epu64(A, B, C)				\
  (__builtin_ia32_vcvtph2uqq512_mask_round ((B),			\
					    _mm512_setzero_si512 (),	\
					    (A),			\
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2qq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epi64 (__m128h __A)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__A,
						  _mm512_setzero_si512 (),
						  (__mmask8) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epi64 (__m512i __A, __mmask8 __B, __m128h __C)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__C, __A, __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epi64 (__mmask8 __A, __m128h __B)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__B,
						  _mm512_setzero_si512 (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epi64 (__m128h __A, int __B)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__A,
						  _mm512_setzero_si512 (),
						  (__mmask8) -1,
						  __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epi64 (__m512i __A, __mmask8 __B, __m128h __C, int __D)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__C, __A, __B, __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epi64 (__mmask8 __A, __m128h __B, int __C)
{
  return __builtin_ia32_vcvttph2qq512_mask_round (__B,
						  _mm512_setzero_si512 (),
						  __A,
						  __C);
}

#else
#define _mm512_cvtt_roundph_epi64(A, B)					\
  (__builtin_ia32_vcvttph2qq512_mask_round ((A),			\
					    _mm512_setzero_si512 (),	\
					    (__mmask8)-1,		\
					    (B)))

#define _mm512_mask_cvtt_roundph_epi64(A, B, C, D)			\
  __builtin_ia32_vcvttph2qq512_mask_round ((C), (A), (B), (D))

#define _mm512_maskz_cvtt_roundph_epi64(A, B, C)			\
  (__builtin_ia32_vcvttph2qq512_mask_round ((B),			\
					    _mm512_setzero_si512 (),	\
					    (A),			\
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2uqq.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epu64 (__m128h __A)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__A,
						   _mm512_setzero_si512 (),
						   (__mmask8) -1,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epu64 (__m512i __A, __mmask8 __B, __m128h __C)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__C, __A, __B,
						   _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epu64 (__mmask8 __A, __m128h __B)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__B,
						   _mm512_setzero_si512 (),
						   __A,
						   _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epu64 (__m128h __A, int __B)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__A,
						   _mm512_setzero_si512 (),
						   (__mmask8) -1,
						   __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epu64 (__m512i __A, __mmask8 __B, __m128h __C, int __D)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__C, __A, __B, __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epu64 (__mmask8 __A, __m128h __B, int __C)
{
  return __builtin_ia32_vcvttph2uqq512_mask_round (__B,
						   _mm512_setzero_si512 (),
						   __A,
						   __C);
}

#else
#define _mm512_cvtt_roundph_epu64(A, B)					\
  (__builtin_ia32_vcvttph2uqq512_mask_round ((A),			\
					     _mm512_setzero_si512 (),	\
					     (__mmask8)-1,		\
					     (B)))

#define _mm512_mask_cvtt_roundph_epu64(A, B, C, D)			\
  __builtin_ia32_vcvttph2uqq512_mask_round ((C), (A), (B), (D))

#define _mm512_maskz_cvtt_roundph_epu64(A, B, C)			\
  (__builtin_ia32_vcvttph2uqq512_mask_round ((B),			\
					     _mm512_setzero_si512 (),	\
					     (A),			\
					     (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtqq2ph.  */
extern __inline __m128h
  __attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtepi64_ph (__m512i __A)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __A,
						 _mm_setzero_ph (),
						 (__mmask8) -1,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepi64_ph (__m128h __A, __mmask8 __B, __m512i __C)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __C,
						 __A,
						 __B,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepi64_ph (__mmask8 __A, __m512i __B)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __B,
						 _mm_setzero_ph (),
						 __A,
						 _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepi64_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __A,
						 _mm_setzero_ph (),
						 (__mmask8) -1,
						 __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepi64_ph (__m128h __A, __mmask8 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __C,
						 __A,
						 __B,
						 __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepi64_ph (__mmask8 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtqq2ph512_mask_round ((__v8di) __B,
						 _mm_setzero_ph (),
						 __A,
						 __C);
}

#else
#define _mm512_cvt_roundepi64_ph(A, B)				\
  (__builtin_ia32_vcvtqq2ph512_mask_round ((__v8di)(A),		\
					   _mm_setzero_ph (),	\
					   (__mmask8)-1,	\
					   (B)))

#define _mm512_mask_cvt_roundepi64_ph(A, B, C, D)			\
  (__builtin_ia32_vcvtqq2ph512_mask_round ((__v8di)(C), (A), (B), (D)))

#define _mm512_maskz_cvt_roundepi64_ph(A, B, C)			\
  (__builtin_ia32_vcvtqq2ph512_mask_round ((__v8di)(B),		\
					   _mm_setzero_ph (),	\
					   (A),			\
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtuqq2ph.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtepu64_ph (__m512i __A)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __A,
						  _mm_setzero_ph (),
						  (__mmask8) -1,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepu64_ph (__m128h __A, __mmask8 __B, __m512i __C)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __C,
						  __A,
						  __B,
						  _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepu64_ph (__mmask8 __A, __m512i __B)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __B,
						  _mm_setzero_ph (),
						  __A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepu64_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __A,
						  _mm_setzero_ph (),
						  (__mmask8) -1,
						  __B);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepu64_ph (__m128h __A, __mmask8 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __C,
						  __A,
						  __B,
						  __D);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepu64_ph (__mmask8 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di) __B,
						  _mm_setzero_ph (),
						  __A,
						  __C);
}

#else
#define _mm512_cvt_roundepu64_ph(A, B)				\
  (__builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di)(A),	\
					    _mm_setzero_ph (),	\
					    (__mmask8)-1,	\
					    (B)))

#define _mm512_mask_cvt_roundepu64_ph(A, B, C, D)			\
  (__builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di)(C), (A), (B), (D)))

#define _mm512_maskz_cvt_roundepu64_ph(A, B, C)			\
  (__builtin_ia32_vcvtuqq2ph512_mask_round ((__v8di)(B),	\
					    _mm_setzero_ph (),	\
					    (A),		\
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtph2w.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epi16 (__m512h __A)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__A,
					      (__v32hi)
					      _mm512_setzero_si512 (),
					      (__mmask32) -1,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epi16 (__m512i __A, __mmask32 __B, __m512h __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__C,
					      (__v32hi) __A,
					      __B,
					      _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epi16 (__mmask32 __A, __m512h __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__B,
					      (__v32hi)
					      _mm512_setzero_si512 (),
					      __A,
					      _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epi16 (__m512h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__A,
					      (__v32hi)
					      _mm512_setzero_si512 (),
					      (__mmask32) -1,
					      __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epi16 (__m512i __A, __mmask32 __B, __m512h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__C,
					      (__v32hi) __A,
					      __B,
					      __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epi16 (__mmask32 __A, __m512h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2w512_mask_round (__B,
					      (__v32hi)
					      _mm512_setzero_si512 (),
					      __A,
					      __C);
}

#else
#define _mm512_cvt_roundph_epi16(A, B)					\
  ((__m512i)__builtin_ia32_vcvtph2w512_mask_round ((A),		\
						      (__v32hi)		\
						      _mm512_setzero_si512 (), \
						      (__mmask32)-1,	\
						      (B)))

#define _mm512_mask_cvt_roundph_epi16(A, B, C, D)			\
  ((__m512i)__builtin_ia32_vcvtph2w512_mask_round ((C),		\
						      (__v32hi)(A),	\
						      (B),		\
						      (D)))

#define _mm512_maskz_cvt_roundph_epi16(A, B, C)				\
  ((__m512i)__builtin_ia32_vcvtph2w512_mask_round ((B),		\
						      (__v32hi)		\
						      _mm512_setzero_si512 (), \
						      (A),		\
						      (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtph2uw.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtph_epu16 (__m512h __A)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__A,
					       (__v32hi)
					       _mm512_setzero_si512 (),
					       (__mmask32) -1,
					       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtph_epu16 (__m512i __A, __mmask32 __B, __m512h __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__C, (__v32hi) __A, __B,
					       _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtph_epu16 (__mmask32 __A, __m512h __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__B,
					       (__v32hi)
					       _mm512_setzero_si512 (),
					       __A,
					       _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundph_epu16 (__m512h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__A,
					       (__v32hi)
					       _mm512_setzero_si512 (),
					       (__mmask32) -1,
					       __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundph_epu16 (__m512i __A, __mmask32 __B, __m512h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__C, (__v32hi) __A, __B, __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundph_epu16 (__mmask32 __A, __m512h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvtph2uw512_mask_round (__B,
					       (__v32hi)
					       _mm512_setzero_si512 (),
					       __A,
					       __C);
}

#else
#define _mm512_cvt_roundph_epu16(A, B)					\
  ((__m512i)								\
   __builtin_ia32_vcvtph2uw512_mask_round ((A),			\
					      (__v32hi)			\
					      _mm512_setzero_si512 (),	\
					      (__mmask32)-1, (B)))

#define _mm512_mask_cvt_roundph_epu16(A, B, C, D)			\
  ((__m512i)								\
   __builtin_ia32_vcvtph2uw512_mask_round ((C), (__v32hi)(A), (B), (D)))

#define _mm512_maskz_cvt_roundph_epu16(A, B, C)				\
  ((__m512i)								\
   __builtin_ia32_vcvtph2uw512_mask_round ((B),			\
					      (__v32hi)			\
					      _mm512_setzero_si512 (),	\
					      (A),			\
					      (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2w.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epi16 (__m512h __A)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__A,
					    (__v32hi)
					    _mm512_setzero_si512 (),
					    (__mmask32) -1,
					    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epi16 (__m512i __A, __mmask32 __B, __m512h __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__C,
					    (__v32hi) __A,
					    __B,
					    _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epi16 (__mmask32 __A, __m512h __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__B,
					    (__v32hi)
					    _mm512_setzero_si512 (),
					    __A,
					    _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epi16 (__m512h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__A,
					    (__v32hi)
					    _mm512_setzero_si512 (),
					    (__mmask32) -1,
					    __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epi16 (__m512i __A, __mmask32 __B,
				__m512h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__C,
					    (__v32hi) __A,
					    __B,
					    __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epi16 (__mmask32 __A, __m512h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2w512_mask_round (__B,
					    (__v32hi)
					    _mm512_setzero_si512 (),
					    __A,
					    __C);
}

#else
#define _mm512_cvtt_roundph_epi16(A, B)				    \
  ((__m512i)							    \
   __builtin_ia32_vcvttph2w512_mask_round ((A),			    \
					   (__v32hi)		    \
					   _mm512_setzero_si512 (), \
					   (__mmask32)-1,	    \
					   (B)))

#define _mm512_mask_cvtt_roundph_epi16(A, B, C, D)		\
  ((__m512i)							\
   __builtin_ia32_vcvttph2w512_mask_round ((C),			\
					   (__v32hi)(A),	\
					   (B),			\
					   (D)))

#define _mm512_maskz_cvtt_roundph_epi16(A, B, C)		    \
  ((__m512i)							    \
   __builtin_ia32_vcvttph2w512_mask_round ((B),			    \
					   (__v32hi)		    \
					   _mm512_setzero_si512 (), \
					   (A),			    \
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvttph2uw.  */
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvttph_epu16 (__m512h __A)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__A,
					     (__v32hi)
					     _mm512_setzero_si512 (),
					     (__mmask32) -1,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvttph_epu16 (__m512i __A, __mmask32 __B, __m512h __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__C,
					     (__v32hi) __A,
					     __B,
					     _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvttph_epu16 (__mmask32 __A, __m512h __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__B,
					     (__v32hi)
					     _mm512_setzero_si512 (),
					     __A,
					     _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtt_roundph_epu16 (__m512h __A, int __B)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__A,
					     (__v32hi)
					     _mm512_setzero_si512 (),
					     (__mmask32) -1,
					     __B);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtt_roundph_epu16 (__m512i __A, __mmask32 __B,
				__m512h __C, int __D)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__C,
					     (__v32hi) __A,
					     __B,
					     __D);
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtt_roundph_epu16 (__mmask32 __A, __m512h __B, int __C)
{
  return (__m512i)
    __builtin_ia32_vcvttph2uw512_mask_round (__B,
					     (__v32hi)
					     _mm512_setzero_si512 (),
					     __A,
					     __C);
}

#else
#define _mm512_cvtt_roundph_epu16(A, B)				     \
  ((__m512i)							     \
   __builtin_ia32_vcvttph2uw512_mask_round ((A),		     \
					    (__v32hi)		     \
					    _mm512_setzero_si512 (), \
					    (__mmask32)-1,	     \
					    (B)))

#define _mm512_mask_cvtt_roundph_epu16(A, B, C, D)		\
  ((__m512i)							\
   __builtin_ia32_vcvttph2uw512_mask_round ((C),		\
					    (__v32hi)(A),	\
					    (B),		\
					    (D)))

#define _mm512_maskz_cvtt_roundph_epu16(A, B, C)		     \
  ((__m512i)							     \
   __builtin_ia32_vcvttph2uw512_mask_round ((B),		     \
					    (__v32hi)		     \
					    _mm512_setzero_si512 (), \
					    (A),		     \
					    (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtw2ph.  */
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtepi16_ph (__m512i __A)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __A,
						_mm512_setzero_ph (),
						(__mmask32) -1,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepi16_ph (__m512h __A, __mmask32 __B, __m512i __C)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __C,
						__A,
						__B,
						_MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepi16_ph (__mmask32 __A, __m512i __B)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __B,
						_mm512_setzero_ph (),
						__A,
						_MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepi16_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __A,
						_mm512_setzero_ph (),
						(__mmask32) -1,
						__B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepi16_ph (__m512h __A, __mmask32 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __C,
						__A,
						__B,
						__D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepi16_ph (__mmask32 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtw2ph512_mask_round ((__v32hi) __B,
						_mm512_setzero_ph (),
						__A,
						__C);
}

#else
#define _mm512_cvt_roundepi16_ph(A, B)				\
  (__builtin_ia32_vcvtw2ph512_mask_round ((__v32hi)(A),		\
					  _mm512_setzero_ph (),	\
					  (__mmask32)-1,	\
					  (B)))

#define _mm512_mask_cvt_roundepi16_ph(A, B, C, D)	\
  (__builtin_ia32_vcvtw2ph512_mask_round ((__v32hi)(C),	\
					  (A),		\
					  (B),		\
					  (D)))

#define _mm512_maskz_cvt_roundepi16_ph(A, B, C)			\
  (__builtin_ia32_vcvtw2ph512_mask_round ((__v32hi)(B),		\
					  _mm512_setzero_ph (),	\
					  (A),			\
					  (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtuw2ph.  */
  extern __inline __m512h
  __attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
  _mm512_cvtepu16_ph (__m512i __A)
  {
    return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __A,
						   _mm512_setzero_ph (),
						   (__mmask32) -1,
						   _MM_FROUND_CUR_DIRECTION);
  }

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtepu16_ph (__m512h __A, __mmask32 __B, __m512i __C)
{
  return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __C,
						 __A,
						 __B,
						 _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtepu16_ph (__mmask32 __A, __m512i __B)
{
  return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __B,
						 _mm512_setzero_ph (),
						 __A,
						 _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvt_roundepu16_ph (__m512i __A, int __B)
{
  return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __A,
						 _mm512_setzero_ph (),
						 (__mmask32) -1,
						 __B);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvt_roundepu16_ph (__m512h __A, __mmask32 __B, __m512i __C, int __D)
{
  return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __C,
						 __A,
						 __B,
						 __D);
}

extern __inline __m512h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvt_roundepu16_ph (__mmask32 __A, __m512i __B, int __C)
{
  return __builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi) __B,
						 _mm512_setzero_ph (),
						 __A,
						 __C);
}

#else
#define _mm512_cvt_roundepu16_ph(A, B)					\
  (__builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi)(A),		\
					   _mm512_setzero_ph (),	\
					   (__mmask32)-1,		\
					   (B)))

#define _mm512_mask_cvt_roundepu16_ph(A, B, C, D)		\
  (__builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi)(C),	\
					   (A),			\
					   (B),			\
					   (D)))

#define _mm512_maskz_cvt_roundepu16_ph(A, B, C)				\
  (__builtin_ia32_vcvtuw2ph512_mask_round ((__v32hi)(B),		\
					   _mm512_setzero_ph (),	\
					   (A),				\
					   (C)))

#endif /* __OPTIMIZE__ */

/* Intrinsics vcvtsh2si, vcvtsh2us.  */
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsh_i32 (__m128h __A)
{
  return (int) __builtin_ia32_vcvtsh2si32_round (__A, _MM_FROUND_CUR_DIRECTION);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsh_u32 (__m128h __A)
{
  return (int) __builtin_ia32_vcvtsh2usi32_round (__A,
						  _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundsh_i32 (__m128h __A, const int __R)
{
  return (int) __builtin_ia32_vcvtsh2si32_round (__A, __R);
}

extern __inline unsigned
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundsh_u32 (__m128h __A, const int __R)
{
  return (int) __builtin_ia32_vcvtsh2usi32_round (__A, __R);
}

#else
#define _mm_cvt_roundsh_i32(A, B)		\
  ((int)__builtin_ia32_vcvtsh2si32_round ((A), (B)))
#define _mm_cvt_roundsh_u32(A, B)		\
  ((int)__builtin_ia32_vcvtsh2usi32_round ((A), (B)))

#endif /* __OPTIMIZE__ */

#ifdef __x86_64__
extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsh_i64 (__m128h __A)
{
  return (long long)
    __builtin_ia32_vcvtsh2si64_round (__A, _MM_FROUND_CUR_DIRECTION);
}

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsh_u64 (__m128h __A)
{
  return (long long)
    __builtin_ia32_vcvtsh2usi64_round (__A, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundsh_i64 (__m128h __A, const int __R)
{
  return (long long) __builtin_ia32_vcvtsh2si64_round (__A, __R);
}

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundsh_u64 (__m128h __A, const int __R)
{
  return (long long) __builtin_ia32_vcvtsh2usi64_round (__A, __R);
}

#else
#define _mm_cvt_roundsh_i64(A, B)			\
  ((long long)__builtin_ia32_vcvtsh2si64_round ((A), (B)))
#define _mm_cvt_roundsh_u64(A, B)			\
  ((long long)__builtin_ia32_vcvtsh2usi64_round ((A), (B)))

#endif /* __OPTIMIZE__ */
#endif /* __x86_64__ */

/* Intrinsics vcvtsi2sh, vcvtusi2sh.  */
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvti32_sh (__m128h __A, int __B)
{
  return __builtin_ia32_vcvtsi2sh32_round (__A, __B, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtu32_sh (__m128h __A, unsigned int __B)
{
  return __builtin_ia32_vcvtusi2sh32_round (__A, __B, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundi32_sh (__m128h __A, int __B, const int __R)
{
  return __builtin_ia32_vcvtsi2sh32_round (__A, __B, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundu32_sh (__m128h __A, unsigned int __B, const int __R)
{
  return __builtin_ia32_vcvtusi2sh32_round (__A, __B, __R);
}

#else
#define _mm_cvt_roundi32_sh(A, B, C)		\
  (__builtin_ia32_vcvtsi2sh32_round ((A), (B), (C)))
#define _mm_cvt_roundu32_sh(A, B, C)		\
  (__builtin_ia32_vcvtusi2sh32_round ((A), (B), (C)))

#endif /* __OPTIMIZE__ */

#ifdef __x86_64__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvti64_sh (__m128h __A, long long __B)
{
  return __builtin_ia32_vcvtsi2sh64_round (__A, __B, _MM_FROUND_CUR_DIRECTION);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtu64_sh (__m128h __A, unsigned long long __B)
{
  return __builtin_ia32_vcvtusi2sh64_round (__A, __B, _MM_FROUND_CUR_DIRECTION);
}

#ifdef __OPTIMIZE__
extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundi64_sh (__m128h __A, long long __B, const int __R)
{
  return __builtin_ia32_vcvtsi2sh64_round (__A, __B, __R);
}

extern __inline __m128h
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvt_roundu64_sh (__m128h __A, unsigned long long __B, const int __R)
{
  return __builtin_ia32_vcvtusi2sh64_round (__A, __B, __R);
}

#else
#define _mm_cvt_roundi64_sh(A, B, C)		\
  (__builtin_ia32_vcvtsi2sh64_round ((A), (B), (C)))
#define _mm_cvt_roundu64_sh(A, B, C)		\
  (__builtin_ia32_vcvtusi2sh64_round ((A), (B), (C)))

#endif /* __OPTIMIZE__ */
#endif /* __x86_64__ */


#ifdef __DISABLE_AVX512FP16__
#undef __DISABLE_AVX512FP16__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512FP16__ */

#endif /* __AVX512FP16INTRIN_H_INCLUDED */
