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

#ifdef __DISABLE_AVX512FP16__
#undef __DISABLE_AVX512FP16__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512FP16__ */

#endif /* __AVX512FP16INTRIN_H_INCLUDED */
