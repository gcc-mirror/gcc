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

#ifdef __DISABLE_AVX512FP16__
#undef __DISABLE_AVX512FP16__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512FP16__ */

#endif /* __AVX512FP16INTRIN_H_INCLUDED */
