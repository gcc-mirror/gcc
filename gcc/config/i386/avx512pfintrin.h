/* Copyright (C) 2013
   Free Software Foundation, Inc.

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
#error "Never use <avx512pfintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512PFINTRIN_H_INCLUDED
#define _AVX512PFINTRIN_H_INCLUDED

#ifndef __AVX512PF__
#pragma GCC push_options
#pragma GCC target("avx512pf")
#define __DISABLE_AVX512PF__
#endif /* __AVX512PF__ */

/* Internal data types for implementing the intrinsics.  */
typedef long long __v8di __attribute__ ((__vector_size__ (64)));
typedef int __v16si __attribute__ ((__vector_size__ (64)));

/* The Intel API is flexible enough that we must allow aliasing with other
   vector types, and their scalar components.  */
typedef long long __m512i __attribute__ ((__vector_size__ (64), __may_alias__));

typedef unsigned char  __mmask8;
typedef unsigned short __mmask16;

#ifdef __OPTIMIZE__
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_prefetch_i32gather_ps (__m512i index, __mmask16 mask,
				   int const *addr, int scale, int hint)
{
  __builtin_ia32_gatherpfdps (mask, (__v16si) index, addr, scale, hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_prefetch_i64gather_ps (__m512i index, __mmask8 mask,
				   int const *addr, int scale, int hint)
{
  __builtin_ia32_gatherpfqps (mask, (__v8di) index, addr, scale, hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_prefetch_i32scatter_ps (int const *addr, __m512i index, int scale,
			       int hint)
{
  __builtin_ia32_scatterpfdps ((__mmask16) 0xFFFF, (__v16si) index, addr, scale,
			       hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_prefetch_i32scatter_ps (int const *addr, __mmask16 mask,
				    __m512i index, int scale, int hint)
{
  __builtin_ia32_scatterpfdps (mask, (__v16si) index, addr, scale, hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_prefetch_i64scatter_ps (int const *addr, __m512i index, int scale,
			       int hint)
{
  __builtin_ia32_scatterpfqps ((__mmask8) 0xFF, (__v8di) index, addr, scale,
			       hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_prefetch_i64scatter_ps (int const *addr, __mmask16 mask,
				    __m512i index, int scale, int hint)
{
  __builtin_ia32_scatterpfqps (mask, (__v8di) index, addr, scale, hint);
}
#else
#define _mm512_mask_prefetch_i32gather_ps(INDEX, MASK, ADDR, SCALE, HINT)    \
  __builtin_ia32_gatherpfdps ((__mmask16)MASK, (__v16si)(__m512i)INDEX,	     \
			      (int const *)ADDR, (int)SCALE, (int)HINT)

#define _mm512_mask_prefetch_i64gather_ps(INDEX, MASK, ADDR, SCALE, HINT)    \
  __builtin_ia32_gatherpfqps ((__mmask8)MASK, (__v8di)(__m512i)INDEX,	     \
			      (int const *)ADDR, (int)SCALE, (int)HINT)

#define _mm512_prefetch_i32scatter_ps(ADDR, INDEX, SCALE, HINT)              \
  __builtin_ia32_scatterpfdps ((__mmask16)0xFFFF, (__v16si)(__m512i)INDEX,   \
			       (int const *)ADDR, (int)SCALE, (int)HINT)

#define _mm512_mask_prefetch_i32scatter_ps(ADDR, MASK, INDEX, SCALE, HINT)   \
  __builtin_ia32_scatterpfdps ((__mmask16)MASK, (__v16si)(__m512i)INDEX,     \
			       (int const *)ADDR, (int)SCALE, (int)HINT)

#define _mm512_prefetch_i64scatter_ps(ADDR, INDEX, SCALE, HINT)              \
  __builtin_ia32_scatterpfqps ((__mmask8)0xFF, (__v8di)(__m512i)INDEX,	     \
			       (int const *)ADDR, (int)SCALE, (int)HINT)

#define _mm512_mask_prefetch_i64scatter_ps(ADDR, MASK, INDEX, SCALE, HINT)   \
  __builtin_ia32_scatterpfqps ((__mmask8)MASK, (__v8di)(__m512i)INDEX,	     \
			       (int const *)ADDR, (int)SCALE, (int)HINT)
#endif

#ifdef __DISABLE_AVX512PF__
#undef __DISABLE_AVX512PF__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512PF__ */

#endif /* _AVX512PFINTRIN_H_INCLUDED */
