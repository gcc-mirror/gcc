/* Copyright (C) 2021-2023 Free Software Foundation, Inc.

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
#error "Never use <avxneconvertintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVXNECONVERTINTRIN_H_INCLUDED
#define _AVXNECONVERTINTRIN_H_INCLUDED

#ifndef __AVXNECONVERT__
#pragma GCC push_options
#pragma GCC target ("avxneconvert")
#define __DISABLE_AVXNECONVERT__
#endif /* __AVXNECONVERT__ */

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_bcstnebf16_ps (const void *__P)
{
  return (__m128) __builtin_ia32_vbcstnebf162ps128 ((const __bf16 *) __P);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bcstnebf16_ps (const void *__P)
{
  return (__m256) __builtin_ia32_vbcstnebf162ps256 ((const __bf16 *) __P);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_bcstnesh_ps (const void *__P)
{
  return (__m128) __builtin_ia32_vbcstnesh2ps128 ((const _Float16 *) __P);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_bcstnesh_ps (const void *__P)
{
  return (__m256) __builtin_ia32_vbcstnesh2ps256 ((const _Float16 *) __P);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneebf16_ps (const __m128bh *__A)
{
  return (__m128) __builtin_ia32_vcvtneebf162ps128 ((const __v8bf *) __A);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneebf16_ps (const __m256bh *__A)
{
  return (__m256) __builtin_ia32_vcvtneebf162ps256 ((const __v16bf *) __A);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneeph_ps (const __m128h *__A)
{
  return (__m128) __builtin_ia32_vcvtneeph2ps128 ((const __v8hf *) __A);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneeph_ps (const __m256h *__A)
{
  return (__m256) __builtin_ia32_vcvtneeph2ps256 ((const __v16hf *) __A);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneobf16_ps (const __m128bh *__A)
{
  return (__m128) __builtin_ia32_vcvtneobf162ps128 ((const __v8bf *) __A);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneobf16_ps (const __m256bh *__A)
{
  return (__m256) __builtin_ia32_vcvtneobf162ps256 ((const __v16bf *) __A);
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneoph_ps (const __m128h *__A)
{
  return (__m128) __builtin_ia32_vcvtneoph2ps128 ((const __v8hf *) __A);
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneoph_ps (const __m256h *__A)
{
  return (__m256) __builtin_ia32_vcvtneoph2ps256 ((const __v16hf *) __A);
}

extern __inline __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneps_avx_pbh (__m128 __A)
{
  return (__m128bh) __builtin_ia32_cvtneps2bf16_v4sf (__A);
}

extern __inline __m128bh
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneps_avx_pbh (__m256 __A)
{
  return (__m128bh) __builtin_ia32_cvtneps2bf16_v8sf (__A);
}

#ifdef __DISABLE_AVXNECONVERT__
#undef __DISABLE_AVXNECONVERT__
#pragma GCC pop_options
#endif /* __DISABLE_AVXNECONVERT__ */

#endif /* _AVXNECONVERTINTRIN_H_INCLUDED */
