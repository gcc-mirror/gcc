/* Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

/* Implemented from the specification included in the Intel C++ Compiler
   User Guide and Reference, version 9.0.

   NOTE: This is NOT a complete implementation of the SSE4 intrinsics!  */

#ifndef NO_WARN_X86_INTRINSICS
/* This header is distributed to simplify porting x86_64 code that
   makes explicit use of Intel intrinsics to powerpc64le.
   It is the user's responsibility to determine if the results are
   acceptable and make additional changes as necessary.
   Note that much code that uses Intel intrinsics can be rewritten in
   standard C or GNU C extensions, which are more portable and better
   optimized across multiple targets.  */
#endif

#ifndef SMMINTRIN_H_
#define SMMINTRIN_H_

#include <altivec.h>
#include <tmmintrin.h>

extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_extract_epi8 (__m128i __X, const int __N)
{
  return (unsigned char) ((__v16qi)__X)[__N & 15];
}

extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_extract_epi32 (__m128i __X, const int __N)
{
  return ((__v4si)__X)[__N & 3];
}

extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_extract_epi64 (__m128i __X, const int __N)
{
  return ((__v2di)__X)[__N & 1];
}

extern __inline int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_extract_ps (__m128 __X, const int __N)
{
  return ((__v4si)__X)[__N & 3];
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_blend_epi16 (__m128i __A, __m128i __B, const int __imm8)
{
  __v16qi __charmask = vec_splats ((signed char) __imm8);
  __charmask = vec_gb (__charmask);
  __v8hu __shortmask = (__v8hu) vec_unpackh (__charmask);
  #ifdef __BIG_ENDIAN__
  __shortmask = vec_reve (__shortmask);
  #endif
  return (__m128i) vec_sel ((__v8hu) __A, (__v8hu) __B, __shortmask);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_blendv_epi8 (__m128i __A, __m128i __B, __m128i __mask)
{
  const __v16qu __seven = vec_splats ((unsigned char) 0x07);
  __v16qu __lmask = vec_sra ((__v16qu) __mask, __seven);
  return (__m128i) vec_sel ((__v16qu) __A, (__v16qu) __B, __lmask);
}

#endif
