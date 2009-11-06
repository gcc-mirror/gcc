/* Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

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

#ifndef _X86INTRIN_H_INCLUDED
# error "Never use <fma4intrin.h> directly; include <x86intrin.h> instead."
#endif

#ifndef _FMA4INTRIN_H_INCLUDED
#define _FMA4INTRIN_H_INCLUDED

#ifndef __FMA4__
# error "FMA4 instruction set not enabled"
#else

/* We need definitions from the SSE4A, SSE3, SSE2 and SSE header files.  */
#include <ammintrin.h>

/* 128b Floating point multiply/add type instructions.  */
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_ps (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfmaddps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmaddpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_ss (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfmaddss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_sd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmaddsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_ps (__m128 __A, __m128 __B, __m128 __C)

{
  return (__m128) __builtin_ia32_vfmsubps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmsubpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_ss (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfmsubss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_sd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmsubsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_ps (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfnmaddps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfnmaddpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_ss (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfnmaddss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_sd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfnmaddsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_ps (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfnmsubps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfnmsubpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_ss (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfnmsubss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_sd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfnmsubsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maddsub_ps (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfmaddsubps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maddsub_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmaddsubpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msubadd_ps (__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_vfmsubaddps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msubadd_pd (__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_vfmsubaddpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

/* 256b Floating point multiply/add type instructions.  */
extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_macc_ps (__m256 __A, __m256 __B, __m256 __C)
{
  return (__m256) __builtin_ia32_vfmaddps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_macc_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfmaddpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_msub_ps (__m256 __A, __m256 __B, __m256 __C)

{
  return (__m256) __builtin_ia32_vfmsubps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_msub_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfmsubpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_nmacc_ps (__m256 __A, __m256 __B, __m256 __C)
{
  return (__m256) __builtin_ia32_vfnmaddps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_nmacc_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfnmaddpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_nmsub_ps (__m256 __A, __m256 __B, __m256 __C)
{
  return (__m256) __builtin_ia32_vfnmsubps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_nmsub_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfnmsubpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maddsub_ps (__m256 __A, __m256 __B, __m256 __C)
{
  return (__m256) __builtin_ia32_vfmaddsubps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maddsub_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfmaddsubpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_msubadd_ps (__m256 __A, __m256 __B, __m256 __C)
{
  return (__m256) __builtin_ia32_vfmsubaddps256 ((__v8sf)__A, (__v8sf)__B, (__v8sf)__C);
}

extern __inline __m256d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_msubadd_pd (__m256d __A, __m256d __B, __m256d __C)
{
  return (__m256d) __builtin_ia32_vfmsubaddpd256 ((__v4df)__A, (__v4df)__B, (__v4df)__C);
}

#endif

#endif
