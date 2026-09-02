/* Copyright (C) 2026 Free Software Foundation, Inc.

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
#error "Never use <acev1intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _ACEV1INTRIN_H_INCLUDED
#define _ACEV1INTRIN_H_INCLUDED

#if !defined(__ACEV1__)
#pragma GCC push_options
#pragma GCC target("acev1")
#define __DISABLE_ACEV1__
#endif /* __ACEV1__ */

#if defined(__x86_64__)

#define _tile_ace_loadconfig(A)			\
  __builtin_ia32_ldtilecfg (A)

#define _tile_ace_storeconfig(A)		\
  __builtin_ia32_sttilecfg (A)

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_ace_release (void)
{
  __asm__ volatile ("tilerelease" ::);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_init ()
{
  __builtin_ia32_bsr0init ();
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_insertfull (__m512i __A, __m512i __B)
{
  __builtin_ia32_bsr0movf ((__v16si) __A, (__v16si) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_inserth (__m512i __A)
{
  __builtin_ia32_bsr0movhinsert ((__v16si) __A);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_extracth ()
{
  return (__m512i) __builtin_ia32_bsr0movhextract ();
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_insertl (__m512i __A)
{
  __builtin_ia32_bsr0movlinsert ((__v16si) __A);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bsr0_extractl ()
{
  return (__m512i) __builtin_ia32_bsr0movlextract ();
}

#ifdef __OPTIMIZE__
extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_ace_zero (const int __A)
{
  __builtin_ia32_tilezero (__A);
}

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_cvtrow_epi32_ps (const int __A, int __B)
{
  return (__m512) __builtin_ia32_tcvtrowd2ps (__A, __B);
}

extern __inline __m512bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_cvtrowh_ps_pbh (const int __A, int __B)
{
  return (__m512bh) __builtin_ia32_tcvtrowps2bf16h (__A, __B);
}

extern __inline __m512bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_cvtrowl_ps_pbh (const int __A, int __B)
{
  return (__m512bh) __builtin_ia32_tcvtrowps2bf16l (__A, __B);
}

extern __inline __m512h
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_cvtrowh_ps_ph (const int __A, int __B)
{
  return (__m512h) __builtin_ia32_tcvtrowps2phh (__A, __B);
}

extern __inline __m512h
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_cvtrowl_ps_ph (const int __A, int __B)
{
  return (__m512h) __builtin_ia32_tcvtrowps2phl (__A, __B);
}

extern __inline __m512i
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_extractrow (const int __A, int __B)
{
  return (__m512i) __builtin_ia32_tilemovrowextract (__A, __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_insertrow (const int __A, __m512i __B, int __C)
{
  __builtin_ia32_tilemovrowinsert (__A, (__v16si) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_insertcol (const int __A, __m512i __B, int __C)
{
  __builtin_ia32_tilemovcolinsert (__A, (__v16si) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op2bf16_ps (const int __W, __m512bh __A, __m512bh __B)
{
  __builtin_ia32_top2bf16ps (__W, (__v32bf) __A, (__v32bf) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4bssd_epi32 (const int __W, __m512i __A, __m512i __B)
{
  __builtin_ia32_top4bssd (__W, (__v64qi) __A, (__v64qi) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4bsud_epi32 (const int __W, __m512i __A, __m512i __B)
{
  __builtin_ia32_top4bsud (__W, (__v64qi) __A, (__v64qi) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4busd_epi32 (const int __W, __m512i __A, __m512i __B)
{
  __builtin_ia32_top4busd (__W, (__v64qi) __A, (__v64qi) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4buud_epi32 (const int __W, __m512i __A, __m512i __B)
{
  __builtin_ia32_top4buud (__W, (__v64qi) __A, (__v64qi) __B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4mxbf8_ps (const int __W, __m512i __A, __m512i __B, const int __C)
{
  __builtin_ia32_top4mxbf8ps (__W, (__v64qi) __A, (__v64qi) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4mxbhf8_ps (const int __W, __m512i __A, __m512i __B, const int __C)
{
  __builtin_ia32_top4mxbhf8ps (__W, (__v64qi) __A, (__v64qi) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4mxhbf8_ps (const int __W, __m512i __A, __m512i __B, const int __C)
{
  __builtin_ia32_top4mxhbf8ps (__W, (__v64qi) __A, (__v64qi) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4mxhf8_ps (const int __W, __m512i __A, __m512i __B, const int __C)
{
  __builtin_ia32_top4mxhf8ps (__W, (__v64qi) __A, (__v64qi) __B, __C);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_op4mxbss_ps (const int __W, __m512i __A, __m512i __B, const int __C)
{
  __builtin_ia32_top4mxbssps (__W, (__v64qi) __A, (__v64qi) __B, __C);
}

#else
#define _tile_ace_zero(A)			\
  __builtin_ia32_tilezero (A);

#define _tile_cvtrow_epi32_ps(A, B)		\
  (__m512) __builtin_ia32_tcvtrowd2ps ((A), (B))

#define _tile_cvtrowh_ps_pbh(A, B)		\
  (__m512bh) __builtin_ia32_tcvtrowps2bf16h ((A), (B))

#define _tile_cvtrowl_ps_pbh(A, B)		\
  (__m512bh) __builtin_ia32_tcvtrowps2bf16l ((A), (B))

#define _tile_cvtrowh_ps_ph(A, B)		\
  (__m512h) __builtin_ia32_tcvtrowps2phh ((A), (B))

#define _tile_cvtrowl_ps_ph(A, B)		\
  (__m512h) __builtin_ia32_tcvtrowps2phl ((A), (B))

#define _tile_extractrow(A, B)			\
  (__m512i) __builtin_ia32_tilemovrowextract ((A), (B))

#define _tile_insertrow(A, B, C)		\
  __builtin_ia32_tilemovrowinsert ((A), (__v16si) (B), (C))

#define _tile_insertcol(A, B, C)		\
  __builtin_ia32_tilemovcolinsert ((A), (__v16si) (B), (C))

#define _tile_op2bf16_ps(W, A, B)		\
  __builtin_ia32_top2bf16ps (W, (__v32bf) (A), (__v32bf) (B))

#define _tile_op4bssd_epi32(W, A, B)		\
  __builtin_ia32_top4bssd (W, (__v64qi) (A), (__v64qi) (B))

#define _tile_op4bsud_epi32(W, A, B)		\
  __builtin_ia32_top4bsud (W, (__v64qi) (A), (__v64qi) (B))

#define _tile_op4busd_epi32(W, A, B)		\
  __builtin_ia32_top4busd (W, (__v64qi) (A), (__v64qi) (B))

#define _tile_op4buud_epi32(W, A, B)		\
  __builtin_ia32_top4buud (W, (__v64qi) (A), (__v64qi) (B))

#define _tile_op4mxbf8_ps(W, A, B, C)		\
  __builtin_ia32_top4mxbf8ps (W, (__v64qi) (A), (__v64qi) (B), C)

#define _tile_op4mxbhf8_ps(W, A, B, C)		\
  __builtin_ia32_top4mxbhf8ps (W, (__v64qi) (A), (__v64qi) (B), C)

#define _tile_op4mxhbf8_ps(W, A, B, C)		\
  __builtin_ia32_top4mxhbf8ps (W, (__v64qi) (A), (__v64qi) (B), C)

#define _tile_op4mxhf8_ps(W, A, B, C)		\
  __builtin_ia32_top4mxhf8ps (W, (__v64qi) (A), (__v64qi) (B), C)

#define _tile_op4mxbss_ps(W, A, B, C)		\
  __builtin_ia32_top4mxbssps (W, (__v64qi) (A), (__v64qi) (B), C)

#endif /* __OPTIMIZE__ */

#endif /* __x86_64__ */

#ifdef __DISABLE_ACEV1__
#undef __DISABLE_ACEV1__
#pragma GCC pop_options
#endif /* __DISABLE_ACEV1__ */

#endif /* _ACEV1INTRIN_H_INCLUDED */
