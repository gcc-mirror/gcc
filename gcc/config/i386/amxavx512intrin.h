/* Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#if !defined _IMMINTRIN_H_INCLUDED
#error "Never use <amxavx512intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AMXAVX512INTRIN_H_INCLUDED
#define _AMXAVX512INTRIN_H_INCLUDED

#if !defined(__AMX_AVX512__)
#pragma GCC push_options
#pragma GCC target("amx-avx512")
#define __DISABLE_AMX_AVX512__
#endif /* __AMX_AVX512__ */

#if defined(__x86_64__)
#define _tile_cvtrowd2ps_internal(src,A)				\
({									\
  __m512 dst;								\
  __asm__ volatile							\
  ("{tcvtrowd2ps\t%1, %%tmm"#src", %0|tcvtrowd2ps\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_cvtrowd2psi_internal(src,imm)				\
({									\
  __m512 dst;								\
  __asm__ volatile							\
  ("{tcvtrowd2ps\t$"#imm", %%tmm"#src", %0|tcvtrowd2ps\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_cvtrowps2bf16h_internal(src,A)				\
({									\
  __m512bh dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2bf16h\t%1, %%tmm"#src", %0|tcvtrowps2bf16h\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_cvtrowps2bf16hi_internal(src,imm)			\
({									\
  __m512bh dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2bf16h\t$"#imm", %%tmm"#src", %0|tcvtrowps2bf16h\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_cvtrowps2bf16l_internal(src,A)				\
({									\
  __m512bh dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2bf16l\t%1, %%tmm"#src", %0|tcvtrowps2bf16l\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_cvtrowps2bf16li_internal(src,imm)			\
({									\
  __m512bh dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2bf16l\t$"#imm", %%tmm"#src", %0|tcvtrowps2bf16l\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_cvtrowps2phh_internal(src,A)				\
({									\
  __m512h dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2phh\t%1, %%tmm"#src", %0|tcvtrowps2phh\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_cvtrowps2phhi_internal(src,imm)				\
({									\
  __m512h dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2phh\t$"#imm", %%tmm"#src", %0|tcvtrowps2phh\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_cvtrowps2phl_internal(src,A)				\
({									\
  __m512h dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2phl\t%1, %%tmm"#src", %0|tcvtrowps2phl\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_cvtrowps2phli_internal(src,imm)				\
({									\
  __m512h dst;								\
  __asm__ volatile							\
  ("{tcvtrowps2phl\t$"#imm", %%tmm"#src", %0|tcvtrowps2phl\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_movrow_internal(src,A)					\
({									\
  __m512 dst;								\
  __asm__ volatile							\
  ("{tilemovrow\t%1, %%tmm"#src", %0|tilemovrow\t%0, %%tmm"#src", %1}"	\
   : "=v" (dst) : "r" ((unsigned) (A)));				\
  dst;									\
})

#define _tile_movrowi_internal(src,imm)					\
({									\
  __m512 dst;								\
  __asm__ volatile							\
  ("{tilemovrow\t$"#imm", %%tmm"#src", %0|tilemovrow\t%0, %%tmm"#src", "#imm"}"	\
   : "=v" (dst) :);							\
  dst;									\
})

#define _tile_cvtrowd2ps(src,A)					\
  _tile_cvtrowd2ps_internal (src,A)

#define _tile_cvtrowd2psi(src,imm)				\
  _tile_cvtrowd2psi_internal (src,imm)

#define _tile_cvtrowps2bf16h(src,A)				\
  _tile_cvtrowps2bf16h_internal (src,A)

#define _tile_cvtrowps2bf16hi(src,imm)				\
  _tile_cvtrowps2bf16hi_internal (src,imm)

#define _tile_cvtrowps2bf16l(src,A)				\
  _tile_cvtrowps2bf16l_internal (src,A)

#define _tile_cvtrowps2bf16li(src,imm)				\
  _tile_cvtrowps2bf16li_internal (src,imm)

#define _tile_cvtrowps2phh(src,A)				\
  _tile_cvtrowps2phh_internal (src,A)

#define _tile_cvtrowps2phhi(src,imm)				\
  _tile_cvtrowps2phhi_internal (src,imm)

#define _tile_cvtrowps2phl(src,A)				\
  _tile_cvtrowps2phl_internal (src,A)

#define _tile_cvtrowps2phli(src,imm)				\
  _tile_cvtrowps2phli_internal (src,imm)

#define _tile_movrow(src,A)					\
  _tile_movrow_internal (src,A)

#define _tile_movrowi(src,imm)					\
  _tile_movrowi_internal (src,imm)

#endif

#ifdef __DISABLE_AMX_AVX512__
#undef __DISABLE_AMX_AVX512__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_AVX512__ */

#endif /* _AMXAVX512INTRIN_H_INCLUDED */
