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

#ifdef __OPTIMIZE__
extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tile_ace_zero (const int __A)
{
  __builtin_ia32_tilezero (__A);
}

#else
#define _tile_ace_zero(A)			\
  __builtin_ia32_tilezero (A);

#endif /* __OPTIMIZE__ */

#endif /* __x86_64__ */

#ifdef __DISABLE_ACEV1__
#undef __DISABLE_ACEV1__
#pragma GCC pop_options
#endif /* __DISABLE_ACEV1__ */

#endif /* _ACEV1INTRIN_H_INCLUDED */
