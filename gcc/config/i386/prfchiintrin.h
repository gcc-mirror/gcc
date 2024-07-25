/* Copyright (C) 2022-2023 Free Software Foundation, Inc.

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

#if !defined _X86GPRINTRIN_H_INCLUDED
# error "Never use <prfchiintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _PRFCHIINTRIN_H_INCLUDED
#define _PRFCHIINTRIN_H_INCLUDED

#ifdef __x86_64__


#ifndef __PREFETCHI__
#pragma GCC push_options
#pragma GCC target("prefetchi")
#define __DISABLE_PREFETCHI__
#endif /* __PREFETCHI__ */

#ifdef __OPTIMIZE__
extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_m_prefetchit0 (void* __P)
{
  __builtin_ia32_prefetchi (__P, 3);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_m_prefetchit1 (void* __P)
{
  __builtin_ia32_prefetchi (__P, 2);
}
#else
#define _m_prefetchit0(P)	\
  __builtin_ia32_prefetchi(P, 3);

#define _m_prefetchit1(P)	\
  __builtin_ia32_prefetchi(P, 2);

#endif

#ifdef __DISABLE_PREFETCHI__
#undef __DISABLE_PREFETCHI__
#pragma GCC pop_options
#endif /* __DISABLE_PREFETCHI__ */

#endif /* __x86_64__ */

#endif /* _PRFCHIINTRIN_H_INCLUDED */
