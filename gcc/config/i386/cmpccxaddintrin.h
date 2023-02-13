/* Copyright (C) 2012-2023 Free Software Foundation, Inc.

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

#ifndef _X86GPRINTRIN_H_INCLUDED
#error "Never use <cmpccxaddintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _CMPCCXADDINTRIN_H_INCLUDED
#define _CMPCCXADDINTRIN_H_INCLUDED

#ifdef __x86_64__

#ifndef __CMPCCXADD__
#pragma GCC push_options
#pragma GCC target("cmpccxadd")
#define __DISABLE_CMPCCXADD__
#endif /* __CMPCCXADD__ */

typedef enum {
    _CMPCCX_O,   /* Overflow.  */
    _CMPCCX_NO,  /* No overflow.  */
    _CMPCCX_B,   /* Below.  */
    _CMPCCX_NB,  /* Not below.  */
    _CMPCCX_Z,   /* Zero.  */
    _CMPCCX_NZ,  /* Not zero.  */
    _CMPCCX_BE,  /* Below or equal.  */
    _CMPCCX_NBE, /* Neither below nor equal.  */
    _CMPCCX_S,   /* Sign.  */
    _CMPCCX_NS,  /* No sign.  */
    _CMPCCX_P,   /* Parity.  */
    _CMPCCX_NP,  /* No parity.  */
    _CMPCCX_L,   /* Less.  */
    _CMPCCX_NL,  /* Not less.  */
    _CMPCCX_LE,  /* Less or equal.  */
    _CMPCCX_NLE, /* Neither less nor equal.  */
} _CMPCCX_ENUM;

#ifdef __OPTIMIZE__
extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_cmpccxadd_epi32 (int *__A, int __B, int __C, const _CMPCCX_ENUM __D)
{
  return __builtin_ia32_cmpccxadd (__A, __B, __C, __D);
}

extern __inline long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_cmpccxadd_epi64 (long long *__A, long long __B, long long __C,
		   const _CMPCCX_ENUM __D)
{
  return __builtin_ia32_cmpccxadd64 (__A, __B, __C, __D);
}
#else
#define _cmpccxadd_epi32(A,B,C,D) \
  __builtin_ia32_cmpccxadd ((int *) (A), (int) (B), (int) (C), \
			    (_CMPCCX_ENUM) (D))
#define _cmpccxadd_epi64(A,B,C,D) \
  __builtin_ia32_cmpccxadd64 ((long long *) (A), (long long) (B), \
			      (long long) (C), (_CMPCCX_ENUM) (D))
#endif

#ifdef __DISABLE_CMPCCXADD__
#undef __DISABLE_CMPCCXADD__
#pragma GCC pop_options
#endif /* __DISABLE_CMPCCXADD__ */

#endif

#endif /* _CMPCCXADDINTRIN_H_INCLUDED */
