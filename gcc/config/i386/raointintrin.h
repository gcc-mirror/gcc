/* Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
#error "Never use <raointintrin.h> directly; include <x86gprintrin.h> instead."
#endif // _X86GPRINTRIN_H_INCLUDED

#ifndef __RAOINTINTRIN_H_INCLUDED
#define __RAOINTINTRIN_H_INCLUDED

#ifndef __RAOINT__
#pragma GCC push_options
#pragma GCC target("raoint")
#define __DISABLE_RAOINT__
#endif /* __RAOINT__ */

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aadd_i32 (int *__A, int __B)
{
  __builtin_ia32_aadd32 ((int *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aand_i32 (int *__A, int __B)
{
  __builtin_ia32_aand32 ((int *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aor_i32 (int *__A, int __B)
{
  __builtin_ia32_aor32 ((int *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_axor_i32 (int *__A, int __B)
{
  __builtin_ia32_axor32 ((int *)__A, __B);
}

#ifdef __x86_64__
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aadd_i64 (long long *__A, long long __B)
{
  __builtin_ia32_aadd64 ((long long *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aand_i64 (long long *__A, long long __B)
{
  __builtin_ia32_aand64 ((long long *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_aor_i64 (long long *__A, long long __B)
{
  __builtin_ia32_aor64 ((long long *)__A, __B);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_axor_i64 (long long *__A, long long __B)
{
  __builtin_ia32_axor64 ((long long *)__A, __B);
}
#endif /* __x86_64__ */

#ifdef __DISABLE_RAOINT__
#undef __DISABLE_RAOINT__
#pragma GCC pop_options
#endif /* __DISABLE_RAOINT__ */

#endif /* __RAOINTINTRIN_H_INCLUDED */
