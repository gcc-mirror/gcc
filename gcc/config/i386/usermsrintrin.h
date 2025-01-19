/* Copyright (C) 2022-2025 Free Software Foundation, Inc.

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
#error "Never use <usermsrintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _USER_MSRINTRIN_H_INCLUDED
#define _USER_MSRINTRIN_H_INCLUDED

#ifdef __x86_64__

#ifndef __USER_MSR__
#pragma GCC push_options
#pragma GCC target("usermsr")
#define __DISABLE_USER_MSR__
#endif /* __USER_MSR__ */

extern __inline unsigned long long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_urdmsr (unsigned long long __A)
{
  return (unsigned long long) __builtin_ia32_urdmsr (__A);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_uwrmsr (unsigned long long __A, unsigned long long __B)
{
  __builtin_ia32_uwrmsr (__A, __B);
}

#ifdef __DISABLE_USER_MSR__
#undef __DISABLE_USER_MSR__
#pragma GCC pop_options
#endif /* __DISABLE_USER_MSR__ */

#endif /* __x86_64__ */

#endif /* _USER_MSRINTRIN_H_INCLUDED */
