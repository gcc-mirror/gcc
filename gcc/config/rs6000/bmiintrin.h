/* Copyright (C) 2010-2023 Free Software Foundation, Inc.

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

/* This header is distributed to simplify porting x86_64 code that
   makes explicit use of Intel intrinsics to powerpc64le.
   It is the user's responsibility to determine if the results are
   acceptable and make additional changes as necessary.
   Note that much code that uses Intel intrinsics can be rewritten in
   standard C or GNU C extensions, which are more portable and better
   optimized across multiple targets.  */

#if !defined _X86GPRINTRIN_H_INCLUDED
# error "Never use <bmiintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _BMIINTRIN_H_INCLUDED
#define _BMIINTRIN_H_INCLUDED

extern __inline unsigned short __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__tzcnt_u16 (unsigned short __X)
{
  return __builtin_ctz (__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__andn_u32 (unsigned int __X, unsigned int __Y)
{
  return (~__X & __Y);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bextr_u32 (unsigned int __X, unsigned int __P, unsigned int __L)
{
  return ((__X << (32 - (__L + __P))) >> (32 - __L));
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bextr_u32 (unsigned int __X, unsigned int __Y)
{
  unsigned int __P, __L;
  __P = __Y & 0xFF;
  __L = (__Y >> 8) & 0xFF;
  return (_bextr_u32 (__X, __P, __L));
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsi_u32 (unsigned int __X)
{
  return (__X & -__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsi_u32 (unsigned int __X)
{
  return __blsi_u32 (__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsmsk_u32 (unsigned int __X)
{
  return (__X ^ (__X - 1));
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsmsk_u32 (unsigned int __X)
{
  return __blsmsk_u32 (__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsr_u32 (unsigned int __X)
{
  return (__X & (__X - 1));
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsr_u32 (unsigned int __X)
{
  return __blsr_u32 (__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__tzcnt_u32 (unsigned int __X)
{
  return __builtin_ctz (__X);
}

extern __inline unsigned int __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tzcnt_u32 (unsigned int __X)
{
  return __builtin_ctz (__X);
}

/* use the 64-bit shift, rotate, and count leading zeros instructions
   for long long.  */
#ifdef  __PPC64__
extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__andn_u64 (unsigned long long __X, unsigned long long __Y)
{
  return (~__X & __Y);
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bextr_u64 (unsigned long long __X, unsigned int __P, unsigned int __L)
{
  return ((__X << (64 - (__L + __P))) >> (64 - __L));
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bextr_u64 (unsigned long long __X, unsigned long long __Y)
{
  unsigned int __P, __L;
  __P = __Y & 0xFF;
  __L = (__Y & 0xFF00) >> 8;
  return (_bextr_u64 (__X, __P, __L));
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsi_u64 (unsigned long long __X)
{
  return __X & -__X;
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsi_u64 (unsigned long long __X)
{
  return __blsi_u64 (__X);
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsmsk_u64 (unsigned long long __X)
{
  return (__X ^ (__X - 1));
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsmsk_u64 (unsigned long long __X)
{
  return __blsmsk_u64 (__X);
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__blsr_u64 (unsigned long long __X)
{
  return (__X & (__X - 1));
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_blsr_u64 (unsigned long long __X)
{
  return __blsr_u64 (__X);
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__tzcnt_u64 (unsigned long long __X)
{
  return __builtin_ctzll (__X);
}

extern __inline unsigned long long __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_tzcnt_u64 (unsigned long long __X)
{
  return __builtin_ctzll (__X);
}
#endif /* __PPC64__  */

#endif /* _BMIINTRIN_H_INCLUDED */
