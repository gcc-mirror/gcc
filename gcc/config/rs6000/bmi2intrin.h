/* Copyright (C) 2011-2019 Free Software Foundation, Inc.

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

#if !defined _X86INTRIN_H_INCLUDED
# error "Never use <bmi2intrin.h> directly; include <x86intrin.h> instead."
#endif

#ifndef _BMI2INTRIN_H_INCLUDED
#define _BMI2INTRIN_H_INCLUDED

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bzhi_u32 (unsigned int __X, unsigned int __Y)
{
  return ((__X << (32 - __Y)) >> (32 - __Y));
}

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mulx_u32 (unsigned int __X, unsigned int __Y, unsigned int *__P)
{
  unsigned long long __res = (unsigned long long) __X * __Y;
  *__P = (unsigned int) (__res >> 32);
  return (unsigned int) __res;
}

#ifdef  __PPC64__
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_bzhi_u64 (unsigned long long __X, unsigned long long __Y)
{
  return ((__X << (64 - __Y)) >> (64 - __Y));
}

/* __int128 requires base 64-bit.  */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mulx_u64 (unsigned long long __X, unsigned long long __Y,
	   unsigned long long *__P)
{
  unsigned __int128 __res = (unsigned __int128) __X * __Y;
  *__P = (unsigned long long) (__res >> 64);
  return (unsigned long long) __res;
}

#ifdef  _ARCH_PWR7
/* popcount and bpermd require power7 minimum.  */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_pdep_u64 (unsigned long long __X, unsigned long long __M)
{
  unsigned long result = 0x0UL;
  const unsigned long mask = 0x8000000000000000UL;
  unsigned long m = __M;
  unsigned long c, t;
  unsigned long p;

  /* The pop-count of the mask gives the number of the bits from
   source to process.  This is also needed to shift bits from the
   source into the correct position for the result.  */
  p = 64 - __builtin_popcountl (__M);

  /* The loop is for the number of '1' bits in the mask and clearing
   each mask bit as it is processed.  */
  while (m != 0)
    {
      c = __builtin_clzl (m);
      t = __X << (p - c);
      m ^= (mask >> c);
      result |= (t & (mask >> c));
      p++;
    }
  return (result);
}

extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_pext_u64 (unsigned long long __X, unsigned long long __M)
{
  unsigned long p = 0x4040404040404040UL; // initial bit permute control
  const unsigned long mask = 0x8000000000000000UL;
  unsigned long m = __M;
  unsigned long c;
  unsigned long result;

  /* if the mask is constant and selects 8 bits or less we can use
   the Power8 Bit permute instruction.  */
  if (__builtin_constant_p (__M) && (__builtin_popcountl (__M) <= 8))
    {
      /* Also if the pext mask is constant, then the popcount is
       constant, we can evaluate the following loop at compile
       time and use a constant bit permute vector.  */
      for (long i = 0; i < __builtin_popcountl (__M); i++)
	{
	  c = __builtin_clzl (m);
	  p = (p << 8) | c;
	  m ^= (mask >> c);
	}
      result = __builtin_bpermd (p, __X);
    }
  else
    {
      p = 64 - __builtin_popcountl (__M);
      result = 0;
      /* We could a use a for loop here, but that combined with
       -funroll-loops can expand to a lot of code.  The while
       loop avoids unrolling and the compiler commons the xor
       from clearing the mask bit with the (m != 0) test.  The
       result is a more compact loop setup and body.  */
      while (m != 0)
	{
	  unsigned long t;
	  c = __builtin_clzl (m);
	  t = (__X & (mask >> c)) >> (p - c);
	  m ^= (mask >> c);
	  result |= (t);
	  p++;
	}
    }
  return (result);
}

/* these 32-bit implementations depend on 64-bit pdep/pext
   which depend on _ARCH_PWR7.  */
extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_pdep_u32 (unsigned int __X, unsigned int __Y)
{
  return _pdep_u64 (__X, __Y);
}

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_pext_u32 (unsigned int __X, unsigned int __Y)
{
  return _pext_u64 (__X, __Y);
}
#endif /* _ARCH_PWR7  */
#endif /* __PPC64__  */

#endif /* _BMI2INTRIN_H_INCLUDED */
