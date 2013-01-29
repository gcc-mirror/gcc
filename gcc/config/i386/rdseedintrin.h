/* Copyright (C) 2012-2013 Free Software Foundation, Inc.

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

#if !defined _X86INTRIN_H_INCLUDED
# error "Never use <rdseedintrin.h> directly; include <x86intrin.h> instead."
#endif

#ifndef __RDSEED__
# error "RDSEED instruction not enabled"
#endif /* __RDSEED__ */

#ifndef _RDSEEDINTRIN_H_INCLUDED
#define _RDSEEDINTRIN_H_INCLUDED

extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdseed16_step (unsigned short *p)
{
    return __builtin_ia32_rdseed_hi_step (p);
}

extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdseed32_step (unsigned int *p)
{
    return __builtin_ia32_rdseed_si_step (p);
}

#ifdef __x86_64__
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdseed64_step (unsigned long long *p)
{
    return __builtin_ia32_rdseed_di_step (p);
}
#endif

#endif /* _RDSEEDINTRIN_H_INCLUDED */
