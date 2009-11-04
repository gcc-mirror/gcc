/* Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

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

#ifndef _X86INTRIN_H_INCLUDED
# error "Never use <lwpintrin.h> directly; include <x86intrin.h> instead."
#endif

#ifndef _LWPINTRIN_H_INCLUDED
#define _LWPINTRIN_H_INCLUDED

#ifndef __LWP__
# error "LWP instruction set not enabled"
#else

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__llwpcb16 (void *pcbAddress)
{
  __builtin_ia32_llwpcb16 (pcbAddress);
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__llwpcb32 (void *pcbAddress)
{
  __builtin_ia32_llwpcb32 (pcbAddress);
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__llwpcb64 (void *pcbAddress)
{
  __builtin_ia32_llwpcb64 (pcbAddress);
}

extern __inline void * __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__slwpcb16 (void)
{
  return __builtin_ia32_slwpcb16 ();
}

extern __inline void * __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__slwpcb32 (void)
{
  return __builtin_ia32_slwpcb32 ();
}

extern __inline void * __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__slwpcb64 (void)
{
  return __builtin_ia32_slwpcb64 ();
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpval16 (unsigned short data2, unsigned int data1, unsigned short flags)
{
  __builtin_ia32_lwpval16 (data2, data1, flags);
}
/*
extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpval32 (unsigned int data2, unsigned int data1, unsigned int flags)
{
  __builtin_ia32_lwpval32 (data2, data1, flags);
}

extern __inline void __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpval64 (unsigned __int64 data2, unsigned int data1, unsigned int flags)
{
  __builtin_ia32_lwpval64 (data2, data1, flags);
}

extern __inline unsigned char __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpins16 (unsigned short data2, unsigned int data1, unsigned short flags)
{
  return __builtin_ia32_lwpins16 (data2, data1, flags);
}

extern __inline unsigned char __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpins32 (unsigned int data2, unsigned int data1, unsigned int flags)
{
  return __builtin_ia32_lwpins32 (data2, data1, flags);
}

extern __inline unsigned char __attribute__((__gnu_inline__, __always_inline__, __artificial__))
__lwpins64 (unsigned __int64 data2, unsigned int data1, unsigned int flags)
{
  return __builtin_ia32_lwpins64 (data2, data1, flags);
}
*/
#endif /* __LWP__ */

#endif /* _LWPINTRIN_H_INCLUDED */
