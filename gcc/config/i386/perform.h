/* Definitions for AT&T assembler syntax for the Intel 80386.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Defines to be able to build libgcc.a with GCC.  */

/* It might seem that these are not important, since gcc 2 will never
   call libgcc for these functions.  But programs might be linked with
   code compiled by gcc 1, and then these will be used.  */

/* The arg names used to be a and b, but `a' appears inside strings
   and that confuses non-ANSI cpp.  */

#define perform_udivsi3(arg0,arg1)					\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = arg0;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (arg1), "d" (dx)); \
  return ax;								\
}

#define perform_divsi3(arg0,arg1)					\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
  register int cx asm("cx");						\
									\
  ax = arg0;								\
  cx = arg1;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=&d" (dx) : "a" (ax), "c" (cx)); \
  return ax;								\
}

#define perform_umodsi3(arg0,arg1)					\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = arg0;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (arg1), "d" (dx)); \
  return dx;								\
}

#define perform_modsi3(arg0,arg1)					\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
  register int cx asm("cx");						\
									\
  ax = arg0;								\
  cx = arg1;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=&d" (dx) : "a" (ax), "c" (cx)); \
  return dx;								\
}

#define perform_fixdfsi(arg0)						\
{									\
  auto unsigned short ostatus;						\
  auto unsigned short nstatus;						\
  auto int ret;								\
  auto double tmp;							\
									\
  &ostatus;			/* guarantee these land in memory */	\
  &nstatus;								\
  &ret;									\
  &tmp;									\
									\
  asm volatile ("fnstcw %0" : "=m" (ostatus));				\
  nstatus = ostatus | 0x0c00;						\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (nstatus));		\
  tmp = arg0;								\
  asm volatile ("fldl %0" : /* no outputs */ : "m" (tmp));		\
  asm volatile ("fistpl %0" : "=m" (ret));				\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (ostatus));		\
									\
  return ret;								\
}

