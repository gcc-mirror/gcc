/* Configuration for GNU compiler for processor running Interix
   Copyright (C) 1993, 1995, 1999, 2001 Free Software Foundation, Inc.
   Donn Terry, Softway Systems, Inc,
   from code
      Contributed by Douglas B. Rupp (drupp@cs.washington.edu)

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Our strategy for finding global constructors is a bit different, although
   not a lot. */
#define DO_GLOBAL_CTORS_BODY						\
do {									\
  int i;								\
  unsigned long nptrs;							\
  func_ptr *p;								\
  asm(									\
       "     .section .ctor_head, \"rw\"\n"				\
       "1:\n"								\
       "     .text \n"							\
       ASM_LOAD_ADDR(1b,%0)						\
       : "=r" (p) : : "cc");						\
  for (nptrs = 0; p[nptrs] != 0; nptrs++);				\
  for (i = nptrs-1; i >= 0; i--)					\
    p[i] ();								\
} while (0) 

#define DO_GLOBAL_DTORS_BODY						\
do {									\
  func_ptr *p;								\
  asm(									\
       "     .section .dtor_head, \"rw\"\n"				\
       "1:\n"								\
       "     .text \n"							\
       ASM_LOAD_ADDR(1b,%0)						\
       : "=r" (p) : : "cc");						\
  while (*p)								\
    {									\
      p++;								\
      (*(p-1)) ();							\
    }									\
} while (0) 
