/* Definitions of target machine for GNU compiler.  Iris version 4.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

#include "mips/iris3.h"

/* Assembler is said to have trouble with .ascii with escape chars.
   The quickest way to avoid the problem is not to use .ascii.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)			\
{							\
  unsigned char *s;					\
  int i;						\
  for (i = 0, s = (unsigned char *)(PTR); i < (LEN); s++, i++)	\
    {							\
      if ((i % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));			\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
}
