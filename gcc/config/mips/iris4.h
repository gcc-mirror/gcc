/* Definitions of target machine for GNU compiler.  Iris version 4.
   Copyright (C) 1991, 1993 Free Software Foundation, Inc.

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

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

#include "mips/iris3.h"

/* Profiling is supported via libprof1.a not -lc_p as in Irix 3.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{!p:%{!pg:%{!static:%{!g*:-lc_s}}}}%{p:libprof1.a%s}%{pg:libprof1.a%s} -lc crtn.o%s"

/* Some assemblers have a bug that causes backslash escaped chars in .ascii
   to be misassembled, so we just completely avoid it.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)			\
do {							\
  unsigned char *s;					\
  int i;						\
  for (i = 0, s = (unsigned char *)(PTR); i < (LEN); s++, i++)	\
    {							\
      if ((i % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));			\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
} while (0)
