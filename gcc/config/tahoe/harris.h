/* Definitions of target machine for GNU compiler.  Harris tahoe version.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.

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


#include "tahoe/tahoe.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dtahoe -Dunix -Dhcx -Asystem(unix) -Acpu(tahoe) -Amachine(tahoe)"

#undef DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO

#undef LIB_SPEC

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 1

/* urem and udiv don't exist on this system.  */
#undef UDIVSI3_LIBCALL
#undef UMODSI3_LIBCALL

/* Operand of .align is not logarithmic.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  LOG ? fprintf (FILE, "\t.align %d\n", 1 << (LOG)) : 0

/* For the same reason, we need .align 2 after casesi.  */
#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '@')							\
    putc ('2', FILE);							\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".bss ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,4\n", (ROUNDED)))

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)			\
  output_file_directive ((FILE), main_input_filename);

#define ASM_OUTPUT_ASCII(FILE, PTR, SIZE)		\
do {							\
  const unsigned char *_p = (PTR);			\
  int _thissize = (SIZE);				\
  fprintf ((FILE), "\t.ascii \"");			\
  for (i = 0; i < _thissize; i++)			\
    {							\
      register int c = _p[i];				\
      if (c >= ' ' && c < 0177 && c != '\"' && c != '\\') \
	putc (c, (FILE));				\
      else						\
	{						\
	  fprintf ((FILE), "\\%o", c);			\
	  if (i < _thissize - 1				\
	      && _p[i + 1] >= '0' && _p[i + 1] <= '9')	\
	    fprintf ((FILE), "\"\n\t.ascii \"");	\
	}						\
    }							\
  fprintf ((FILE), "\"\n");				\
} while (0)
