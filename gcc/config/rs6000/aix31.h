/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running AIX version 3.1.
   Copyright (C) 1993,1997, 2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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


#include "rs6000/rs6000.h"
#include "rs6000/aix.h"

/* Output something to declare an external symbol to the assembler.  Most
   assemblers don't need this.

   If we haven't already, add "[RW]" (or "[DS]" for a function) to the
   name.  Normally we write this out along with the name.  In the few cases
   where we can't, it gets stripped off.  */

#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
{ rtx _symref = XEXP (DECL_RTL (DECL), 0);	\
  if ((TREE_CODE (DECL) == VAR_DECL		\
       || TREE_CODE (DECL) == FUNCTION_DECL)	\
      && (NAME)[strlen (NAME) - 1] != ']')	\
    {						\
      char *_name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5); \
      strcpy (_name, XSTR (_symref, 0));	\
      strcat (_name, TREE_CODE (DECL) == FUNCTION_DECL ? "[DS]" : "[RW]"); \
      XSTR (_symref, 0) = _name;		\
    }						\
  fputs ("\t.extern ", FILE);			\
  assemble_name (FILE, XSTR (_symref, 0));	\
  if (TREE_CODE (DECL) == FUNCTION_DECL)	\
    {						\
      fputs ("\n\t.extern .", FILE);		\
      RS6000_OUTPUT_BASENAME (FILE, XSTR (_symref, 0));	\
    }						\
  putc ('\n', FILE);				\
}

/* Similar, but for libcall.  We only have to worry about the function name,
   not that of the descriptor. */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
{ fputs ("\t.extern .", FILE);			\
  assemble_name (FILE, XSTR (FUN, 0));		\
  putc ('\n', FILE);				\
}

/* AIX 3.2 defined _AIX32, but older versions do not.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_IBMR2 -D_AIX -Asystem(unix) -Asystem(aix) -Acpu(rs6000) -Amachine(rs6000)"

/* AIX 3.1 uses bit 15 in CROR as the magic nop.  */
#undef RS6000_CALL_GLUE
#define RS6000_CALL_GLUE "cror 15,15,15"

/* AIX 3.1 does not prepend underscores to itrunc, uitrunc, or mcount.  */
#undef RS6000_ITRUNC
#define RS6000_ITRUNC "itrunc"
#undef RS6000_UITRUNC
#define RS6000_UITRUNC "uitrunc"
#undef RS6000_MCOUNT
#define RS6000_MCOUNT ".mcount"

