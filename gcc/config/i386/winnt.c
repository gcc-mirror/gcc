/* Subroutines for insn-output.c for Windows NT.
   Contributed by Douglas Rupp (drupp@cs.washington.edu)
   Copyright (C) 1995 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "tree.h"
#include "flags.h"

/* Return string which is the former assembler name modified with a 
   suffix consisting of an atsign (@) followed by the number of bytes of 
   arguments */

char *
gen_stdcall_suffix (decl)
  tree decl;
{
  int total = 0;
  char *asmname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *newsym;

  if (TYPE_ARG_TYPES (TREE_TYPE (decl)))
    if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (decl)))) 
        == void_type_node)
      {
	tree formal_type = TYPE_ARG_TYPES (TREE_TYPE (decl));

	while (TREE_VALUE (formal_type) != void_type_node)
	  {
	    int parm_size
	      = TREE_INT_CST_LOW (TYPE_SIZE (TREE_VALUE (formal_type)));
	    /* Must round up to include padding.  This is done the same
	       way as in store_one_arg.  */
	    parm_size = ((parm_size + PARM_BOUNDARY - 1)
			 / PARM_BOUNDARY * PARM_BOUNDARY);
	    total += parm_size;
	    formal_type = TREE_CHAIN (formal_type);
	  }
      }

  newsym = xmalloc (strlen (asmname) + 10);
  sprintf (newsym, "%s@%d", asmname, total/BITS_PER_UNIT);
  return IDENTIFIER_POINTER (get_identifier (newsym));
}

/* Cover function for UNIQUE_SECTION.  */

void
i386_pe_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len;
  char *name,*string,*prefix;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* The object is put in, for example, section .text$foo.
     The linker will then ultimately place them in .text
     (everything from the $ on is stripped).  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    prefix = ".text$";
  else if (DECL_READONLY_SECTION (decl, reloc))
    prefix = ".rdata$";
  else
    prefix = ".data$";
  len = strlen (name) + strlen (prefix);
  string = alloca (len + 1);
  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}
