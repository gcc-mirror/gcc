/* Subroutines for insn-output.c for NetWare.
   Contributed by Jan Beulich (jbeulich@novell.com)
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "output.h"
#include "tree.h"
#include "flags.h"
#include "tm_p.h"
#include "toplev.h"
#include "ggc.h"


/* Return string which is the former assembler name modified with an 
   underscore prefix and a suffix consisting of an atsign (@) followed
   by the number of bytes of arguments */

static const char *
gen_stdcall_decoration (tree decl)
{
  unsigned total = 0;
  /* ??? This probably should use XSTR (XEXP (DECL_RTL (decl), 0), 0) instead
     of DECL_ASSEMBLER_NAME.  */
  const char *asmname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *newsym;

  if (TYPE_ARG_TYPES (TREE_TYPE (decl)))
    if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (decl))))
        == void_type_node)
      {
	tree formal_type = TYPE_ARG_TYPES (TREE_TYPE (decl));

	/* Quit if we hit an incomplete type.  Error is reported
	   by convert_arguments in c-typeck.c or cp/typeck.c.  */
	while (TREE_VALUE (formal_type) != void_type_node
	       && COMPLETE_TYPE_P (TREE_VALUE (formal_type)))	
	  {
	    unsigned parm_size
	      = TREE_INT_CST_LOW (TYPE_SIZE (TREE_VALUE (formal_type)));
	    /* Must round up to include padding.  This is done the same
	       way as in store_one_arg.  */
	    parm_size = ((parm_size + PARM_BOUNDARY - 1)
			 / PARM_BOUNDARY * PARM_BOUNDARY);
	    total += parm_size;
	    formal_type = TREE_CHAIN (formal_type);
	  }
      }

  newsym = alloca (1 + strlen (asmname) + 1 + 10 + 1);
  return IDENTIFIER_POINTER (get_identifier_with_length (newsym,
	sprintf (newsym, "_%s@%u", asmname, total / BITS_PER_UNIT)));
}

/* Return string which is the former assembler name modified with a
   prefix consisting of FASTCALL_PREFIX and a suffix consisting of an
   atsign (@) followed by the number of bytes of arguments.  */

static const char *
gen_fastcall_decoration (tree decl)
{
  unsigned total = 0;
  const char *asmname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *newsym;

  if (TYPE_ARG_TYPES (TREE_TYPE (decl)))
    if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (decl))))
        == void_type_node)
      {
	tree formal_type = TYPE_ARG_TYPES (TREE_TYPE (decl));

	/* Quit if we hit an incomplete type.  Error is reported
	   by convert_arguments in c-typeck.c or cp/typeck.c.  */
	while (TREE_VALUE (formal_type) != void_type_node
	       && COMPLETE_TYPE_P (TREE_VALUE (formal_type)))	
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

  newsym = alloca (1 + strlen (asmname) + 1 + 10 + 1);
  return IDENTIFIER_POINTER (get_identifier_with_length (newsym,
	sprintf (newsym, "%c%s@%d", FASTCALL_PREFIX, asmname,
		 total / BITS_PER_UNIT)));
}

/* Return string which is the former assembler name modified with an 
   _n@ prefix where n represents the number of arguments passed in
   registers */

static const char *
gen_regparm_prefix (tree decl, unsigned nregs)
{
  unsigned total = 0;
  /* ??? This probably should use XSTR (XEXP (DECL_RTL (decl), 0), 0) instead
     of DECL_ASSEMBLER_NAME.  */
  const char *asmname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  char *newsym;

  if (TYPE_ARG_TYPES (TREE_TYPE (decl)))
    if (TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (decl))))
        == void_type_node)
      {
	tree formal_type = TYPE_ARG_TYPES (TREE_TYPE (decl));

	/* Quit if we hit an incomplete type.  Error is reported
	   by convert_arguments in c-typeck.c or cp/typeck.c.  */
	while (TREE_VALUE (formal_type) != void_type_node
	       && COMPLETE_TYPE_P (TREE_VALUE (formal_type)))	
	  {
	    unsigned parm_size
	      = TREE_INT_CST_LOW (TYPE_SIZE (TREE_VALUE (formal_type)));
	    /* Must round up to include padding.  This is done the same
	       way as in store_one_arg.  */
	    parm_size = ((parm_size + PARM_BOUNDARY - 1)
			 / PARM_BOUNDARY * PARM_BOUNDARY);
	    total += parm_size;
	    formal_type = TREE_CHAIN (formal_type);
	  }
      }

  if (nregs > total / BITS_PER_WORD)
    nregs = total / BITS_PER_WORD;
  if (nregs > 9) abort();
  newsym = alloca (2 + strlen (asmname) + 1 + 1);
  return IDENTIFIER_POINTER (get_identifier_with_length (newsym,
	sprintf (newsym, "_%u@%s", nregs, asmname)));
}

void
i386_nlm_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (first
      && TREE_CODE (decl) == FUNCTION_DECL
      && *IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)) != '*')
    {
      tree type_attributes = TYPE_ATTRIBUTES (TREE_TYPE (decl));
      rtx rtlname = XEXP (rtl, 0);
      if (GET_CODE (rtlname) == MEM)
	rtlname = XEXP (rtlname, 0);
      if (lookup_attribute ("stdcall", type_attributes))
	XSTR (rtlname, 0) = gen_stdcall_decoration (decl);
      else if (lookup_attribute ("fastcall", type_attributes))
	XSTR (rtlname, 0) = gen_fastcall_decoration (decl);
      else
	{
	  tree attr = lookup_attribute ("regparm", type_attributes);

	  if (attr)
	    XSTR (rtlname, 0) =
	      gen_regparm_prefix (decl,
		      TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr))));
	}
    }
}

/* Strip the stdcall/fastcall/regparm pre-/suffix.  */

const char *
i386_nlm_strip_name_encoding (const char *str)
{
  const char *name = default_strip_name_encoding (str);

  if (*str != '*' && (*name == '_' || *name == '@'))
    {
      const char *p = strchr (name + 1, '@');

      if (p)
	{
	  ++name;
	  if (ISDIGIT (p[1]))
	    name = ggc_alloc_string (name, p - name);
	  else if (!ISDIGIT (*name) || ++name != p)
	    abort();
	}
    }
  return name;
}
