/* Subroutines for insn-output.c for NetWare.
   Contributed by Jan Beulich (jbeulich@novell.com)
   Copyright (C) 2004, 2005, 2007, 2008 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
#include "langhooks.h"
#include "ggc.h"

/* Return string which is the function name, identified by ID, modified
   with PREFIX and a suffix consisting of an atsign (@) followed by the
   number of bytes of arguments.  If ID is NULL use the DECL_NAME as base.
   Return NULL if no change required.  */

static tree
gen_stdcall_or_fastcall_decoration (tree decl, tree id, char prefix)
{
  unsigned HOST_WIDE_INT total = 0;
  const char *old_str = IDENTIFIER_POINTER (id != NULL_TREE ? id : DECL_NAME (decl));
  char *new_str;
  tree type = TREE_TYPE (decl);

  if (prototype_p (type))
    {
      tree arg;
      function_args_iterator args_iter;

      /* This attribute is ignored for variadic functions.  */ 
      if (stdarg_p (type))
	return NULL_TREE;

      /* Quit if we hit an incomplete type.  Error is reported
	 by convert_arguments in c-typeck.c or cp/typeck.c.  */
      FOREACH_FUNCTION_ARGS(type, arg, args_iter)
	{
	  HOST_WIDE_INT parm_size;
	  unsigned HOST_WIDE_INT parm_boundary_bytes;

	  if (! COMPLETE_TYPE_P (arg))
	    break;

	  parm_size = int_size_in_bytes (arg);
	  if (parm_size < 0)
	    break;

	  parm_boundary_bytes = PARM_BOUNDARY / BITS_PER_UNIT;

	  /* Must round up to include padding.  This is done the same
	     way as in store_one_arg.  */
	  total += (parm_size + parm_boundary_bytes - 1)
		   / parm_boundary_bytes * parm_boundary_bytes;
	}
    }

  new_str = XALLOCAVEC (char, 1 + strlen (old_str) + 1 + 10 + 1);
  sprintf (new_str, "%c%s@" HOST_WIDE_INT_PRINT_UNSIGNED,
	   prefix, old_str, total);

  return get_identifier (new_str);
}

/* Return string which is the function name, identified by ID, modified
   with an _n@ prefix (where n represents the number of arguments passed in
   registers).  If ID is NULL use the DECL_NAME as base.
   Return NULL if no change required.  */

static tree
gen_regparm_prefix (tree decl, tree id, unsigned int nregs)
{
  unsigned HOST_WIDE_INT total = 0;
  const char *old_str = IDENTIFIER_POINTER (id != NULL_TREE ? id : DECL_NAME (decl));
  char *new_str;
  tree type = TREE_TYPE (decl);

  if (prototype_p (type))
    {
      tree arg;
      function_args_iterator args_iter;

      /* This attribute is ignored for variadic functions.  */ 
      if (stdarg_p (type))
	return NULL_TREE;

      /* Quit if we hit an incomplete type.  Error is reported
	 by convert_arguments in c-typeck.c or cp/typeck.c.  */
      FOREACH_FUNCTION_ARGS(type, arg, args_iter)
	{
	  HOST_WIDE_INT parm_size;
	  unsigned HOST_WIDE_INT parm_boundary_bytes;

	  if (! COMPLETE_TYPE_P (arg))
	    break;

	  parm_size = int_size_in_bytes (arg);
	  if (parm_size < 0)
	    break;

	  parm_boundary_bytes = PARM_BOUNDARY / BITS_PER_UNIT;

	  /* Must round up to include padding.  This is done the same
	     way as in store_one_arg.  */
	  total += (parm_size + parm_boundary_bytes - 1)
		   / parm_boundary_bytes * parm_boundary_bytes;
	}
    }

  if (nregs > total / UNITS_PER_WORD)
    nregs = total / UNITS_PER_WORD;
  gcc_assert (nregs <= 9);
  new_str = XALLOCAVEC (char, 3 + strlen (old_str) + 1);
  sprintf (new_str, "_%u@%s", nregs, old_str);

  return get_identifier (new_str);
}

/* Maybe decorate and get a new identifier for the DECL of a stdcall or
   fastcall function. The original identifier is supplied in ID. */

static tree
i386_nlm_maybe_mangle_decl_assembler_name (tree decl, tree id)
{
  tree type_attributes = TYPE_ATTRIBUTES (TREE_TYPE (decl));
  tree new_id;

  if (lookup_attribute ("stdcall", type_attributes))
    new_id = gen_stdcall_or_fastcall_decoration (decl, id, '_');
  else if (lookup_attribute ("fastcall", type_attributes))
    new_id = gen_stdcall_or_fastcall_decoration (decl, id, FASTCALL_PREFIX);
  else if ((new_id = lookup_attribute ("regparm", type_attributes)))
    new_id = gen_regparm_prefix (decl, id,
		  TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (new_id))));
  else
    new_id = NULL_TREE;

  return new_id;
}

/* This is used as a target hook to modify the DECL_ASSEMBLER_NAME
   in the language-independent default hook
   langhooks.c:lhd_set_decl_assembler_name ()
   and in cp/mangle.c:mangle_decl ().  */
tree
i386_nlm_mangle_decl_assembler_name (tree decl, tree id)
{
  tree new_id = TREE_CODE (decl) == FUNCTION_DECL
		? i386_nlm_maybe_mangle_decl_assembler_name (decl, id)
		: NULL_TREE;

  return (new_id ? new_id : id);
}

void
i386_nlm_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (TREE_CODE (decl) == FUNCTION_DECL
      /* Do not change the identifier if a verbatim asmspec
	 or if stdcall suffix already added.  */
      && *IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)) != '*'
      && !strchr (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), '@')
      /* FIXME:  Imported stdcall names are not modified by the Ada frontend.
	 Check and decorate the RTL name now.  */
      && strcmp (lang_hooks.name, "GNU Ada") == 0)
    {
      rtx symbol = XEXP (rtl, 0);
      tree new_id;
      tree old_id = DECL_ASSEMBLER_NAME (decl);

      gcc_assert (GET_CODE (symbol) == SYMBOL_REF);

      if ((new_id = i386_nlm_maybe_mangle_decl_assembler_name (decl, old_id)))
	XSTR (symbol, 0) = IDENTIFIER_POINTER (new_id);
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
	  else
	    {
	      gcc_assert (ISDIGIT (*name));
	      name++;
	      gcc_assert (name == p);
	    }
	}
    }
  return name;
}
