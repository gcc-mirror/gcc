/* M32C Pragma support
   Copyright (C) 2004-2016 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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
#include "c-family/c-common.h"
#include "c-family/c-pragma.h"
#include "m32c-protos.h"

/* Implements the "GCC memregs" pragma.  This pragma takes only an
   integer, and is semantically identical to the -memregs= command
   line option.  The only catch is, the programmer should only use
   this pragma at the beginning of the file (preferably, in some
   project-wide header) to avoid ABI changes related to changing the
   list of available "registers".  */
static void
m32c_pragma_memregs (cpp_reader * reader ATTRIBUTE_UNUSED)
{
  /* on off */
  tree val;
  enum cpp_ttype type;
  HOST_WIDE_INT i;

  type = pragma_lex (&val);
  if (type == CPP_NUMBER)
    {
      if (tree_fits_uhwi_p (val))
	{
	  i = tree_to_uhwi (val);

	  type = pragma_lex (&val);
	  if (type != CPP_EOF)
	    warning (0, "junk at end of #pragma GCC memregs [0..16]");

	  if (0 <= i && i <= 16)
	    {
	      if (!ok_to_change_target_memregs)
		{
		  warning (0,
			   "#pragma GCC memregs must precede any function decls");
		  return;
		}
	      target_memregs = i;
	      m32c_conditional_register_usage ();
	    }
	  else
	    {
	      warning (0, "#pragma GCC memregs takes a number [0..16]");
	    }

	  return;
	}
    }

  error ("#pragma GCC memregs takes a number [0..16]");
}

/* Implements the "pragma ADDRESS" pragma.  This pragma takes a
   variable name and an address, and arranges for that variable to be
   "at" that address.  The variable is also made volatile.  */
static void
m32c_pragma_address (cpp_reader * reader ATTRIBUTE_UNUSED)
{
  /* on off */
  tree var, addr;
  enum cpp_ttype type;

  type = pragma_lex (&var);
  if (type == CPP_NAME)
    {
      type = pragma_lex (&addr);
      if (type == CPP_NUMBER)
	{
	  if (var != error_mark_node)
	    {
	      unsigned uaddr = tree_to_uhwi (addr);
	      m32c_note_pragma_address (IDENTIFIER_POINTER (var), uaddr);
	    }

	  type = pragma_lex (&var);
	  if (type != CPP_EOF)
	    {
	      error ("junk at end of #pragma ADDRESS");
	    }
	  return;
	}
    }
  error ("malformed #pragma ADDRESS variable address");
}

/* Implements REGISTER_TARGET_PRAGMAS.  */
void
m32c_register_pragmas (void)
{
  c_register_pragma ("GCC", "memregs", m32c_pragma_memregs);
  c_register_pragma (NULL, "ADDRESS", m32c_pragma_address);
  c_register_pragma (NULL, "address", m32c_pragma_address);

  /* R8C and M16C have 16-bit pointers in a 20-bit address zpace.
     M32C has 24-bit pointers in a 24-bit address space, so does not
     need far pointers, but we accept the qualifier anyway, as a
     no-op.  */
  if (TARGET_A16)
    c_register_addr_space ("__far", ADDR_SPACE_FAR);
  else
    c_register_addr_space ("__far", ADDR_SPACE_GENERIC);
}
