/* PRU target specific pragmas
   Copyright (C) 2015-2020 Free Software Foundation, Inc.
   Contributed by Dimitar Dimitrov <dimitar@dinux.eu>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "c-family/c-pragma.h"
#include "c-family/c-common.h"
#include "diagnostic-core.h"
#include "cpplib.h"
#include "pru-protos.h"


/* Implements the "pragma CTABLE_ENTRY" pragma.  This pragma takes a
   CTABLE index and an address, and instructs the compiler that
   LBCO/SBCO can be used on that base address.

   WARNING: Only immediate constant addresses are currently supported.  */
static void
pru_pragma_ctable_entry (cpp_reader * reader ATTRIBUTE_UNUSED)
{
  tree ctable_index, base_addr;
  enum cpp_ttype type;

  type = pragma_lex (&ctable_index);
  if (type == CPP_NUMBER && tree_fits_uhwi_p (ctable_index))
    {
      type = pragma_lex (&base_addr);
      if (type == CPP_NUMBER  && tree_fits_uhwi_p (base_addr))
	{
	  unsigned HOST_WIDE_INT i = tree_to_uhwi (ctable_index);
	  unsigned HOST_WIDE_INT base = tree_to_uhwi (base_addr);

	  type = pragma_lex (&base_addr);
	  if (type != CPP_EOF)
	    error ("junk at end of %<#pragma CTABLE_ENTRY%>");
	  else if (i >= ARRAY_SIZE (pru_ctable))
	    error ("%<CTABLE_ENTRY%> index %" HOST_WIDE_INT_PRINT "d"
		   " is not valid", i);
	  else if (pru_ctable[i].valid && pru_ctable[i].base != base)
	    error ("redefinition of %<CTABLE_ENTRY "
		   "%" HOST_WIDE_INT_PRINT "d%>", i);
	  else
	    {
	      if (base & 0xff)
		warning (0, "%<CTABLE_ENTRY%> base address is not "
			    "a multiple of 256");
	      pru_ctable[i].base = base;
	      pru_ctable[i].valid = true;
	    }
	  return;
	}
    }
  error ("malformed %<#pragma CTABLE_ENTRY%> variable address");
}

/* Implements REGISTER_TARGET_PRAGMAS.  */
void
pru_register_pragmas (void)
{
  c_register_pragma (NULL, "ctable_entry", pru_pragma_ctable_entry);
  c_register_pragma (NULL, "CTABLE_ENTRY", pru_pragma_ctable_entry);
}
