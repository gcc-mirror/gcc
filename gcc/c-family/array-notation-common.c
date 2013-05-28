/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains the builtin functions for Array
   notations.
   Copyright (C) 2013  Free Software Foundation, Inc.
   Contributed by Balaji V. Iyer <balaji.v.iyer@intel.com>,
                  Intel Corporation

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h" 
#include "coretypes.h"
#include "tree.h"
#include "langhooks.h" 
#include "tree-iterator.h"
#include "diagnostic-core.h"


/* Returns true if the function call in FNDECL is  __sec_implicit_index.  */

bool
is_sec_implicit_index_fn (tree fndecl)
{
  if (TREE_CODE (fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND (fndecl, 0);

  return
    (TREE_CODE (fndecl) == FUNCTION_DECL
     && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
     && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CILKPLUS_SEC_IMPLICIT_INDEX);
}

/* Returns the first and only argument for FN, which should be a
   sec_implicit_index function.  FN's location in the source file is as 
   indicated by LOCATION.  The argument to FN must be a constant integer
   expression, otherwise returns -1.  */

HOST_WIDE_INT
extract_sec_implicit_index_arg (location_t location, tree fn)
{
  tree fn_arg;
  HOST_WIDE_INT return_int = 0;

  if (TREE_CODE (fn) == CALL_EXPR)
    {
      fn_arg = CALL_EXPR_ARG (fn, 0);
      if (TREE_CODE (fn_arg) == INTEGER_CST)
	return_int = int_cst_value (fn_arg);
      else
	{
	  /* If the location is unknown, and if fn has a location, then use that
	     information so that the user has a better idea where the error
	     could be.  */
	  if (location == UNKNOWN_LOCATION && EXPR_HAS_LOCATION (fn))
	    location = EXPR_LOCATION (fn);
	  error_at (location, "__sec_implicit_index parameter must be an " 
		    "integer constant expression");
	  return -1;
	}
    }
  return return_int;
}
