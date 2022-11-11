/*
   Execute symbolically all paths of the function.  Iterate loops only once․
   Copyright (C) 2006-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.   */

#include "symb-execute-all-paths.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "cfgloop.h"
#include "gimple-range.h"
#include "tree-scalar-evolution.h"
#include "hwint.h"
#include "gimple-pretty-print.h"
#include "function.h"

void get_function_local_ssa_vars (function *fun)
{
  unsigned ix;
  tree name;
  // get ssa names of the function, yet print sizes and names
  FOR_EACH_SSA_NAME (ix, name, fun)
    {
      if (TREE_CODE (TREE_TYPE (name)) == INTEGER_TYPE)
      {
	if (TYPE_UNSIGNED (TREE_TYPE (name)))
	{
	  if (dump_file)
	    fprintf (dump_file,
		   "unsigned "); //we need this info in symb execution
	}
	if (dump_file)
	  fprintf (dump_file, " size is %ld ",
		 tree_to_shwi (TYPE_SIZE (TREE_TYPE (name))));
      } else if (TREE_CODE (TREE_TYPE (name)) == POINTER_TYPE)
      {
	if (dump_file)
	  fprintf (stderr, "pointer type size is %ld ",
		 tree_to_shwi (TYPE_SIZE (TREE_TYPE (name))));
      } else
	continue;
     if (dump_file)
     {
       print_generic_expr (dump_file, name, dump_flags);
       fprintf (dump_file, "\n");
     }
    }
}

void make_symbolic_function_arguments_and_sizes (function *fun)
{
  // Get size and name of function's arguments
  for (tree arg = DECL_ARGUMENTS (fun->decl); arg; arg = DECL_CHAIN (arg))
  {
    if (TREE_CODE (DECL_SIZE (arg)) == INTEGER_CST && DECL_NAME (arg))
    {
      if (dump_file)
	fprintf (dump_file, "%s : %ld ",
		 IDENTIFIER_POINTER (DECL_NAME (arg)),
		 tree_to_shwi (DECL_SIZE (arg)));
    } else
      if (dump_file)
	fprintf (dump_file, "Argument not const or no name");
  }
}

