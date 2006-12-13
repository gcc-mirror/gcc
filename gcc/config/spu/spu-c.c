/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cpplib.h"
#include "tree.h"
#include "c-tree.h"
#include "c-pragma.h"
#include "function.h"
#include "rtl.h"
#include "expr.h"
#include "errors.h"
#include "tm_p.h"
#include "langhooks.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "recog.h"
#include "optabs.h"
#include "spu-builtins.h"


/* target hook for resolve_overloaded_builtin(). Returns a function call
   RTX if we can resolve the overloaded builtin */
tree
spu_resolve_overloaded_builtin (tree fndecl, tree fnargs)
{
#define SCALAR_TYPE_P(t) (INTEGRAL_TYPE_P (t) \
			  || SCALAR_FLOAT_TYPE_P (t) \
			  || POINTER_TYPE_P (t))
  spu_function_code new_fcode, fcode =
    DECL_FUNCTION_CODE (fndecl) - END_BUILTINS;
  struct spu_builtin_description *desc;
  tree match = NULL_TREE;

  /* The vector types are not available if the backend is not initialized.  */
  gcc_assert (!flag_preprocess_only);

  desc = &spu_builtins[fcode];
  if (desc->type != B_OVERLOAD)
    return NULL_TREE;

  /* Compare the signature of each internal builtin function with the
     function arguments until a match is found. */

  for (new_fcode = fcode + 1; spu_builtins[new_fcode].type == B_INTERNAL;
       new_fcode++)
    {
      tree decl = spu_builtins[new_fcode].fndecl;
      tree params = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree arg, param;
      int p;

      for (param = params, arg = fnargs, p = 0;
	   param != void_list_node;
	   param = TREE_CHAIN (param), arg = TREE_CHAIN (arg), p++)
	{
	  tree var, arg_type, param_type = TREE_VALUE (param);

	  if (!arg)
	    {
	      error ("insufficient arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  var = TREE_VALUE (arg);

	  if (TREE_CODE (var) == NON_LVALUE_EXPR)
	    var = TREE_OPERAND (var, 0);

	  if (TREE_CODE (var) == ERROR_MARK)
	    return NULL_TREE;	/* Let somebody else deal with the problem. */

	  arg_type = TREE_TYPE (var);

	  /* The intrinsics spec does not specify precisely how to
	     resolve generic intrinsics.  We require an exact match
	     for vector types and let C do it's usual parameter type
	     checking/promotions for scalar arguments, except for the
	     first argument of intrinsics which don't have a vector
	     parameter. */
	  if ((!SCALAR_TYPE_P (param_type)
	       || !SCALAR_TYPE_P (arg_type)
	       || ((fcode == SPU_SPLATS || fcode == SPU_PROMOTE
		    || fcode == SPU_HCMPEQ || fcode == SPU_HCMPGT
		    || fcode == SPU_MASKB || fcode == SPU_MASKH
		    || fcode == SPU_MASKW) && p == 0))
	      && !comptypes (TYPE_MAIN_VARIANT (param_type),
			     TYPE_MAIN_VARIANT (arg_type)))
	    break;
	}
      if (param == void_list_node)
	{
	  if (arg)
	    {
	      error ("too many arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  match = decl;
	  break;
	}
    }

  if (match == NULL_TREE)
    {
      error ("parameter list does not match a valid signature for %s()",
	     desc->name);
      return error_mark_node;
    }

  return build_function_call (match, fnargs);
#undef SCALAR_TYPE_P
}


void
spu_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  builtin_define_std ("__SPU__");
  cpp_assert (pfile, "cpu=spu");
  cpp_assert (pfile, "machine=spu");
  builtin_define_std ("__vector=__attribute__((__spu_vector__))");
}

void
spu_c_common_override_options (void)
{ 
  if (!TARGET_STD_MAIN)
    {
      /* Don't give warnings about the main() function.  */
      warn_main = 0;
    }
}
