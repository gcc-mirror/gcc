/* Lowering routines for formal holes.
   Copyright (C) 2026 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Get the symbol associated with the formal hole P. *ADDRP is set to `true' if
   the string denotation in the formal hole starts with `&'.  */

static const char *
get_hole_symbol (NODE_T *p, bool *addrp)
{
  NODE_T *str = NEXT_SUB (p);
  if (IS (str, LANGUAGE_INDICANT))
    FORWARD (str);
  gcc_assert (IS (str, TERTIARY));
  while (str != NO_NODE && !IS (str, ROW_CHAR_DENOTATION))
    str = SUB (str);
  gcc_assert (IS (str, ROW_CHAR_DENOTATION));

  const char *cstr = NSYMBOL (str);
  if (strlen (cstr) > 0 && cstr[0] == '&' && addrp != NULL)
    {
      *addrp = true;
      cstr = cstr + 1;
    }

  return a68_string_process_breaks (p, cstr);
}

/* Build and return a var decl providing access to the formal hole P.  */

tree
a68_wrap_formal_var_hole (NODE_T *p)
{
  gcc_assert (!IS (MOID (p), PROC_SYMBOL));
  const char *symbol = get_hole_symbol (p, NULL /* addrp */);
  return a68_make_formal_hole_decl (p, symbol);
}

/* Build the body for a wrapper to the formal hole in P, which is of a proc
   mode.  The body is installed in the function_decl WRAPPER.  */

void
a68_wrap_formal_proc_hole (NODE_T *p, tree wrapper)
{
  gcc_assert (IS (MOID (p), PROC_SYMBOL));

  bool addrp;
  const char *symbol = get_hole_symbol (p, &addrp);
  gcc_assert (addrp == false);

  /* Create a wrapper function.  */

  MOID_T *m = MOID (p);

  /* Determine how many arguments we need for the wrapped function. */
  int wrapped_nargs = 0;
  for (PACK_T *z = PACK (m); z != NO_PACK; FORWARD (z))
    {
      if (MOID(z) == M_STRING)
	wrapped_nargs += 3;
      else
	wrapped_nargs += 1;
    }

  /* Now build the type of the wrapped function.  */

  tree *wrapped_args_types = XALLOCAVEC (tree, wrapped_nargs);
  int nwrappedarg = 0;
  for (PACK_T *z = PACK (m); z != NO_PACK; FORWARD (z))
    {
      if (MOID (z) == M_STRING)
	{
	  wrapped_args_types[nwrappedarg++] = build_pointer_type (a68_char_type);
	  wrapped_args_types[nwrappedarg++] = size_type_node;
	  wrapped_args_types[nwrappedarg++] = size_type_node;
	}
      else
	{
	      wrapped_args_types[nwrappedarg++] = CTYPE (MOID (z));
	}
    }

  tree wrapper_ret_type = TREE_TYPE (TREE_TYPE (wrapper));
  tree wrapped_type = build_function_type_array (wrapper_ret_type,
						 wrapped_nargs,
						 wrapped_args_types);
      
  /* And a decl for the wrapped function.  */
  tree wrapped = build_decl (UNKNOWN_LOCATION,
			     FUNCTION_DECL,
			     get_identifier (symbol),
			     wrapped_type);
  DECL_EXTERNAL (wrapped) = 1;
  TREE_PUBLIC (wrapped) = 1;
  DECL_ARTIFICIAL (wrapped) = 1;
  DECL_VISIBILITY (wrapped) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (wrapped) = 1;

  announce_function (wrapper);

  vec<tree, va_gc> *wrapped_args;
  vec_alloc (wrapped_args, wrapped_nargs);
  for (PACK_T *z = PACK (m); z != NO_PACK; FORWARD (z))
    {
      if (MOID (z) == M_STRING)
	{
	  tree str = a68_low_func_param (wrapper, "str", CTYPE (M_STRING));
	  DECL_ARGUMENTS (wrapper) = chainon (str, DECL_ARGUMENTS (wrapper));

	  tree s = a68_multiple_elements (str);
	  tree len = a68_multiple_num_elems (str);
	  tree stride = a68_multiple_stride (str, size_zero_node /* dim */);

	  wrapped_args->quick_push (s);
	  wrapped_args->quick_push (len);
	  wrapped_args->quick_push (stride);
	}
      else
	{
	  tree a = a68_low_func_param (wrapper, "param", CTYPE (MOID (z)));
	  DECL_ARGUMENTS (wrapper) = chainon (a, DECL_ARGUMENTS (wrapper));
	  wrapped_args->quick_push (a);
	}
    }
  DECL_ARGUMENTS (wrapper) = nreverse (DECL_ARGUMENTS (wrapper));

  a68_push_function_range (wrapper, wrapper_ret_type, true /* top_level */);

  /* We need a pointer to a function type.  */
  if (!POINTER_TYPE_P (TREE_TYPE (wrapped)))
    wrapped = fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (wrapped)),
			   wrapped);

  tree body = build_call_vec (TREE_TYPE (wrapped_type), wrapped, wrapped_args);
  a68_pop_function_range (body);
}
