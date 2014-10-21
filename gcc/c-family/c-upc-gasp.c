/* c-upc-gasp.c: UPC GASP (GAS Performance) instrumentation support
   Copyright (C) 2005-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.
   Based on original Implementation by Adam Leko <leko@hcs.ufl.edu>.

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
#include "tree.h"
#include "stringpool.h"
#include "ggc.h"
#include "hashtab.h"
#include "machmode.h"
#include "hard-reg-set.h"
#include "input.h"
#include "c/c-tree.h"
#include "tree-iterator.h"
#include "langhooks.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "output.h"
#include "toplev.h"
#include "function.h"
#include "target.h"
#include "c-common.h"
#include "c-pragma.h"
#include "c-upc.h"
#include "c-upc-gasp.h"
#include "c-upc-rts-names.h"

static tree build_string_ref (const char *string);

/* Build a reference to a literal string.
   (cribbed from mf_build_string in tree-mudflap.c) */

static tree
build_string_ref (const char *string)
{
  size_t len = strlen (string);
  tree result = build_string (len + 1, string);
  TREE_TYPE (result) = build_array_type
    (char_type_node, build_index_type (build_int_cst (NULL_TREE, len)));
  TREE_CONSTANT (result) = 1;
  TREE_READONLY (result) = 1;
  TREE_STATIC (result) = 1;
  result = build1 (ADDR_EXPR, build_pointer_type (char_type_node), result);
  return result;
}

/* Add source args to the argument list.  */

tree
upc_gasp_add_src_args (tree args, const char *filename, int lineno)
{
  return chainon (args,
		  tree_cons (NULL_TREE, build_string_ref (filename),
			     tree_cons (NULL_TREE,
					build_int_cst (NULL_TREE, lineno),
					NULL_TREE)));
}

/* Instrument `upc_forall' statement begin/end.
   Return a call to the profiling function.  */

tree
upc_instrument_forall (location_t loc, int start)
{
  const char *filename = LOCATION_FILE (loc);
  const int lineno = LOCATION_LINE (loc);
  tree pfunc;

  pfunc = lookup_name (get_identifier (UPC_INSTRUMENT_FORALL));
  if (!pfunc)
    internal_error ("UPC profiling function `%s' not found",
		    UPC_INSTRUMENT_FORALL);

  return build_call_expr (pfunc, 3,
			  build_int_cst (NULL_TREE, start),
			  build_string_ref (filename),
			  build_int_cst (NULL_TREE, lineno));
}

/* If UPC function profiling has been enabled, rewrite the
   body of FNDECL so that the GASP intrumentation function
   is called before the body of the function is executed,
   and then after it is executed (as a TRY_FINALLY_EXPR).  */

void
upc_instrument_func (tree fndecl)
{
  tree tf, x, pfunc, bind;
  const char *filename, *funcname;
  int lineno;

  /* Skip, if profiling disabled via #pragma pupc.  */
  if (!get_upc_pupc_mode ())
    return;

  pfunc = lookup_name (get_identifier (UPC_INSTRUMENT_FUNC));
  if (!pfunc)
    internal_error ("UPC profiling function `%s' not found",
		    UPC_INSTRUMENT_FUNC);
  funcname = "<unknown>";
  if (DECL_NAME (fndecl))
    funcname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  lineno = DECL_SOURCE_LINE (fndecl);
  filename = DECL_SOURCE_FILE (fndecl);
  tf = build2 (TRY_FINALLY_EXPR, void_type_node, NULL, NULL);
  TREE_SIDE_EFFECTS (tf) = 1;
  x = DECL_SAVED_TREE (fndecl);
  append_to_statement_list (x, &TREE_OPERAND (tf, 0));
  x = build_call_expr (pfunc, 4, integer_zero_node,	/* start == 0 */
		       build_string_ref (funcname),
		       build_string_ref (filename),
		       build_int_cst (NULL_TREE, lineno));
  append_to_statement_list (x, &TREE_OPERAND (tf, 1));

  bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
  TREE_SIDE_EFFECTS (bind) = 1;
  x = build_call_expr (pfunc, 4, integer_one_node,	/* start == 1 */
		       build_string_ref (funcname),
		       build_string_ref (filename),
		       build_int_cst (NULL_TREE, lineno));
  append_to_statement_list (x, &BIND_EXPR_BODY (bind));
  append_to_statement_list (tf, &BIND_EXPR_BODY (bind));

  DECL_SAVED_TREE (fndecl) = bind;

}
