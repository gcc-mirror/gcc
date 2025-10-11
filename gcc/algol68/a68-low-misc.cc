/* Lower miscellaneous tree nodes to GENERIC.
   Copyright (C) 2025 Jose E. Marchesi.

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

/* Lower an assertion.

     assertion : assert symbol, enclosed clause.
*/

tree
a68_lower_assertion (NODE_T *p, LOW_CTX_T ctx)
{
  if (!OPTION_ASSERT (&A68_JOB))
    return a68_get_empty();

  /* Build the call to the assert run-time function.  */
  unsigned int lineno = NUMBER (LINE (INFO (p)));
  const char *filename_str = FILENAME (LINE (INFO (p)));
  tree filename = build_string_literal (strlen (filename_str) + 1,
					filename_str);
  tree call = a68_build_libcall (A68_LIBCALL_ASSERT,
				 void_type_node, 2,
				 filename,
				 build_int_cst (unsigned_type_node, lineno));
  /* Check condition and call assert if required.  */
  tree assertion = fold_build2_loc (a68_get_node_location (p),
				    COMPOUND_EXPR,
				    a68_void_type,
				    build2_loc (a68_get_node_location (p),
						TRUTH_ORIF_EXPR,
						a68_int_type,
						a68_lower_tree (NEXT (SUB (p)), ctx),
						fold_build2 (COMPOUND_EXPR,
							     a68_int_type,
							     call,
							     build_int_cst (a68_int_type, 0))),
				    a68_get_empty ());
  TREE_SIDE_EFFECTS (assertion) = 1;
  return assertion;
}

/* Lower a jump to a label.

     jump : goto symbol, identifier;
            identifier.

   A jump lowers into a ({ GOTO_EXPR; EMPTY }).  */

tree
a68_lower_jump (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *label_identifier = SUB (p);
  MOID_T *jump_mode = MOID (p);
  if (!IS (label_identifier, IDENTIFIER))
    FORWARD (label_identifier);

  /* Create LABEL_DECL if necessary and chain it in both current block and bind
     expression.  */
  if (TAX_TREE_DECL (TAX (label_identifier)) == NULL_TREE)
    {
      tree label_decl = build_decl (a68_get_node_location (label_identifier),
				    LABEL_DECL,
				    a68_get_mangled_identifier (NSYMBOL (label_identifier)),
				    void_type_node);
      TAX_TREE_DECL (TAX (label_identifier)) = label_decl;
    }

  MOID (label_identifier) = M_VOID;
  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  CTYPE (jump_mode),
			  fold_build1_loc (a68_get_node_location (p),
					   GOTO_EXPR,
					   void_type_node,
					   a68_lower_tree (label_identifier, ctx)),
			  a68_get_skip_tree (jump_mode));
}

/* Lower a parameter into a chain of PARAM_DECLs.

     parameter : declarer, identifier;
                 parameter, comma symbol, identifier.
*/

tree
a68_lower_parameter (NODE_T *p, LOW_CTX_T ctx)
{
  tree prev_parm_decls = NULL_TREE;
  NODE_T *identifier = NO_NODE;
  if (IS (SUB (p), PARAMETER))
    {
      prev_parm_decls = a68_lower_tree (SUB (p), ctx);
      identifier = NEXT (NEXT (SUB (p)));
    }
  else
    identifier = NEXT (SUB (p));

  /* Create the PARM_DECL.  */
  tree parm_decl = build_decl (a68_get_node_location (p),
			       PARM_DECL,
			       a68_get_mangled_identifier (NSYMBOL (identifier)),
			       CTYPE (MOID (identifier)));
  DECL_CONTEXT (parm_decl) = current_function_decl;
  DECL_ARG_TYPE (parm_decl) = TREE_TYPE (parm_decl);
  TAX_TREE_DECL (TAX (identifier)) = parm_decl;

  layout_decl (parm_decl, 0);

  if (prev_parm_decls != NULL)
    return chainon (prev_parm_decls, parm_decl);
  else
    return parm_decl;
}

/* Lower a list of parameters into a chain of PARAM_DECLs.

        parameter list : parameter;
                         parameter list; comma symbol; parameter.
*/

tree
a68_lower_parameter_list (NODE_T *p, LOW_CTX_T ctx)
{
  tree parm_decl = NULL_TREE;
  tree prev_parm_decls = NULL_TREE;
  if (IS (SUB (p), PARAMETER_LIST))
    {
      prev_parm_decls = a68_lower_tree (SUB (p), ctx);
      parm_decl = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);
    }
  else
    parm_decl = a68_lower_tree (SUB (p), ctx);

  gcc_assert (parm_decl != NULL_TREE);
  if (prev_parm_decls != NULL)
    return chainon (prev_parm_decls, parm_decl);
  else
    return parm_decl;
}

/* Lower a parameter pack into a chain of PARAM_DECLs.

     parameter pack : open symbol, parameter list, close symbol.
*/

tree
a68_lower_parameter_pack (NODE_T *p, LOW_CTX_T ctx)
{
  /* Lower the contained PARAMETER_LIST.  */
  return a68_lower_tree (NEXT (SUB (p)), ctx);
}

/* Lower an applied operator.

   Applied operators lower into a function object that gets one argument in
   case of monadic operators, or two arguments in case of dyadic operators.  */

tree
a68_lower_operator (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* This is an user defined operator.  Handle it in a similar way than applied
     identifiers.  */
  tree func_decl = TAX_TREE_DECL (TAX (p));
  if (func_decl == NULL_TREE)
    {
      bool external = (MOIF (TAX (p)) != NO_MOIF);
      const char *extern_symbol = EXTERN_SYMBOL (TAX (p));
      if (IN_PROC (TAX (p)))
	{
	  if (external)
	    func_decl = a68_make_proc_identity_declaration_decl (p,
								 NAME (MOIF (TAX (p))),
								 true /* indicant */,
								 external,
								 extern_symbol);
	  else
	    func_decl = a68_make_proc_identity_declaration_decl (p,
								 ctx.module_definition_name,
								 true /* indicant */);
	}
      else
	{
	  if (external)
	    func_decl = a68_make_identity_declaration_decl (p, NAME (MOIF (TAX (p))),
							    true /* indicant */, external,
							    extern_symbol);
	  else
	    func_decl = a68_make_identity_declaration_decl (p, ctx.module_definition_name,
							    true /* indicant */);
	}
      TAX_TREE_DECL (TAX (p)) = func_decl;
    }
  return func_decl;
}
