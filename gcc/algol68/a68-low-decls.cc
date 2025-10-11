/* Lower mode, identity and variable declarations to GENERIC.
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

/* Lower one or more mode declarations.

     mode declaration : mode symbol, defining indicant,
                        equals symbol, declarer;
                        mode symbol, defining indicant,
			equals symbol, void symbol;
                        mode declaration, comma symbol,
			defining indicant, equals symbol, declarer;
                        mode declaration, comma symbol,
			defining indicant, equals symbol, void symbol.

   Each mode declaration lowers into a TYPE_DECL, which are chained in the
   current block.  This function returns void_node.

   Note that the defining indicant is already annotated with the declared mode
   so there is no need to go hunting for the declarer in the subtree.  */

tree
a68_lower_mode_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *defining_indicant = NO_NODE;

  if (IS (SUB (p), MODE_DECLARATION))
    {
      a68_lower_tree (SUB (p), ctx);
      defining_indicant = NEXT (NEXT (SUB (p)));
    }
  else
    {
      gcc_assert (IS (SUB (p), MODE_SYMBOL));
      defining_indicant = NEXT (SUB (p));
    }

  /* Create a TYPE_DECL declaration for the defined mode and chain it in the
     current block.  */
  tree ctype = CTYPE (MOID (defining_indicant));
  tree decl_name = a68_get_mangled_indicant (NSYMBOL (defining_indicant),
					     ctx.module_definition_name);
  tree decl = build_decl (a68_get_node_location (p),
			  TYPE_DECL, decl_name, ctype);
  SET_DECL_ASSEMBLER_NAME (decl, decl_name);
  TREE_PUBLIC (decl) = 1;
  TYPE_CONTEXT (ctype) = DECL_CONTEXT (decl);
  TYPE_NAME (ctype) = decl;
  TYPE_STUB_DECL (ctype) = decl;
  a68_add_decl (decl);

  return void_node;
}

/* Lower one or more variable declarations.

     variable declaration : qualifier, declarer, defining identifier,
                            assign symbol, unit;
     			    qualifier, declarer, defining identiifer;
			    qualifier, declarer, defining identifier;
			    declarer, defining identifier, assign symbol, unit;
			    declarer, defining identifier;
			    variable declaration, comma symbol,
			    defining identifier, assign symbol, unit;
			    variable declaration, comma symbol,
			    defining identifier;

  Each variable declaration lowers into a VAR_DECL, which are chained in the
  current block.  This function also returns an expression with code to
  initialize the variable in case there is an initializer.

  If the variable declaration implies a LOC generator then the VAR_DECL for REF
  AMODE declares a value of type CTYPE (AMODE).  This is an optimization in
  order to avoid indirect addressing.  If the variable declaration implies a
  HEAP generator, however, then the VAR_DECL declares a value of type pointer
  to CTYPE (AMODE0.  In this later case no optimization is possible and it has
  exactly the same effect than an identity declaration `REF AMODE
  defining_identifier = HEAP AMODE'.

  Note that the defining identifier is annotated with its mode, so there is no
  need to go hunting for the declarer in the subtree.  */

tree
a68_lower_variable_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *defining_identifier, *unit;
  NODE_T *declarer = NO_NODE;

  tree sub_expr = NULL_TREE;

  if (IS (SUB (p), VARIABLE_DECLARATION))
    {
      LOW_CTX_T new_ctx = ctx;
      new_ctx.declarer = &declarer;
      sub_expr = a68_lower_tree (SUB (p), new_ctx);
      defining_identifier = NEXT (NEXT (SUB (p)));
    }
  else if (IS (SUB (p), QUALIFIER))
    {
      /* The qualifier determines what kind of generator is used in the
	 variable declaration.  This is already annotated in the tax entry for
	 the definining identifier.  */
      declarer = NEXT (SUB (p));
      defining_identifier = NEXT (NEXT (SUB (p)));
    }
  else if (IS (SUB (p), DECLARER))
    {
      declarer = SUB (p);
      defining_identifier = NEXT (SUB (p));
    }
  else
    gcc_unreachable ();

  /* Communicate declarer upward.  */
  if (ctx.declarer != NULL)
    *ctx.declarer = declarer;

  /* See if this variable declaration features an initializing unit.  */
  if (NEXT (defining_identifier) != NO_NODE)
    {
      gcc_assert (NEXT (defining_identifier)
		  && IS (NEXT (defining_identifier), ASSIGN_SYMBOL)
		  && NEXT (NEXT (defining_identifier)));
      unit = NEXT (NEXT (defining_identifier));
    }
  else
    unit = NO_NODE;

  /* If not done already by an applied identifier in lower_identifier, create a
     declaration for the defined entity and chain it in the current block.  The
     declaration has an initial value of SKIP.  */
  tree var_decl = TAX_TREE_DECL (TAX (defining_identifier));
  if (var_decl == NULL_TREE)
    {
      var_decl = a68_make_variable_declaration_decl (defining_identifier,
						     ctx.module_definition_name);
      TAX_TREE_DECL (TAX (defining_identifier)) = var_decl;
    }

  /* If the variable declaration is in a public range then add the declaration
     to the publicized declarations list.  Otherwise chain the declaration in
     the proper block and bind it.  */
  if (PUBLIC_RANGE (TABLE (TAX (defining_identifier))))
    vec_safe_push (A68_MODULE_DEFINITION_DECLS, var_decl);
  else
    a68_add_decl (var_decl);

  /* Add a decl_expr in the current range.  */
  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (var_decl),
				      var_decl));

  tree expr = NULL_TREE;

  /* Allocate memory for the declared variables.

     This is done differently depending on the sample generator used in the
     variable declaration, be it explicit or the default LOC.

     If the LOC generator is used and the value has no rows, it means it
     doesnt' need a dynamic part and the var_decl created above is not a
     pointer.  So the memory has been allocated already and there is nothing
     else to do at this point.

     If the HEAP generator is used, or if the generated value has rows, it
     means the var_decl created above is a pointer.  We need to run a generator
     to get the memory with descriptors filled in.  Note that we cannot set the
     pointer as the initial of the var_decl because the bouns in the actual
     declarer shall be elaborated at the point of the code where the
     declaration appears, not at the beginning of its reach.  Note that the
     mode of the declarer will be always a REF, since this is a varaible
     declaration: the referred mode is what we pass to the a68_low_generator.

     If the STATIC generator is used, the var_decl created above is not a
     pointer.  The static part of the value has been already allocated, and if
     the value needs a dynamic part (i.e. if it has rows) then it is allocated
     using the heap.  Note how we allocate the whole value (including the
     static part) and then we copy if over the var_decl.  */

  if (HEAP (TAX (defining_identifier)) == STATIC_SYMBOL)
    {
      if (HAS_ROWS (SUB (MOID (defining_identifier))))
	{
	  expr = fold_build2 (MODIFY_EXPR, TREE_TYPE (var_decl),
			      var_decl,
			      fold_build1 (INDIRECT_REF,
					   TREE_TYPE (var_decl),
					   a68_low_generator (declarer,
							      SUB (MOID (declarer)),
							      true /* heap */,
							      ctx)));
	}
    }
  else
    {
      bool heap = HEAP (TAX (defining_identifier)) == HEAP_SYMBOL;
      if (heap || HAS_ROWS (SUB (MOID (defining_identifier))))
	{
	  gcc_assert(IS_REF (MOID (declarer)));
	  expr = fold_build2 (MODIFY_EXPR, TREE_TYPE (var_decl),
			      var_decl,
			      a68_low_generator (declarer,
						 SUB (MOID (declarer)),
						 heap, ctx));
	}
    }

  if (unit != NO_NODE)
    {
      tree rhs = a68_lower_tree (unit, ctx);
      tree assignation = a68_low_assignation (p,
					      var_decl, MOID (defining_identifier),
					      rhs, MOID (unit));
      if (expr != NULL_TREE)
	expr = fold_build2_loc (a68_get_node_location (p),
				COMPOUND_EXPR,
				TREE_TYPE (assignation),
				expr, assignation);
      else
	expr = assignation;
    }

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_expr != NULL_TREE)
    {
      if (expr != NULL_TREE)
	expr = fold_build2_loc (a68_get_node_location (p),
				COMPOUND_EXPR,
				TREE_TYPE (var_decl),
				sub_expr,
				expr);
      else
	expr = sub_expr;
    }

  return expr;
}

/* Lower one or more identity declarations.

     identity declaration : declarer, defining identifier,
                            equals symbol, unit;
                            identity declaration, comma symbol,
			    defining identifier, equals symbol, unit;

   Each identity declaration lowers into a declaration.

   VAR_DECL with both TREE_CONSTANT and TREE_READONLY set.  Note that we cannot
   use CONST_DECL because of two reasons.  First, CONST_DECL only works for
   scalar modes.  Second, since Algol 68 allows usage of identifiers before
   they get declared, each declaration adds a declaration with a SKIP initial
   value, and also an assignation of the value at the declaration point.  This
   function also returns an expression with code to initialize the declared
   constant.  */

tree
a68_lower_identity_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  tree unit_tree = NULL_TREE;
  tree sub_expr = NULL_TREE;

  /* Note that the formal declarer in the construct is not used.  This is
     because it is already reflected in the mode of the identity
     declaration.  */

  NODE_T *defining_identifier;
  if (IS (SUB (p), IDENTITY_DECLARATION))
    {
      sub_expr = a68_lower_tree (SUB (p), ctx);
      defining_identifier = NEXT (NEXT (SUB (p)));
    }
  else if (IS (SUB (p), DECLARER))
    {
      defining_identifier = NEXT (SUB (p));
    }
  else
    gcc_unreachable ();

  NODE_T *unit = NEXT (NEXT (defining_identifier));

  /* If not done already by an applied identifier in lower_identifier, create a
     declaration for the defined entity and chain it in the current block.  The
     declaration has an initial value of SKIP.  */
  tree id_decl = TAX_TREE_DECL (TAX (defining_identifier));
  if (id_decl == NULL_TREE)
    {
      id_decl = a68_make_identity_declaration_decl (defining_identifier,
						    ctx.module_definition_name);
      TAX_TREE_DECL (TAX (defining_identifier)) = id_decl;
    }

  /* If the identity declaration is in a public range then add the declaration
     to the publicized declarations list.  Otherwise chain the declaration in
     the proper block and bind it.  */
  if (PUBLIC_RANGE (TABLE (TAX (defining_identifier))))
    vec_safe_push (A68_MODULE_DEFINITION_DECLS, id_decl);
  else
    a68_add_decl (id_decl);

  /* Prepare the DECL_EXPR.  */
  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (id_decl),
				      id_decl));

  unit_tree = a68_lower_tree (unit, ctx);
  unit_tree = a68_consolidate_ref (MOID (unit), unit_tree);
  tree expr = a68_low_ascription (MOID (defining_identifier),
				  id_decl, unit_tree);

  /* If the ascribed value is constant, mark the declaration as constant.  */
  TREE_CONSTANT (id_decl) = TREE_CONSTANT (unit_tree);

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_expr != NULL_TREE)
    {
      if (expr != NULL_TREE)
	expr = fold_build2_loc (a68_get_node_location (p),
				COMPOUND_EXPR,
				TREE_TYPE (id_decl),
				sub_expr,
				expr);
      else
	expr = sub_expr;
    }

  return expr;
}

/* Lower a declarer.

     declarer : indicant;
     		longety, indicant;
     		shortety, indicant;
		flex symbol, declarer;
		flex symbol, bounds, declarer;
		flex symbol, formal bounds, declarer;
		bounds, declarer;
		formal bounds, declarer;
                ref symbol, declarer;
		struct symbol, structure pack;
		union symbol, union pack;
                proc symbol, declarer;
		proc symbol, formal declarers, declarer;
		proc symbol, formal declarers, void symbol;


  This handler lowes a DECLARER tree into an expression that evaluates to the
  size of the actual declarer.  Note that this is a self-contained handler and
  it does traverse the sub-tree on its own.  */

tree
a68_lower_declarer (NODE_T *p ATTRIBUTE_UNUSED,
		    LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Lower a declaration list.

    declaration list : mode declaration;
                       priority declaration;
                       brief operator declaration;
                       operator declaration;
                       identity declaration;
                       procedure declaration;
                       procedure variable declaration;
                       variable declaration;
                       environ name;
		       declaration list, comma symbol, declaration list;

   Process the subtree, which produces declarations associated with the current
   context and which get added to the current block.  The list of declarations
   gets returned in nested compound expressions.  */

tree
a68_lower_declaration_list (NODE_T *p, LOW_CTX_T ctx)
{
  if (IS (SUB (p), DECLARATION_LIST))
    {
      tree left = a68_lower_tree (SUB (p), ctx);
      tree right = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

      /* The trees `left' and `right' may be NULL_TREE if the declarations
	 under them didn't have an initializing expression.  In that case,
	 replace them by nops which are removed at fold time.  This is ugly,
	 but works.  */
      if (left == NULL_TREE)
	left = integer_zero_node;
      if (right == NULL_TREE)
	right = integer_zero_node;

      return fold_build2_loc (a68_get_node_location (p),
			      COMPOUND_EXPR,
			      void_type_node,
			      left, right);
    }
  else
    return a68_lower_tree (SUB (p), ctx);
}

/* Lower a procedure declaration.

     procedure declaration : proc symbol, defining identifier, assign symbol, routine text;
                             procedure declaration, comma symbol,
			     defining identifier, equals symbol, routine text.

   Each procedure declaration lowers into a declaration.  */

tree
a68_lower_procedure_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  tree sub_func_decl = NULL_TREE;
  NODE_T *defining_identifier;
  if (IS (SUB (p), PROCEDURE_DECLARATION))
    {
      sub_func_decl = a68_lower_tree (SUB (p), ctx);
      defining_identifier = NEXT (NEXT (SUB (p)));
    }
  else if (IS (SUB (p), PROC_SYMBOL))
    {
      defining_identifier = NEXT (SUB (p));
    }
  else
    gcc_unreachable ();

  NODE_T *routine_text = NEXT (NEXT (defining_identifier));

  /* Lower the routine text to get a function decl.  */
  ctx.proc_decl_identifier = defining_identifier;
  ctx.proc_decl_operator = false;
  tree func_decl = a68_lower_tree (routine_text, ctx);

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_func_decl != NULL_TREE)
    {
      if (func_decl != NULL_TREE)
	func_decl = fold_build2_loc (a68_get_node_location (p),
				     COMPOUND_EXPR,
				     TREE_TYPE (func_decl),
				     sub_func_decl,
				     func_decl);
      else
	func_decl = sub_func_decl;
    }

  return func_decl;
}

/* Lower a procedure variable declaration.

     procedure variable declaration
       : proc symbol, defining identifier, assign symbol, routine text;
         qualifier, proc symbol, defining identifier, assign symbol, routine text;
	 procedure variable declaration, comma symbol, defining identiier, assign symbol, routine text.

   This lowers into the declaration of a VAR_DECL which is a pointer to the
   free standing routine yielded by the routine text.  */

tree
a68_lower_procedure_variable_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  tree sub_decl = NULL_TREE;
  NODE_T *defining_identifier;
  if (IS (SUB (p), PROCEDURE_VARIABLE_DECLARATION))
    {
      sub_decl = a68_lower_tree (SUB (p), ctx);
      defining_identifier = NEXT (NEXT (SUB (p)));
    }
  else if (IS (SUB (p), PROC_SYMBOL))
    defining_identifier = NEXT (SUB (p));
  else if (IS (SUB (p), QUALIFIER))
    /* The qualifier determines what kind of generator is used in the variable
       declaration.  This is already annotated in the tax entry for the
       definining identifier.  */
    defining_identifier = NEXT (NEXT (SUB (p)));
  else
    gcc_unreachable ();
  NODE_T *routine_text = NEXT (NEXT (defining_identifier));

  /* The routine text lowers into a pointer to function.  */
  ctx.proc_decl_identifier = NO_NODE;
  ctx.proc_decl_operator = false;
  tree routine = a68_lower_tree (routine_text, ctx);

  /* Create a declaration for the proc variable, if that hasn't been done
     already.  */
  tree decl = TAX_TREE_DECL (TAX (defining_identifier));
  if (decl == NULL_TREE)
    {
      decl = a68_make_variable_declaration_decl (defining_identifier,
						ctx.module_definition_name);
      TAX_TREE_DECL (TAX (defining_identifier)) = decl;
    }

  /* If the variable declaration is in a public range then add the declaration
     to the publicized declarations list.  Otherwise chain the declaration in
     the proper block and bind it.  */
  if (PUBLIC_RANGE (TABLE (TAX (defining_identifier))))
    vec_safe_push (A68_MODULE_DEFINITION_DECLS, decl);
  else
    a68_add_decl (decl);

  /* Add a decl_expr in the current range.  */
  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (decl),
				      decl));
  /* Initialize.

     If the variable is heap allocated then the var_decl created above is a
     pointer.  We don't allocate the actual function on the heap, because the
     scope of procedures is not global.  */
  bool heap = HEAP (TAX (defining_identifier)) == HEAP_SYMBOL;
  a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (decl), decl,
			     heap ? fold_build1 (ADDR_EXPR, TREE_TYPE (decl),
						 routine) : routine));

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_decl != NULL_TREE)
    {
      if (decl != NULL_TREE)
	decl = fold_build2_loc (a68_get_node_location (p),
				COMPOUND_EXPR,
				TREE_TYPE (decl),
				sub_decl,
				decl);
      else
	decl = sub_decl;
    }

  return decl;
}

/* Lower a priority declaration.

   This lowers to nothing.  Operator priority is fully handled by the parser in
   order to decide which operator declaration corresponds to each applied
   operator.  */

tree
a68_lower_priority_declaration (NODE_T *p ATTRIBUTE_UNUSED,
				LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* Lower a brief operator declaration.

     brief operator declaration
       : op symbol, defining operator, equals symbol, routine text;
         brief operator declaration, comma symbol, defining operator, equals symbol, routine text.

   The declarations low in a series of FUNCTION_DECLs, one per declared
   operator.  */

tree
a68_lower_brief_operator_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  tree sub_func_decl = NULL_TREE;
  NODE_T *defining_operator;

  if (IS (SUB (p), BRIEF_OPERATOR_DECLARATION))
    {
      sub_func_decl = a68_lower_tree (SUB (p), ctx);
      defining_operator = NEXT (NEXT (SUB (p)));
    }
  else
    defining_operator = NEXT (SUB (p));
  NODE_T *routine_text = NEXT (NEXT (defining_operator));

  /* Lower the routine text to get a function decl.  */
  ctx.proc_decl_identifier = defining_operator;
  ctx.proc_decl_operator = true;
  tree func_decl = a68_lower_tree (routine_text, ctx);

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_func_decl != NULL_TREE)
    {
      if (func_decl != NULL_TREE)
	func_decl = fold_build2_loc (a68_get_node_location (p),
				     COMPOUND_EXPR,
				     TREE_TYPE (func_decl),
				     sub_func_decl,
				     func_decl);
      else
	func_decl = sub_func_decl;
    }

  return func_decl;
}

/* Lower an operator declaration.

     operator declaration : operator plan, defining operator, equals symbol, unit;
                            operator declaration, comma symbol, defining operator, equals symbol, unit.

   Each operator declaration lowers into a declaration.  */

tree
a68_lower_operator_declaration (NODE_T *p, LOW_CTX_T ctx)
{
  tree sub_op_decl = NULL_TREE;
  NODE_T *defining_operator;

  if (IS (SUB (p), OPERATOR_DECLARATION))
    {
      sub_op_decl = a68_lower_tree (SUB (p), ctx);
      defining_operator = NEXT (NEXT (SUB (p)));
    }
  else
    defining_operator = NEXT (SUB (p));
  NODE_T *unit = NEXT (NEXT (defining_operator));

  tree op_decl = TAX_TREE_DECL (TAX (defining_operator));
  if (op_decl == NULL_TREE)
    {
      op_decl = a68_make_identity_declaration_decl (defining_operator,
						    ctx.module_definition_name,
						    true /* indicant */);
      TAX_TREE_DECL (TAX (defining_operator)) = op_decl;
    }

  /* If the identity declaration is in a public range then add the declaration
     to the publicized declarations list.  Otherwise chain the declaration in
     the proper block and bind it.  */
  if (PUBLIC_RANGE (TABLE (TAX (defining_operator))))
    vec_safe_push (A68_MODULE_DEFINITION_DECLS, op_decl);
  else
    a68_add_decl (op_decl);

  /* Prepare the DECL_EXPR.  */
  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (op_decl),
				      op_decl));
  /* Initialize.  */
  a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (op_decl), op_decl,
			     a68_lower_tree (unit, ctx)));

  /* Tail in a compound expression with sub declarations, if any.  */
  if (sub_op_decl != NULL_TREE)
    {
      if (op_decl != NULL_TREE)
	op_decl = fold_build2_loc (a68_get_node_location (p),
				   COMPOUND_EXPR,
				   TREE_TYPE (op_decl),
				   sub_op_decl,
				   op_decl);
      else
	op_decl = sub_op_decl;
    }

  return op_decl;
}
