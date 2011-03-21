/* Forward propagation of expressions for single use variables.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "tree-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "langhooks.h"
#include "flags.h"
#include "gimple.h"
#include "expr.h"

/* This pass propagates the RHS of assignment statements into use
   sites of the LHS of the assignment.  It's basically a specialized
   form of tree combination.   It is hoped all of this can disappear
   when we have a generalized tree combiner.

   One class of common cases we handle is forward propagating a single use
   variable into a COND_EXPR.

     bb0:
       x = a COND b;
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
       if (a COND b) goto ... else goto ...

   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).

   Or (assuming c1 and c2 are constants):

     bb0:
       x = a + c1;
       if (x EQ/NEQ c2) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a EQ/NEQ (c2 - c1)) goto ... else goto ...

   Similarly for x = a - c1.

   Or

     bb0:
       x = !a
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a == 0) goto ... else goto ...

   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).
   For these cases, we propagate A into all, possibly more than one,
   COND_EXPRs that use X.

   Or

     bb0:
       x = (typecast) a
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a != 0) goto ... else goto ...

   (Assuming a is an integral type and x is a boolean or x is an
    integral and a is a boolean.)

   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).
   For these cases, we propagate A into all, possibly more than one,
   COND_EXPRs that use X.

   In addition to eliminating the variable and the statement which assigns
   a value to the variable, we may be able to later thread the jump without
   adding insane complexity in the dominator optimizer.

   Also note these transformations can cascade.  We handle this by having
   a worklist of COND_EXPR statements to examine.  As we make a change to
   a statement, we put it back on the worklist to examine on the next
   iteration of the main loop.

   A second class of propagation opportunities arises for ADDR_EXPR
   nodes.

     ptr = &x->y->z;
     res = *ptr;

   Will get turned into

     res = x->y->z;

   Or
     ptr = (type1*)&type2var;
     res = *ptr

   Will get turned into (if type1 and type2 are the same size
   and neither have volatile on them):
     res = VIEW_CONVERT_EXPR<type1>(type2var)

   Or

     ptr = &x[0];
     ptr2 = ptr + <constant>;

   Will get turned into

     ptr2 = &x[constant/elementsize];

  Or

     ptr = &x[0];
     offset = index * element_size;
     offset_p = (pointer) offset;
     ptr2 = ptr + offset_p

  Will get turned into:

     ptr2 = &x[index];

  Or
    ssa = (int) decl
    res = ssa & 1

  Provided that decl has known alignment >= 2, will get turned into

    res = 0

  We also propagate casts into SWITCH_EXPR and COND_EXPR conditions to
  allow us to remove the cast and {NOT_EXPR,NEG_EXPR} into a subsequent
  {NOT_EXPR,NEG_EXPR}.

   This will (of course) be extended as other needs arise.  */

static bool forward_propagate_addr_expr (tree name, tree rhs);

/* Set to true if we delete EH edges during the optimization.  */
static bool cfg_changed;

static tree rhs_to_tree (tree type, gimple stmt);

/* Get the next statement we can propagate NAME's value into skipping
   trivial copies.  Returns the statement that is suitable as a
   propagation destination or NULL_TREE if there is no such one.
   This only returns destinations in a single-use chain.  FINAL_NAME_P
   if non-NULL is written to the ssa name that represents the use.  */

static gimple
get_prop_dest_stmt (tree name, tree *final_name_p)
{
  use_operand_p use;
  gimple use_stmt;

  do {
    /* If name has multiple uses, bail out.  */
    if (!single_imm_use (name, &use, &use_stmt))
      return NULL;

    /* If this is not a trivial copy, we found it.  */
    if (!gimple_assign_ssa_name_copy_p (use_stmt)
	|| gimple_assign_rhs1 (use_stmt) != name)
      break;

    /* Continue searching uses of the copy destination.  */
    name = gimple_assign_lhs (use_stmt);
  } while (1);

  if (final_name_p)
    *final_name_p = name;

  return use_stmt;
}

/* Get the statement we can propagate from into NAME skipping
   trivial copies.  Returns the statement which defines the
   propagation source or NULL_TREE if there is no such one.
   If SINGLE_USE_ONLY is set considers only sources which have
   a single use chain up to NAME.  If SINGLE_USE_P is non-null,
   it is set to whether the chain to NAME is a single use chain
   or not.  SINGLE_USE_P is not written to if SINGLE_USE_ONLY is set.  */

static gimple
get_prop_source_stmt (tree name, bool single_use_only, bool *single_use_p)
{
  bool single_use = true;

  do {
    gimple def_stmt = SSA_NAME_DEF_STMT (name);

    if (!has_single_use (name))
      {
	single_use = false;
	if (single_use_only)
	  return NULL;
      }

    /* If name is defined by a PHI node or is the default def, bail out.  */
    if (!is_gimple_assign (def_stmt))
      return NULL;

    /* If def_stmt is not a simple copy, we possibly found it.  */
    if (!gimple_assign_ssa_name_copy_p (def_stmt))
      {
	tree rhs;

	if (!single_use_only && single_use_p)
	  *single_use_p = single_use;

	/* We can look through pointer conversions in the search
	   for a useful stmt for the comparison folding.  */
	rhs = gimple_assign_rhs1 (def_stmt);
	if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt))
	    && TREE_CODE (rhs) == SSA_NAME
	    && POINTER_TYPE_P (TREE_TYPE (gimple_assign_lhs (def_stmt)))
	    && POINTER_TYPE_P (TREE_TYPE (rhs)))
	  name = rhs;
	else
	  return def_stmt;
      }
    else
      {
	/* Continue searching the def of the copy source name.  */
	name = gimple_assign_rhs1 (def_stmt);
      }
  } while (1);
}

/* Checks if the destination ssa name in DEF_STMT can be used as
   propagation source.  Returns true if so, otherwise false.  */

static bool
can_propagate_from (gimple def_stmt)
{
  use_operand_p use_p;
  ssa_op_iter iter;

  gcc_assert (is_gimple_assign (def_stmt));

  /* If the rhs has side-effects we cannot propagate from it.  */
  if (gimple_has_volatile_ops (def_stmt))
    return false;

  /* If the rhs is a load we cannot propagate from it.  */
  if (TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt)) == tcc_reference
      || TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt)) == tcc_declaration)
    return false;

  /* Constants can be always propagated.  */
  if (gimple_assign_single_p (def_stmt)
      && is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt)))
    return true;

  /* We cannot propagate ssa names that occur in abnormal phi nodes.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, def_stmt, iter, SSA_OP_USE)
    if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (use_p)))
      return false;

  /* If the definition is a conversion of a pointer to a function type,
     then we can not apply optimizations as some targets require
     function pointers to be canonicalized and in this case this
     optimization could eliminate a necessary canonicalization.  */
  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
    {
      tree rhs = gimple_assign_rhs1 (def_stmt);
      if (POINTER_TYPE_P (TREE_TYPE (rhs))
          && TREE_CODE (TREE_TYPE (TREE_TYPE (rhs))) == FUNCTION_TYPE)
        return false;
    }

  return true;
}

/* Remove a copy chain ending in NAME along the defs.
   If NAME was replaced in its only use then this function can be used
   to clean up dead stmts.  Returns true if cleanup-cfg has to run.  */

static bool
remove_prop_source_from_use (tree name)
{
  gimple_stmt_iterator gsi;
  gimple stmt;
  bool cfg_changed = false;

  do {
    basic_block bb;

    if (!has_zero_uses (name))
      return cfg_changed;

    stmt = SSA_NAME_DEF_STMT (name);
    gsi = gsi_for_stmt (stmt);
    bb = gimple_bb (stmt);
    release_defs (stmt);
    gsi_remove (&gsi, true);
    cfg_changed |= gimple_purge_dead_eh_edges (bb);

    name = (gimple_assign_copy_p (stmt)) ? gimple_assign_rhs1 (stmt) : NULL;
  } while (name && TREE_CODE (name) == SSA_NAME);

  return cfg_changed;
}

/* Return the rhs of a gimple_assign STMT in a form of a single tree,
   converted to type TYPE.

   This should disappear, but is needed so we can combine expressions and use
   the fold() interfaces. Long term, we need to develop folding and combine
   routines that deal with gimple exclusively . */

static tree
rhs_to_tree (tree type, gimple stmt)
{
  location_t loc = gimple_location (stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);
  if (get_gimple_rhs_class (code) == GIMPLE_TERNARY_RHS)
    return fold_build3_loc (loc, code, type, gimple_assign_rhs1 (stmt),
			    gimple_assign_rhs2 (stmt),
			    gimple_assign_rhs3 (stmt));
  else if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
    return fold_build2_loc (loc, code, type, gimple_assign_rhs1 (stmt),
			gimple_assign_rhs2 (stmt));
  else if (get_gimple_rhs_class (code) == GIMPLE_UNARY_RHS)
    return build1 (code, type, gimple_assign_rhs1 (stmt));
  else if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    return gimple_assign_rhs1 (stmt);
  else
    gcc_unreachable ();
}

/* Combine OP0 CODE OP1 in the context of a COND_EXPR.  Returns
   the folded result in a form suitable for COND_EXPR_COND or
   NULL_TREE, if there is no suitable simplified form.  If
   INVARIANT_ONLY is true only gimple_min_invariant results are
   considered simplified.  */

static tree
combine_cond_expr_cond (location_t loc, enum tree_code code, tree type,
			tree op0, tree op1, bool invariant_only)
{
  tree t;

  gcc_assert (TREE_CODE_CLASS (code) == tcc_comparison);

  t = fold_binary_loc (loc, code, type, op0, op1);
  if (!t)
    return NULL_TREE;

  /* Require that we got a boolean type out if we put one in.  */
  gcc_assert (TREE_CODE (TREE_TYPE (t)) == TREE_CODE (type));

  /* Canonicalize the combined condition for use in a COND_EXPR.  */
  t = canonicalize_cond_expr_cond (t);

  /* Bail out if we required an invariant but didn't get one.  */
  if (!t || (invariant_only && !is_gimple_min_invariant (t)))
    return NULL_TREE;

  return t;
}

/* Propagate from the ssa name definition statements of COND_EXPR
   in GIMPLE_COND statement STMT into the conditional if that simplifies it.
   Returns zero if no statement was changed, one if there were
   changes and two if cfg_cleanup needs to run.

   This must be kept in sync with forward_propagate_into_cond.  */

static int
forward_propagate_into_gimple_cond (gimple stmt)
{
  int did_something = 0;
  location_t loc = gimple_location (stmt);

  do {
    tree tmp = NULL_TREE;
    tree name = NULL_TREE, rhs0 = NULL_TREE, rhs1 = NULL_TREE;
    gimple def_stmt;
    bool single_use0_p = false, single_use1_p = false;
    enum tree_code code = gimple_cond_code (stmt);

    /* We can do tree combining on SSA_NAME and comparison expressions.  */
    if (TREE_CODE_CLASS (gimple_cond_code (stmt)) == tcc_comparison)
      {
	/* For comparisons use the first operand, that is likely to
	   simplify comparisons against constants.  */
	if (TREE_CODE (gimple_cond_lhs (stmt)) == SSA_NAME)
	  {
	    name = gimple_cond_lhs (stmt);
	    def_stmt = get_prop_source_stmt (name, false, &single_use0_p);
	    if (def_stmt && can_propagate_from (def_stmt))
	      {
		tree op1 = gimple_cond_rhs (stmt);
		rhs0 = rhs_to_tree (TREE_TYPE (op1), def_stmt);
		tmp = combine_cond_expr_cond (loc, code, boolean_type_node,
					      rhs0, op1, !single_use0_p);
	      }
	  }
	/* If that wasn't successful, try the second operand.  */
	if (tmp == NULL_TREE
	    && TREE_CODE (gimple_cond_rhs (stmt)) == SSA_NAME)
	  {
	    tree op0 = gimple_cond_lhs (stmt);
	    name = gimple_cond_rhs (stmt);
	    def_stmt = get_prop_source_stmt (name, false, &single_use1_p);
	    if (!def_stmt || !can_propagate_from (def_stmt))
	      return did_something;

	    rhs1 = rhs_to_tree (TREE_TYPE (op0), def_stmt);
	    tmp = combine_cond_expr_cond (loc, code, boolean_type_node, op0,
					  rhs1, !single_use1_p);
	  }
	/* If that wasn't successful either, try both operands.  */
	if (tmp == NULL_TREE
	    && rhs0 != NULL_TREE
	    && rhs1 != NULL_TREE)
	  tmp = combine_cond_expr_cond (loc, code, boolean_type_node, rhs0,
					fold_convert_loc (loc,
							  TREE_TYPE (rhs0),
							  rhs1),
					!(single_use0_p && single_use1_p));
      }

    if (tmp)
      {
	if (dump_file && tmp)
	  {
            tree cond = build2 (gimple_cond_code (stmt),
				boolean_type_node,
				gimple_cond_lhs (stmt),
				gimple_cond_rhs (stmt));
	    fprintf (dump_file, "  Replaced '");
	    print_generic_expr (dump_file, cond, 0);
	    fprintf (dump_file, "' with '");
	    print_generic_expr (dump_file, tmp, 0);
	    fprintf (dump_file, "'\n");
	  }

        gimple_cond_set_condition_from_tree (stmt, unshare_expr (tmp));
	update_stmt (stmt);

	/* Remove defining statements.  */
	if (remove_prop_source_from_use (name)
	    || is_gimple_min_invariant (tmp))
	  did_something = 2;
	else if (did_something == 0)
	  did_something = 1;

	/* Continue combining.  */
	continue;
      }

    break;
  } while (1);

  return did_something;
}


/* Propagate from the ssa name definition statements of COND_EXPR
   in the rhs of statement STMT into the conditional if that simplifies it.
   Returns zero if no statement was changed, one if there were
   changes and two if cfg_cleanup needs to run.

   This must be kept in sync with forward_propagate_into_gimple_cond.  */

static int
forward_propagate_into_cond (gimple_stmt_iterator *gsi_p)
{
  gimple stmt = gsi_stmt (*gsi_p);
  location_t loc = gimple_location (stmt);
  int did_something = 0;

  do {
    tree tmp = NULL_TREE;
    tree cond = gimple_assign_rhs1 (stmt);
    tree name, rhs0 = NULL_TREE, rhs1 = NULL_TREE;
    gimple def_stmt;
    bool single_use0_p = false, single_use1_p = false;

    /* We can do tree combining on SSA_NAME and comparison expressions.  */
    if (COMPARISON_CLASS_P (cond)
	&& TREE_CODE (TREE_OPERAND (cond, 0)) == SSA_NAME)
      {
	/* For comparisons use the first operand, that is likely to
	   simplify comparisons against constants.  */
	name = TREE_OPERAND (cond, 0);
	def_stmt = get_prop_source_stmt (name, false, &single_use0_p);
	if (def_stmt && can_propagate_from (def_stmt))
	  {
	    tree op1 = TREE_OPERAND (cond, 1);
	    rhs0 = rhs_to_tree (TREE_TYPE (op1), def_stmt);
	    tmp = combine_cond_expr_cond (loc, TREE_CODE (cond),
					  boolean_type_node,
					  rhs0, op1, !single_use0_p);
	  }
	/* If that wasn't successful, try the second operand.  */
	if (tmp == NULL_TREE
	    && TREE_CODE (TREE_OPERAND (cond, 1)) == SSA_NAME)
	  {
	    tree op0 = TREE_OPERAND (cond, 0);
	    name = TREE_OPERAND (cond, 1);
	    def_stmt = get_prop_source_stmt (name, false, &single_use1_p);
	    if (!def_stmt || !can_propagate_from (def_stmt))
	      return did_something;

	    rhs1 = rhs_to_tree (TREE_TYPE (op0), def_stmt);
	    tmp = combine_cond_expr_cond (loc, TREE_CODE (cond),
					  boolean_type_node,
					  op0, rhs1, !single_use1_p);
	  }
	/* If that wasn't successful either, try both operands.  */
	if (tmp == NULL_TREE
	    && rhs0 != NULL_TREE
	    && rhs1 != NULL_TREE)
	  tmp = combine_cond_expr_cond (loc, TREE_CODE (cond),
					boolean_type_node,
					rhs0,
					fold_convert_loc (loc,
							  TREE_TYPE (rhs0),
							  rhs1),
					!(single_use0_p && single_use1_p));
      }
    else if (TREE_CODE (cond) == SSA_NAME)
      {
	name = cond;
	def_stmt = get_prop_source_stmt (name, true, NULL);
	if (def_stmt || !can_propagate_from (def_stmt))
	  return did_something;

	rhs0 = gimple_assign_rhs1 (def_stmt);
	tmp = combine_cond_expr_cond (loc, NE_EXPR, boolean_type_node, rhs0,
				      build_int_cst (TREE_TYPE (rhs0), 0),
				      false);
      }

    if (tmp)
      {
	if (dump_file && tmp)
	  {
	    fprintf (dump_file, "  Replaced '");
	    print_generic_expr (dump_file, cond, 0);
	    fprintf (dump_file, "' with '");
	    print_generic_expr (dump_file, tmp, 0);
	    fprintf (dump_file, "'\n");
	  }

	gimple_assign_set_rhs_from_tree (gsi_p, unshare_expr (tmp));
	stmt = gsi_stmt (*gsi_p);
	update_stmt (stmt);

	/* Remove defining statements.  */
	if (remove_prop_source_from_use (name)
	    || is_gimple_min_invariant (tmp))
	  did_something = 2;
	else if (did_something == 0)
	  did_something = 1;

	/* Continue combining.  */
	continue;
      }

    break;
  } while (1);

  return did_something;
}

/* We've just substituted an ADDR_EXPR into stmt.  Update all the
   relevant data structures to match.  */

static void
tidy_after_forward_propagate_addr (gimple stmt)
{
  /* We may have turned a trapping insn into a non-trapping insn.  */
  if (maybe_clean_or_replace_eh_stmt (stmt, stmt)
      && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
    cfg_changed = true;

  if (TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR)
     recompute_tree_invariant_for_addr_expr (gimple_assign_rhs1 (stmt));
}

/* DEF_RHS contains the address of the 0th element in an array.
   USE_STMT uses type of DEF_RHS to compute the address of an
   arbitrary element within the array.  The (variable) byte offset
   of the element is contained in OFFSET.

   We walk back through the use-def chains of OFFSET to verify that
   it is indeed computing the offset of an element within the array
   and extract the index corresponding to the given byte offset.

   We then try to fold the entire address expression into a form
   &array[index].

   If we are successful, we replace the right hand side of USE_STMT
   with the new address computation.  */

static bool
forward_propagate_addr_into_variable_array_index (tree offset,
						  tree def_rhs,
						  gimple_stmt_iterator *use_stmt_gsi)
{
  tree index, tunit;
  gimple offset_def, use_stmt = gsi_stmt (*use_stmt_gsi);
  tree new_rhs, tmp;

  if (TREE_CODE (TREE_OPERAND (def_rhs, 0)) == ARRAY_REF)
    tunit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (def_rhs)));
  else if (TREE_CODE (TREE_TYPE (TREE_OPERAND (def_rhs, 0))) == ARRAY_TYPE)
    tunit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (TREE_TYPE (def_rhs))));
  else
    return false;
  if (!host_integerp (tunit, 1))
    return false;

  /* Get the offset's defining statement.  */
  offset_def = SSA_NAME_DEF_STMT (offset);

  /* Try to find an expression for a proper index.  This is either a
     multiplication expression by the element size or just the ssa name we came
     along in case the element size is one. In that case, however, we do not
     allow multiplications because they can be computing index to a higher
     level dimension (PR 37861). */
  if (integer_onep (tunit))
    {
      if (is_gimple_assign (offset_def)
	  && gimple_assign_rhs_code (offset_def) == MULT_EXPR)
	return false;

      index = offset;
    }
  else
    {
      /* The statement which defines OFFSET before type conversion
         must be a simple GIMPLE_ASSIGN.  */
      if (!is_gimple_assign (offset_def))
	return false;

      /* The RHS of the statement which defines OFFSET must be a
	 multiplication of an object by the size of the array elements.
	 This implicitly verifies that the size of the array elements
	 is constant.  */
     if (gimple_assign_rhs_code (offset_def) == MULT_EXPR
	 && TREE_CODE (gimple_assign_rhs2 (offset_def)) == INTEGER_CST
	 && tree_int_cst_equal (gimple_assign_rhs2 (offset_def), tunit))
       {
	 /* The first operand to the MULT_EXPR is the desired index.  */
	 index = gimple_assign_rhs1 (offset_def);
       }
     /* If we have idx * tunit + CST * tunit re-associate that.  */
     else if ((gimple_assign_rhs_code (offset_def) == PLUS_EXPR
	       || gimple_assign_rhs_code (offset_def) == MINUS_EXPR)
	      && TREE_CODE (gimple_assign_rhs1 (offset_def)) == SSA_NAME
	      && TREE_CODE (gimple_assign_rhs2 (offset_def)) == INTEGER_CST
	      && (tmp = div_if_zero_remainder (EXACT_DIV_EXPR,
					       gimple_assign_rhs2 (offset_def),
					       tunit)) != NULL_TREE)
       {
	 gimple offset_def2 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (offset_def));
	 if (is_gimple_assign (offset_def2)
	     && gimple_assign_rhs_code (offset_def2) == MULT_EXPR
	     && TREE_CODE (gimple_assign_rhs2 (offset_def2)) == INTEGER_CST
	     && tree_int_cst_equal (gimple_assign_rhs2 (offset_def2), tunit))
	   {
	     index = fold_build2 (gimple_assign_rhs_code (offset_def),
				  TREE_TYPE (offset),
				  gimple_assign_rhs1 (offset_def2), tmp);
	   }
	 else
	   return false;
       }
     else
	return false;
    }

  /* Replace the pointer addition with array indexing.  */
  index = force_gimple_operand_gsi (use_stmt_gsi, index, true, NULL_TREE,
				    true, GSI_SAME_STMT);
  if (TREE_CODE (TREE_OPERAND (def_rhs, 0)) == ARRAY_REF)
    {
      new_rhs = unshare_expr (def_rhs);
      TREE_OPERAND (TREE_OPERAND (new_rhs, 0), 1) = index;
    }
  else
    {
      new_rhs = build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (TREE_TYPE (def_rhs))),
			unshare_expr (TREE_OPERAND (def_rhs, 0)),
			index, integer_zero_node, NULL_TREE);
      new_rhs = build_fold_addr_expr (new_rhs);
      if (!useless_type_conversion_p (TREE_TYPE (gimple_assign_lhs (use_stmt)),
				      TREE_TYPE (new_rhs)))
	{
	  new_rhs = force_gimple_operand_gsi (use_stmt_gsi, new_rhs, true,
					      NULL_TREE, true, GSI_SAME_STMT);
	  new_rhs = fold_convert (TREE_TYPE (gimple_assign_lhs (use_stmt)),
				  new_rhs);
	}
    }
  gimple_assign_set_rhs_from_tree (use_stmt_gsi, new_rhs);
  use_stmt = gsi_stmt (*use_stmt_gsi);

  /* That should have created gimple, so there is no need to
     record information to undo the propagation.  */
  fold_stmt_inplace (use_stmt);
  tidy_after_forward_propagate_addr (use_stmt);
  return true;
}

/* NAME is a SSA_NAME representing DEF_RHS which is of the form
   ADDR_EXPR <whatever>.

   Try to forward propagate the ADDR_EXPR into the use USE_STMT.
   Often this will allow for removal of an ADDR_EXPR and INDIRECT_REF
   node or for recovery of array indexing from pointer arithmetic.

   Return true if the propagation was successful (the propagation can
   be not totally successful, yet things may have been changed).  */

static bool
forward_propagate_addr_expr_1 (tree name, tree def_rhs,
			       gimple_stmt_iterator *use_stmt_gsi,
			       bool single_use_p)
{
  tree lhs, rhs, rhs2, array_ref;
  gimple use_stmt = gsi_stmt (*use_stmt_gsi);
  enum tree_code rhs_code;
  bool res = true;

  gcc_assert (TREE_CODE (def_rhs) == ADDR_EXPR);

  lhs = gimple_assign_lhs (use_stmt);
  rhs_code = gimple_assign_rhs_code (use_stmt);
  rhs = gimple_assign_rhs1 (use_stmt);

  /* Trivial cases.  The use statement could be a trivial copy or a
     useless conversion.  Recurse to the uses of the lhs as copyprop does
     not copy through different variant pointers and FRE does not catch
     all useless conversions.  Treat the case of a single-use name and
     a conversion to def_rhs type separate, though.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && ((rhs_code == SSA_NAME && rhs == name)
	  || CONVERT_EXPR_CODE_P (rhs_code)))
    {
      /* Only recurse if we don't deal with a single use or we cannot
	 do the propagation to the current statement.  In particular
	 we can end up with a conversion needed for a non-invariant
	 address which we cannot do in a single statement.  */
      if (!single_use_p
	  || (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (def_rhs))
	      && (!is_gimple_min_invariant (def_rhs)
		  || (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		      && POINTER_TYPE_P (TREE_TYPE (def_rhs))
		      && (TYPE_PRECISION (TREE_TYPE (lhs))
			  > TYPE_PRECISION (TREE_TYPE (def_rhs)))))))
	return forward_propagate_addr_expr (lhs, def_rhs);

      gimple_assign_set_rhs1 (use_stmt, unshare_expr (def_rhs));
      if (useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (def_rhs)))
	gimple_assign_set_rhs_code (use_stmt, TREE_CODE (def_rhs));
      else
	gimple_assign_set_rhs_code (use_stmt, NOP_EXPR);
      return true;
    }

  /* Propagate through constant pointer adjustments.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && rhs_code == POINTER_PLUS_EXPR
      && rhs == name
      && TREE_CODE (gimple_assign_rhs2 (use_stmt)) == INTEGER_CST)
    {
      tree new_def_rhs;
      /* As we come here with non-invariant addresses in def_rhs we need
         to make sure we can build a valid constant offsetted address
	 for further propagation.  Simply rely on fold building that
	 and check after the fact.  */
      new_def_rhs = fold_build2 (MEM_REF, TREE_TYPE (TREE_TYPE (rhs)),
				 def_rhs,
				 fold_convert (ptr_type_node,
					       gimple_assign_rhs2 (use_stmt)));
      if (TREE_CODE (new_def_rhs) == MEM_REF
	  && !is_gimple_mem_ref_addr (TREE_OPERAND (new_def_rhs, 0)))
	return false;
      new_def_rhs = build_fold_addr_expr_with_type (new_def_rhs,
						    TREE_TYPE (rhs));

      /* Recurse.  If we could propagate into all uses of lhs do not
	 bother to replace into the current use but just pretend we did.  */
      if (TREE_CODE (new_def_rhs) == ADDR_EXPR
	  && forward_propagate_addr_expr (lhs, new_def_rhs))
	return true;

      if (useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (new_def_rhs)))
	gimple_assign_set_rhs_with_ops (use_stmt_gsi, TREE_CODE (new_def_rhs),
					new_def_rhs, NULL_TREE);
      else if (is_gimple_min_invariant (new_def_rhs))
	gimple_assign_set_rhs_with_ops (use_stmt_gsi, NOP_EXPR,
					new_def_rhs, NULL_TREE);
      else
	return false;
      gcc_assert (gsi_stmt (*use_stmt_gsi) == use_stmt);
      update_stmt (use_stmt);
      return true;
    }

  /* Now strip away any outer COMPONENT_REF/ARRAY_REF nodes from the LHS.
     ADDR_EXPR will not appear on the LHS.  */
  lhs = gimple_assign_lhs (use_stmt);
  while (handled_component_p (lhs))
    lhs = TREE_OPERAND (lhs, 0);

  /* Now see if the LHS node is a MEM_REF using NAME.  If so,
     propagate the ADDR_EXPR into the use of NAME and fold the result.  */
  if (TREE_CODE (lhs) == MEM_REF
      && TREE_OPERAND (lhs, 0) == name)
    {
      tree def_rhs_base;
      HOST_WIDE_INT def_rhs_offset;
      /* If the address is invariant we can always fold it.  */
      if ((def_rhs_base = get_addr_base_and_unit_offset (TREE_OPERAND (def_rhs, 0),
							 &def_rhs_offset)))
	{
	  double_int off = mem_ref_offset (lhs);
	  tree new_ptr;
	  off = double_int_add (off,
				shwi_to_double_int (def_rhs_offset));
	  if (TREE_CODE (def_rhs_base) == MEM_REF)
	    {
	      off = double_int_add (off, mem_ref_offset (def_rhs_base));
	      new_ptr = TREE_OPERAND (def_rhs_base, 0);
	    }
	  else
	    new_ptr = build_fold_addr_expr (def_rhs_base);
	  TREE_OPERAND (lhs, 0) = new_ptr;
	  TREE_OPERAND (lhs, 1)
	    = double_int_to_tree (TREE_TYPE (TREE_OPERAND (lhs, 1)), off);
	  tidy_after_forward_propagate_addr (use_stmt);
	  /* Continue propagating into the RHS if this was not the only use.  */
	  if (single_use_p)
	    return true;
	}
      /* If the LHS is a plain dereference and the value type is the same as
         that of the pointed-to type of the address we can put the
	 dereferenced address on the LHS preserving the original alias-type.  */
      else if (gimple_assign_lhs (use_stmt) == lhs
	       && useless_type_conversion_p
	            (TREE_TYPE (TREE_OPERAND (def_rhs, 0)),
		     TREE_TYPE (gimple_assign_rhs1 (use_stmt))))
	{
	  tree *def_rhs_basep = &TREE_OPERAND (def_rhs, 0);
	  tree new_offset, new_base, saved;
	  while (handled_component_p (*def_rhs_basep))
	    def_rhs_basep = &TREE_OPERAND (*def_rhs_basep, 0);
	  saved = *def_rhs_basep;
	  if (TREE_CODE (*def_rhs_basep) == MEM_REF)
	    {
	      new_base = TREE_OPERAND (*def_rhs_basep, 0);
	      new_offset
		= int_const_binop (PLUS_EXPR, TREE_OPERAND (lhs, 1),
				   TREE_OPERAND (*def_rhs_basep, 1), 0);
	    }
	  else
	    {
	      new_base = build_fold_addr_expr (*def_rhs_basep);
	      new_offset = TREE_OPERAND (lhs, 1);
	    }
	  *def_rhs_basep = build2 (MEM_REF, TREE_TYPE (*def_rhs_basep),
				   new_base, new_offset);
	  gimple_assign_set_lhs (use_stmt,
				 unshare_expr (TREE_OPERAND (def_rhs, 0)));
	  *def_rhs_basep = saved;
	  tidy_after_forward_propagate_addr (use_stmt);
	  /* Continue propagating into the RHS if this was not the
	     only use.  */
	  if (single_use_p)
	    return true;
	}
      else
	/* We can have a struct assignment dereferencing our name twice.
	   Note that we didn't propagate into the lhs to not falsely
	   claim we did when propagating into the rhs.  */
	res = false;
    }

  /* Strip away any outer COMPONENT_REF, ARRAY_REF or ADDR_EXPR
     nodes from the RHS.  */
  rhs = gimple_assign_rhs1 (use_stmt);
  if (TREE_CODE (rhs) == ADDR_EXPR)
    rhs = TREE_OPERAND (rhs, 0);
  while (handled_component_p (rhs))
    rhs = TREE_OPERAND (rhs, 0);

  /* Now see if the RHS node is a MEM_REF using NAME.  If so,
     propagate the ADDR_EXPR into the use of NAME and fold the result.  */
  if (TREE_CODE (rhs) == MEM_REF
      && TREE_OPERAND (rhs, 0) == name)
    {
      tree def_rhs_base;
      HOST_WIDE_INT def_rhs_offset;
      if ((def_rhs_base = get_addr_base_and_unit_offset (TREE_OPERAND (def_rhs, 0),
							 &def_rhs_offset)))
	{
	  double_int off = mem_ref_offset (rhs);
	  tree new_ptr;
	  off = double_int_add (off,
				shwi_to_double_int (def_rhs_offset));
	  if (TREE_CODE (def_rhs_base) == MEM_REF)
	    {
	      off = double_int_add (off, mem_ref_offset (def_rhs_base));
	      new_ptr = TREE_OPERAND (def_rhs_base, 0);
	    }
	  else
	    new_ptr = build_fold_addr_expr (def_rhs_base);
	  TREE_OPERAND (rhs, 0) = new_ptr;
	  TREE_OPERAND (rhs, 1)
	    = double_int_to_tree (TREE_TYPE (TREE_OPERAND (rhs, 1)), off);
	  fold_stmt_inplace (use_stmt);
	  tidy_after_forward_propagate_addr (use_stmt);
	  return res;
	}
      /* If the LHS is a plain dereference and the value type is the same as
         that of the pointed-to type of the address we can put the
	 dereferenced address on the LHS preserving the original alias-type.  */
      else if (gimple_assign_rhs1 (use_stmt) == rhs
	       && useless_type_conversion_p
		    (TREE_TYPE (gimple_assign_lhs (use_stmt)),
		     TREE_TYPE (TREE_OPERAND (def_rhs, 0))))
	{
	  tree *def_rhs_basep = &TREE_OPERAND (def_rhs, 0);
	  tree new_offset, new_base, saved;
	  while (handled_component_p (*def_rhs_basep))
	    def_rhs_basep = &TREE_OPERAND (*def_rhs_basep, 0);
	  saved = *def_rhs_basep;
	  if (TREE_CODE (*def_rhs_basep) == MEM_REF)
	    {
	      new_base = TREE_OPERAND (*def_rhs_basep, 0);
	      new_offset
		= int_const_binop (PLUS_EXPR, TREE_OPERAND (rhs, 1),
				   TREE_OPERAND (*def_rhs_basep, 1), 0);
	    }
	  else
	    {
	      new_base = build_fold_addr_expr (*def_rhs_basep);
	      new_offset = TREE_OPERAND (rhs, 1);
	    }
	  *def_rhs_basep = build2 (MEM_REF, TREE_TYPE (*def_rhs_basep),
				   new_base, new_offset);
	  gimple_assign_set_rhs1 (use_stmt,
				  unshare_expr (TREE_OPERAND (def_rhs, 0)));
	  *def_rhs_basep = saved;
	  fold_stmt_inplace (use_stmt);
	  tidy_after_forward_propagate_addr (use_stmt);
	  return res;
	}
    }

  /* If the use of the ADDR_EXPR is not a POINTER_PLUS_EXPR, there
     is nothing to do. */
  if (gimple_assign_rhs_code (use_stmt) != POINTER_PLUS_EXPR
      || gimple_assign_rhs1 (use_stmt) != name)
    return false;

  /* The remaining cases are all for turning pointer arithmetic into
     array indexing.  They only apply when we have the address of
     element zero in an array.  If that is not the case then there
     is nothing to do.  */
  array_ref = TREE_OPERAND (def_rhs, 0);
  if ((TREE_CODE (array_ref) != ARRAY_REF
       || TREE_CODE (TREE_TYPE (TREE_OPERAND (array_ref, 0))) != ARRAY_TYPE
       || TREE_CODE (TREE_OPERAND (array_ref, 1)) != INTEGER_CST)
      && TREE_CODE (TREE_TYPE (array_ref)) != ARRAY_TYPE)
    return false;

  rhs2 = gimple_assign_rhs2 (use_stmt);
  /* Try to optimize &x[C1] p+ C2 where C2 is a multiple of the size
     of the elements in X into &x[C1 + C2/element size].  */
  if (TREE_CODE (rhs2) == INTEGER_CST)
    {
      tree new_rhs = maybe_fold_stmt_addition (gimple_location (use_stmt),
	  				       TREE_TYPE (def_rhs),
					       def_rhs, rhs2);
      if (new_rhs)
	{
	  tree type = TREE_TYPE (gimple_assign_lhs (use_stmt));
	  new_rhs = unshare_expr (new_rhs);
	  if (!useless_type_conversion_p (type, TREE_TYPE (new_rhs)))
	    {
	      if (!is_gimple_min_invariant (new_rhs))
		new_rhs = force_gimple_operand_gsi (use_stmt_gsi, new_rhs,
						    true, NULL_TREE,
						    true, GSI_SAME_STMT);
	      new_rhs = fold_convert (type, new_rhs);
	    }
	  gimple_assign_set_rhs_from_tree (use_stmt_gsi, new_rhs);
	  use_stmt = gsi_stmt (*use_stmt_gsi);
	  update_stmt (use_stmt);
	  tidy_after_forward_propagate_addr (use_stmt);
	  return true;
	}
    }

  /* Try to optimize &x[0] p+ OFFSET where OFFSET is defined by
     converting a multiplication of an index by the size of the
     array elements, then the result is converted into the proper
     type for the arithmetic.  */
  if (TREE_CODE (rhs2) == SSA_NAME
      && (TREE_CODE (array_ref) != ARRAY_REF
	  || integer_zerop (TREE_OPERAND (array_ref, 1)))
      && useless_type_conversion_p (TREE_TYPE (name), TREE_TYPE (def_rhs))
      /* Avoid problems with IVopts creating PLUS_EXPRs with a
	 different type than their operands.  */
      && useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (def_rhs)))
    return forward_propagate_addr_into_variable_array_index (rhs2, def_rhs,
							     use_stmt_gsi);
  return false;
}

/* STMT is a statement of the form SSA_NAME = ADDR_EXPR <whatever>.

   Try to forward propagate the ADDR_EXPR into all uses of the SSA_NAME.
   Often this will allow for removal of an ADDR_EXPR and INDIRECT_REF
   node or for recovery of array indexing from pointer arithmetic.
   Returns true, if all uses have been propagated into.  */

static bool
forward_propagate_addr_expr (tree name, tree rhs)
{
  int stmt_loop_depth = gimple_bb (SSA_NAME_DEF_STMT (name))->loop_depth;
  imm_use_iterator iter;
  gimple use_stmt;
  bool all = true;
  bool single_use_p = has_single_use (name);

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, name)
    {
      bool result;
      tree use_rhs;

      /* If the use is not in a simple assignment statement, then
	 there is nothing we can do.  */
      if (gimple_code (use_stmt) != GIMPLE_ASSIGN)
	{
	  if (!is_gimple_debug (use_stmt))
	    all = false;
	  continue;
	}

      /* If the use is in a deeper loop nest, then we do not want
	 to propagate non-invariant ADDR_EXPRs into the loop as that
	 is likely adding expression evaluations into the loop.  */
      if (gimple_bb (use_stmt)->loop_depth > stmt_loop_depth
	  && !is_gimple_min_invariant (rhs))
	{
	  all = false;
	  continue;
	}

      {
	gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	result = forward_propagate_addr_expr_1 (name, rhs, &gsi,
						single_use_p);
	/* If the use has moved to a different statement adjust
	   the update machinery for the old statement too.  */
	if (use_stmt != gsi_stmt (gsi))
	  {
	    update_stmt (use_stmt);
	    use_stmt = gsi_stmt (gsi);
	  }

	update_stmt (use_stmt);
      }
      all &= result;

      /* Remove intermediate now unused copy and conversion chains.  */
      use_rhs = gimple_assign_rhs1 (use_stmt);
      if (result
	  && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	  && TREE_CODE (use_rhs) == SSA_NAME
	  && has_zero_uses (gimple_assign_lhs (use_stmt)))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	  release_defs (use_stmt);
	  gsi_remove (&gsi, true);
	}
    }

  return all && has_zero_uses (name);
}

/* Forward propagate the comparison defined in STMT like
   cond_1 = x CMP y to uses of the form
     a_1 = (T')cond_1
     a_1 = !cond_1
     a_1 = cond_1 != 0
   Returns true if stmt is now unused.  */

static bool
forward_propagate_comparison (gimple stmt)
{
  tree name = gimple_assign_lhs (stmt);
  gimple use_stmt;
  tree tmp = NULL_TREE;

  /* Don't propagate ssa names that occur in abnormal phis.  */
  if ((TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (stmt)))
      || (TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME
        && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs2 (stmt))))
    return false;

  /* Do not un-cse comparisons.  But propagate through copies.  */
  use_stmt = get_prop_dest_stmt (name, &name);
  if (!use_stmt)
    return false;

  /* Conversion of the condition result to another integral type.  */
  if (is_gimple_assign (use_stmt)
      && (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (use_stmt))
	  || TREE_CODE_CLASS (gimple_assign_rhs_code (use_stmt))
	     == tcc_comparison
          || gimple_assign_rhs_code (use_stmt) == TRUTH_NOT_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (use_stmt))))
    {
      tree lhs = gimple_assign_lhs (use_stmt);

      /* We can propagate the condition into a conversion.  */
      if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (use_stmt)))
	{
	  /* Avoid using fold here as that may create a COND_EXPR with
	     non-boolean condition as canonical form.  */
	  tmp = build2 (gimple_assign_rhs_code (stmt), TREE_TYPE (lhs),
                        gimple_assign_rhs1 (stmt), gimple_assign_rhs2 (stmt));
	}
      /* We can propagate the condition into X op CST where op
	 is EQ_EXPR or NE_EXPR and CST is either one or zero.  */
      else if (TREE_CODE_CLASS (gimple_assign_rhs_code (use_stmt))
              == tcc_comparison
             && TREE_CODE (gimple_assign_rhs1 (use_stmt)) == SSA_NAME
             && TREE_CODE (gimple_assign_rhs2 (use_stmt)) == INTEGER_CST)
      {
        enum tree_code code = gimple_assign_rhs_code (use_stmt);
        tree cst = gimple_assign_rhs2 (use_stmt);
	tree cond;

	cond = build2 (gimple_assign_rhs_code (stmt),
		       TREE_TYPE (cst),
		       gimple_assign_rhs1 (stmt),
		       gimple_assign_rhs2 (stmt));

        tmp = combine_cond_expr_cond (gimple_location (use_stmt),
				      code, TREE_TYPE (lhs),
				      cond, cst, false);
        if (tmp == NULL_TREE)
          return false;
      }
      /* We can propagate the condition into a statement that
	 computes the logical negation of the comparison result.  */
      else if (gimple_assign_rhs_code (use_stmt) == TRUTH_NOT_EXPR)
	{
	  tree type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	  bool nans = HONOR_NANS (TYPE_MODE (type));
	  enum tree_code code;
	  code = invert_tree_comparison (gimple_assign_rhs_code (stmt), nans);
	  if (code == ERROR_MARK)
	    return false;

	  tmp = build2 (code, TREE_TYPE (lhs), gimple_assign_rhs1 (stmt),
                        gimple_assign_rhs2 (stmt));
	}
      else
	return false;

      {
	gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	gimple_assign_set_rhs_from_tree (&gsi, unshare_expr (tmp));
	use_stmt = gsi_stmt (gsi);
	update_stmt (use_stmt);
      }

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  tree old_rhs = rhs_to_tree (TREE_TYPE (gimple_assign_lhs (stmt)),
                                      stmt);
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, old_rhs, dump_flags);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, tmp, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      /* Remove defining statements.  */
      return remove_prop_source_from_use (name);
    }

  return false;
}

/* If we have lhs = ~x (STMT), look and see if earlier we had x = ~y.
   If so, we can change STMT into lhs = y which can later be copy
   propagated.  Similarly for negation.

   This could trivially be formulated as a forward propagation
   to immediate uses.  However, we already had an implementation
   from DOM which used backward propagation via the use-def links.

   It turns out that backward propagation is actually faster as
   there's less work to do for each NOT/NEG expression we find.
   Backwards propagation needs to look at the statement in a single
   backlink.  Forward propagation needs to look at potentially more
   than one forward link.  */

static void
simplify_not_neg_expr (gimple_stmt_iterator *gsi_p)
{
  gimple stmt = gsi_stmt (*gsi_p);
  tree rhs = gimple_assign_rhs1 (stmt);
  gimple rhs_def_stmt = SSA_NAME_DEF_STMT (rhs);

  /* See if the RHS_DEF_STMT has the same form as our statement.  */
  if (is_gimple_assign (rhs_def_stmt)
      && gimple_assign_rhs_code (rhs_def_stmt) == gimple_assign_rhs_code (stmt))
    {
      tree rhs_def_operand = gimple_assign_rhs1 (rhs_def_stmt);

      /* Verify that RHS_DEF_OPERAND is a suitable SSA_NAME.  */
      if (TREE_CODE (rhs_def_operand) == SSA_NAME
	  && ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs_def_operand))
	{
	  gimple_assign_set_rhs_from_tree (gsi_p, rhs_def_operand);
	  stmt = gsi_stmt (*gsi_p);
	  update_stmt (stmt);
	}
    }
}

/* STMT is a SWITCH_EXPR for which we attempt to find equivalent forms of
   the condition which we may be able to optimize better.  */

static void
simplify_gimple_switch (gimple stmt)
{
  tree cond = gimple_switch_index (stmt);
  tree def, to, ti;
  gimple def_stmt;

  /* The optimization that we really care about is removing unnecessary
     casts.  That will let us do much better in propagating the inferred
     constant at the switch target.  */
  if (TREE_CODE (cond) == SSA_NAME)
    {
      def_stmt = SSA_NAME_DEF_STMT (cond);
      if (is_gimple_assign (def_stmt))
	{
	  if (gimple_assign_rhs_code (def_stmt) == NOP_EXPR)
	    {
	      int need_precision;
	      bool fail;

	      def = gimple_assign_rhs1 (def_stmt);

	      /* ??? Why was Jeff testing this?  We are gimple...  */
	      gcc_checking_assert (is_gimple_val (def));

	      to = TREE_TYPE (cond);
	      ti = TREE_TYPE (def);

	      /* If we have an extension that preserves value, then we
		 can copy the source value into the switch.  */

	      need_precision = TYPE_PRECISION (ti);
	      fail = false;
	      if (! INTEGRAL_TYPE_P (ti))
		fail = true;
	      else if (TYPE_UNSIGNED (to) && !TYPE_UNSIGNED (ti))
		fail = true;
	      else if (!TYPE_UNSIGNED (to) && TYPE_UNSIGNED (ti))
		need_precision += 1;
	      if (TYPE_PRECISION (to) < need_precision)
		fail = true;

	      if (!fail)
		{
		  gimple_switch_set_index (stmt, def);
		  update_stmt (stmt);
		}
	    }
	}
    }
}

/* For pointers p2 and p1 return p2 - p1 if the
   difference is known and constant, otherwise return NULL.  */

static tree
constant_pointer_difference (tree p1, tree p2)
{
  int i, j;
#define CPD_ITERATIONS 5
  tree exps[2][CPD_ITERATIONS];
  tree offs[2][CPD_ITERATIONS];
  int cnt[2];

  for (i = 0; i < 2; i++)
    {
      tree p = i ? p1 : p2;
      tree off = size_zero_node;
      gimple stmt;
      enum tree_code code;

      /* For each of p1 and p2 we need to iterate at least
	 twice, to handle ADDR_EXPR directly in p1/p2,
	 SSA_NAME with ADDR_EXPR or POINTER_PLUS_EXPR etc.
	 on definition's stmt RHS.  Iterate a few extra times.  */
      j = 0;
      do
	{
	  if (!POINTER_TYPE_P (TREE_TYPE (p)))
	    break;
	  if (TREE_CODE (p) == ADDR_EXPR)
	    {
	      tree q = TREE_OPERAND (p, 0);
	      HOST_WIDE_INT offset;
	      tree base = get_addr_base_and_unit_offset (q, &offset);
	      if (base)
		{
		  q = base;
		  if (offset)
		    off = size_binop (PLUS_EXPR, off, size_int (offset));
		}
	      if (TREE_CODE (q) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (q, 0)) == SSA_NAME)
		{
		  p = TREE_OPERAND (q, 0);
		  off = size_binop (PLUS_EXPR, off,
				    double_int_to_tree (sizetype,
							mem_ref_offset (q)));
		}
	      else
		{
		  exps[i][j] = q;
		  offs[i][j++] = off;
		  break;
		}
	    }
	  if (TREE_CODE (p) != SSA_NAME)
	    break;
	  exps[i][j] = p;
	  offs[i][j++] = off;
	  if (j == CPD_ITERATIONS)
	    break;
	  stmt = SSA_NAME_DEF_STMT (p);
	  if (!is_gimple_assign (stmt) || gimple_assign_lhs (stmt) != p)
	    break;
	  code = gimple_assign_rhs_code (stmt);
	  if (code == POINTER_PLUS_EXPR)
	    {
	      if (TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST)
		break;
	      off = size_binop (PLUS_EXPR, off, gimple_assign_rhs2 (stmt));
	      p = gimple_assign_rhs1 (stmt);
	    }
	  else if (code == ADDR_EXPR || code == NOP_EXPR)
	    p = gimple_assign_rhs1 (stmt);
	  else
	    break;
	}
      while (1);
      cnt[i] = j;
    }

  for (i = 0; i < cnt[0]; i++)
    for (j = 0; j < cnt[1]; j++)
      if (exps[0][i] == exps[1][j])
	return size_binop (MINUS_EXPR, offs[0][i], offs[1][j]);

  return NULL_TREE;
}

/* *GSI_P is a GIMPLE_CALL to a builtin function.
   Optimize
   memcpy (p, "abcd", 4);
   memset (p + 4, ' ', 3);
   into
   memcpy (p, "abcd   ", 7);
   call if the latter can be stored by pieces during expansion.  */

static bool
simplify_builtin_call (gimple_stmt_iterator *gsi_p, tree callee2)
{
  gimple stmt1, stmt2 = gsi_stmt (*gsi_p);
  tree vuse = gimple_vuse (stmt2);
  if (vuse == NULL)
    return false;
  stmt1 = SSA_NAME_DEF_STMT (vuse);

  switch (DECL_FUNCTION_CODE (callee2))
    {
    case BUILT_IN_MEMSET:
      if (gimple_call_num_args (stmt2) != 3
	  || gimple_call_lhs (stmt2)
	  || CHAR_BIT != 8
	  || BITS_PER_UNIT != 8)
	break;
      else
	{
	  tree callee1;
	  tree ptr1, src1, str1, off1, len1, lhs1;
	  tree ptr2 = gimple_call_arg (stmt2, 0);
	  tree val2 = gimple_call_arg (stmt2, 1);
	  tree len2 = gimple_call_arg (stmt2, 2);
	  tree diff, vdef, new_str_cst;
	  gimple use_stmt;
	  unsigned int ptr1_align;
	  unsigned HOST_WIDE_INT src_len;
	  char *src_buf;
	  use_operand_p use_p;

	  if (!host_integerp (val2, 0)
	      || !host_integerp (len2, 1))
	    break;
	  if (is_gimple_call (stmt1))
	    {
	      /* If first stmt is a call, it needs to be memcpy
		 or mempcpy, with string literal as second argument and
		 constant length.  */
	      callee1 = gimple_call_fndecl (stmt1);
	      if (callee1 == NULL_TREE
		  || DECL_BUILT_IN_CLASS (callee1) != BUILT_IN_NORMAL
		  || gimple_call_num_args (stmt1) != 3)
		break;
	      if (DECL_FUNCTION_CODE (callee1) != BUILT_IN_MEMCPY
		  && DECL_FUNCTION_CODE (callee1) != BUILT_IN_MEMPCPY)
		break;
	      ptr1 = gimple_call_arg (stmt1, 0);
	      src1 = gimple_call_arg (stmt1, 1);
	      len1 = gimple_call_arg (stmt1, 2);
	      lhs1 = gimple_call_lhs (stmt1);
	      if (!host_integerp (len1, 1))
		break;
	      str1 = string_constant (src1, &off1);
	      if (str1 == NULL_TREE)
		break;
	      if (!host_integerp (off1, 1)
		  || compare_tree_int (off1, TREE_STRING_LENGTH (str1) - 1) > 0
		  || compare_tree_int (len1, TREE_STRING_LENGTH (str1)
					     - tree_low_cst (off1, 1)) > 0
		  || TREE_CODE (TREE_TYPE (str1)) != ARRAY_TYPE
		  || TYPE_MODE (TREE_TYPE (TREE_TYPE (str1)))
		     != TYPE_MODE (char_type_node))
		break;
	    }
	  else if (gimple_assign_single_p (stmt1))
	    {
	      /* Otherwise look for length 1 memcpy optimized into
		 assignment.  */
    	      ptr1 = gimple_assign_lhs (stmt1);
	      src1 = gimple_assign_rhs1 (stmt1);
	      if (TREE_CODE (ptr1) != MEM_REF
		  || TYPE_MODE (TREE_TYPE (ptr1)) != TYPE_MODE (char_type_node)
		  || !host_integerp (src1, 0))
		break;
	      ptr1 = build_fold_addr_expr (ptr1);
	      callee1 = NULL_TREE;
	      len1 = size_one_node;
	      lhs1 = NULL_TREE;
	      off1 = size_zero_node;
	      str1 = NULL_TREE;
	    }
	  else
	    break;

	  diff = constant_pointer_difference (ptr1, ptr2);
	  if (diff == NULL && lhs1 != NULL)
	    {
	      diff = constant_pointer_difference (lhs1, ptr2);
	      if (DECL_FUNCTION_CODE (callee1) == BUILT_IN_MEMPCPY
		  && diff != NULL)
		diff = size_binop (PLUS_EXPR, diff,
				   fold_convert (sizetype, len1));
	    }
	  /* If the difference between the second and first destination pointer
	     is not constant, or is bigger than memcpy length, bail out.  */
	  if (diff == NULL
	      || !host_integerp (diff, 1)
	      || tree_int_cst_lt (len1, diff))
	    break;

	  /* Use maximum of difference plus memset length and memcpy length
	     as the new memcpy length, if it is too big, bail out.  */
	  src_len = tree_low_cst (diff, 1);
	  src_len += tree_low_cst (len2, 1);
	  if (src_len < (unsigned HOST_WIDE_INT) tree_low_cst (len1, 1))
	    src_len = tree_low_cst (len1, 1);
	  if (src_len > 1024)
	    break;

	  /* If mempcpy value is used elsewhere, bail out, as mempcpy
	     with bigger length will return different result.  */
	  if (lhs1 != NULL_TREE
	      && DECL_FUNCTION_CODE (callee1) == BUILT_IN_MEMPCPY
	      && (TREE_CODE (lhs1) != SSA_NAME
		  || !single_imm_use (lhs1, &use_p, &use_stmt)
		  || use_stmt != stmt2))
	    break;

	  /* If anything reads memory in between memcpy and memset
	     call, the modified memcpy call might change it.  */
	  vdef = gimple_vdef (stmt1);
	  if (vdef != NULL
	      && (!single_imm_use (vdef, &use_p, &use_stmt)
		  || use_stmt != stmt2))
	    break;

	  ptr1_align = get_pointer_alignment (ptr1, BIGGEST_ALIGNMENT);
	  /* Construct the new source string literal.  */
	  src_buf = XALLOCAVEC (char, src_len + 1);
	  if (callee1)
	    memcpy (src_buf,
		    TREE_STRING_POINTER (str1) + tree_low_cst (off1, 1),
		    tree_low_cst (len1, 1));
	  else
	    src_buf[0] = tree_low_cst (src1, 0);
	  memset (src_buf + tree_low_cst (diff, 1),
		  tree_low_cst (val2, 1), tree_low_cst (len2, 1));
	  src_buf[src_len] = '\0';
	  /* Neither builtin_strncpy_read_str nor builtin_memcpy_read_str
	     handle embedded '\0's.  */
	  if (strlen (src_buf) != src_len)
	    break;
	  rtl_profile_for_bb (gimple_bb (stmt2));
	  /* If the new memcpy wouldn't be emitted by storing the literal
	     by pieces, this optimization might enlarge .rodata too much,
	     as commonly used string literals couldn't be shared any
	     longer.  */
	  if (!can_store_by_pieces (src_len,
				    builtin_strncpy_read_str,
				    src_buf, ptr1_align, false))
	    break;

	  new_str_cst = build_string_literal (src_len, src_buf);
	  if (callee1)
	    {
	      /* If STMT1 is a mem{,p}cpy call, adjust it and remove
		 memset call.  */
	      if (lhs1 && DECL_FUNCTION_CODE (callee1) == BUILT_IN_MEMPCPY)
		gimple_call_set_lhs (stmt1, NULL_TREE);
	      gimple_call_set_arg (stmt1, 1, new_str_cst);
	      gimple_call_set_arg (stmt1, 2,
				   build_int_cst (TREE_TYPE (len1), src_len));
	      update_stmt (stmt1);
	      unlink_stmt_vdef (stmt2);
	      gsi_remove (gsi_p, true);
	      release_defs (stmt2);
	      if (lhs1 && DECL_FUNCTION_CODE (callee1) == BUILT_IN_MEMPCPY)
		release_ssa_name (lhs1);
	      return true;
	    }
	  else
	    {
	      /* Otherwise, if STMT1 is length 1 memcpy optimized into
		 assignment, remove STMT1 and change memset call into
		 memcpy call.  */
	      gimple_stmt_iterator gsi = gsi_for_stmt (stmt1);

	      if (!is_gimple_val (ptr1))
		ptr1 = force_gimple_operand_gsi (gsi_p, ptr1, true, NULL_TREE,
						 true, GSI_SAME_STMT);
	      gimple_call_set_fndecl (stmt2, built_in_decls [BUILT_IN_MEMCPY]);
	      gimple_call_set_arg (stmt2, 0, ptr1);
	      gimple_call_set_arg (stmt2, 1, new_str_cst);
	      gimple_call_set_arg (stmt2, 2,
				   build_int_cst (TREE_TYPE (len2), src_len));
	      unlink_stmt_vdef (stmt1);
	      gsi_remove (&gsi, true);
	      release_defs (stmt1);
	      update_stmt (stmt2);
	      return false;
	    }
	}
      break;
    default:
      break;
    }
  return false;
}

/* Run bitwise and assignments throug the folder.  If the first argument is an
   ssa name that is itself a result of a typecast of an ADDR_EXPR to an
   integer, feed the ADDR_EXPR to the folder rather than the ssa name.
*/

static void
simplify_bitwise_and (gimple_stmt_iterator *gsi, gimple stmt)
{
  tree res;
  tree arg1 = gimple_assign_rhs1 (stmt);
  tree arg2 = gimple_assign_rhs2 (stmt);

  if (TREE_CODE (arg2) != INTEGER_CST)
    return;

  if (TREE_CODE (arg1) == SSA_NAME && !SSA_NAME_IS_DEFAULT_DEF (arg1))
    {
      gimple def = SSA_NAME_DEF_STMT (arg1);

      if (gimple_assign_cast_p (def)
	  && INTEGRAL_TYPE_P (gimple_expr_type (def)))
	{
	  tree op = gimple_assign_rhs1 (def);

	  if (TREE_CODE (op) == ADDR_EXPR)
	    arg1 = op;
	}
    }

  res = fold_binary_loc (gimple_location (stmt),
		     BIT_AND_EXPR, TREE_TYPE (gimple_assign_lhs (stmt)),
		     arg1, arg2);
  if (res && is_gimple_min_invariant (res))
    {
      gimple_assign_set_rhs_from_tree (gsi, res);
      update_stmt (stmt);
    }
  return;
}


/* Perform re-associations of the plus or minus statement STMT that are
   always permitted.  Returns true if the CFG was changed.  */

static bool
associate_plusminus (gimple stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);
  gimple_stmt_iterator gsi;
  bool changed;

  /* We can't reassociate at all for saturating types.  */
  if (TYPE_SATURATING (TREE_TYPE (rhs1)))
    return false;

  /* First contract negates.  */
  do
    {
      changed = false;

      /* A +- (-B) -> A -+ B.  */
      if (TREE_CODE (rhs2) == SSA_NAME)
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (rhs2);
	  if (is_gimple_assign (def_stmt)
	      && gimple_assign_rhs_code (def_stmt) == NEGATE_EXPR)
	    {
	      code = (code == MINUS_EXPR) ? PLUS_EXPR : MINUS_EXPR;
	      gimple_assign_set_rhs_code (stmt, code);
	      rhs2 = gimple_assign_rhs1 (def_stmt);
	      gimple_assign_set_rhs2 (stmt, rhs2);
	      gimple_set_modified (stmt, true);
	      changed = true;
	    }
	}

      /* (-A) + B -> B - A.  */
      if (TREE_CODE (rhs1) == SSA_NAME
	  && code == PLUS_EXPR)
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (rhs1);
	  if (is_gimple_assign (def_stmt)
	      && gimple_assign_rhs_code (def_stmt) == NEGATE_EXPR)
	    {
	      code = MINUS_EXPR;
	      gimple_assign_set_rhs_code (stmt, code);
	      rhs1 = rhs2;
	      gimple_assign_set_rhs1 (stmt, rhs1);
	      rhs2 = gimple_assign_rhs1 (def_stmt);
	      gimple_assign_set_rhs2 (stmt, rhs2);
	      gimple_set_modified (stmt, true);
	      changed = true;
	    }
	}
    }
  while (changed);

  /* We can't reassociate floating-point or fixed-point plus or minus
     because of saturation to +-Inf.  */
  if (FLOAT_TYPE_P (TREE_TYPE (rhs1))
      || FIXED_POINT_TYPE_P (TREE_TYPE (rhs1)))
    goto out;

  /* Second match patterns that allow contracting a plus-minus pair
     irrespective of overflow issues.

	(A +- B) - A       ->  +- B
	(A +- B) -+ B      ->  A
	(CST +- A) +- CST  ->  CST +- A
	(A + CST) +- CST   ->  A + CST
	~A + A             ->  -1
	~A + 1             ->  -A 
	A - (A +- B)       ->  -+ B
	A +- (B +- A)      ->  +- B
	CST +- (CST +- A)  ->  CST +- A
	CST +- (A +- CST)  ->  CST +- A
	A + ~A             ->  -1

     via commutating the addition and contracting operations to zero
     by reassociation.  */

  gsi = gsi_for_stmt (stmt);
  if (TREE_CODE (rhs1) == SSA_NAME)
    {
      gimple def_stmt = SSA_NAME_DEF_STMT (rhs1);
      if (is_gimple_assign (def_stmt))
	{
	  enum tree_code def_code = gimple_assign_rhs_code (def_stmt);
	  if (def_code == PLUS_EXPR
	      || def_code == MINUS_EXPR)
	    {
	      tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	      tree def_rhs2 = gimple_assign_rhs2 (def_stmt);
	      if (operand_equal_p (def_rhs1, rhs2, 0)
		  && code == MINUS_EXPR)
		{
		  /* (A +- B) - A -> +- B.  */
		  code = ((def_code == PLUS_EXPR)
			  ? TREE_CODE (def_rhs2) : NEGATE_EXPR);
		  rhs1 = def_rhs2;
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	      else if (operand_equal_p (def_rhs2, rhs2, 0)
		       && code != def_code)
		{
		  /* (A +- B) -+ B -> A.  */
		  code = TREE_CODE (def_rhs1);
		  rhs1 = def_rhs1;
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	      else if (TREE_CODE (rhs2) == INTEGER_CST
		       && TREE_CODE (def_rhs1) == INTEGER_CST)
		{
		  /* (CST +- A) +- CST -> CST +- A.  */
		  tree cst = fold_binary (code, TREE_TYPE (rhs1),
					  def_rhs1, rhs2);
		  if (cst && !TREE_OVERFLOW (cst))
		    {
		      code = def_code;
		      gimple_assign_set_rhs_code (stmt, code);
		      rhs1 = cst;
		      gimple_assign_set_rhs1 (stmt, rhs1);
		      rhs2 = def_rhs2;
		      gimple_assign_set_rhs2 (stmt, rhs2);
		      gimple_set_modified (stmt, true);
		    }
		}
	      else if (TREE_CODE (rhs2) == INTEGER_CST
		       && TREE_CODE (def_rhs2) == INTEGER_CST
		       && def_code == PLUS_EXPR)
		{
		  /* (A + CST) +- CST -> A + CST.  */
		  tree cst = fold_binary (code, TREE_TYPE (rhs1),
					  def_rhs2, rhs2);
		  if (cst && !TREE_OVERFLOW (cst))
		    {
		      code = PLUS_EXPR;
		      gimple_assign_set_rhs_code (stmt, code);
		      rhs1 = def_rhs1;
		      gimple_assign_set_rhs1 (stmt, rhs1);
		      rhs2 = cst;
		      gimple_assign_set_rhs2 (stmt, rhs2);
		      gimple_set_modified (stmt, true);
		    }
		}
	    }
	  else if (def_code == BIT_NOT_EXPR
		   && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    {
	      tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	      if (code == PLUS_EXPR
		  && operand_equal_p (def_rhs1, rhs2, 0))
		{
		  /* ~A + A -> -1.  */
		  code = INTEGER_CST;
		  rhs1 = build_int_cst (TREE_TYPE (rhs2), -1);
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	      else if (code == PLUS_EXPR
		       && integer_onep (rhs1))
		{
		  /* ~A + 1 -> -A.  */
		  code = NEGATE_EXPR;
		  rhs1 = def_rhs1;
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	    }
	}
    }

  if (rhs2 && TREE_CODE (rhs2) == SSA_NAME)
    {
      gimple def_stmt = SSA_NAME_DEF_STMT (rhs2);
      if (is_gimple_assign (def_stmt))
	{
	  enum tree_code def_code = gimple_assign_rhs_code (def_stmt);
	  if (def_code == PLUS_EXPR
	      || def_code == MINUS_EXPR)
	    {
	      tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	      tree def_rhs2 = gimple_assign_rhs2 (def_stmt);
	      if (operand_equal_p (def_rhs1, rhs1, 0)
		  && code == MINUS_EXPR)
		{
		  /* A - (A +- B) -> -+ B.  */
		  code = ((def_code == PLUS_EXPR)
			  ? NEGATE_EXPR : TREE_CODE (def_rhs2));
		  rhs1 = def_rhs2;
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	      else if (operand_equal_p (def_rhs2, rhs1, 0)
		       && code != def_code)
		{
		  /* A +- (B +- A) -> +- B.  */
		  code = ((code == PLUS_EXPR)
			  ? TREE_CODE (def_rhs1) : NEGATE_EXPR);
		  rhs1 = def_rhs1;
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	      else if (TREE_CODE (rhs1) == INTEGER_CST
		       && TREE_CODE (def_rhs1) == INTEGER_CST)
		{
		  /* CST +- (CST +- A) -> CST +- A.  */
		  tree cst = fold_binary (code, TREE_TYPE (rhs2),
					  rhs1, def_rhs1);
		  if (cst && !TREE_OVERFLOW (cst))
		    {
		      code = (code == def_code ? PLUS_EXPR : MINUS_EXPR);
		      gimple_assign_set_rhs_code (stmt, code);
		      rhs1 = cst;
		      gimple_assign_set_rhs1 (stmt, rhs1);
		      rhs2 = def_rhs2;
		      gimple_assign_set_rhs2 (stmt, rhs2);
		      gimple_set_modified (stmt, true);
		    }
		}
	      else if (TREE_CODE (rhs1) == INTEGER_CST
		       && TREE_CODE (def_rhs2) == INTEGER_CST)
		{
		  /* CST +- (A +- CST) -> CST +- A.  */
		  tree cst = fold_binary (def_code == code
					  ? PLUS_EXPR : MINUS_EXPR,
					  TREE_TYPE (rhs2),
					  rhs1, def_rhs2);
		  if (cst && !TREE_OVERFLOW (cst))
		    {
		      rhs1 = cst;
		      gimple_assign_set_rhs1 (stmt, rhs1);
		      rhs2 = def_rhs1;
		      gimple_assign_set_rhs2 (stmt, rhs2);
		      gimple_set_modified (stmt, true);
		    }
		}
	    }
	  else if (def_code == BIT_NOT_EXPR
		   && INTEGRAL_TYPE_P (TREE_TYPE (rhs2)))
	    {
	      tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	      if (code == PLUS_EXPR
		  && operand_equal_p (def_rhs1, rhs1, 0))
		{
		  /* A + ~A -> -1.  */
		  code = INTEGER_CST;
		  rhs1 = build_int_cst (TREE_TYPE (rhs1), -1);
		  rhs2 = NULL_TREE;
		  gimple_assign_set_rhs_with_ops (&gsi, code, rhs1, NULL_TREE);
		  gcc_assert (gsi_stmt (gsi) == stmt);
		  gimple_set_modified (stmt, true);
		}
	    }
	}
    }

out:
  if (gimple_modified_p (stmt))
    {
      fold_stmt_inplace (stmt);
      update_stmt (stmt);
      if (maybe_clean_or_replace_eh_stmt (stmt, stmt)
	  && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
	return true;
    }

  return false;
}

/* Main entry point for the forward propagation optimizer.  */

static unsigned int
tree_ssa_forward_propagate_single_use_vars (void)
{
  basic_block bb;
  unsigned int todoflags = 0;

  cfg_changed = false;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;

      /* Note we update GSI within the loop as necessary.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  gimple stmt = gsi_stmt (gsi);

	  /* If this statement sets an SSA_NAME to an address,
	     try to propagate the address into the uses of the SSA_NAME.  */
	  if (is_gimple_assign (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);

	      if (TREE_CODE (lhs) != SSA_NAME)
		{
		  gsi_next (&gsi);
		  continue;
		}

	      if (gimple_assign_rhs_code (stmt) == ADDR_EXPR
		  /* Handle pointer conversions on invariant addresses
		     as well, as this is valid gimple.  */
		  || (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
		      && TREE_CODE (rhs) == ADDR_EXPR
		      && POINTER_TYPE_P (TREE_TYPE (lhs))))
		{
		  tree base = get_base_address (TREE_OPERAND (rhs, 0));
		  if ((!base
		       || !DECL_P (base)
		       || decl_address_invariant_p (base))
		      && !stmt_references_abnormal_ssa_name (stmt)
		      && forward_propagate_addr_expr (lhs, rhs))
		    {
		      release_defs (stmt);
		      todoflags |= TODO_remove_unused_locals;
		      gsi_remove (&gsi, true);
		    }
		  else
		    gsi_next (&gsi);
		}
	      else if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
		       && can_propagate_from (stmt))
		{
		  if (TREE_CODE (gimple_assign_rhs2 (stmt)) == INTEGER_CST
		      /* ???  Better adjust the interface to that function
			 instead of building new trees here.  */
		      && forward_propagate_addr_expr
		           (lhs,
			    build1 (ADDR_EXPR,
				    TREE_TYPE (rhs),
				    fold_build2 (MEM_REF,
						 TREE_TYPE (TREE_TYPE (rhs)),
						 rhs,
						 fold_convert
						   (ptr_type_node,
						    gimple_assign_rhs2 (stmt))))))
		    {
		      release_defs (stmt);
		      todoflags |= TODO_remove_unused_locals;
		      gsi_remove (&gsi, true);
		    }
		  else if (is_gimple_min_invariant (rhs))
		    {
		      /* Make sure to fold &a[0] + off_1 here.  */
		      fold_stmt_inplace (stmt);
		      update_stmt (stmt);
		      if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
			gsi_next (&gsi);
		    }
		  else
		    gsi_next (&gsi);
		}
	      else if ((gimple_assign_rhs_code (stmt) == BIT_NOT_EXPR
		        || gimple_assign_rhs_code (stmt) == NEGATE_EXPR)
		       && TREE_CODE (rhs) == SSA_NAME)
		{
		  simplify_not_neg_expr (&gsi);
		  gsi_next (&gsi);
		}
	      else if (gimple_assign_rhs_code (stmt) == COND_EXPR)
                {
                  /* In this case the entire COND_EXPR is in rhs1. */
		  int did_something;
		  fold_defer_overflow_warnings ();
                  did_something = forward_propagate_into_cond (&gsi);
		  stmt = gsi_stmt (gsi);
		  if (did_something == 2)
		    cfg_changed = true;
		  fold_undefer_overflow_warnings (!TREE_NO_WARNING (rhs)
		    && did_something, stmt, WARN_STRICT_OVERFLOW_CONDITIONAL);
		  gsi_next (&gsi);
                }
	      else if (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt))
					== tcc_comparison)
		{
		  if (forward_propagate_comparison (stmt))
		    cfg_changed = true;
		  gsi_next (&gsi);
		}
	      else if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR)
		{
		  simplify_bitwise_and (&gsi, stmt);
		  gsi_next (&gsi);
		}
	      else if (gimple_assign_rhs_code (stmt) == PLUS_EXPR
		       || gimple_assign_rhs_code (stmt) == MINUS_EXPR)
		{
		  cfg_changed |= associate_plusminus (stmt);
		  gsi_next (&gsi);
		}
	      else
		gsi_next (&gsi);
	    }
	  else if (gimple_code (stmt) == GIMPLE_SWITCH)
	    {
	      simplify_gimple_switch (stmt);
	      gsi_next (&gsi);
	    }
	  else if (gimple_code (stmt) == GIMPLE_COND)
	    {
	      int did_something;
	      fold_defer_overflow_warnings ();
	      did_something = forward_propagate_into_gimple_cond (stmt);
	      if (did_something == 2)
		cfg_changed = true;
	      fold_undefer_overflow_warnings (did_something, stmt,
					      WARN_STRICT_OVERFLOW_CONDITIONAL);
	      gsi_next (&gsi);
	    }
	  else if (is_gimple_call (stmt))
	    {
	      tree callee = gimple_call_fndecl (stmt);
	      if (callee == NULL_TREE
		  || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
		  || !simplify_builtin_call (&gsi, callee))
		gsi_next (&gsi);
	    }
	  else
	    gsi_next (&gsi);
	}
    }

  if (cfg_changed)
    todoflags |= TODO_cleanup_cfg;
  return todoflags;
}


static bool
gate_forwprop (void)
{
  return flag_tree_forwprop;
}

struct gimple_opt_pass pass_forwprop =
{
 {
  GIMPLE_PASS,
  "forwprop",			/* name */
  gate_forwprop,		/* gate */
  tree_ssa_forward_propagate_single_use_vars,	/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_FORWPROP,		/* tv_id */
  PROP_cfg | PROP_ssa,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func
  | TODO_ggc_collect
  | TODO_update_ssa
  | TODO_verify_ssa		/* todo_flags_finish */
 }
};

