/* Forward propagation of expressions for single use variables.
   Copyright (C) 2004-2018 Free Software Foundation, Inc.

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs-query.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-dom.h"
#include "builtins.h"
#include "tree-cfgcleanup.h"
#include "cfganal.h"
#include "optabs-tree.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"

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

static bool forward_propagate_addr_expr (tree, tree, bool);

/* Set to true if we delete dead edges during the optimization.  */
static bool cfg_changed;

static tree rhs_to_tree (tree type, gimple *stmt);

static bitmap to_purge;

/* Const-and-copy lattice.  */
static vec<tree> lattice;

/* Set the lattice entry for NAME to VAL.  */
static void
fwprop_set_lattice_val (tree name, tree val)
{
  if (TREE_CODE (name) == SSA_NAME)
    {
      if (SSA_NAME_VERSION (name) >= lattice.length ())
	{
	  lattice.reserve (num_ssa_names - lattice.length ());
	  lattice.quick_grow_cleared (num_ssa_names);
	}
      lattice[SSA_NAME_VERSION (name)] = val;
    }
}

/* Invalidate the lattice entry for NAME, done when releasing SSA names.  */
static void
fwprop_invalidate_lattice (tree name)
{
  if (name
      && TREE_CODE (name) == SSA_NAME
      && SSA_NAME_VERSION (name) < lattice.length ())
    lattice[SSA_NAME_VERSION (name)] = NULL_TREE;
}


/* Get the statement we can propagate from into NAME skipping
   trivial copies.  Returns the statement which defines the
   propagation source or NULL_TREE if there is no such one.
   If SINGLE_USE_ONLY is set considers only sources which have
   a single use chain up to NAME.  If SINGLE_USE_P is non-null,
   it is set to whether the chain to NAME is a single use chain
   or not.  SINGLE_USE_P is not written to if SINGLE_USE_ONLY is set.  */

static gimple *
get_prop_source_stmt (tree name, bool single_use_only, bool *single_use_p)
{
  bool single_use = true;

  do {
    gimple *def_stmt = SSA_NAME_DEF_STMT (name);

    if (!has_single_use (name))
      {
	single_use = false;
	if (single_use_only)
	  return NULL;
      }

    /* If name is defined by a PHI node or is the default def, bail out.  */
    if (!is_gimple_assign (def_stmt))
      return NULL;

    /* If def_stmt is a simple copy, continue looking.  */
    if (gimple_assign_rhs_code (def_stmt) == SSA_NAME)
      name = gimple_assign_rhs1 (def_stmt);
    else
      {
	if (!single_use_only && single_use_p)
	  *single_use_p = single_use;

	return def_stmt;
      }
  } while (1);
}

/* Checks if the destination ssa name in DEF_STMT can be used as
   propagation source.  Returns true if so, otherwise false.  */

static bool
can_propagate_from (gimple *def_stmt)
{
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
  if (stmt_references_abnormal_ssa_name (def_stmt))
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

/* Remove a chain of dead statements starting at the definition of
   NAME.  The chain is linked via the first operand of the defining statements.
   If NAME was replaced in its only use then this function can be used
   to clean up dead stmts.  The function handles already released SSA
   names gracefully.
   Returns true if cleanup-cfg has to run.  */

static bool
remove_prop_source_from_use (tree name)
{
  gimple_stmt_iterator gsi;
  gimple *stmt;
  bool cfg_changed = false;

  do {
    basic_block bb;

    if (SSA_NAME_IN_FREE_LIST (name)
	|| SSA_NAME_IS_DEFAULT_DEF (name)
	|| !has_zero_uses (name))
      return cfg_changed;

    stmt = SSA_NAME_DEF_STMT (name);
    if (gimple_code (stmt) == GIMPLE_PHI
	|| gimple_has_side_effects (stmt))
      return cfg_changed;

    bb = gimple_bb (stmt);
    gsi = gsi_for_stmt (stmt);
    unlink_stmt_vdef (stmt);
    if (gsi_remove (&gsi, true))
      bitmap_set_bit (to_purge, bb->index);
    fwprop_invalidate_lattice (gimple_get_lhs (stmt));
    release_defs (stmt);

    name = is_gimple_assign (stmt) ? gimple_assign_rhs1 (stmt) : NULL_TREE;
  } while (name && TREE_CODE (name) == SSA_NAME);

  return cfg_changed;
}

/* Return the rhs of a gassign *STMT in a form of a single tree,
   converted to type TYPE.

   This should disappear, but is needed so we can combine expressions and use
   the fold() interfaces. Long term, we need to develop folding and combine
   routines that deal with gimple exclusively . */

static tree
rhs_to_tree (tree type, gimple *stmt)
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
combine_cond_expr_cond (gimple *stmt, enum tree_code code, tree type,
			tree op0, tree op1, bool invariant_only)
{
  tree t;

  gcc_assert (TREE_CODE_CLASS (code) == tcc_comparison);

  fold_defer_overflow_warnings ();
  t = fold_binary_loc (gimple_location (stmt), code, type, op0, op1);
  if (!t)
    {
      fold_undefer_overflow_warnings (false, NULL, 0);
      return NULL_TREE;
    }

  /* Require that we got a boolean type out if we put one in.  */
  gcc_assert (TREE_CODE (TREE_TYPE (t)) == TREE_CODE (type));

  /* Canonicalize the combined condition for use in a COND_EXPR.  */
  t = canonicalize_cond_expr_cond (t);

  /* Bail out if we required an invariant but didn't get one.  */
  if (!t || (invariant_only && !is_gimple_min_invariant (t)))
    {
      fold_undefer_overflow_warnings (false, NULL, 0);
      return NULL_TREE;
    }

  fold_undefer_overflow_warnings (!gimple_no_warning_p (stmt), stmt, 0);

  return t;
}

/* Combine the comparison OP0 CODE OP1 at LOC with the defining statements
   of its operand.  Return a new comparison tree or NULL_TREE if there
   were no simplifying combines.  */

static tree
forward_propagate_into_comparison_1 (gimple *stmt,
				     enum tree_code code, tree type,
				     tree op0, tree op1)
{
  tree tmp = NULL_TREE;
  tree rhs0 = NULL_TREE, rhs1 = NULL_TREE;
  bool single_use0_p = false, single_use1_p = false;

  /* For comparisons use the first operand, that is likely to
     simplify comparisons against constants.  */
  if (TREE_CODE (op0) == SSA_NAME)
    {
      gimple *def_stmt = get_prop_source_stmt (op0, false, &single_use0_p);
      if (def_stmt && can_propagate_from (def_stmt))
	{
	  enum tree_code def_code = gimple_assign_rhs_code (def_stmt);
	  bool invariant_only_p = !single_use0_p;

	  rhs0 = rhs_to_tree (TREE_TYPE (op1), def_stmt);

	  /* Always combine comparisons or conversions from booleans.  */
	  if (TREE_CODE (op1) == INTEGER_CST
	      && ((CONVERT_EXPR_CODE_P (def_code)
		   && TREE_CODE (TREE_TYPE (TREE_OPERAND (rhs0, 0)))
		      == BOOLEAN_TYPE)
		  || TREE_CODE_CLASS (def_code) == tcc_comparison))
	    invariant_only_p = false;

	  tmp = combine_cond_expr_cond (stmt, code, type,
					rhs0, op1, invariant_only_p);
	  if (tmp)
	    return tmp;
	}
    }

  /* If that wasn't successful, try the second operand.  */
  if (TREE_CODE (op1) == SSA_NAME)
    {
      gimple *def_stmt = get_prop_source_stmt (op1, false, &single_use1_p);
      if (def_stmt && can_propagate_from (def_stmt))
	{
	  rhs1 = rhs_to_tree (TREE_TYPE (op0), def_stmt);
	  tmp = combine_cond_expr_cond (stmt, code, type,
					op0, rhs1, !single_use1_p);
	  if (tmp)
	    return tmp;
	}
    }

  /* If that wasn't successful either, try both operands.  */
  if (rhs0 != NULL_TREE
      && rhs1 != NULL_TREE)
    tmp = combine_cond_expr_cond (stmt, code, type,
				  rhs0, rhs1,
				  !(single_use0_p && single_use1_p));

  return tmp;
}

/* Propagate from the ssa name definition statements of the assignment
   from a comparison at *GSI into the conditional if that simplifies it.
   Returns 1 if the stmt was modified and 2 if the CFG needs cleanup,
   otherwise returns 0.  */

static int 
forward_propagate_into_comparison (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree tmp;
  bool cfg_changed = false;
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);

  /* Combine the comparison with defining statements.  */
  tmp = forward_propagate_into_comparison_1 (stmt,
					     gimple_assign_rhs_code (stmt),
					     type, rhs1, rhs2);
  if (tmp && useless_type_conversion_p (type, TREE_TYPE (tmp)))
    {
      gimple_assign_set_rhs_from_tree (gsi, tmp);
      fold_stmt (gsi);
      update_stmt (gsi_stmt (*gsi));

      if (TREE_CODE (rhs1) == SSA_NAME)
	cfg_changed |= remove_prop_source_from_use (rhs1);
      if (TREE_CODE (rhs2) == SSA_NAME)
	cfg_changed |= remove_prop_source_from_use (rhs2);
      return cfg_changed ? 2 : 1;
    }

  return 0;
}

/* Propagate from the ssa name definition statements of COND_EXPR
   in GIMPLE_COND statement STMT into the conditional if that simplifies it.
   Returns zero if no statement was changed, one if there were
   changes and two if cfg_cleanup needs to run.

   This must be kept in sync with forward_propagate_into_cond.  */

static int
forward_propagate_into_gimple_cond (gcond *stmt)
{
  tree tmp;
  enum tree_code code = gimple_cond_code (stmt);
  bool cfg_changed = false;
  tree rhs1 = gimple_cond_lhs (stmt);
  tree rhs2 = gimple_cond_rhs (stmt);

  /* We can do tree combining on SSA_NAME and comparison expressions.  */
  if (TREE_CODE_CLASS (gimple_cond_code (stmt)) != tcc_comparison)
    return 0;

  tmp = forward_propagate_into_comparison_1 (stmt, code,
					     boolean_type_node,
					     rhs1, rhs2);
  if (tmp)
    {
      if (dump_file && tmp)
	{
	  fprintf (dump_file, "  Replaced '");
	  print_gimple_expr (dump_file, stmt, 0);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, tmp);
	  fprintf (dump_file, "'\n");
	}

      gimple_cond_set_condition_from_tree (stmt, unshare_expr (tmp));
      update_stmt (stmt);

      if (TREE_CODE (rhs1) == SSA_NAME)
	cfg_changed |= remove_prop_source_from_use (rhs1);
      if (TREE_CODE (rhs2) == SSA_NAME)
	cfg_changed |= remove_prop_source_from_use (rhs2);
      return (cfg_changed || is_gimple_min_invariant (tmp)) ? 2 : 1;
    }

  /* Canonicalize _Bool == 0 and _Bool != 1 to _Bool != 0 by swapping edges.  */
  if ((TREE_CODE (TREE_TYPE (rhs1)) == BOOLEAN_TYPE
       || (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	   && TYPE_PRECISION (TREE_TYPE (rhs1)) == 1))
      && ((code == EQ_EXPR
	   && integer_zerop (rhs2))
	  || (code == NE_EXPR
	      && integer_onep (rhs2))))
    {
      basic_block bb = gimple_bb (stmt);
      gimple_cond_set_code (stmt, NE_EXPR);
      gimple_cond_set_rhs (stmt, build_zero_cst (TREE_TYPE (rhs1)));
      EDGE_SUCC (bb, 0)->flags ^= (EDGE_TRUE_VALUE|EDGE_FALSE_VALUE);
      EDGE_SUCC (bb, 1)->flags ^= (EDGE_TRUE_VALUE|EDGE_FALSE_VALUE);
      return 1;
    }

  return 0;
}


/* Propagate from the ssa name definition statements of COND_EXPR
   in the rhs of statement STMT into the conditional if that simplifies it.
   Returns true zero if the stmt was changed.  */

static bool
forward_propagate_into_cond (gimple_stmt_iterator *gsi_p)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  tree tmp = NULL_TREE;
  tree cond = gimple_assign_rhs1 (stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);

  /* We can do tree combining on SSA_NAME and comparison expressions.  */
  if (COMPARISON_CLASS_P (cond))
    tmp = forward_propagate_into_comparison_1 (stmt, TREE_CODE (cond),
					       TREE_TYPE (cond),
					       TREE_OPERAND (cond, 0),
					       TREE_OPERAND (cond, 1));
  else if (TREE_CODE (cond) == SSA_NAME)
    {
      enum tree_code def_code;
      tree name = cond;
      gimple *def_stmt = get_prop_source_stmt (name, true, NULL);
      if (!def_stmt || !can_propagate_from (def_stmt))
	return 0;

      def_code = gimple_assign_rhs_code (def_stmt);
      if (TREE_CODE_CLASS (def_code) == tcc_comparison)
	tmp = fold_build2_loc (gimple_location (def_stmt),
			       def_code,
			       TREE_TYPE (cond),
			       gimple_assign_rhs1 (def_stmt),
			       gimple_assign_rhs2 (def_stmt));
    }

  if (tmp
      && is_gimple_condexpr (tmp))
    {
      if (dump_file && tmp)
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, cond);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, tmp);
	  fprintf (dump_file, "'\n");
	}

      if ((code == VEC_COND_EXPR) ? integer_all_onesp (tmp)
				  : integer_onep (tmp))
	gimple_assign_set_rhs_from_tree (gsi_p, gimple_assign_rhs2 (stmt));
      else if (integer_zerop (tmp))
	gimple_assign_set_rhs_from_tree (gsi_p, gimple_assign_rhs3 (stmt));
      else
	gimple_assign_set_rhs1 (stmt, unshare_expr (tmp));
      stmt = gsi_stmt (*gsi_p);
      update_stmt (stmt);

      return true;
    }

  return 0;
}

/* We've just substituted an ADDR_EXPR into stmt.  Update all the
   relevant data structures to match.  */

static void
tidy_after_forward_propagate_addr (gimple *stmt)
{
  /* We may have turned a trapping insn into a non-trapping insn.  */
  if (maybe_clean_or_replace_eh_stmt (stmt, stmt))
    bitmap_set_bit (to_purge, gimple_bb (stmt)->index);

  if (TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR)
     recompute_tree_invariant_for_addr_expr (gimple_assign_rhs1 (stmt));
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
  gimple *use_stmt = gsi_stmt (*use_stmt_gsi);
  enum tree_code rhs_code;
  bool res = true;

  gcc_assert (TREE_CODE (def_rhs) == ADDR_EXPR);

  lhs = gimple_assign_lhs (use_stmt);
  rhs_code = gimple_assign_rhs_code (use_stmt);
  rhs = gimple_assign_rhs1 (use_stmt);

  /* Do not perform copy-propagation but recurse through copy chains.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && rhs_code == SSA_NAME)
    return forward_propagate_addr_expr (lhs, def_rhs, single_use_p);

  /* The use statement could be a conversion.  Recurse to the uses of the
     lhs as copyprop does not copy through pointer to integer to pointer
     conversions and FRE does not catch all cases either.
     Treat the case of a single-use name and
     a conversion to def_rhs type separate, though.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && CONVERT_EXPR_CODE_P (rhs_code))
    {
      /* If there is a point in a conversion chain where the types match
         so we can remove a conversion re-materialize the address here
	 and stop.  */
      if (single_use_p
	  && useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (def_rhs)))
	{
	  gimple_assign_set_rhs1 (use_stmt, unshare_expr (def_rhs));
	  gimple_assign_set_rhs_code (use_stmt, TREE_CODE (def_rhs));
	  return true;
	}

      /* Else recurse if the conversion preserves the address value.  */
      if ((INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	   || POINTER_TYPE_P (TREE_TYPE (lhs)))
	  && (TYPE_PRECISION (TREE_TYPE (lhs))
	      >= TYPE_PRECISION (TREE_TYPE (def_rhs))))
	return forward_propagate_addr_expr (lhs, def_rhs, single_use_p);

      return false;
    }

  /* If this isn't a conversion chain from this on we only can propagate
     into compatible pointer contexts.  */
  if (!types_compatible_p (TREE_TYPE (name), TREE_TYPE (def_rhs)))
    return false;

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
	  && forward_propagate_addr_expr (lhs, new_def_rhs, single_use_p))
	return true;

      if (useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (new_def_rhs)))
	gimple_assign_set_rhs_with_ops (use_stmt_gsi, TREE_CODE (new_def_rhs),
					new_def_rhs);
      else if (is_gimple_min_invariant (new_def_rhs))
	gimple_assign_set_rhs_with_ops (use_stmt_gsi, NOP_EXPR, new_def_rhs);
      else
	return false;
      gcc_assert (gsi_stmt (*use_stmt_gsi) == use_stmt);
      update_stmt (use_stmt);
      return true;
    }

  /* Now strip away any outer COMPONENT_REF/ARRAY_REF nodes from the LHS.
     ADDR_EXPR will not appear on the LHS.  */
  tree *lhsp = gimple_assign_lhs_ptr (use_stmt);
  while (handled_component_p (*lhsp))
    lhsp = &TREE_OPERAND (*lhsp, 0);
  lhs = *lhsp;

  /* Now see if the LHS node is a MEM_REF using NAME.  If so,
     propagate the ADDR_EXPR into the use of NAME and fold the result.  */
  if (TREE_CODE (lhs) == MEM_REF
      && TREE_OPERAND (lhs, 0) == name)
    {
      tree def_rhs_base;
      poly_int64 def_rhs_offset;
      /* If the address is invariant we can always fold it.  */
      if ((def_rhs_base = get_addr_base_and_unit_offset (TREE_OPERAND (def_rhs, 0),
							 &def_rhs_offset)))
	{
	  poly_offset_int off = mem_ref_offset (lhs);
	  tree new_ptr;
	  off += def_rhs_offset;
	  if (TREE_CODE (def_rhs_base) == MEM_REF)
	    {
	      off += mem_ref_offset (def_rhs_base);
	      new_ptr = TREE_OPERAND (def_rhs_base, 0);
	    }
	  else
	    new_ptr = build_fold_addr_expr (def_rhs_base);
	  TREE_OPERAND (lhs, 0) = new_ptr;
	  TREE_OPERAND (lhs, 1)
	    = wide_int_to_tree (TREE_TYPE (TREE_OPERAND (lhs, 1)), off);
	  tidy_after_forward_propagate_addr (use_stmt);
	  /* Continue propagating into the RHS if this was not the only use.  */
	  if (single_use_p)
	    return true;
	}
      /* If the LHS is a plain dereference and the value type is the same as
         that of the pointed-to type of the address we can put the
	 dereferenced address on the LHS preserving the original alias-type.  */
      else if (integer_zerop (TREE_OPERAND (lhs, 1))
	       && ((gimple_assign_lhs (use_stmt) == lhs
		    && useless_type_conversion_p
		         (TREE_TYPE (TREE_OPERAND (def_rhs, 0)),
		          TREE_TYPE (gimple_assign_rhs1 (use_stmt))))
		   || types_compatible_p (TREE_TYPE (lhs),
					  TREE_TYPE (TREE_OPERAND (def_rhs, 0))))
	       /* Don't forward anything into clobber stmts if it would result
		  in the lhs no longer being a MEM_REF.  */
	       && (!gimple_clobber_p (use_stmt)
		   || TREE_CODE (TREE_OPERAND (def_rhs, 0)) == MEM_REF))
	{
	  tree *def_rhs_basep = &TREE_OPERAND (def_rhs, 0);
	  tree new_offset, new_base, saved, new_lhs;
	  while (handled_component_p (*def_rhs_basep))
	    def_rhs_basep = &TREE_OPERAND (*def_rhs_basep, 0);
	  saved = *def_rhs_basep;
	  if (TREE_CODE (*def_rhs_basep) == MEM_REF)
	    {
	      new_base = TREE_OPERAND (*def_rhs_basep, 0);
	      new_offset = fold_convert (TREE_TYPE (TREE_OPERAND (lhs, 1)),
					 TREE_OPERAND (*def_rhs_basep, 1));
	    }
	  else
	    {
	      new_base = build_fold_addr_expr (*def_rhs_basep);
	      new_offset = TREE_OPERAND (lhs, 1);
	    }
	  *def_rhs_basep = build2 (MEM_REF, TREE_TYPE (*def_rhs_basep),
				   new_base, new_offset);
	  TREE_THIS_VOLATILE (*def_rhs_basep) = TREE_THIS_VOLATILE (lhs);
	  TREE_SIDE_EFFECTS (*def_rhs_basep) = TREE_SIDE_EFFECTS (lhs);
	  TREE_THIS_NOTRAP (*def_rhs_basep) = TREE_THIS_NOTRAP (lhs);
	  new_lhs = unshare_expr (TREE_OPERAND (def_rhs, 0));
	  *lhsp = new_lhs;
	  TREE_THIS_VOLATILE (new_lhs) = TREE_THIS_VOLATILE (lhs);
	  TREE_SIDE_EFFECTS (new_lhs) = TREE_SIDE_EFFECTS (lhs);
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
  tree *rhsp = gimple_assign_rhs1_ptr (use_stmt);
  if (TREE_CODE (*rhsp) == ADDR_EXPR)
    rhsp = &TREE_OPERAND (*rhsp, 0);
  while (handled_component_p (*rhsp))
    rhsp = &TREE_OPERAND (*rhsp, 0);
  rhs = *rhsp;

  /* Now see if the RHS node is a MEM_REF using NAME.  If so,
     propagate the ADDR_EXPR into the use of NAME and fold the result.  */
  if (TREE_CODE (rhs) == MEM_REF
      && TREE_OPERAND (rhs, 0) == name)
    {
      tree def_rhs_base;
      poly_int64 def_rhs_offset;
      if ((def_rhs_base = get_addr_base_and_unit_offset (TREE_OPERAND (def_rhs, 0),
							 &def_rhs_offset)))
	{
	  poly_offset_int off = mem_ref_offset (rhs);
	  tree new_ptr;
	  off += def_rhs_offset;
	  if (TREE_CODE (def_rhs_base) == MEM_REF)
	    {
	      off += mem_ref_offset (def_rhs_base);
	      new_ptr = TREE_OPERAND (def_rhs_base, 0);
	    }
	  else
	    new_ptr = build_fold_addr_expr (def_rhs_base);
	  TREE_OPERAND (rhs, 0) = new_ptr;
	  TREE_OPERAND (rhs, 1)
	    = wide_int_to_tree (TREE_TYPE (TREE_OPERAND (rhs, 1)), off);
	  fold_stmt_inplace (use_stmt_gsi);
	  tidy_after_forward_propagate_addr (use_stmt);
	  return res;
	}
      /* If the RHS is a plain dereference and the value type is the same as
         that of the pointed-to type of the address we can put the
	 dereferenced address on the RHS preserving the original alias-type.  */
      else if (integer_zerop (TREE_OPERAND (rhs, 1))
	       && ((gimple_assign_rhs1 (use_stmt) == rhs
		    && useless_type_conversion_p
		         (TREE_TYPE (gimple_assign_lhs (use_stmt)),
		          TREE_TYPE (TREE_OPERAND (def_rhs, 0))))
		   || types_compatible_p (TREE_TYPE (rhs),
					  TREE_TYPE (TREE_OPERAND (def_rhs, 0)))))
	{
	  tree *def_rhs_basep = &TREE_OPERAND (def_rhs, 0);
	  tree new_offset, new_base, saved, new_rhs;
	  while (handled_component_p (*def_rhs_basep))
	    def_rhs_basep = &TREE_OPERAND (*def_rhs_basep, 0);
	  saved = *def_rhs_basep;
	  if (TREE_CODE (*def_rhs_basep) == MEM_REF)
	    {
	      new_base = TREE_OPERAND (*def_rhs_basep, 0);
	      new_offset = fold_convert (TREE_TYPE (TREE_OPERAND (rhs, 1)),
					 TREE_OPERAND (*def_rhs_basep, 1));
	    }
	  else
	    {
	      new_base = build_fold_addr_expr (*def_rhs_basep);
	      new_offset = TREE_OPERAND (rhs, 1);
	    }
	  *def_rhs_basep = build2 (MEM_REF, TREE_TYPE (*def_rhs_basep),
				   new_base, new_offset);
	  TREE_THIS_VOLATILE (*def_rhs_basep) = TREE_THIS_VOLATILE (rhs);
	  TREE_SIDE_EFFECTS (*def_rhs_basep) = TREE_SIDE_EFFECTS (rhs);
	  TREE_THIS_NOTRAP (*def_rhs_basep) = TREE_THIS_NOTRAP (rhs);
	  new_rhs = unshare_expr (TREE_OPERAND (def_rhs, 0));
	  *rhsp = new_rhs;
	  TREE_THIS_VOLATILE (new_rhs) = TREE_THIS_VOLATILE (rhs);
	  TREE_SIDE_EFFECTS (new_rhs) = TREE_SIDE_EFFECTS (rhs);
	  *def_rhs_basep = saved;
	  fold_stmt_inplace (use_stmt_gsi);
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
  /* Optimize &x[C1] p+ C2 to  &x p+ C3 with C3 = C1 * element_size + C2.  */
  if (TREE_CODE (rhs2) == INTEGER_CST)
    {
      tree new_rhs = build1_loc (gimple_location (use_stmt),
				 ADDR_EXPR, TREE_TYPE (def_rhs),
				 fold_build2 (MEM_REF,
					      TREE_TYPE (TREE_TYPE (def_rhs)),
					      unshare_expr (def_rhs),
					      fold_convert (ptr_type_node,
							    rhs2)));
      gimple_assign_set_rhs_from_tree (use_stmt_gsi, new_rhs);
      use_stmt = gsi_stmt (*use_stmt_gsi);
      update_stmt (use_stmt);
      tidy_after_forward_propagate_addr (use_stmt);
      return true;
    }

  return false;
}

/* STMT is a statement of the form SSA_NAME = ADDR_EXPR <whatever>.

   Try to forward propagate the ADDR_EXPR into all uses of the SSA_NAME.
   Often this will allow for removal of an ADDR_EXPR and INDIRECT_REF
   node or for recovery of array indexing from pointer arithmetic.

   PARENT_SINGLE_USE_P tells if, when in a recursive invocation, NAME was
   the single use in the previous invocation.  Pass true when calling
   this as toplevel.

   Returns true, if all uses have been propagated into.  */

static bool
forward_propagate_addr_expr (tree name, tree rhs, bool parent_single_use_p)
{
  imm_use_iterator iter;
  gimple *use_stmt;
  bool all = true;
  bool single_use_p = parent_single_use_p && has_single_use (name);

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, name)
    {
      bool result;
      tree use_rhs;

      /* If the use is not in a simple assignment statement, then
	 there is nothing we can do.  */
      if (!is_gimple_assign (use_stmt))
	{
	  if (!is_gimple_debug (use_stmt))
	    all = false;
	  continue;
	}

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
      all &= result;

      /* Remove intermediate now unused copy and conversion chains.  */
      use_rhs = gimple_assign_rhs1 (use_stmt);
      if (result
	  && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	  && TREE_CODE (use_rhs) == SSA_NAME
	  && has_zero_uses (gimple_assign_lhs (use_stmt)))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	  fwprop_invalidate_lattice (gimple_get_lhs (use_stmt));
	  release_defs (use_stmt);
	  gsi_remove (&gsi, true);
	}
    }

  return all && has_zero_uses (name);
}


/* Helper function for simplify_gimple_switch.  Remove case labels that
   have values outside the range of the new type.  */

static void
simplify_gimple_switch_label_vec (gswitch *stmt, tree index_type)
{
  unsigned int branch_num = gimple_switch_num_labels (stmt);
  auto_vec<tree> labels (branch_num);
  unsigned int i, len;

  /* Collect the existing case labels in a VEC, and preprocess it as if
     we are gimplifying a GENERIC SWITCH_EXPR.  */
  for (i = 1; i < branch_num; i++)
    labels.quick_push (gimple_switch_label (stmt, i));
  preprocess_case_label_vec_for_gimple (labels, index_type, NULL);

  /* If any labels were removed, replace the existing case labels
     in the GIMPLE_SWITCH statement with the correct ones.
     Note that the type updates were done in-place on the case labels,
     so we only have to replace the case labels in the GIMPLE_SWITCH
     if the number of labels changed.  */
  len = labels.length ();
  if (len < branch_num - 1)
    {
      bitmap target_blocks;
      edge_iterator ei;
      edge e;

      /* Corner case: *all* case labels have been removed as being
	 out-of-range for INDEX_TYPE.  Push one label and let the
	 CFG cleanups deal with this further.  */
      if (len == 0)
	{
	  tree label, elt;

	  label = CASE_LABEL (gimple_switch_default_label (stmt));
	  elt = build_case_label (build_int_cst (index_type, 0), NULL, label);
	  labels.quick_push (elt);
	  len = 1;
	}

      for (i = 0; i < labels.length (); i++)
	gimple_switch_set_label (stmt, i + 1, labels[i]);
      for (i++ ; i < branch_num; i++)
	gimple_switch_set_label (stmt, i, NULL_TREE);
      gimple_switch_set_num_labels (stmt, len + 1);

      /* Cleanup any edges that are now dead.  */
      target_blocks = BITMAP_ALLOC (NULL);
      for (i = 0; i < gimple_switch_num_labels (stmt); i++)
	{
	  tree elt = gimple_switch_label (stmt, i);
	  basic_block target = label_to_block (cfun, CASE_LABEL (elt));
	  bitmap_set_bit (target_blocks, target->index);
	}
      for (ei = ei_start (gimple_bb (stmt)->succs); (e = ei_safe_edge (ei)); )
	{
	  if (! bitmap_bit_p (target_blocks, e->dest->index))
	    {
	      remove_edge (e);
	      cfg_changed = true;
	      free_dominance_info (CDI_DOMINATORS);
	    }
	  else
	    ei_next (&ei);
	} 
      BITMAP_FREE (target_blocks);
    }
}

/* STMT is a SWITCH_EXPR for which we attempt to find equivalent forms of
   the condition which we may be able to optimize better.  */

static bool
simplify_gimple_switch (gswitch *stmt)
{
  /* The optimization that we really care about is removing unnecessary
     casts.  That will let us do much better in propagating the inferred
     constant at the switch target.  */
  tree cond = gimple_switch_index (stmt);
  if (TREE_CODE (cond) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (cond);
      if (gimple_assign_cast_p (def_stmt))
	{
	  tree def = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (def) != SSA_NAME)
	    return false;

	  /* If we have an extension or sign-change that preserves the
	     values we check against then we can copy the source value into
	     the switch.  */
	  tree ti = TREE_TYPE (def);
	  if (INTEGRAL_TYPE_P (ti)
	      && TYPE_PRECISION (ti) <= TYPE_PRECISION (TREE_TYPE (cond)))
	    {
	      size_t n = gimple_switch_num_labels (stmt);
	      tree min = NULL_TREE, max = NULL_TREE;
	      if (n > 1)
		{
		  min = CASE_LOW (gimple_switch_label (stmt, 1));
		  if (CASE_HIGH (gimple_switch_label (stmt, n - 1)))
		    max = CASE_HIGH (gimple_switch_label (stmt, n - 1));
		  else
		    max = CASE_LOW (gimple_switch_label (stmt, n - 1));
		}
	      if ((!min || int_fits_type_p (min, ti))
		  && (!max || int_fits_type_p (max, ti)))
		{
		  gimple_switch_set_index (stmt, def);
		  simplify_gimple_switch_label_vec (stmt, ti);
		  update_stmt (stmt);
		  return true;
		}
	    }
	}
    }

  return false;
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
      gimple *stmt;
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
	      poly_int64 offset;
	      tree base = get_addr_base_and_unit_offset (q, &offset);
	      if (base)
		{
		  q = base;
		  if (maybe_ne (offset, 0))
		    off = size_binop (PLUS_EXPR, off, size_int (offset));
		}
	      if (TREE_CODE (q) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (q, 0)) == SSA_NAME)
		{
		  p = TREE_OPERAND (q, 0);
		  off = size_binop (PLUS_EXPR, off,
				    wide_int_to_tree (sizetype,
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
	  else if (code == ADDR_EXPR || CONVERT_EXPR_CODE_P (code))
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
  gimple *stmt1, *stmt2 = gsi_stmt (*gsi_p);
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
	  gimple *use_stmt;
	  unsigned int ptr1_align;
	  unsigned HOST_WIDE_INT src_len;
	  char *src_buf;
	  use_operand_p use_p;

	  if (!tree_fits_shwi_p (val2)
	      || !tree_fits_uhwi_p (len2)
	      || compare_tree_int (len2, 1024) == 1)
	    break;
	  if (is_gimple_call (stmt1))
	    {
	      /* If first stmt is a call, it needs to be memcpy
		 or mempcpy, with string literal as second argument and
		 constant length.  */
	      callee1 = gimple_call_fndecl (stmt1);
	      if (callee1 == NULL_TREE
		  || !fndecl_built_in_p (callee1, BUILT_IN_NORMAL)
		  || gimple_call_num_args (stmt1) != 3)
		break;
	      if (DECL_FUNCTION_CODE (callee1) != BUILT_IN_MEMCPY
		  && DECL_FUNCTION_CODE (callee1) != BUILT_IN_MEMPCPY)
		break;
	      ptr1 = gimple_call_arg (stmt1, 0);
	      src1 = gimple_call_arg (stmt1, 1);
	      len1 = gimple_call_arg (stmt1, 2);
	      lhs1 = gimple_call_lhs (stmt1);
	      if (!tree_fits_uhwi_p (len1))
		break;
	      str1 = string_constant (src1, &off1, NULL, NULL);
	      if (str1 == NULL_TREE)
		break;
	      if (!tree_fits_uhwi_p (off1)
		  || compare_tree_int (off1, TREE_STRING_LENGTH (str1) - 1) > 0
		  || compare_tree_int (len1, TREE_STRING_LENGTH (str1)
					     - tree_to_uhwi (off1)) > 0
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
		  || !tree_fits_shwi_p (src1))
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
	      || !tree_fits_uhwi_p (diff)
	      || tree_int_cst_lt (len1, diff)
	      || compare_tree_int (diff, 1024) == 1)
	    break;

	  /* Use maximum of difference plus memset length and memcpy length
	     as the new memcpy length, if it is too big, bail out.  */
	  src_len = tree_to_uhwi (diff);
	  src_len += tree_to_uhwi (len2);
	  if (src_len < tree_to_uhwi (len1))
	    src_len = tree_to_uhwi (len1);
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

	  ptr1_align = get_pointer_alignment (ptr1);
	  /* Construct the new source string literal.  */
	  src_buf = XALLOCAVEC (char, src_len + 1);
	  if (callee1)
	    memcpy (src_buf,
		    TREE_STRING_POINTER (str1) + tree_to_uhwi (off1),
		    tree_to_uhwi (len1));
	  else
	    src_buf[0] = tree_to_shwi (src1);
	  memset (src_buf + tree_to_uhwi (diff),
		  tree_to_shwi (val2), tree_to_uhwi (len2));
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

	  new_str_cst = build_string_literal (src_len + 1, src_buf);
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
	      fwprop_invalidate_lattice (gimple_get_lhs (stmt2));
	      release_defs (stmt2);
	      if (lhs1 && DECL_FUNCTION_CODE (callee1) == BUILT_IN_MEMPCPY)
		{
		  fwprop_invalidate_lattice (lhs1);
		  release_ssa_name (lhs1);
		}
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
	      gimple_call_set_fndecl (stmt2,
				      builtin_decl_explicit (BUILT_IN_MEMCPY));
	      gimple_call_set_arg (stmt2, 0, ptr1);
	      gimple_call_set_arg (stmt2, 1, new_str_cst);
	      gimple_call_set_arg (stmt2, 2,
				   build_int_cst (TREE_TYPE (len2), src_len));
	      unlink_stmt_vdef (stmt1);
	      gsi_remove (&gsi, true);
	      fwprop_invalidate_lattice (gimple_get_lhs (stmt1));
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

/* Given a ssa_name in NAME see if it was defined by an assignment and
   set CODE to be the code and ARG1 to the first operand on the rhs and ARG2
   to the second operand on the rhs. */

static inline void
defcodefor_name (tree name, enum tree_code *code, tree *arg1, tree *arg2)
{
  gimple *def;
  enum tree_code code1;
  tree arg11;
  tree arg21;
  tree arg31;
  enum gimple_rhs_class grhs_class;

  code1 = TREE_CODE (name);
  arg11 = name;
  arg21 = NULL_TREE;
  arg31 = NULL_TREE;
  grhs_class = get_gimple_rhs_class (code1);

  if (code1 == SSA_NAME)
    {
      def = SSA_NAME_DEF_STMT (name);
      
      if (def && is_gimple_assign (def)
	  && can_propagate_from (def))
	{
	  code1 = gimple_assign_rhs_code (def);
	  arg11 = gimple_assign_rhs1 (def);
          arg21 = gimple_assign_rhs2 (def);
          arg31 = gimple_assign_rhs3 (def);
	}
    }
  else if (grhs_class != GIMPLE_SINGLE_RHS)
    code1 = ERROR_MARK;

  *code = code1;
  *arg1 = arg11;
  if (arg2)
    *arg2 = arg21;
  if (arg31)
    *code = ERROR_MARK;
}


/* Recognize rotation patterns.  Return true if a transformation
   applied, otherwise return false.

   We are looking for X with unsigned type T with bitsize B, OP being
   +, | or ^, some type T2 wider than T.  For:
   (X << CNT1) OP (X >> CNT2)				iff CNT1 + CNT2 == B
   ((T) ((T2) X << CNT1)) OP ((T) ((T2) X >> CNT2))	iff CNT1 + CNT2 == B

   transform these into:
   X r<< CNT1

   Or for:
   (X << Y) OP (X >> (B - Y))
   (X << (int) Y) OP (X >> (int) (B - Y))
   ((T) ((T2) X << Y)) OP ((T) ((T2) X >> (B - Y)))
   ((T) ((T2) X << (int) Y)) OP ((T) ((T2) X >> (int) (B - Y)))
   (X << Y) | (X >> ((-Y) & (B - 1)))
   (X << (int) Y) | (X >> (int) ((-Y) & (B - 1)))
   ((T) ((T2) X << Y)) | ((T) ((T2) X >> ((-Y) & (B - 1))))
   ((T) ((T2) X << (int) Y)) | ((T) ((T2) X >> (int) ((-Y) & (B - 1))))

   transform these into:
   X r<< Y

   Or for:
   (X << (Y & (B - 1))) | (X >> ((-Y) & (B - 1)))
   (X << (int) (Y & (B - 1))) | (X >> (int) ((-Y) & (B - 1)))
   ((T) ((T2) X << (Y & (B - 1)))) | ((T) ((T2) X >> ((-Y) & (B - 1))))
   ((T) ((T2) X << (int) (Y & (B - 1)))) \
     | ((T) ((T2) X >> (int) ((-Y) & (B - 1))))

   transform these into:
   X r<< (Y & (B - 1))

   Note, in the patterns with T2 type, the type of OP operands
   might be even a signed type, but should have precision B.
   Expressions with & (B - 1) should be recognized only if B is
   a power of 2.  */

static bool
simplify_rotate (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree arg[2], rtype, rotcnt = NULL_TREE;
  tree def_arg1[2], def_arg2[2];
  enum tree_code def_code[2];
  tree lhs;
  int i;
  bool swapped_p = false;
  gimple *g;

  arg[0] = gimple_assign_rhs1 (stmt);
  arg[1] = gimple_assign_rhs2 (stmt);
  rtype = TREE_TYPE (arg[0]);

  /* Only create rotates in complete modes.  Other cases are not
     expanded properly.  */
  if (!INTEGRAL_TYPE_P (rtype)
      || !type_has_mode_precision_p (rtype))
    return false;

  for (i = 0; i < 2; i++)
    defcodefor_name (arg[i], &def_code[i], &def_arg1[i], &def_arg2[i]);

  /* Look through narrowing conversions.  */
  if (CONVERT_EXPR_CODE_P (def_code[0])
      && CONVERT_EXPR_CODE_P (def_code[1])
      && INTEGRAL_TYPE_P (TREE_TYPE (def_arg1[0]))
      && INTEGRAL_TYPE_P (TREE_TYPE (def_arg1[1]))
      && TYPE_PRECISION (TREE_TYPE (def_arg1[0]))
	 == TYPE_PRECISION (TREE_TYPE (def_arg1[1]))
      && TYPE_PRECISION (TREE_TYPE (def_arg1[0])) > TYPE_PRECISION (rtype)
      && has_single_use (arg[0])
      && has_single_use (arg[1]))
    {
      for (i = 0; i < 2; i++)
	{
	  arg[i] = def_arg1[i];
	  defcodefor_name (arg[i], &def_code[i], &def_arg1[i], &def_arg2[i]);
	}
    }

  /* One operand has to be LSHIFT_EXPR and one RSHIFT_EXPR.  */
  for (i = 0; i < 2; i++)
    if (def_code[i] != LSHIFT_EXPR && def_code[i] != RSHIFT_EXPR)
      return false;
    else if (!has_single_use (arg[i]))
      return false;
  if (def_code[0] == def_code[1])
    return false;

  /* If we've looked through narrowing conversions before, look through
     widening conversions from unsigned type with the same precision
     as rtype here.  */
  if (TYPE_PRECISION (TREE_TYPE (def_arg1[0])) != TYPE_PRECISION (rtype))
    for (i = 0; i < 2; i++)
      {
	tree tem;
	enum tree_code code;
	defcodefor_name (def_arg1[i], &code, &tem, NULL);
	if (!CONVERT_EXPR_CODE_P (code)
	    || !INTEGRAL_TYPE_P (TREE_TYPE (tem))
	    || TYPE_PRECISION (TREE_TYPE (tem)) != TYPE_PRECISION (rtype))
	  return false;
	def_arg1[i] = tem;
      }
  /* Both shifts have to use the same first operand.  */
  if (!operand_equal_for_phi_arg_p (def_arg1[0], def_arg1[1])
      || !types_compatible_p (TREE_TYPE (def_arg1[0]),
			      TREE_TYPE (def_arg1[1])))
    return false;
  if (!TYPE_UNSIGNED (TREE_TYPE (def_arg1[0])))
    return false;

  /* CNT1 + CNT2 == B case above.  */
  if (tree_fits_uhwi_p (def_arg2[0])
      && tree_fits_uhwi_p (def_arg2[1])
      && tree_to_uhwi (def_arg2[0])
	 + tree_to_uhwi (def_arg2[1]) == TYPE_PRECISION (rtype))
    rotcnt = def_arg2[0];
  else if (TREE_CODE (def_arg2[0]) != SSA_NAME
	   || TREE_CODE (def_arg2[1]) != SSA_NAME)
    return false;
  else
    {
      tree cdef_arg1[2], cdef_arg2[2], def_arg2_alt[2];
      enum tree_code cdef_code[2];
      /* Look through conversion of the shift count argument.
	 The C/C++ FE cast any shift count argument to integer_type_node.
	 The only problem might be if the shift count type maximum value
	 is equal or smaller than number of bits in rtype.  */
      for (i = 0; i < 2; i++)
	{
	  def_arg2_alt[i] = def_arg2[i];
	  defcodefor_name (def_arg2[i], &cdef_code[i],
			   &cdef_arg1[i], &cdef_arg2[i]);
	  if (CONVERT_EXPR_CODE_P (cdef_code[i])
	      && INTEGRAL_TYPE_P (TREE_TYPE (cdef_arg1[i]))
	      && TYPE_PRECISION (TREE_TYPE (cdef_arg1[i]))
		 > floor_log2 (TYPE_PRECISION (rtype))
	      && type_has_mode_precision_p (TREE_TYPE (cdef_arg1[i])))
	    {
	      def_arg2_alt[i] = cdef_arg1[i];
	      defcodefor_name (def_arg2_alt[i], &cdef_code[i],
			       &cdef_arg1[i], &cdef_arg2[i]);
	    }
	}
      for (i = 0; i < 2; i++)
	/* Check for one shift count being Y and the other B - Y,
	   with optional casts.  */
	if (cdef_code[i] == MINUS_EXPR
	    && tree_fits_shwi_p (cdef_arg1[i])
	    && tree_to_shwi (cdef_arg1[i]) == TYPE_PRECISION (rtype)
	    && TREE_CODE (cdef_arg2[i]) == SSA_NAME)
	  {
	    tree tem;
	    enum tree_code code;

	    if (cdef_arg2[i] == def_arg2[1 - i]
		|| cdef_arg2[i] == def_arg2_alt[1 - i])
	      {
		rotcnt = cdef_arg2[i];
		break;
	      }
	    defcodefor_name (cdef_arg2[i], &code, &tem, NULL);
	    if (CONVERT_EXPR_CODE_P (code)
		&& INTEGRAL_TYPE_P (TREE_TYPE (tem))
		&& TYPE_PRECISION (TREE_TYPE (tem))
		 > floor_log2 (TYPE_PRECISION (rtype))
		&& type_has_mode_precision_p (TREE_TYPE (tem))
		&& (tem == def_arg2[1 - i]
		    || tem == def_arg2_alt[1 - i]))
	      {
		rotcnt = tem;
		break;
	      }
	  }
	/* The above sequence isn't safe for Y being 0,
	   because then one of the shifts triggers undefined behavior.
	   This alternative is safe even for rotation count of 0.
	   One shift count is Y and the other (-Y) & (B - 1).
	   Or one shift count is Y & (B - 1) and the other (-Y) & (B - 1).  */
	else if (cdef_code[i] == BIT_AND_EXPR
		 && pow2p_hwi (TYPE_PRECISION (rtype))
		 && tree_fits_shwi_p (cdef_arg2[i])
		 && tree_to_shwi (cdef_arg2[i])
		    == TYPE_PRECISION (rtype) - 1
		 && TREE_CODE (cdef_arg1[i]) == SSA_NAME
		 && gimple_assign_rhs_code (stmt) == BIT_IOR_EXPR)
	  {
	    tree tem;
	    enum tree_code code;

	    defcodefor_name (cdef_arg1[i], &code, &tem, NULL);
	    if (CONVERT_EXPR_CODE_P (code)
		&& INTEGRAL_TYPE_P (TREE_TYPE (tem))
		&& TYPE_PRECISION (TREE_TYPE (tem))
		 > floor_log2 (TYPE_PRECISION (rtype))
		&& type_has_mode_precision_p (TREE_TYPE (tem)))
	      defcodefor_name (tem, &code, &tem, NULL);

	    if (code == NEGATE_EXPR)
	      {
		if (tem == def_arg2[1 - i] || tem == def_arg2_alt[1 - i])
		  {
		    rotcnt = tem;
		    break;
		  }
		tree tem2;
		defcodefor_name (tem, &code, &tem2, NULL);
		if (CONVERT_EXPR_CODE_P (code)
		    && INTEGRAL_TYPE_P (TREE_TYPE (tem2))
		    && TYPE_PRECISION (TREE_TYPE (tem2))
		       > floor_log2 (TYPE_PRECISION (rtype))
		    && type_has_mode_precision_p (TREE_TYPE (tem2)))
		  {
		    if (tem2 == def_arg2[1 - i]
			|| tem2 == def_arg2_alt[1 - i])
		      {
			rotcnt = tem2;
			break;
		      }
		  }
		else
		  tem2 = NULL_TREE;

		if (cdef_code[1 - i] == BIT_AND_EXPR
		    && tree_fits_shwi_p (cdef_arg2[1 - i])
		    && tree_to_shwi (cdef_arg2[1 - i])
		       == TYPE_PRECISION (rtype) - 1
		    && TREE_CODE (cdef_arg1[1 - i]) == SSA_NAME)
		  {
		    if (tem == cdef_arg1[1 - i]
			|| tem2 == cdef_arg1[1 - i])
		      {
			rotcnt = def_arg2[1 - i];
			break;
		      }
		    tree tem3;
		    defcodefor_name (cdef_arg1[1 - i], &code, &tem3, NULL);
		    if (CONVERT_EXPR_CODE_P (code)
			&& INTEGRAL_TYPE_P (TREE_TYPE (tem3))
			&& TYPE_PRECISION (TREE_TYPE (tem3))
			   > floor_log2 (TYPE_PRECISION (rtype))
			&& type_has_mode_precision_p (TREE_TYPE (tem3)))
		      {
			if (tem == tem3 || tem2 == tem3)
			  {
			    rotcnt = def_arg2[1 - i];
			    break;
			  }
		      }
		  }
	      }
	  }
      if (rotcnt == NULL_TREE)
	return false;
      swapped_p = i != 1;
    }

  if (!useless_type_conversion_p (TREE_TYPE (def_arg2[0]),
				  TREE_TYPE (rotcnt)))
    {
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (def_arg2[0])),
			       NOP_EXPR, rotcnt);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      rotcnt = gimple_assign_lhs (g);
    }
  lhs = gimple_assign_lhs (stmt);
  if (!useless_type_conversion_p (rtype, TREE_TYPE (def_arg1[0])))
    lhs = make_ssa_name (TREE_TYPE (def_arg1[0]));
  g = gimple_build_assign (lhs,
			   ((def_code[0] == LSHIFT_EXPR) ^ swapped_p)
			   ? LROTATE_EXPR : RROTATE_EXPR, def_arg1[0], rotcnt);
  if (!useless_type_conversion_p (rtype, TREE_TYPE (def_arg1[0])))
    {
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (gimple_assign_lhs (stmt), NOP_EXPR, lhs);
    }
  gsi_replace (gsi, g, false);
  return true;
}

/* Combine an element access with a shuffle.  Returns true if there were
   any changes made, else it returns false.  */
 
static bool
simplify_bitfield_ref (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  gimple *def_stmt;
  tree op, op0, op1, op2;
  tree elem_type;
  unsigned idx, size;
  enum tree_code code;

  op = gimple_assign_rhs1 (stmt);
  gcc_checking_assert (TREE_CODE (op) == BIT_FIELD_REF);

  op0 = TREE_OPERAND (op, 0);
  if (TREE_CODE (op0) != SSA_NAME
      || TREE_CODE (TREE_TYPE (op0)) != VECTOR_TYPE)
    return false;

  def_stmt = get_prop_source_stmt (op0, false, NULL);
  if (!def_stmt || !can_propagate_from (def_stmt))
    return false;

  op1 = TREE_OPERAND (op, 1);
  op2 = TREE_OPERAND (op, 2);
  code = gimple_assign_rhs_code (def_stmt);

  if (code == CONSTRUCTOR)
    {
      tree tem = fold_ternary (BIT_FIELD_REF, TREE_TYPE (op),
			       gimple_assign_rhs1 (def_stmt), op1, op2);
      if (!tem || !valid_gimple_rhs_p (tem))
	return false;
      gimple_assign_set_rhs_from_tree (gsi, tem);
      update_stmt (gsi_stmt (*gsi));
      return true;
    }

  elem_type = TREE_TYPE (TREE_TYPE (op0));
  if (TREE_TYPE (op) != elem_type)
    return false;

  size = TREE_INT_CST_LOW (TYPE_SIZE (elem_type));
  if (maybe_ne (bit_field_size (op), size))
    return false;

  if (code == VEC_PERM_EXPR
      && constant_multiple_p (bit_field_offset (op), size, &idx))
    {
      tree p, m, tem;
      unsigned HOST_WIDE_INT nelts;
      m = gimple_assign_rhs3 (def_stmt);
      if (TREE_CODE (m) != VECTOR_CST
	  || !VECTOR_CST_NELTS (m).is_constant (&nelts))
	return false;
      idx = TREE_INT_CST_LOW (VECTOR_CST_ELT (m, idx));
      idx %= 2 * nelts;
      if (idx < nelts)
	{
	  p = gimple_assign_rhs1 (def_stmt);
	}
      else
	{
	  p = gimple_assign_rhs2 (def_stmt);
	  idx -= nelts;
	}
      tem = build3 (BIT_FIELD_REF, TREE_TYPE (op),
		    unshare_expr (p), op1, bitsize_int (idx * size));
      gimple_assign_set_rhs1 (stmt, tem);
      fold_stmt (gsi);
      update_stmt (gsi_stmt (*gsi));
      return true;
    }

  return false;
}

/* Determine whether applying the 2 permutations (mask1 then mask2)
   gives back one of the input.  */

static int
is_combined_permutation_identity (tree mask1, tree mask2)
{
  tree mask;
  unsigned HOST_WIDE_INT nelts, i, j;
  bool maybe_identity1 = true;
  bool maybe_identity2 = true;

  gcc_checking_assert (TREE_CODE (mask1) == VECTOR_CST
		       && TREE_CODE (mask2) == VECTOR_CST);
  mask = fold_ternary (VEC_PERM_EXPR, TREE_TYPE (mask1), mask1, mask1, mask2);
  if (mask == NULL_TREE || TREE_CODE (mask) != VECTOR_CST)
    return 0;

  if (!VECTOR_CST_NELTS (mask).is_constant (&nelts))
    return 0;
  for (i = 0; i < nelts; i++)
    {
      tree val = VECTOR_CST_ELT (mask, i);
      gcc_assert (TREE_CODE (val) == INTEGER_CST);
      j = TREE_INT_CST_LOW (val) & (2 * nelts - 1);
      if (j == i)
	maybe_identity2 = false;
      else if (j == i + nelts)
	maybe_identity1 = false;
      else
	return 0;
    }
  return maybe_identity1 ? 1 : maybe_identity2 ? 2 : 0;
}

/* Combine a shuffle with its arguments.  Returns 1 if there were any
   changes made, 2 if cfg-cleanup needs to run.  Else it returns 0.  */
 
static int
simplify_permutation (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  gimple *def_stmt;
  tree op0, op1, op2, op3, arg0, arg1;
  enum tree_code code;
  bool single_use_op0 = false;

  gcc_checking_assert (gimple_assign_rhs_code (stmt) == VEC_PERM_EXPR);

  op0 = gimple_assign_rhs1 (stmt);
  op1 = gimple_assign_rhs2 (stmt);
  op2 = gimple_assign_rhs3 (stmt);

  if (TREE_CODE (op2) != VECTOR_CST)
    return 0;

  if (TREE_CODE (op0) == VECTOR_CST)
    {
      code = VECTOR_CST;
      arg0 = op0;
    }
  else if (TREE_CODE (op0) == SSA_NAME)
    {
      def_stmt = get_prop_source_stmt (op0, false, &single_use_op0);
      if (!def_stmt || !can_propagate_from (def_stmt))
	return 0;

      code = gimple_assign_rhs_code (def_stmt);
      arg0 = gimple_assign_rhs1 (def_stmt);
    }
  else
    return 0;

  /* Two consecutive shuffles.  */
  if (code == VEC_PERM_EXPR)
    {
      tree orig;
      int ident;

      if (op0 != op1)
	return 0;
      op3 = gimple_assign_rhs3 (def_stmt);
      if (TREE_CODE (op3) != VECTOR_CST)
	return 0;
      ident = is_combined_permutation_identity (op3, op2);
      if (!ident)
	return 0;
      orig = (ident == 1) ? gimple_assign_rhs1 (def_stmt)
			  : gimple_assign_rhs2 (def_stmt);
      gimple_assign_set_rhs1 (stmt, unshare_expr (orig));
      gimple_assign_set_rhs_code (stmt, TREE_CODE (orig));
      gimple_set_num_ops (stmt, 2);
      update_stmt (stmt);
      return remove_prop_source_from_use (op0) ? 2 : 1;
    }

  /* Shuffle of a constructor.  */
  else if (code == CONSTRUCTOR || code == VECTOR_CST)
    {
      tree opt;
      bool ret = false;
      if (op0 != op1)
	{
	  if (TREE_CODE (op0) == SSA_NAME && !single_use_op0)
	    return 0;

	  if (TREE_CODE (op1) == VECTOR_CST)
	    arg1 = op1;
	  else if (TREE_CODE (op1) == SSA_NAME)
	    {
	      enum tree_code code2;

	      gimple *def_stmt2 = get_prop_source_stmt (op1, true, NULL);
	      if (!def_stmt2 || !can_propagate_from (def_stmt2))
		return 0;

	      code2 = gimple_assign_rhs_code (def_stmt2);
	      if (code2 != CONSTRUCTOR && code2 != VECTOR_CST)
		return 0;
	      arg1 = gimple_assign_rhs1 (def_stmt2);
	    }
	  else
	    return 0;
	}
      else
	{
	  /* Already used twice in this statement.  */
	  if (TREE_CODE (op0) == SSA_NAME && num_imm_uses (op0) > 2)
	    return 0;
	  arg1 = arg0;
	}
      opt = fold_ternary (VEC_PERM_EXPR, TREE_TYPE (op0), arg0, arg1, op2);
      if (!opt
	  || (TREE_CODE (opt) != CONSTRUCTOR && TREE_CODE (opt) != VECTOR_CST))
	return 0;
      gimple_assign_set_rhs_from_tree (gsi, opt);
      update_stmt (gsi_stmt (*gsi));
      if (TREE_CODE (op0) == SSA_NAME)
	ret = remove_prop_source_from_use (op0);
      if (op0 != op1 && TREE_CODE (op1) == SSA_NAME)
	ret |= remove_prop_source_from_use (op1);
      return ret ? 2 : 1;
    }

  return 0;
}

/* Recognize a VEC_PERM_EXPR.  Returns true if there were any changes.  */

static bool
simplify_vector_constructor (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  gimple *def_stmt;
  tree op, op2, orig[2], type, elem_type;
  unsigned elem_size, i;
  unsigned HOST_WIDE_INT nelts;
  enum tree_code code, conv_code;
  constructor_elt *elt;
  bool maybe_ident;

  gcc_checking_assert (gimple_assign_rhs_code (stmt) == CONSTRUCTOR);

  op = gimple_assign_rhs1 (stmt);
  type = TREE_TYPE (op);
  gcc_checking_assert (TREE_CODE (type) == VECTOR_TYPE);

  if (!TYPE_VECTOR_SUBPARTS (type).is_constant (&nelts))
    return false;
  elem_type = TREE_TYPE (type);
  elem_size = TREE_INT_CST_LOW (TYPE_SIZE (elem_type));

  vec_perm_builder sel (nelts, nelts, 1);
  orig[0] = NULL;
  orig[1] = NULL;
  conv_code = ERROR_MARK;
  maybe_ident = true;
  FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (op), i, elt)
    {
      tree ref, op1;

      if (i >= nelts)
	return false;

      if (TREE_CODE (elt->value) != SSA_NAME)
	return false;
      def_stmt = get_prop_source_stmt (elt->value, false, NULL);
      if (!def_stmt)
	return false;
      code = gimple_assign_rhs_code (def_stmt);
      if (code == FLOAT_EXPR
	  || code == FIX_TRUNC_EXPR)
	{
	  op1 = gimple_assign_rhs1 (def_stmt);
	  if (conv_code == ERROR_MARK)
	    {
	      if (maybe_ne (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (elt->value))),
			    GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op1)))))
		return false;
	      conv_code = code;
	    }
	  else if (conv_code != code)
	    return false;
	  if (TREE_CODE (op1) != SSA_NAME)
	    return false;
	  def_stmt = SSA_NAME_DEF_STMT (op1);
	  if (! is_gimple_assign (def_stmt))
	    return false;
	  code = gimple_assign_rhs_code (def_stmt);
	}
      if (code != BIT_FIELD_REF)
	return false;
      op1 = gimple_assign_rhs1 (def_stmt);
      ref = TREE_OPERAND (op1, 0);
      unsigned int j;
      for (j = 0; j < 2; ++j)
	{
	  if (!orig[j])
	    {
	      if (TREE_CODE (ref) != SSA_NAME)
		return false;
	      if (! VECTOR_TYPE_P (TREE_TYPE (ref))
		  || ! useless_type_conversion_p (TREE_TYPE (op1),
						  TREE_TYPE (TREE_TYPE (ref))))
		return false;
	      if (j && !useless_type_conversion_p (TREE_TYPE (orig[0]),
						   TREE_TYPE (ref)))
		return false;
	      orig[j] = ref;
	      break;
	    }
	  else if (ref == orig[j])
	    break;
	}
      if (j == 2)
	return false;

      unsigned int elt;
      if (maybe_ne (bit_field_size (op1), elem_size)
	  || !constant_multiple_p (bit_field_offset (op1), elem_size, &elt))
	return false;
      if (j)
	elt += nelts;
      if (elt != i)
	maybe_ident = false;
      sel.quick_push (elt);
    }
  if (i < nelts)
    return false;

  if (! VECTOR_TYPE_P (TREE_TYPE (orig[0]))
      || maybe_ne (TYPE_VECTOR_SUBPARTS (type),
		   TYPE_VECTOR_SUBPARTS (TREE_TYPE (orig[0]))))
    return false;

  tree tem;
  if (conv_code != ERROR_MARK
      && (! supportable_convert_operation (conv_code, type,
					   TREE_TYPE (orig[0]),
					   &tem, &conv_code)
	  || conv_code == CALL_EXPR))
    return false;

  if (maybe_ident)
    {
      if (conv_code == ERROR_MARK)
	gimple_assign_set_rhs_from_tree (gsi, orig[0]);
      else
	gimple_assign_set_rhs_with_ops (gsi, conv_code, orig[0],
					NULL_TREE, NULL_TREE);
    }
  else
    {
      tree mask_type;

      vec_perm_indices indices (sel, orig[1] ? 2 : 1, nelts);
      if (!can_vec_perm_const_p (TYPE_MODE (type), indices))
	return false;
      mask_type
	= build_vector_type (build_nonstandard_integer_type (elem_size, 1),
			     nelts);
      if (GET_MODE_CLASS (TYPE_MODE (mask_type)) != MODE_VECTOR_INT
	  || maybe_ne (GET_MODE_SIZE (TYPE_MODE (mask_type)),
		       GET_MODE_SIZE (TYPE_MODE (type))))
	return false;
      op2 = vec_perm_indices_to_tree (mask_type, indices);
      if (!orig[1])
	orig[1] = orig[0];
      if (conv_code == ERROR_MARK)
	gimple_assign_set_rhs_with_ops (gsi, VEC_PERM_EXPR, orig[0],
					orig[1], op2);
      else
	{
	  gimple *perm
	    = gimple_build_assign (make_ssa_name (TREE_TYPE (orig[0])),
				   VEC_PERM_EXPR, orig[0], orig[1], op2);
	  orig[0] = gimple_assign_lhs (perm);
	  gsi_insert_before (gsi, perm, GSI_SAME_STMT);
	  gimple_assign_set_rhs_with_ops (gsi, conv_code, orig[0],
					  NULL_TREE, NULL_TREE);
	}
    }
  update_stmt (gsi_stmt (*gsi));
  return true;
}


/* Primitive "lattice" function for gimple_simplify.  */

static tree
fwprop_ssa_val (tree name)
{
  /* First valueize NAME.  */
  if (TREE_CODE (name) == SSA_NAME
      && SSA_NAME_VERSION (name) < lattice.length ())
    {
      tree val = lattice[SSA_NAME_VERSION (name)];
      if (val)
	name = val;
    }
  /* We continue matching along SSA use-def edges for SSA names
     that are not single-use.  Currently there are no patterns
     that would cause any issues with that.  */
  return name;
}

/* Main entry point for the forward propagation and statement combine
   optimizer.  */

namespace {

const pass_data pass_data_forwprop =
{
  GIMPLE_PASS, /* type */
  "forwprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_FORWPROP, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_forwprop : public gimple_opt_pass
{
public:
  pass_forwprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_forwprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_forwprop (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_forwprop; }
  virtual unsigned int execute (function *);

}; // class pass_forwprop

unsigned int
pass_forwprop::execute (function *fun)
{
  unsigned int todoflags = 0;

  cfg_changed = false;

  /* Combine stmts with the stmts defining their operands.  Do that
     in an order that guarantees visiting SSA defs before SSA uses.  */
  lattice.create (num_ssa_names);
  lattice.quick_grow_cleared (num_ssa_names);
  int *postorder = XNEWVEC (int, n_basic_blocks_for_fn (fun));
  int postorder_num = pre_and_rev_post_order_compute_fn (cfun, NULL,
							 postorder, false);
  auto_vec<gimple *, 4> to_fixup;
  to_purge = BITMAP_ALLOC (NULL);
  for (int i = 0; i < postorder_num; ++i)
    {
      gimple_stmt_iterator gsi;
      basic_block bb = BASIC_BLOCK_FOR_FN (fun, postorder[i]);

      /* Propagate into PHIs and record degenerate ones in the lattice.  */
      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gphi *phi = si.phi ();
	  tree res = gimple_phi_result (phi);
	  if (virtual_operand_p (res))
	    continue;

	  use_operand_p use_p;
	  ssa_op_iter it;
	  tree first = NULL_TREE;
	  bool all_same = true;
	  FOR_EACH_PHI_ARG (use_p, phi, it, SSA_OP_USE)
	    {
	      tree use = USE_FROM_PTR (use_p);
	      tree tem = fwprop_ssa_val (use);
	      if (! first)
		first = tem;
	      else if (! operand_equal_p (first, tem, 0))
		all_same = false;
	      if (tem != use
		  && may_propagate_copy (use, tem))
		propagate_value (use_p, tem);
	    }
	  if (all_same)
	    fwprop_set_lattice_val (res, first);
	}

      /* Apply forward propagation to all stmts in the basic-block.
	 Note we update GSI within the loop as necessary.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree lhs, rhs;
	  enum tree_code code;

	  if (!is_gimple_assign (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  lhs = gimple_assign_lhs (stmt);
	  rhs = gimple_assign_rhs1 (stmt);
	  code = gimple_assign_rhs_code (stmt);
	  if (TREE_CODE (lhs) != SSA_NAME
	      || has_zero_uses (lhs))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  /* If this statement sets an SSA_NAME to an address,
	     try to propagate the address into the uses of the SSA_NAME.  */
	  if (code == ADDR_EXPR
	      /* Handle pointer conversions on invariant addresses
		 as well, as this is valid gimple.  */
	      || (CONVERT_EXPR_CODE_P (code)
		  && TREE_CODE (rhs) == ADDR_EXPR
		  && POINTER_TYPE_P (TREE_TYPE (lhs))))
	    {
	      tree base = get_base_address (TREE_OPERAND (rhs, 0));
	      if ((!base
		   || !DECL_P (base)
		   || decl_address_invariant_p (base))
		  && !stmt_references_abnormal_ssa_name (stmt)
		  && forward_propagate_addr_expr (lhs, rhs, true))
		{
		  fwprop_invalidate_lattice (gimple_get_lhs (stmt));
		  release_defs (stmt);
		  gsi_remove (&gsi, true);
		}
	      else
		gsi_next (&gsi);
	    }
	  else if (code == POINTER_PLUS_EXPR)
	    {
	      tree off = gimple_assign_rhs2 (stmt);
	      if (TREE_CODE (off) == INTEGER_CST
		  && can_propagate_from (stmt)
		  && !simple_iv_increment_p (stmt)
		  /* ???  Better adjust the interface to that function
		     instead of building new trees here.  */
		  && forward_propagate_addr_expr
		       (lhs,
			build1_loc (gimple_location (stmt),
				    ADDR_EXPR, TREE_TYPE (rhs),
				    fold_build2 (MEM_REF,
						 TREE_TYPE (TREE_TYPE (rhs)),
						 rhs,
						 fold_convert (ptr_type_node,
							       off))), true))
		{
		  fwprop_invalidate_lattice (gimple_get_lhs (stmt));
		  release_defs (stmt);
		  gsi_remove (&gsi, true);
		}
	      else if (is_gimple_min_invariant (rhs))
		{
		  /* Make sure to fold &a[0] + off_1 here.  */
		  fold_stmt_inplace (&gsi);
		  update_stmt (stmt);
		  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
		    gsi_next (&gsi);
		}
	      else
		gsi_next (&gsi);
	    }
	  else if (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE
		   && gimple_assign_load_p (stmt)
		   && !gimple_has_volatile_ops (stmt)
		   && (TREE_CODE (gimple_assign_rhs1 (stmt))
		       != TARGET_MEM_REF)
		   && !stmt_can_throw_internal (stmt))
	    {
	      /* Rewrite loads used only in real/imagpart extractions to
	         component-wise loads.  */
	      use_operand_p use_p;
	      imm_use_iterator iter;
	      bool rewrite = true;
	      FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
		{
		  gimple *use_stmt = USE_STMT (use_p);
		  if (is_gimple_debug (use_stmt))
		    continue;
		  if (!is_gimple_assign (use_stmt)
		      || (gimple_assign_rhs_code (use_stmt) != REALPART_EXPR
			  && gimple_assign_rhs_code (use_stmt) != IMAGPART_EXPR))
		    {
		      rewrite = false;
		      break;
		    }
		}
	      if (rewrite)
		{
		  gimple *use_stmt;
		  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
		    {
		      if (is_gimple_debug (use_stmt))
			{
			  if (gimple_debug_bind_p (use_stmt))
			    {
			      gimple_debug_bind_reset_value (use_stmt);
			      update_stmt (use_stmt);
			    }
			  continue;
			}

		      tree new_rhs = build1 (gimple_assign_rhs_code (use_stmt),
					     TREE_TYPE (TREE_TYPE (rhs)),
					     unshare_expr (rhs));
		      gimple *new_stmt
			= gimple_build_assign (gimple_assign_lhs (use_stmt),
					       new_rhs);

		      location_t loc = gimple_location (use_stmt);
		      gimple_set_location (new_stmt, loc);
		      gimple_stmt_iterator gsi2 = gsi_for_stmt (use_stmt);
		      unlink_stmt_vdef (use_stmt);
		      gsi_remove (&gsi2, true);

		      gsi_insert_before (&gsi, new_stmt, GSI_SAME_STMT);
		    }

		  release_defs (stmt);
		  gsi_remove (&gsi, true);
		}
	      else
		gsi_next (&gsi);
	    }
	  else if (code == COMPLEX_EXPR)
	    {
	      /* Rewrite stores of a single-use complex build expression
	         to component-wise stores.  */
	      use_operand_p use_p;
	      gimple *use_stmt;
	      if (single_imm_use (lhs, &use_p, &use_stmt)
		  && gimple_store_p (use_stmt)
		  && !gimple_has_volatile_ops (use_stmt)
		  && is_gimple_assign (use_stmt)
		  && (TREE_CODE (gimple_assign_lhs (use_stmt))
		      != TARGET_MEM_REF))
		{
		  tree use_lhs = gimple_assign_lhs (use_stmt);
		  tree new_lhs = build1 (REALPART_EXPR,
					 TREE_TYPE (TREE_TYPE (use_lhs)),
					 unshare_expr (use_lhs));
		  gimple *new_stmt = gimple_build_assign (new_lhs, rhs);
		  location_t loc = gimple_location (use_stmt);
		  gimple_set_location (new_stmt, loc);
		  gimple_set_vuse (new_stmt, gimple_vuse (use_stmt));
		  gimple_set_vdef (new_stmt, make_ssa_name (gimple_vop (cfun)));
		  SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
		  gimple_set_vuse (use_stmt, gimple_vdef (new_stmt));
		  gimple_stmt_iterator gsi2 = gsi_for_stmt (use_stmt);
		  gsi_insert_before (&gsi2, new_stmt, GSI_SAME_STMT);

		  new_lhs = build1 (IMAGPART_EXPR,
				    TREE_TYPE (TREE_TYPE (use_lhs)),
				    unshare_expr (use_lhs));
		  gimple_assign_set_lhs (use_stmt, new_lhs);
		  gimple_assign_set_rhs1 (use_stmt, gimple_assign_rhs2 (stmt));
		  update_stmt (use_stmt);

		  release_defs (stmt);
		  gsi_remove (&gsi, true);
		}
	      else
		gsi_next (&gsi);
	    }
	  else
	    gsi_next (&gsi);
	}

      /* Combine stmts with the stmts defining their operands.
	 Note we update GSI within the loop as necessary.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple *orig_stmt = stmt;
	  bool changed = false;
	  bool was_noreturn = (is_gimple_call (stmt)
			       && gimple_call_noreturn_p (stmt));

	  /* Mark stmt as potentially needing revisiting.  */
	  gimple_set_plf (stmt, GF_PLF_1, false);

	  if (fold_stmt (&gsi, fwprop_ssa_val))
	    {
	      changed = true;
	      stmt = gsi_stmt (gsi);
	      if (maybe_clean_or_replace_eh_stmt (orig_stmt, stmt))
		bitmap_set_bit (to_purge, bb->index);
	      if (!was_noreturn
		  && is_gimple_call (stmt) && gimple_call_noreturn_p (stmt))
		to_fixup.safe_push (stmt);
	      /* Cleanup the CFG if we simplified a condition to
	         true or false.  */
	      if (gcond *cond = dyn_cast <gcond *> (stmt))
		if (gimple_cond_true_p (cond)
		    || gimple_cond_false_p (cond))
		  cfg_changed = true;
	      update_stmt (stmt);
	    }

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_ASSIGN:
	      {
		tree rhs1 = gimple_assign_rhs1 (stmt);
		enum tree_code code = gimple_assign_rhs_code (stmt);

		if (code == COND_EXPR
		    || code == VEC_COND_EXPR)
		  {
		    /* In this case the entire COND_EXPR is in rhs1. */
		    if (forward_propagate_into_cond (&gsi))
		      {
			changed = true;
			stmt = gsi_stmt (gsi);
		      }
		  }
		else if (TREE_CODE_CLASS (code) == tcc_comparison)
		  {
		    int did_something;
		    did_something = forward_propagate_into_comparison (&gsi);
		    if (did_something == 2)
		      cfg_changed = true;
		    changed = did_something != 0;
		  }
		else if ((code == PLUS_EXPR
			  || code == BIT_IOR_EXPR
			  || code == BIT_XOR_EXPR)
			 && simplify_rotate (&gsi))
		  changed = true;
		else if (code == VEC_PERM_EXPR)
		  {
		    int did_something = simplify_permutation (&gsi);
		    if (did_something == 2)
		      cfg_changed = true;
		    changed = did_something != 0;
		  }
		else if (code == BIT_FIELD_REF)
		  changed = simplify_bitfield_ref (&gsi);
                else if (code == CONSTRUCTOR
                         && TREE_CODE (TREE_TYPE (rhs1)) == VECTOR_TYPE)
                  changed = simplify_vector_constructor (&gsi);
		break;
	      }

	    case GIMPLE_SWITCH:
	      changed = simplify_gimple_switch (as_a <gswitch *> (stmt));
	      break;

	    case GIMPLE_COND:
	      {
		int did_something
		  = forward_propagate_into_gimple_cond (as_a <gcond *> (stmt));
		if (did_something == 2)
		  cfg_changed = true;
		changed = did_something != 0;
		break;
	      }

	    case GIMPLE_CALL:
	      {
		tree callee = gimple_call_fndecl (stmt);
		if (callee != NULL_TREE
		    && fndecl_built_in_p (callee, BUILT_IN_NORMAL))
		  changed = simplify_builtin_call (&gsi, callee);
		break;
	      }

	    default:;
	    }

	  if (changed)
	    {
	      /* If the stmt changed then re-visit it and the statements
		 inserted before it.  */
	      for (; !gsi_end_p (gsi); gsi_prev (&gsi))
		if (gimple_plf (gsi_stmt (gsi), GF_PLF_1))
		  break;
	      if (gsi_end_p (gsi))
		gsi = gsi_start_bb (bb);
	      else
		gsi_next (&gsi);
	    }
	  else
	    {
	      /* Stmt no longer needs to be revisited.  */
	      gimple_set_plf (stmt, GF_PLF_1, true);

	      /* Fill up the lattice.  */
	      if (gimple_assign_single_p (stmt))
		{
		  tree lhs = gimple_assign_lhs (stmt);
		  tree rhs = gimple_assign_rhs1 (stmt);
		  if (TREE_CODE (lhs) == SSA_NAME)
		    {
		      tree val = lhs;
		      if (TREE_CODE (rhs) == SSA_NAME)
			val = fwprop_ssa_val (rhs);
		      else if (is_gimple_min_invariant (rhs))
			val = rhs;
		      fwprop_set_lattice_val (lhs, val);
		    }
		}

	      gsi_next (&gsi);
	    }
	}
    }
  free (postorder);
  lattice.release ();

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the walk.  Do this
     in reverse order so we don't inadvertedly remove a stmt we want to
     fixup by visiting a dominating now noreturn call first.  */
  while (!to_fixup.is_empty ())
    {
      gimple *stmt = to_fixup.pop ();
      if (dump_file && dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Fixing up noreturn call ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      cfg_changed |= fixup_noreturn_call (stmt);
    }

  cfg_changed |= gimple_purge_all_dead_eh_edges (to_purge);
  BITMAP_FREE (to_purge);

  if (cfg_changed)
    todoflags |= TODO_cleanup_cfg;

  return todoflags;
}

} // anon namespace

gimple_opt_pass *
make_pass_forwprop (gcc::context *ctxt)
{
  return new pass_forwprop (ctxt);
}
