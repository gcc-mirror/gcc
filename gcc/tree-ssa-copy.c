/* Copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "output.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "cfgloop.h"

/* This file implements the copy propagation pass and provides a
   handful of interfaces for performing const/copy propagation and
   simple expression replacement which keep variable annotations
   up-to-date.

   We require that for any copy operation where the RHS and LHS have
   a non-null memory tag the memory tag be the same.   It is OK
   for one or both of the memory tags to be NULL.

   We also require tracking if a variable is dereferenced in a load or
   store operation.

   We enforce these requirements by having all copy propagation and
   replacements of one SSA_NAME with a different SSA_NAME to use the
   APIs defined in this file.  */

/* Return true if we may propagate ORIG into DEST, false otherwise.  */

bool
may_propagate_copy (tree dest, tree orig)
{
  tree type_d = TREE_TYPE (dest);
  tree type_o = TREE_TYPE (orig);

  /* If ORIG flows in from an abnormal edge, it cannot be propagated.  */
  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  /* If DEST is an SSA_NAME that flows from an abnormal edge, then it
     cannot be replaced.  */
  if (TREE_CODE (dest) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (dest))
    return false;
  
  /* Do not copy between types for which we *do* need a conversion.  */
  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  /* Propagating virtual operands is always ok.  */
  if (TREE_CODE (dest) == SSA_NAME && !is_gimple_reg (dest))
    {
      /* But only between virtual operands.  */
      gcc_assert (TREE_CODE (orig) == SSA_NAME && !is_gimple_reg (orig));

      return true;
    }

  /* Anything else is OK.  */
  return true;
}

/* Like may_propagate_copy, but use as the destination expression
   the principal expression (typically, the RHS) contained in
   statement DEST.  This is more efficient when working with the
   gimple tuples representation.  */

bool
may_propagate_copy_into_stmt (gimple dest, tree orig)
{
  tree type_d;
  tree type_o;

  /* If the statement is a switch or a single-rhs assignment,
     then the expression to be replaced by the propagation may
     be an SSA_NAME.  Fortunately, there is an explicit tree
     for the expression, so we delegate to may_propagate_copy.  */

  if (gimple_assign_single_p (dest))
    return may_propagate_copy (gimple_assign_rhs1 (dest), orig);
  else if (gimple_code (dest) == GIMPLE_SWITCH)
    return may_propagate_copy (gimple_switch_index (dest), orig);

  /* In other cases, the expression is not materialized, so there
     is no destination to pass to may_propagate_copy.  On the other
     hand, the expression cannot be an SSA_NAME, so the analysis
     is much simpler.  */

  if (TREE_CODE (orig) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (orig))
    return false;

  if (is_gimple_assign (dest))
    type_d = TREE_TYPE (gimple_assign_lhs (dest));
  else if (gimple_code (dest) == GIMPLE_COND)
    type_d = boolean_type_node;
  else if (is_gimple_call (dest)
           && gimple_call_lhs (dest) != NULL_TREE)
    type_d = TREE_TYPE (gimple_call_lhs (dest));
  else
    gcc_unreachable ();

  type_o = TREE_TYPE (orig);

  if (!useless_type_conversion_p (type_d, type_o))
    return false;

  return true;
}

/* Similarly, but we know that we're propagating into an ASM_EXPR.  */

bool
may_propagate_copy_into_asm (tree dest)
{
  /* Hard register operands of asms are special.  Do not bypass.  */
  return !(TREE_CODE (dest) == SSA_NAME
	   && TREE_CODE (SSA_NAME_VAR (dest)) == VAR_DECL
	   && DECL_HARD_REGISTER (SSA_NAME_VAR (dest)));
}


/* Common code for propagate_value and replace_exp.

   Replace use operand OP_P with VAL.  FOR_PROPAGATION indicates if the
   replacement is done to propagate a value or not.  */

static void
replace_exp_1 (use_operand_p op_p, tree val,
    	       bool for_propagation ATTRIBUTE_UNUSED)
{
#if defined ENABLE_CHECKING
  tree op = USE_FROM_PTR (op_p);
  gcc_assert (!(for_propagation
		&& TREE_CODE (op) == SSA_NAME
		&& TREE_CODE (val) == SSA_NAME
		&& !may_propagate_copy (op, val)));
#endif

  if (TREE_CODE (val) == SSA_NAME)
    SET_USE (op_p, val);
  else
    SET_USE (op_p, unsave_expr_now (val));
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the operand pointed to by OP_P.

   Use this version for const/copy propagation as it will perform additional
   checks to ensure validity of the const/copy propagation.  */

void
propagate_value (use_operand_p op_p, tree val)
{
  replace_exp_1 (op_p, val, true);
}

/* Replace *OP_P with value VAL (assumed to be a constant or another SSA_NAME).

   Use this version when not const/copy propagating values.  For example,
   PRE uses this version when building expressions as they would appear
   in specific blocks taking into account actions of PHI nodes.  */

void
replace_exp (use_operand_p op_p, tree val)
{
  replace_exp_1 (op_p, val, false);
}


/* Propagate the value VAL (assumed to be a constant or another SSA_NAME)
   into the tree pointed to by OP_P.

   Use this version for const/copy propagation when SSA operands are not
   available.  It will perform the additional checks to ensure validity of
   the const/copy propagation, but will not update any operand information.
   Be sure to mark the stmt as modified.  */

void
propagate_tree_value (tree *op_p, tree val)
{
#if defined ENABLE_CHECKING
  gcc_assert (!(TREE_CODE (val) == SSA_NAME
                && *op_p
		&& TREE_CODE (*op_p) == SSA_NAME
		&& !may_propagate_copy (*op_p, val)));
#endif

  if (TREE_CODE (val) == SSA_NAME)
    *op_p = val;
  else
    *op_p = unsave_expr_now (val);
}


/* Like propagate_tree_value, but use as the operand to replace
   the principal expression (typically, the RHS) contained in the
   statement referenced by iterator GSI.  Note that it is not
   always possible to update the statement in-place, so a new
   statement may be created to replace the original.  */

void
propagate_tree_value_into_stmt (gimple_stmt_iterator *gsi, tree val)
{
  gimple stmt = gsi_stmt (*gsi);

  if (is_gimple_assign (stmt))
    {
      tree expr = NULL_TREE;
      if (gimple_assign_single_p (stmt))
        expr = gimple_assign_rhs1 (stmt);
      propagate_tree_value (&expr, val);
      gimple_assign_set_rhs_from_tree (gsi, expr);
      stmt = gsi_stmt (*gsi);
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    {
      tree lhs = NULL_TREE;
      tree rhs = fold_convert (TREE_TYPE (val), integer_zero_node);
      propagate_tree_value (&lhs, val);
      gimple_cond_set_code (stmt, NE_EXPR);
      gimple_cond_set_lhs (stmt, lhs);
      gimple_cond_set_rhs (stmt, rhs);
    }
  else if (is_gimple_call (stmt)
           && gimple_call_lhs (stmt) != NULL_TREE)
    {
      gimple new_stmt;

      tree expr = NULL_TREE;
      propagate_tree_value (&expr, val);
      new_stmt = gimple_build_assign (gimple_call_lhs (stmt), expr);
      move_ssa_defining_stmt_for_defs (new_stmt, stmt);
      gsi_replace (gsi, new_stmt, false);
    }
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    propagate_tree_value (gimple_switch_index_ptr (stmt), val);
  else
    gcc_unreachable ();
}

/*---------------------------------------------------------------------------
				Copy propagation
---------------------------------------------------------------------------*/
/* During propagation, we keep chains of variables that are copies of
   one another.  If variable X_i is a copy of X_j and X_j is a copy of
   X_k, COPY_OF will contain:

   	COPY_OF[i].VALUE = X_j
	COPY_OF[j].VALUE = X_k
	COPY_OF[k].VALUE = X_k

   After propagation, the copy-of value for each variable X_i is
   converted into the final value by walking the copy-of chains and
   updating COPY_OF[i].VALUE to be the last element of the chain.  */
static prop_value_t *copy_of;

/* Used in set_copy_of_val to determine if the last link of a copy-of
   chain has changed.  */
static tree *cached_last_copy_of;


/* Return true if this statement may generate a useful copy.  */

static bool
stmt_may_generate_copy (gimple stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (stmt));

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  /* If the statement has volatile operands, it won't generate a
     useful copy.  */
  if (gimple_has_volatile_ops (stmt))
    return false;

  /* Statements with loads and/or stores will never generate a useful copy.  */
  if (gimple_vuse (stmt))
    return false;

  /* Otherwise, the only statements that generate useful copies are
     assignments whose RHS is just an SSA name that doesn't flow
     through abnormal edges.  */
  return (gimple_assign_rhs_code (stmt) == SSA_NAME
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (stmt)));
}


/* Return the copy-of value for VAR.  */

static inline prop_value_t *
get_copy_of_val (tree var)
{
  prop_value_t *val = &copy_of[SSA_NAME_VERSION (var)];

  if (val->value == NULL_TREE
      && !stmt_may_generate_copy (SSA_NAME_DEF_STMT (var)))
    {
      /* If the variable will never generate a useful copy relation,
	 make it its own copy.  */
      val->value = var;
    }

  return val;
}


/* Return last link in the copy-of chain for VAR.  */

static tree
get_last_copy_of (tree var)
{
  tree last;
  int i;

  /* Traverse COPY_OF starting at VAR until we get to the last
     link in the chain.  Since it is possible to have cycles in PHI
     nodes, the copy-of chain may also contain cycles.
     
     To avoid infinite loops and to avoid traversing lengthy copy-of
     chains, we artificially limit the maximum number of chains we are
     willing to traverse.

     The value 5 was taken from a compiler and runtime library
     bootstrap and a mixture of C and C++ code from various sources.
     More than 82% of all copy-of chains were shorter than 5 links.  */
#define LIMIT	5

  last = var;
  for (i = 0; i < LIMIT; i++)
    {
      tree copy = copy_of[SSA_NAME_VERSION (last)].value;
      if (copy == NULL_TREE || copy == last)
	break;
      last = copy;
    }

  /* If we have reached the limit, then we are either in a copy-of
     cycle or the copy-of chain is too long.  In this case, just
     return VAR so that it is not considered a copy of anything.  */
  return (i < LIMIT ? last : var);
}


/* Set FIRST to be the first variable in the copy-of chain for DEST.
   If DEST's copy-of value or its copy-of chain has changed, return
   true.

   MEM_REF is the memory reference where FIRST is stored.  This is
   used when DEST is a non-register and we are copy propagating loads
   and stores.  */

static inline bool
set_copy_of_val (tree dest, tree first)
{
  unsigned int dest_ver = SSA_NAME_VERSION (dest);
  tree old_first, old_last, new_last;
  
  /* Set FIRST to be the first link in COPY_OF[DEST].  If that
     changed, return true.  */
  old_first = copy_of[dest_ver].value;
  copy_of[dest_ver].value = first;

  if (old_first != first)
    return true;

  /* If FIRST and OLD_FIRST are the same, we need to check whether the
     copy-of chain starting at FIRST ends in a different variable.  If
     the copy-of chain starting at FIRST ends up in a different
     variable than the last cached value we had for DEST, then return
     true because DEST is now a copy of a different variable.

     This test is necessary because even though the first link in the
     copy-of chain may not have changed, if any of the variables in
     the copy-of chain changed its final value, DEST will now be the
     copy of a different variable, so we have to do another round of
     propagation for everything that depends on DEST.  */
  old_last = cached_last_copy_of[dest_ver];
  new_last = get_last_copy_of (dest);
  cached_last_copy_of[dest_ver] = new_last;

  return (old_last != new_last);
}


/* Dump the copy-of value for variable VAR to FILE.  */

static void
dump_copy_of (FILE *file, tree var)
{
  tree val;
  sbitmap visited;

  print_generic_expr (file, var, dump_flags);

  if (TREE_CODE (var) != SSA_NAME)
    return;
    
  visited = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (visited);
  SET_BIT (visited, SSA_NAME_VERSION (var));
  
  fprintf (file, " copy-of chain: ");

  val = var;
  print_generic_expr (file, val, 0);
  fprintf (file, " ");
  while (copy_of[SSA_NAME_VERSION (val)].value)
    {
      fprintf (file, "-> ");
      val = copy_of[SSA_NAME_VERSION (val)].value;
      print_generic_expr (file, val, 0);
      fprintf (file, " ");
      if (TEST_BIT (visited, SSA_NAME_VERSION (val)))
        break;
      SET_BIT (visited, SSA_NAME_VERSION (val));
    }

  val = get_copy_of_val (var)->value;
  if (val == NULL_TREE)
    fprintf (file, "[UNDEFINED]");
  else if (val != var)
    fprintf (file, "[COPY]");
  else
    fprintf (file, "[NOT A COPY]");
  
  sbitmap_free (visited);
}


/* Evaluate the RHS of STMT.  If it produces a valid copy, set the LHS
   value and store the LHS into *RESULT_P.  If STMT generates more
   than one name (i.e., STMT is an aliased store), it is enough to
   store the first name in the VDEF list into *RESULT_P.  After
   all, the names generated will be VUSEd in the same statements.  */

static enum ssa_prop_result
copy_prop_visit_assignment (gimple stmt, tree *result_p)
{
  tree lhs, rhs;
  prop_value_t *rhs_val;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  

  gcc_assert (gimple_assign_rhs_code (stmt) == SSA_NAME);

  rhs_val = get_copy_of_val (rhs);

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      /* Straight copy between two SSA names.  First, make sure that
	 we can propagate the RHS into uses of LHS.  */
      if (!may_propagate_copy (lhs, rhs))
	return SSA_PROP_VARYING;

      /* Notice that in the case of assignments, we make the LHS be a
	 copy of RHS's value, not of RHS itself.  This avoids keeping
	 unnecessary copy-of chains (assignments cannot be in a cycle
	 like PHI nodes), speeding up the propagation process.
	 This is different from what we do in copy_prop_visit_phi_node. 
	 In those cases, we are interested in the copy-of chains.  */
      *result_p = lhs;
      if (set_copy_of_val (*result_p, rhs_val->value))
	return SSA_PROP_INTERESTING;
      else
	return SSA_PROP_NOT_INTERESTING;
    }

  return SSA_PROP_VARYING;
}


/* Visit the GIMPLE_COND STMT.  Return SSA_PROP_INTERESTING
   if it can determine which edge will be taken.  Otherwise, return
   SSA_PROP_VARYING.  */

static enum ssa_prop_result
copy_prop_visit_cond_stmt (gimple stmt, edge *taken_edge_p)
{
  enum ssa_prop_result retval = SSA_PROP_VARYING;
  location_t loc = gimple_location (stmt);

  tree op0 = gimple_cond_lhs (stmt);
  tree op1 = gimple_cond_rhs (stmt);

  /* The only conditionals that we may be able to compute statically
     are predicates involving two SSA_NAMEs.  */
  if (TREE_CODE (op0) == SSA_NAME && TREE_CODE (op1) == SSA_NAME)
    {
      op0 = get_last_copy_of (op0);
      op1 = get_last_copy_of (op1);

      /* See if we can determine the predicate's value.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Trying to determine truth value of ");
	  fprintf (dump_file, "predicate ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      /* We can fold COND and get a useful result only when we have
	 the same SSA_NAME on both sides of a comparison operator.  */
      if (op0 == op1)
	{
	  tree folded_cond = fold_binary_loc (loc, gimple_cond_code (stmt),
                                          boolean_type_node, op0, op1);
	  if (folded_cond)
	    {
	      basic_block bb = gimple_bb (stmt);
	      *taken_edge_p = find_taken_edge (bb, folded_cond);
	      if (*taken_edge_p)
		retval = SSA_PROP_INTERESTING;
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS) && *taken_edge_p)
    fprintf (dump_file, "\nConditional will always take edge %d->%d\n",
	     (*taken_edge_p)->src->index, (*taken_edge_p)->dest->index);

  return retval;
}


/* Evaluate statement STMT.  If the statement produces a new output
   value, return SSA_PROP_INTERESTING and store the SSA_NAME holding
   the new value in *RESULT_P.

   If STMT is a conditional branch and we can determine its truth
   value, set *TAKEN_EDGE_P accordingly.

   If the new value produced by STMT is varying, return
   SSA_PROP_VARYING.  */

static enum ssa_prop_result
copy_prop_visit_stmt (gimple stmt, edge *taken_edge_p, tree *result_p)
{
  enum ssa_prop_result retval;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting statement:\n");
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
      fprintf (dump_file, "\n");
    }

  if (gimple_assign_single_p (stmt)
      && TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      /* If the statement is a copy assignment, evaluate its RHS to
	 see if the lattice value of its output has changed.  */
      retval = copy_prop_visit_assignment (stmt, result_p);
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    {
      /* See if we can determine which edge goes out of a conditional
	 jump.  */
      retval = copy_prop_visit_cond_stmt (stmt, taken_edge_p);
    }
  else
    retval = SSA_PROP_VARYING;

  if (retval == SSA_PROP_VARYING)
    {
      tree def;
      ssa_op_iter i;

      /* Any other kind of statement is not interesting for constant
	 propagation and, therefore, not worth simulating.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "No interesting values produced.\n");

      /* The assignment is not a copy operation.  Don't visit this
	 statement again and mark all the definitions in the statement
	 to be copies of nothing.  */
      FOR_EACH_SSA_TREE_OPERAND (def, stmt, i, SSA_OP_ALL_DEFS)
	set_copy_of_val (def, def);
    }

  return retval;
}


/* Visit PHI node PHI.  If all the arguments produce the same value,
   set it to be the value of the LHS of PHI.  */

static enum ssa_prop_result
copy_prop_visit_phi_node (gimple phi)
{
  enum ssa_prop_result retval;
  unsigned i;
  prop_value_t phi_val = { 0, NULL_TREE };

  tree lhs = gimple_phi_result (phi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
      fprintf (dump_file, "\n\n");
    }

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      prop_value_t *arg_val;
      tree arg = gimple_phi_arg_def (phi, i);
      edge e = gimple_phi_arg_edge (phi, i);

      /* We don't care about values flowing through non-executable
	 edges.  */
      if (!(e->flags & EDGE_EXECUTABLE))
	continue;

      /* Constants in the argument list never generate a useful copy.
	 Similarly, names that flow through abnormal edges cannot be
	 used to derive copies.  */
      if (TREE_CODE (arg) != SSA_NAME || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (arg))
	{
	  phi_val.value = lhs;
	  break;
	}

      /* Avoid copy propagation from an inner into an outer loop.
	 Otherwise, this may move loop variant variables outside of
	 their loops and prevent coalescing opportunities.  If the
	 value was loop invariant, it will be hoisted by LICM and
	 exposed for copy propagation.  Not a problem for virtual
	 operands though.  */
      if (is_gimple_reg (lhs)
	  && loop_depth_of_name (arg) > loop_depth_of_name (lhs))
	{
	  phi_val.value = lhs;
	  break;
	}

      /* If the LHS appears in the argument list, ignore it.  It is
	 irrelevant as a copy.  */
      if (arg == lhs || get_last_copy_of (arg) == lhs)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\tArgument #%d: ", i);
	  dump_copy_of (dump_file, arg);
	  fprintf (dump_file, "\n");
	}

      arg_val = get_copy_of_val (arg);

      /* If the LHS didn't have a value yet, make it a copy of the
	 first argument we find.  Notice that while we make the LHS be
	 a copy of the argument itself, we take the memory reference
	 from the argument's value so that we can compare it to the
	 memory reference of all the other arguments.  */
      if (phi_val.value == NULL_TREE)
	{
	  phi_val.value = arg_val->value ? arg_val->value : arg;
	  continue;
	}

      /* If PHI_VAL and ARG don't have a common copy-of chain, then
	 this PHI node cannot be a copy operation.  Also, if we are
	 copy propagating stores and these two arguments came from
	 different memory references, they cannot be considered
	 copies.  */
      if (get_last_copy_of (phi_val.value) != get_last_copy_of (arg))
	{
	  phi_val.value = lhs;
	  break;
	}
    }

  if (phi_val.value &&  may_propagate_copy (lhs, phi_val.value)
      && set_copy_of_val (lhs, phi_val.value))
    retval = (phi_val.value != lhs) ? SSA_PROP_INTERESTING : SSA_PROP_VARYING;
  else
    retval = SSA_PROP_NOT_INTERESTING;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPHI node ");
      dump_copy_of (dump_file, lhs);
      fprintf (dump_file, "\nTelling the propagator to ");
      if (retval == SSA_PROP_INTERESTING)
	fprintf (dump_file, "add SSA edges out of this PHI and continue.");
      else if (retval == SSA_PROP_VARYING)
	fprintf (dump_file, "add SSA edges out of this PHI and never visit again.");
      else
	fprintf (dump_file, "do nothing with SSA edges and keep iterating.");
      fprintf (dump_file, "\n\n");
    }

  return retval;
}


/* Initialize structures used for copy propagation.   PHIS_ONLY is true
   if we should only consider PHI nodes as generating copy propagation
   opportunities.  */

static void
init_copy_prop (void)
{
  basic_block bb;

  copy_of = XCNEWVEC (prop_value_t, num_ssa_names);

  cached_last_copy_of = XCNEWVEC (tree, num_ssa_names);

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator si;
      int depth = bb->loop_depth;

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple stmt = gsi_stmt (si);
	  ssa_op_iter iter;
          tree def;

	  /* The only statements that we care about are those that may
	     generate useful copies.  We also need to mark conditional
	     jumps so that their outgoing edges are added to the work
	     lists of the propagator.

	     Avoid copy propagation from an inner into an outer loop.
	     Otherwise, this may move loop variant variables outside of
	     their loops and prevent coalescing opportunities.  If the
	     value was loop invariant, it will be hoisted by LICM and
	     exposed for copy propagation.  */
	  if (stmt_ends_bb_p (stmt))
            prop_set_simulate_again (stmt, true);
	  else if (stmt_may_generate_copy (stmt)
                   /* Since we are iterating over the statements in
                      BB, not the phi nodes, STMT will always be an
                      assignment.  */
                   && loop_depth_of_name (gimple_assign_rhs1 (stmt)) <= depth)
            prop_set_simulate_again (stmt, true);
	  else
            prop_set_simulate_again (stmt, false);

	  /* Mark all the outputs of this statement as not being
	     the copy of anything.  */
	  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
            if (!prop_simulate_again_p (stmt))
	      set_copy_of_val (def, def);
	    else
	      cached_last_copy_of[SSA_NAME_VERSION (def)] = def;
	}

      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
	{
          gimple phi = gsi_stmt (si);
          tree def;

	  def = gimple_phi_result (phi);
	  if (!is_gimple_reg (def)
	      /* In loop-closed SSA form do not copy-propagate through
	         PHI nodes.  Technically this is only needed for loop
		 exit PHIs, but this is difficult to query.  */
	      || (current_loops
		  && gimple_phi_num_args (phi) == 1
		  && loops_state_satisfies_p (LOOP_CLOSED_SSA)))
            prop_set_simulate_again (phi, false);
	  else
            prop_set_simulate_again (phi, true);

	  if (!prop_simulate_again_p (phi))
	    set_copy_of_val (def, def);
	  else
	    cached_last_copy_of[SSA_NAME_VERSION (def)] = def;
	}
    }
}


/* Deallocate memory used in copy propagation and do final
   substitution.  */

static void
fini_copy_prop (void)
{
  size_t i;
  prop_value_t *tmp;
  
  /* Set the final copy-of value for each variable by traversing the
     copy-of chains.  */
  tmp = XCNEWVEC (prop_value_t, num_ssa_names);
  for (i = 1; i < num_ssa_names; i++)
    {
      tree var = ssa_name (i);
      if (!var
	  || !copy_of[i].value
	  || copy_of[i].value == var)
	continue;

      tmp[i].value = get_last_copy_of (var);

      /* In theory the points-to solution of all members of the
         copy chain is their intersection.  For now we do not bother
	 to compute this but only make sure we do not lose points-to
	 information completely by setting the points-to solution
	 of the representative to the first solution we find if
	 it doesn't have one already.  */
      if (tmp[i].value != var
	  && POINTER_TYPE_P (TREE_TYPE (var))
	  && SSA_NAME_PTR_INFO (var)
	  && !SSA_NAME_PTR_INFO (tmp[i].value))
	duplicate_ssa_name_ptr_info (tmp[i].value, SSA_NAME_PTR_INFO (var));
    }

  substitute_and_fold (tmp, false);

  free (cached_last_copy_of);
  free (copy_of);
  free (tmp);
}


/* Main entry point to the copy propagator.

   PHIS_ONLY is true if we should only consider PHI nodes as generating
   copy propagation opportunities. 

   The algorithm propagates the value COPY-OF using ssa_propagate.  For
   every variable X_i, COPY-OF(X_i) indicates which variable is X_i created
   from.  The following example shows how the algorithm proceeds at a
   high level:

	    1	a_24 = x_1
	    2	a_2 = PHI <a_24, x_1>
	    3	a_5 = PHI <a_2>
	    4	x_1 = PHI <x_298, a_5, a_2>

   The end result should be that a_2, a_5, a_24 and x_1 are a copy of
   x_298.  Propagation proceeds as follows.

   Visit #1: a_24 is copy-of x_1.  Value changed.
   Visit #2: a_2 is copy-of x_1.  Value changed.
   Visit #3: a_5 is copy-of x_1.  Value changed.
   Visit #4: x_1 is copy-of x_298.  Value changed.
   Visit #1: a_24 is copy-of x_298.  Value changed.
   Visit #2: a_2 is copy-of x_298.  Value changed.
   Visit #3: a_5 is copy-of x_298.  Value changed.
   Visit #4: x_1 is copy-of x_298.  Stable state reached.
   
   When visiting PHI nodes, we only consider arguments that flow
   through edges marked executable by the propagation engine.  So,
   when visiting statement #2 for the first time, we will only look at
   the first argument (a_24) and optimistically assume that its value
   is the copy of a_24 (x_1).

   The problem with this approach is that it may fail to discover copy
   relations in PHI cycles.  Instead of propagating copy-of
   values, we actually propagate copy-of chains.  For instance:

   		A_3 = B_1;
		C_9 = A_3;
		D_4 = C_9;
		X_i = D_4;

   In this code fragment, COPY-OF (X_i) = { D_4, C_9, A_3, B_1 }.
   Obviously, we are only really interested in the last value of the
   chain, however the propagator needs to access the copy-of chain
   when visiting PHI nodes.

   To represent the copy-of chain, we use the array COPY_CHAINS, which
   holds the first link in the copy-of chain for every variable.
   If variable X_i is a copy of X_j, which in turn is a copy of X_k,
   the array will contain:

		COPY_CHAINS[i] = X_j
		COPY_CHAINS[j] = X_k
		COPY_CHAINS[k] = X_k

   Keeping copy-of chains instead of copy-of values directly becomes
   important when visiting PHI nodes.  Suppose that we had the
   following PHI cycle, such that x_52 is already considered a copy of
   x_53:

	    1	x_54 = PHI <x_53, x_52>
	    2	x_53 = PHI <x_898, x_54>
   
   Visit #1: x_54 is copy-of x_53 (because x_52 is copy-of x_53)
   Visit #2: x_53 is copy-of x_898 (because x_54 is a copy of x_53,
				    so it is considered irrelevant
				    as a copy).
   Visit #1: x_54 is copy-of nothing (x_53 is a copy-of x_898 and
				      x_52 is a copy of x_53, so
				      they don't match)
   Visit #2: x_53 is copy-of nothing

   This problem is avoided by keeping a chain of copies, instead of
   the final copy-of value.  Propagation will now only keep the first
   element of a variable's copy-of chain.  When visiting PHI nodes,
   arguments are considered equal if their copy-of chains end in the
   same variable.  So, as long as their copy-of chains overlap, we
   know that they will be a copy of the same variable, regardless of
   which variable that may be).
   
   Propagation would then proceed as follows (the notation a -> b
   means that a is a copy-of b):

   Visit #1: x_54 = PHI <x_53, x_52>
		x_53 -> x_53
		x_52 -> x_53
		Result: x_54 -> x_53.  Value changed.  Add SSA edges.

   Visit #1: x_53 = PHI <x_898, x_54>
   		x_898 -> x_898
		x_54 -> x_53
		Result: x_53 -> x_898.  Value changed.  Add SSA edges.

   Visit #2: x_54 = PHI <x_53, x_52>
   		x_53 -> x_898
		x_52 -> x_53 -> x_898
		Result: x_54 -> x_898.  Value changed.  Add SSA edges.

   Visit #2: x_53 = PHI <x_898, x_54>
   		x_898 -> x_898
		x_54 -> x_898
		Result: x_53 -> x_898.  Value didn't change.  Stable state

   Once the propagator stabilizes, we end up with the desired result
   x_53 and x_54 are both copies of x_898.  */

static unsigned int
execute_copy_prop (void)
{
  init_copy_prop ();
  ssa_propagate (copy_prop_visit_stmt, copy_prop_visit_phi_node);
  fini_copy_prop ();
  return 0;
}

static bool
gate_copy_prop (void)
{
  return flag_tree_copy_prop != 0;
}

struct gimple_opt_pass pass_copy_prop =
{
 {
  GIMPLE_PASS,
  "copyprop",				/* name */
  gate_copy_prop,			/* gate */
  execute_copy_prop,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_COPY_PROP,			/* tv_id */
  PROP_ssa | PROP_cfg,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg
    | TODO_dump_func
    | TODO_ggc_collect
    | TODO_verify_ssa
    | TODO_update_ssa			/* todo_flags_finish */
 }
};
