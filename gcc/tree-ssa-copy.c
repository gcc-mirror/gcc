/* Copy propagation and SSA_NAME replacement support routines.
   Copyright (C) 2004-2016 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-propagate.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-niter.h"


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

/*---------------------------------------------------------------------------
				Copy propagation
---------------------------------------------------------------------------*/
/* Lattice for copy-propagation.  The lattice is initialized to
   UNDEFINED (value == NULL) for SSA names that can become a copy
   of something or VARYING (value == self) if not (see get_copy_of_val
   and stmt_may_generate_copy).  Other values make the name a COPY
   of that value.

   When visiting a statement or PHI node the lattice value for an
   SSA name can transition from UNDEFINED to COPY to VARYING.  */

struct prop_value_t {
    /* Copy-of value.  */
    tree value;
};

static prop_value_t *copy_of;
static unsigned n_copy_of;


/* Return true if this statement may generate a useful copy.  */

static bool
stmt_may_generate_copy (gimple *stmt)
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
  return ((gimple_assign_rhs_code (stmt) == SSA_NAME
	   && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (stmt)))
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt)));
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

/* Return the variable VAR is a copy of or VAR if VAR isn't the result
   of a copy.  */

static inline tree
valueize_val (tree var)
{
  if (TREE_CODE (var) == SSA_NAME)
    {
      tree val = get_copy_of_val (var)->value;
      if (val)
	return val;
    }
  return var;
}

/* Set VAL to be the copy of VAR.  If that changed return true.  */

static inline bool
set_copy_of_val (tree var, tree val)
{
  unsigned int ver = SSA_NAME_VERSION (var);
  tree old;

  /* Set FIRST to be the first link in COPY_OF[DEST].  If that
     changed, return true.  */
  old = copy_of[ver].value;
  copy_of[ver].value = val;

  if (old != val
      || (val && !operand_equal_p (old, val, 0)))
    return true;

  return false;
}


/* Dump the copy-of value for variable VAR to FILE.  */

static void
dump_copy_of (FILE *file, tree var)
{
  tree val;

  print_generic_expr (file, var, dump_flags);
  if (TREE_CODE (var) != SSA_NAME)
    return;

  val = copy_of[SSA_NAME_VERSION (var)].value;
  fprintf (file, " copy-of chain: ");
  print_generic_expr (file, var, 0);
  fprintf (file, " ");
  if (!val)
    fprintf (file, "[UNDEFINED]");
  else if (val == var)
    fprintf (file, "[NOT A COPY]");
  else
    {
      fprintf (file, "-> ");
      print_generic_expr (file, val, 0);
      fprintf (file, " ");
      fprintf (file, "[COPY]");
    }
}


/* Evaluate the RHS of STMT.  If it produces a valid copy, set the LHS
   value and store the LHS into *RESULT_P.  */

static enum ssa_prop_result
copy_prop_visit_assignment (gimple *stmt, tree *result_p)
{
  tree lhs, rhs;

  lhs = gimple_assign_lhs (stmt);
  rhs = valueize_val (gimple_assign_rhs1 (stmt));

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      /* Straight copy between two SSA names.  First, make sure that
	 we can propagate the RHS into uses of LHS.  */
      if (!may_propagate_copy (lhs, rhs))
	return SSA_PROP_VARYING;

      *result_p = lhs;
      if (set_copy_of_val (*result_p, rhs))
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
copy_prop_visit_cond_stmt (gimple *stmt, edge *taken_edge_p)
{
  enum ssa_prop_result retval = SSA_PROP_VARYING;
  location_t loc = gimple_location (stmt);

  tree op0 = valueize_val (gimple_cond_lhs (stmt));
  tree op1 = valueize_val (gimple_cond_rhs (stmt));

  /* See if we can determine the predicate's value.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Trying to determine truth value of ");
      fprintf (dump_file, "predicate ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  /* Fold COND and see whether we get a useful result.  */
  tree folded_cond = fold_binary_loc (loc, gimple_cond_code (stmt),
				      boolean_type_node, op0, op1);
  if (folded_cond)
    {
      basic_block bb = gimple_bb (stmt);
      *taken_edge_p = find_taken_edge (bb, folded_cond);
      if (*taken_edge_p)
	retval = SSA_PROP_INTERESTING;
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
copy_prop_visit_stmt (gimple *stmt, edge *taken_edge_p, tree *result_p)
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
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
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
copy_prop_visit_phi_node (gphi *phi)
{
  enum ssa_prop_result retval;
  unsigned i;
  prop_value_t phi_val = { NULL_TREE };

  tree lhs = gimple_phi_result (phi);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
    }

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      prop_value_t *arg_val;
      tree arg_value;
      tree arg = gimple_phi_arg_def (phi, i);
      edge e = gimple_phi_arg_edge (phi, i);

      /* We don't care about values flowing through non-executable
	 edges.  */
      if (!(e->flags & EDGE_EXECUTABLE))
	continue;

      /* Names that flow through abnormal edges cannot be used to
	 derive copies.  */
      if (TREE_CODE (arg) == SSA_NAME && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (arg))
	{
	  phi_val.value = lhs;
	  break;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\tArgument #%d: ", i);
	  dump_copy_of (dump_file, arg);
	  fprintf (dump_file, "\n");
	}

      if (TREE_CODE (arg) == SSA_NAME)
	{
	  arg_val = get_copy_of_val (arg);

	  /* If we didn't visit the definition of arg yet treat it as
	     UNDEFINED.  This also handles PHI arguments that are the
	     same as lhs.  We'll come here again.  */
	  if (!arg_val->value)
	    continue;

	  arg_value = arg_val->value;
	}
      else
	arg_value = valueize_val (arg);

      /* In loop-closed SSA form do not copy-propagate SSA-names across
	 loop exit edges.  */
      if (loops_state_satisfies_p (LOOP_CLOSED_SSA)
	  && TREE_CODE (arg_value) == SSA_NAME
	  && loop_exit_edge_p (e->src->loop_father, e))
	{
	  phi_val.value = lhs;
	  break;
	}

      /* If the LHS didn't have a value yet, make it a copy of the
	 first argument we find.   */
      if (phi_val.value == NULL_TREE)
	{
	  phi_val.value = arg_value;
	  continue;
	}

      /* If PHI_VAL and ARG don't have a common copy-of chain, then
	 this PHI node cannot be a copy operation.  */
      if (phi_val.value != arg_value
	  && !operand_equal_p (phi_val.value, arg_value, 0))
	{
	  phi_val.value = lhs;
	  break;
	}
    }

  if (phi_val.value
      && may_propagate_copy (lhs, phi_val.value)
      && set_copy_of_val (lhs, phi_val.value))
    retval = (phi_val.value != lhs) ? SSA_PROP_INTERESTING : SSA_PROP_VARYING;
  else
    retval = SSA_PROP_NOT_INTERESTING;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "PHI node ");
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


/* Initialize structures used for copy propagation.  */

static void
init_copy_prop (void)
{
  basic_block bb;

  n_copy_of = num_ssa_names;
  copy_of = XCNEWVEC (prop_value_t, n_copy_of);

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  ssa_op_iter iter;
          tree def;

	  /* The only statements that we care about are those that may
	     generate useful copies.  We also need to mark conditional
	     jumps so that their outgoing edges are added to the work
	     lists of the propagator.  */
	  if (stmt_ends_bb_p (stmt))
            prop_set_simulate_again (stmt, true);
	  else if (stmt_may_generate_copy (stmt))
            prop_set_simulate_again (stmt, true);
	  else
            prop_set_simulate_again (stmt, false);

	  /* Mark all the outputs of this statement as not being
	     the copy of anything.  */
	  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_ALL_DEFS)
            if (!prop_simulate_again_p (stmt))
	      set_copy_of_val (def, def);
	}

      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
          gphi *phi = si.phi ();
          tree def;

	  def = gimple_phi_result (phi);
	  if (virtual_operand_p (def))
            prop_set_simulate_again (phi, false);
	  else
            prop_set_simulate_again (phi, true);

	  if (!prop_simulate_again_p (phi))
	    set_copy_of_val (def, def);
	}
    }
}

/* Callback for substitute_and_fold to get at the final copy-of values.  */

static tree
get_value (tree name)
{
  tree val;
  if (SSA_NAME_VERSION (name) >= n_copy_of)
    return NULL_TREE;
  val = copy_of[SSA_NAME_VERSION (name)].value;
  if (val && val != name)
    return val;
  return NULL_TREE;
}

/* Deallocate memory used in copy propagation and do final
   substitution.  */

static bool
fini_copy_prop (void)
{
  unsigned i;

  /* Set the final copy-of value for each variable by traversing the
     copy-of chains.  */
  for (i = 1; i < num_ssa_names; i++)
    {
      tree var = ssa_name (i);
      if (!var
	  || !copy_of[i].value
	  || copy_of[i].value == var)
	continue;

      /* In theory the points-to solution of all members of the
         copy chain is their intersection.  For now we do not bother
	 to compute this but only make sure we do not lose points-to
	 information completely by setting the points-to solution
	 of the representative to the first solution we find if
	 it doesn't have one already.  */
      if (copy_of[i].value != var
	  && TREE_CODE (copy_of[i].value) == SSA_NAME)
	{
	  basic_block copy_of_bb
	    = gimple_bb (SSA_NAME_DEF_STMT (copy_of[i].value));
	  basic_block var_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
	  if (POINTER_TYPE_P (TREE_TYPE (var))
	      && SSA_NAME_PTR_INFO (var)
	      && !SSA_NAME_PTR_INFO (copy_of[i].value))
	    {
	      duplicate_ssa_name_ptr_info (copy_of[i].value,
					   SSA_NAME_PTR_INFO (var));
	      /* Points-to information is cfg insensitive,
		 but alignment info might be cfg sensitive, if it
		 e.g. is derived from VRP derived non-zero bits.
		 So, do not copy alignment info if the two SSA_NAMEs
		 aren't defined in the same basic block.  */
	      if (var_bb != copy_of_bb)
		mark_ptr_info_alignment_unknown
				(SSA_NAME_PTR_INFO (copy_of[i].value));
	    }
	  else if (!POINTER_TYPE_P (TREE_TYPE (var))
		   && SSA_NAME_RANGE_INFO (var)
		   && !SSA_NAME_RANGE_INFO (copy_of[i].value)
		   && var_bb == copy_of_bb)
	    duplicate_ssa_name_range_info (copy_of[i].value,
					   SSA_NAME_RANGE_TYPE (var),
					   SSA_NAME_RANGE_INFO (var));
	}
    }

  bool changed = substitute_and_fold (get_value, NULL, true);
  if (changed)
    {
      free_numbers_of_iterations_estimates (cfun);
      if (scev_initialized_p ())
	scev_reset ();
    }

  free (copy_of);

  return changed;
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
   is the copy of a_24 (x_1).  */

static unsigned int
execute_copy_prop (void)
{
  init_copy_prop ();
  ssa_propagate (copy_prop_visit_stmt, copy_prop_visit_phi_node);
  if (fini_copy_prop ())
    return TODO_cleanup_cfg;
  return 0;
}

namespace {

const pass_data pass_data_copy_prop =
{
  GIMPLE_PASS, /* type */
  "copyprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_COPY_PROP, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_copy_prop : public gimple_opt_pass
{
public:
  pass_copy_prop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_copy_prop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_copy_prop (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_copy_prop != 0; }
  virtual unsigned int execute (function *) { return execute_copy_prop (); }

}; // class pass_copy_prop

} // anon namespace

gimple_opt_pass *
make_pass_copy_prop (gcc::context *ctxt)
{
  return new pass_copy_prop (ctxt);
}
