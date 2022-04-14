/* Functions for analyzing the OpenACC loop structure from Graphite.

   Copyright (C) 2021 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "cfgloop.h"

#include "internal-fn.h"
#include "gimple.h"
#include "tree-cfg.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "print-tree.h"

#include "gimple-ssa.h"
#include "gimple-iterator.h"
#include "tree-phinodes.h"
#include "tree-ssa-operands.h"
#include "ssa-iterators.h"
#include "omp-general.h"
#include "graphite-oacc.h"

unsigned
gimple_call_internal_kind (gimple *call)
{
  return TREE_INT_CST_LOW (gimple_call_arg (call, 0));
}

static bool inline gimple_call_ifn_unique_p (gimple *call,
					     enum ifn_unique_kind kind)
{
  if (!gimple_call_internal_p (call, IFN_UNIQUE))
    return false;

  return kind == gimple_call_internal_kind (call);
}

static bool inline goacc_reduction_call_p (gimple *call)
{
  return gimple_call_internal_p (call, IFN_GOACC_REDUCTION);
}

static bool inline goacc_reduction_call_p (gimple *call,
					   enum ifn_goacc_reduction_kind kind)
{
  return gimple_call_internal_p (call, IFN_GOACC_REDUCTION)
	 && gimple_call_internal_kind (call) == kind;
}

/* Check if VAR is private in the OpenACC loop that encloses the cfg LOOP. The
   function returns TRUE if there is an IFN_UNIQUE_OACC_PRIVATE call in the
   head sequence that precedes the CFG loop. */

bool
is_oacc_private (tree var, loop_p loop)
{
  return false;

  if (TREE_CODE (var) == SSA_NAME)
    {
      if (!SSA_NAME_VAR (var))
	return false;

      var = SSA_NAME_VAR (var);
    }

  gcc_checking_assert (TREE_CODE (var) == VAR_DECL);

  if (!loop)
    return false;

  basic_block bb = loop->header;
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  while (bb != entry_bb)
    {
      bb = get_immediate_dominator (CDI_DOMINATORS, bb);
      gimple *stmt = last_stmt (bb);
      if (!stmt)
	continue;

      /* We are looking for the sequence of IFN_UNIQUE calls at the
	  head of the current OpenACC loop. */
      if (!gimple_call_internal_p (stmt, IFN_UNIQUE))
	continue;

      enum ifn_unique_kind kind
	  = (enum ifn_unique_kind)TREE_INT_CST_LOW (gimple_call_arg (stmt, 0));

      /* The head mark that starts the current OpenACC loop.
	 Private calls above here are irrelevant. Stop. */
      if (kind == IFN_UNIQUE_OACC_HEAD_MARK && gimple_call_num_args (stmt) > 2)
	break;

      if (kind != IFN_UNIQUE_OACC_PRIVATE)
	continue;

      tree private_var = gimple_call_arg (stmt, 3);

      if (TREE_CODE (private_var) == ADDR_EXPR)
	private_var = TREE_OPERAND (private_var, 0);

      if (var == private_var)
	return true;
    }

  return false;
}

void
oacc_add_private_var_kills (loop_p loop, vec<tree> *kills)
{
  gcc_checking_assert (loop);

  basic_block bb = loop->header;
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  while (bb != entry_bb)
    {
      bb = get_immediate_dominator (CDI_DOMINATORS, bb);

      gimple *stmt = last_stmt (bb);
      if (!stmt)
	continue;

      /* We are looking for the sequence of IFN_UNIQUE calls at the head of the
	 current OpenACC loop. */

      if (!gimple_call_ifn_unique_p (stmt, IFN_UNIQUE_OACC_HEAD_MARK))
	continue;

      /* The head mark that starts the current OpenACC loop.
         Private calls above here are irrelevant. Stop. */
      if (gimple_call_num_args (stmt) > 2)
	break;

      if (!gimple_call_ifn_unique_p (stmt, IFN_UNIQUE_OACC_PRIVATE))
	continue;

      tree private_var = gimple_call_arg (stmt, 3);

      gcc_checking_assert (TREE_CODE (private_var) == ADDR_EXPR);
      private_var = TREE_OPERAND (private_var, 0);
      kills->safe_push (private_var);
    }
}

typedef std::pair<gcall *, gcall *> gcall_pair;

/* Returns a pair that contains the internal function calls that start
   and end the head sequence of the OpenACC loop enclosing the cfg
   loop LOOP or a pair of NULL pointers if LOOP is not enclosed in a
   OpenACC LOOP. */

gcall_pair
find_oacc_head_marks (loop_p loop)
{
  basic_block bb = loop->header;
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  gcall *top_head_mark = NULL;
  gcall *bottom_head_mark = NULL;

  while (bb != entry_bb)
    {
      bb = get_immediate_dominator (CDI_DOMINATORS, bb);

      gimple *stmt = last_stmt (bb);
      if (!stmt)
	continue;

      /* Look for IFN_UNIQUE calls in the head of OpenACC loop. */
      if (!gimple_call_ifn_unique_p (stmt, IFN_UNIQUE_OACC_HEAD_MARK))
	continue;

      if (!bottom_head_mark)
	{
	  bottom_head_mark = as_a<gcall *> (stmt);
	  continue;
	}

      /* The head mark that starts the current OpenACC loop can be
	 recognized by the number of call arguments, cf. omp-low.c.  */
      if (gimple_call_num_args (stmt) > 3)
	{
	  top_head_mark = as_a<gcall *> (stmt);
	  break;
	}
    }

  gcc_checking_assert ((top_head_mark && bottom_head_mark)
		       || (!top_head_mark && !bottom_head_mark));

  return gcall_pair (top_head_mark, bottom_head_mark);
}

/* Returns the internal function call that starts the tail sequence of the
   OpenACC loop that encloses the CFG loop LOOP or NULL if LOOP is not
   contained in an OpenACC loop. */

gcall *
find_oacc_top_tail_mark (loop_p loop)
{
  gcall_pair head_marks = find_oacc_head_marks (loop);

  if (!head_marks.first || !head_marks.second)
    return NULL;

  tree data_dep = gimple_call_lhs (head_marks.second);
  gcc_checking_assert (has_single_use (data_dep));

  gimple *tail_mark;
  use_operand_p use_p;
  single_imm_use (data_dep, &use_p, &tail_mark);

  return as_a<gcall *> (tail_mark);
}

/* Returns a pair containing the internal function calls that start and end the
   tail sequence of the OpenACC loop that encloses the cfg loop LOOP or a pair
   of NULL pointers if LOOP does not belong to an OpenACC loop. */

gcall_pair
find_oacc_tail_marks (loop_p loop)
{
  gcall *top_tail_mark = find_oacc_top_tail_mark (loop);

  if (!top_tail_mark)
    return gcall_pair (NULL, NULL);

  tree data_dep = gimple_call_lhs (top_tail_mark);
  gimple *stmt = top_tail_mark;

  while (data_dep && has_single_use (data_dep))
    {
      use_operand_p use_p;
      single_imm_use (data_dep, &use_p, &stmt);
      data_dep = gimple_call_lhs (stmt);

      gcc_checking_assert (gimple_call_internal_p (stmt));
    }

  gcall *end_tail_mark = as_a<gcall *> (stmt);

  gcc_checking_assert (
      gimple_call_ifn_unique_p (end_tail_mark, IFN_UNIQUE_OACC_TAIL_MARK));

  return gcall_pair (top_tail_mark, end_tail_mark);
}

/* Add all ssa names to VARS that can be reached from PHI by a
   phi node walk. */

static void
collect_oacc_reduction_vars_phi_walk (gphi *phi, hash_set<tree> &vars)
{
  use_operand_p use_p;
  ssa_op_iter iter;
  FOR_EACH_PHI_ARG (use_p, phi, iter, SSA_OP_ALL_USES)
  {
    tree use = USE_FROM_PTR (use_p);
    if (TREE_CODE (use) != SSA_NAME)
      continue;

    if (vars.contains (use))
      continue;

    gimple *def_stmt = SSA_NAME_DEF_STMT (use);
    vars.add (use);

    gphi *use_phi = dyn_cast<gphi *> (def_stmt);
    if (use_phi)
      {
	collect_oacc_reduction_vars_phi_walk (use_phi, vars);

	continue;
      }
  }
}

/* Returns true iff following the immediate use chain from the
   IFN_GOACC_REDUCTION call CALL leads out of loop that contains CALL. */

static bool
reduction_use_in_outer_loop_p (gcall *call)
{
  gcc_checking_assert (goacc_reduction_call_p (call));

  tree data_dep = gimple_call_lhs (call);

  /* The IFN_GOACC_REDUCTION_CALLS are linked in a chain through
     immediate uses. Move to the end of this chain. */
  gimple *stmt = call;
  while (data_dep && has_single_use (data_dep))
    {
      use_operand_p use_p;
      single_imm_use (data_dep, &use_p, &stmt);

      if (!goacc_reduction_call_p (stmt))
	return true;

      data_dep = gimple_call_lhs (stmt);
    }

  gcc_checking_assert (goacc_reduction_call_p (stmt));

  /* Call starting further reduction use in outer loop. */
  if (goacc_reduction_call_p (stmt, IFN_GOACC_REDUCTION_SETUP))
    return true;

  /* Reduction use ends with last internal call in present loop. */
  if (goacc_reduction_call_p (stmt, IFN_GOACC_REDUCTION_TEARDOWN))
    return false;
  gcc_unreachable ();
}

/* Add all ssa names to VARS that can be reached from BB by walking
   through the phi nodes which start at the result of an OpenACC
   reduction computation in BB. */

static void
collect_oacc_reduction_vars_in_bb (basic_block bb, hash_set<tree> &vars)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!goacc_reduction_call_p (stmt, IFN_GOACC_REDUCTION_FINI))
	continue;

      tree var = gimple_call_arg (stmt, 2);
      gcc_checking_assert (TREE_CODE (var) == SSA_NAME);

      if (vars.contains (var))
	continue;

      gimple *def_stmt = SSA_NAME_DEF_STMT (var);

      if (gimple_code (def_stmt) != GIMPLE_PHI)
	{
	  gcc_checking_assert (goacc_reduction_call_p (def_stmt));

	  continue;
	}

      gcc_checking_assert (
	  goacc_reduction_call_p (stmt, IFN_GOACC_REDUCTION_FINI));
      gcc_checking_assert (gimple_code (def_stmt) == GIMPLE_PHI);

      if (reduction_use_in_outer_loop_p (as_a<gcall *> (stmt)))
	vars.add (var);

      collect_oacc_reduction_vars_phi_walk (static_cast<gphi *> (def_stmt),
					    vars);
    }
}

/* Add all ssa names to VARS that are defined by phi nodes in the header of LOOP
   such that at least one argument of the phi belongs to VARS. */

static void
collect_oacc_reduction_vars_in_loop_header (loop_p loop, hash_set<tree> &vars)
{
  for (gphi_iterator gpi = gsi_start_phis (loop->header); !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi = const_cast<gphi *> (gpi.phi ());

      use_operand_p use_p;
      ssa_op_iter iter;
      FOR_EACH_PHI_ARG (use_p, phi, iter, SSA_OP_ALL_USES)
      {
	tree use = USE_FROM_PTR (use_p);
	if (vars.contains (use))
	  vars.add (gimple_phi_result (phi));
      }
    }
}

/* Find the ssa names that belong to an OpenACC reduction in the OpenACC loop
   that surrounds the cfg loop LOOP and add them to VARS.  LOOP must be
   contained in an OpenACC loop.

   Since the reductions have not and cannot be lowered before execution of the
   Graphite pass because their lowering is device dependent, Graphite needs to
   simulate the privatization of the reduction variables by removing
   dependences between the iteration instances of the loop and the dependences
   arising from copying the initial value of the reduction variable in and the
   result out.

   The OpenACC lowering will copy the results of reduction computations at the
   IFN_GOACC_REDUCTION_FINI calls.  The main reduction statement can thus be
   identified by walking from those calls through all encountered phi nodes
   until we reach a gimple assignment statement. The ssa name defined by this
   statement as well as the ssa_names encountered in the phis along the way are
   recorded in VARS. In addition, the ssa name defined by each phi which uses a
   previously identified reduction variable in LOOP's header will also be added
   to VARS. */

void
collect_oacc_reduction_vars (loop_p loop, hash_set<tree> &vars)
{
  gcall_pair tail = find_oacc_tail_marks (loop);
  bool in_openacc_loop = tail.first != NULL;

  if (!in_openacc_loop)
    return;

  const gcall *top_mark = tail.first;
  const gcall *bottom_mark = tail.second;

  basic_block bb = top_mark->bb;
  gcc_checking_assert (single_succ_p (bb));

  do
    {
      bb = single_succ (bb);
      collect_oacc_reduction_vars_in_bb (bb, vars);
    }
  while (bb != bottom_mark->bb && single_succ_p (bb));

  collect_oacc_reduction_vars_in_loop_header (loop, vars);
}

static void collect_oacc_privatized_vars_phi_walk_visit_phi_uses (
    tree var, hash_set<tree> &vars, hash_set<tree> &visited);

/* Add all ssa names to VARS that can be reached from PHI by a phi node walk. */

static void
collect_oacc_privatized_vars_phi_walk (gphi *phi, hash_set<tree> &vars,
				       hash_set<tree> &visited)
{
  tree var = PHI_RESULT (phi);
  bool existed = vars.add (var);
  if (existed)
    return;

  use_operand_p use_p;
  ssa_op_iter iter;
  FOR_EACH_PHI_ARG (use_p, phi, iter, SSA_OP_ALL_USES)
  {
    tree use = USE_FROM_PTR (use_p);
    if (TREE_CODE (use) != SSA_NAME)
      continue;

    if (visited.contains (use))
      continue;

    gimple *def_stmt = SSA_NAME_DEF_STMT (use);
    gphi *use_phi = dyn_cast<gphi *> (def_stmt);
    if (use_phi)
      {
	collect_oacc_privatized_vars_phi_walk (use_phi, vars, visited);
	visited.add (use);
	continue;
      }

    vars.add (use);

    /* Visit the uses of USE in other phi nodes. This is used to get from loop
       exit phis in inner loops to the loop entry phis. */

    collect_oacc_privatized_vars_phi_walk_visit_phi_uses (use, vars, visited);
    visited.add (use);
  }
}

/* Records all uses of VAR in phis in VARS and continues the phi walk on each
   such use. */

static void
collect_oacc_privatized_vars_phi_walk_visit_phi_uses (tree var,
						      hash_set<tree> &vars,
						      hash_set<tree> &visited)
{
  imm_use_iterator iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
  {
    tree use = USE_FROM_PTR (use_p);
    if (TREE_CODE (use) != SSA_NAME)
      continue;

    if (visited.contains (use))
      continue;

    gimple *use_stmt = USE_STMT (use_p);
    gphi *use_phi = dyn_cast<gphi *> (use_stmt);

    if (use_phi)
      {
	visited.add (PHI_RESULT (use_phi));
	collect_oacc_privatized_vars_phi_walk (use_phi, vars, visited);
	continue;
      }

    if (TREE_CODE (use) == SSA_NAME
	&& SSA_NAME_VAR (use) == SSA_NAME_VAR (var))
      {
	if (!vars.add (use))
	  collect_oacc_privatized_vars_phi_walk_visit_phi_uses (use, vars,
								visited);
	continue;
      }
  }

  return;
}

/* Return the first IFN_UNIQUE call with the given KIND that follows the tail
   sequence of the OpenACC loop surrounding LOOP. */

static gcall *
find_ifn_unique_call_below (loop_p loop, enum ifn_unique_kind kind)
{
  gcall_pair tail = find_oacc_tail_marks (loop);
  bool in_openacc_loop = tail.first != NULL;

  if (!in_openacc_loop)
    return NULL;

  edge exit = single_exit (loop);
  basic_block bb = exit->dest;
  while ((bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb)))
    {
      gimple *stmt = last_stmt (bb);

      if (!stmt)
	continue;

      if (gimple_call_ifn_unique_p (stmt, kind))
	return static_cast<gcall *> (stmt);
    }

  return NULL;
}

/* Return the IFN_UNIQUE_OACC_PRIVATE_SCALAR call which follows the tail
   sequence of the OpenACC loop surrounding LOOP. */

gcall *
get_oacc_private_scalars_call (loop_p loop)
{
  return find_ifn_unique_call_below (loop, IFN_UNIQUE_OACC_PRIVATE_SCALAR);
}

/* Return the IFN_UNIQUE_OACC_FIRSTPRIVATE call which follows the tail
   sequence of the OpenACC loop surrounding LOOP. */

gcall *
get_oacc_firstprivate_call (loop_p loop)
{
  return find_ifn_unique_call_below (loop, IFN_UNIQUE_OACC_FIRSTPRIVATE);
}

/* Find the ssa names that belong to the computation of variables that are
   "private" in the OpenACC loop that surrounds the CFG loop LOOP and add them
   to VARS.  LOOP must be contained in an OpenACC loop.

   The CFG loop structure of OpenACC loops does not directly reflect the
   privatization of the variable since the original loop has been enclosed in a
   "chunking" loop. The "private" scalars variables are alive in those two
   outermost CFG loops and the corresponding phis must be ignored by Graphite in
   order to recognize the parallelizability of the loop. Omp-low.c places a
   special internal function call after the outermost loop of a parallel region
   whose arguments list the "private" variables that are considered here */

void
collect_oacc_privatized_vars (gcall *marker, hash_set<tree> &vars)
{
  if (!marker)
    return;

  gcc_checking_assert (marker->bb->loop_father->num == 0);

  /* Search for phis that can be reached from the vars listed in the
     PRIVATE_SCALARS_CALL's arguments. */

  const unsigned n = gimple_call_num_args (marker);
  for (unsigned i = 1; i < n; ++i)
    {
      tree arg = gimple_call_arg (marker, i);

      if (TREE_CODE (arg) != SSA_NAME)
	continue;

      gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
      gphi *phi = dyn_cast<gphi *> (def_stmt);
      if (!phi)
	{
	  /* If the argument does not point to a phi, then it must be some value
	     defined outside of any OpenACC loop nest, i.e. a parameter of the
	     loop-nest. */
	  gcc_checking_assert (!def_stmt->bb
			       || def_stmt->bb->loop_father->num == 0);
	  continue;
	}

      hash_set<tree> visited;
      collect_oacc_privatized_vars_phi_walk (phi, vars, visited);
    }
}

/* Return true if LOOP is an OpenACC loop with an "auto" clause, false otherwise. */

static bool
oacc_loop_with_auto_clause_p (loop_p loop)
{
  gcall_pair head_marks = find_oacc_head_marks (loop);

  if (!head_marks.first)
    return false;

  unsigned flags = TREE_INT_CST_LOW (gimple_call_arg (head_marks.first, 3));
  return flags & OLF_AUTO;
}

/* Return true if FUN is an outlined OpenACC function that contains loops with
   "auto" clauses. */

static bool
function_has_auto_loops_p (function *fun)
{
  gcc_checking_assert (oacc_function_p (fun));

  for (auto loop : loops_list (fun, 0))
  if (oacc_loop_with_auto_clause_p (loop))
    return true;

  return false;
}

/* Return true if Graphite might analyze outlined OpenACC functions for the kind
   of target region for which FUN was created. The actual decision whether
   Graphite runs on FUN may be subject to further restrictions. */

bool
graphite_analyze_oacc_target_region_type_p (function *fun)
{
  gcc_checking_assert (oacc_function_p (fun));

  bool is_oacc_parallel
      = lookup_attribute ("oacc parallel",
			  DECL_ATTRIBUTES (current_function_decl))
	!= NULL;

  bool is_oacc_parallel_kernels_graphite
      = lookup_attribute ("oacc parallel_kernels_graphite",
			  DECL_ATTRIBUTES (current_function_decl))
	!= NULL;

  return is_oacc_parallel || is_oacc_parallel_kernels_graphite;
}

/* Return true if FUN is an outlined OpenACC function that is going to be
   analyzed by Graphite. */

bool
graphite_analyze_oacc_function_p (function *fun)
{
  gcc_checking_assert (oacc_function_p (fun));

  return graphite_analyze_oacc_target_region_type_p (cfun)
	 && function_has_auto_loops_p (cfun);
}
