/* Single entry single exit control flow regions.
   Copyright (C) 2008-2019 Free Software Foundation, Inc.
   Contributed by Jan Sjodin <jan.sjodin@amd.com> and
   Sebastian Pop <sebastian.pop@amd.com>.

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
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "cfganal.h"
#include "sese.h"

/* For a USE in BB, if BB is outside REGION, mark the USE in the
   LIVEOUTS set.  */

static void
sese_build_liveouts_use (sese_info_p region, bitmap liveouts, basic_block bb,
			 tree use)
{
  gcc_assert (!bb_in_sese_p (bb, region->region));
  if (TREE_CODE (use) != SSA_NAME)
    return;

  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));

  if (!def_bb || !bb_in_sese_p (def_bb, region->region))
    return;

  unsigned ver = SSA_NAME_VERSION (use);
  bitmap_set_bit (liveouts, ver);
}

/* Marks for rewrite all the SSA_NAMES defined in REGION and that are
   used in BB that is outside of the REGION.  */

static void
sese_build_liveouts_bb (sese_info_p region, basic_block bb)
{
  ssa_op_iter iter;
  use_operand_p use_p;

  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    FOR_EACH_PHI_ARG (use_p, bsi.phi (), iter, SSA_OP_USE)
      sese_build_liveouts_use (region, region->liveout,
			       bb, USE_FROM_PTR (use_p));

  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);

      bitmap liveouts = region->liveout;
      if (is_gimple_debug (stmt))
	liveouts = region->debug_liveout;

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	sese_build_liveouts_use (region, liveouts, bb, USE_FROM_PTR (use_p));
    }
}

/* Reset debug stmts that reference SSA_NAMES defined in REGION that
   are not marked as liveouts.  */

static void
sese_reset_debug_liveouts (sese_info_p region)
{
  bitmap_iterator bi;
  unsigned i;
  EXECUTE_IF_AND_COMPL_IN_BITMAP (region->debug_liveout, region->liveout,
				  0, i, bi)
    {
      tree name = ssa_name (i);
      auto_vec<gimple *, 4> stmts;
      gimple *use_stmt;
      imm_use_iterator use_iter;
      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, name)
	{
	  if (! is_gimple_debug (use_stmt)
	      || bb_in_sese_p (gimple_bb (use_stmt), region->region))
	    continue;
	  stmts.safe_push (use_stmt);
	}
      while (!stmts.is_empty ())
	{
	  gimple *stmt = stmts.pop ();
	  gimple_debug_bind_reset_value (stmt);
	  update_stmt (stmt);
	}
    }
}

/* Build the LIVEOUTS of REGION: the set of variables defined inside
   and used outside the REGION.  */

void
sese_build_liveouts (sese_info_p region)
{
  basic_block bb;

  gcc_assert (region->liveout == NULL
	      && region->debug_liveout == NULL);

  region->liveout = BITMAP_ALLOC (NULL);
  region->debug_liveout = BITMAP_ALLOC (NULL);

  /* FIXME: We could start iterating form the successor of sese.  */
  FOR_EACH_BB_FN (bb, cfun)
    if (!bb_in_sese_p (bb, region->region))
      sese_build_liveouts_bb (region, bb);
}

/* Builds a new SESE region from edges ENTRY and EXIT.  */

sese_info_p
new_sese_info (edge entry, edge exit)
{
  sese_info_p region = XNEW (class sese_info_t);

  region->region.entry = entry;
  region->region.exit = exit;
  region->liveout = NULL;
  region->debug_liveout = NULL;
  region->params.create (3);
  region->rename_map = new hash_map <tree, tree>;
  region->bbs.create (3);

  return region;
}

/* Deletes REGION.  */

void
free_sese_info (sese_info_p region)
{
  region->params.release ();
  BITMAP_FREE (region->liveout);
  BITMAP_FREE (region->debug_liveout);

  delete region->rename_map;
  region->rename_map = NULL;
  region->bbs.release ();

  XDELETE (region);
}

/* Add exit phis for USE on EXIT.  */

static void
sese_add_exit_phis_edge (basic_block exit, tree use, edge false_e, edge true_e)
{
  gphi *phi = create_phi_node (NULL_TREE, exit);
  create_new_def_for (use, phi, gimple_phi_result_ptr (phi));
  add_phi_arg (phi, use, false_e, UNKNOWN_LOCATION);
  add_phi_arg (phi, use, true_e, UNKNOWN_LOCATION);
  update_stmt (phi);
}

/* Insert in the block BB phi nodes for variables defined in REGION
   and used outside the REGION.  The code generation moves REGION in
   the else clause of an "if (1)" and generates code in the then
   clause that is at this point empty:

   | if (1)
   |   empty;
   | else
   |   REGION;
*/

void
sese_insert_phis_for_liveouts (sese_info_p region, basic_block bb,
			       edge false_e, edge true_e)
{
  if (MAY_HAVE_DEBUG_BIND_STMTS)
    sese_reset_debug_liveouts (region);

  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (region->liveout, 0, i, bi)
    if (!virtual_operand_p (ssa_name (i)))
      sese_add_exit_phis_edge (bb, ssa_name (i), false_e, true_e);
}

/* Returns the outermost loop in SCOP that contains BB.  */

class loop *
outermost_loop_in_sese_1 (sese_l &region, basic_block bb)
{
  class loop *nest;

  nest = bb->loop_father;
  while (loop_outer (nest)
	 && loop_in_sese_p (loop_outer (nest), region))
    nest = loop_outer (nest);

  return nest;
}

/* Same as outermost_loop_in_sese_1, returns the outermost loop
   containing BB in REGION, but makes sure that the returned loop
   belongs to the REGION, and so this returns the first loop in the
   REGION when the loop containing BB does not belong to REGION.  */

loop_p
outermost_loop_in_sese (sese_l &region, basic_block bb)
{
  loop_p nest = outermost_loop_in_sese_1 (region, bb);

  if (loop_in_sese_p (nest, region))
    return nest;

  /* When the basic block BB does not belong to a loop in the region,
     return the first loop in the region.  */
  nest = nest->inner;
  while (nest)
    if (loop_in_sese_p (nest, region))
      break;
    else
      nest = nest->next;

  gcc_assert (nest);
  return nest;
}

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag set.  */

edge
get_true_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_TRUE_VALUE)
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Returns the first successor edge of BB with EDGE_TRUE_VALUE flag cleared.  */

edge
get_false_edge_from_guard_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (!(e->flags & EDGE_TRUE_VALUE))
      return e;

  gcc_unreachable ();
  return NULL;
}

/* Moves REGION in a condition expression:
   | if (1)
   |   ;
   | else
   |   REGION;
*/

ifsese
move_sese_in_condition (sese_info_p region)
{
  basic_block region_entry_dest = region->region.entry->dest;
  basic_block pred_block = split_edge (region->region.entry);
  basic_block merge_block = split_edge (region->region.exit);

  edge true_edge = make_edge (pred_block, merge_block, EDGE_TRUE_VALUE);
  edge false_edge = find_edge (pred_block, region_entry_dest);
  false_edge->flags &= ~EDGE_FALLTHRU;
  false_edge->flags |= EDGE_FALSE_VALUE;
  gimple_stmt_iterator gsi = gsi_last_bb (pred_block);
  gcond *cond = gimple_build_cond (NE_EXPR, integer_one_node, integer_zero_node,
				   NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, cond, GSI_CONTINUE_LINKING);
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, merge_block, pred_block);

  ifsese if_region = XNEW (ifsese_s);
  if_region->region = XCNEW (sese_info_t);
  if_region->true_region = XCNEW (sese_info_t);
  if_region->false_region = XCNEW (sese_info_t);
  if_region->region->region.entry = single_pred_edge (pred_block);
  if_region->region->region.exit = single_succ_edge (merge_block);
  if_region->false_region->region.entry = false_edge;
  if_region->false_region->region.exit = region->region.exit;
  if_region->true_region->region.entry = true_edge;
  if_region->true_region->region.exit
    = single_succ_edge (split_edge (true_edge));

  region->region = if_region->false_region->region;

  return if_region;
}

/* Replaces the condition of the IF_REGION with CONDITION:
   | if (CONDITION)
   |   true_region;
   | else
   |   false_region;
*/

void
set_ifsese_condition (ifsese if_region, tree condition)
{
  sese_info_p region = if_region->region;
  edge entry = region->region.entry;
  basic_block bb = entry->dest;
  gimple *last = last_stmt (bb);
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gcond *cond_stmt;

  gcc_assert (gimple_code (last) == GIMPLE_COND);

  gsi_remove (&gsi, true);
  gsi = gsi_last_bb (bb);
  condition = force_gimple_operand_gsi (&gsi, condition, true, NULL,
					false, GSI_NEW_STMT);
  cond_stmt = gimple_build_cond_from_tree (condition, NULL_TREE, NULL_TREE);
  gsi = gsi_last_bb (bb);
  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);
}

/* Return true when T is defined outside REGION or when no definitions are
   variant in REGION.  When HAS_VDEFS is a valid pointer, sets HAS_VDEFS to true
   when T depends on memory that may change in REGION.  */

bool
invariant_in_sese_p_rec (tree t, const sese_l &region, bool *has_vdefs)
{
  if (!defined_in_sese_p (t, region))
    return true;

  gimple *stmt = SSA_NAME_DEF_STMT (t);

  if (gimple_code (stmt) == GIMPLE_PHI
      || gimple_code (stmt) == GIMPLE_CALL)
    return false;

  /* VDEF is variant when it is in the region.  */
  if (gimple_vdef (stmt))
    {
      if (has_vdefs)
	*has_vdefs = true;
      return false;
    }

  /* A VUSE may or may not be variant following the VDEFs.  */
  if (tree vuse = gimple_vuse (stmt))
    return invariant_in_sese_p_rec (vuse, region, has_vdefs);

  ssa_op_iter iter;
  use_operand_p use_p;
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);

      if (!defined_in_sese_p (use, region))
	continue;

      if (!invariant_in_sese_p_rec (use, region, has_vdefs))
	return false;
    }

  return true;
}

/* Return true when DEF can be analyzed in REGION by the scalar
   evolution analyzer.  */

bool
scev_analyzable_p (tree def, sese_l &region)
{
  loop_p loop;
  tree scev;
  tree type = TREE_TYPE (def);

  /* When Graphite generates code for a scev, the code generator
     expresses the scev in function of a single induction variable.
     This is unsafe for floating point computations, as it may replace
     a floating point sum reduction with a multiplication.  The
     following test returns false for non integer types to avoid such
     problems.  */
  if (!INTEGRAL_TYPE_P (type)
      && !POINTER_TYPE_P (type))
    return false;

  loop = loop_containing_stmt (SSA_NAME_DEF_STMT (def));
  scev = scalar_evolution_in_region (region, loop, def);

  return (!chrec_contains_undetermined (scev)
	  && (TREE_CODE (scev) != SSA_NAME
	      || !defined_in_sese_p (scev, region))
	  && scev_is_linear_expression (scev)
	  && (! loop
	      || ! loop_in_sese_p (loop, region)
	      || ! chrec_contains_symbols_defined_in_loop (scev, loop->num)));
}

/* Returns the scalar evolution of T in REGION.  Every variable that
   is not defined in the REGION is considered a parameter.  */

tree
scalar_evolution_in_region (const sese_l &region, loop_p loop, tree t)
{
  /* SCOP parameters.  */
  if (TREE_CODE (t) == SSA_NAME
      && !defined_in_sese_p (t, region))
    return t;

  if (!loop_in_sese_p (loop, region))
    loop = NULL;

  return instantiate_scev (region.entry, loop,
			   analyze_scalar_evolution (loop, t));
}

/* Return true if BB is empty, contains only DEBUG_INSNs.  */

bool
sese_trivially_empty_bb_p (basic_block bb)
{         
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    if (!is_gimple_debug (gsi_stmt (gsi))
	&& gimple_code (gsi_stmt (gsi)) != GIMPLE_LABEL)
      return false;

  return true;
}

/* Pretty print edge E to FILE.  */

void
print_edge (FILE *file, const_edge e)
{
  fprintf (file, "edge (bb_%d, bb_%d)", e->src->index, e->dest->index);
}

/* Pretty print sese S to FILE.  */

void
print_sese (FILE *file, const sese_l &s)
{
  fprintf (file, "(entry_"); print_edge (file, s.entry);
  fprintf (file, ", exit_"); print_edge (file, s.exit);
  fprintf (file, ")\n");
}

/* Pretty print edge E to STDERR.  */

DEBUG_FUNCTION void
debug_edge (const_edge e)
{
  print_edge (stderr, e);
}

/* Pretty print sese S to STDERR.  */

DEBUG_FUNCTION void
debug_sese (const sese_l &s)
{
  print_sese (stderr, s);
}
