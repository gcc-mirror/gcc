/* Single entry single exit control flow regions.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.
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
#include "sese.h"
#include "tree-ssa-propagate.h"

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
sese_build_liveouts_bb (sese_info_p region, bitmap liveouts, basic_block bb)
{
  edge e;
  edge_iterator ei;
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (gphi_iterator bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi);
	 gsi_next (&bsi))
      sese_build_liveouts_use (region, liveouts, bb,
			       PHI_ARG_DEF_FROM_EDGE (bsi.phi (), e));

  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);

      if (is_gimple_debug (stmt))
	continue;

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	sese_build_liveouts_use (region, liveouts, bb, USE_FROM_PTR (use_p));
    }
}

/* For a USE in BB, return true if BB is outside REGION and it's not
   in the LIVEOUTS set.  */

static bool
sese_bad_liveouts_use (sese_info_p region, bitmap liveouts, basic_block bb,
		       tree use)
{
  gcc_assert (!bb_in_sese_p (bb, region->region));

  if (TREE_CODE (use) != SSA_NAME)
    return false;

  unsigned ver = SSA_NAME_VERSION (use);

  /* If it's in liveouts, the variable will get a new PHI node, and
     the debug use will be properly adjusted.  */
  if (bitmap_bit_p (liveouts, ver))
    return false;

  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));

  if (!def_bb || !bb_in_sese_p (def_bb, region->region))
    return false;

  return true;
}

/* Reset debug stmts that reference SSA_NAMES defined in REGION that
   are not marked as liveouts.  */

static void
sese_reset_debug_liveouts_bb (sese_info_p region, bitmap liveouts,
			      basic_block bb)
{
  gimple_stmt_iterator bsi;
  ssa_op_iter iter;
  use_operand_p use_p;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);

      if (!is_gimple_debug (stmt))
	continue;

      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	if (sese_bad_liveouts_use (region, liveouts, bb,
				   USE_FROM_PTR (use_p)))
	  {
	    gimple_debug_bind_reset_value (stmt);
	    update_stmt (stmt);
	    break;
	  }
    }
}

/* Build the LIVEOUTS of REGION: the set of variables defined inside
   and used outside the REGION.  */

static void
sese_build_liveouts (sese_info_p region, bitmap liveouts)
{
  basic_block bb;

  /* FIXME: We could start iterating form the successor of sese.  */
  FOR_EACH_BB_FN (bb, cfun)
    if (!bb_in_sese_p (bb, region->region))
      sese_build_liveouts_bb (region, liveouts, bb);

  /* FIXME: We could start iterating form the successor of sese.  */
  if (MAY_HAVE_DEBUG_STMTS)
    FOR_EACH_BB_FN (bb, cfun)
      if (!bb_in_sese_p (bb, region->region))
	sese_reset_debug_liveouts_bb (region, liveouts, bb);
}

/* Builds a new SESE region from edges ENTRY and EXIT.  */

sese_info_p
new_sese_info (edge entry, edge exit)
{
  sese_info_p region = XNEW (struct sese_info_t);

  region->region.entry = entry;
  region->region.exit = exit;
  region->loop_nest.create (3);
  region->params.create (3);
  region->rename_map = new rename_map_t;
  region->parameter_rename_map = new parameter_rename_map_t;
  region->copied_bb_map = new bb_map_t;
  region->bbs.create (3);
  region->incomplete_phis.create (3);


  return region;
}

/* Deletes REGION.  */

void
free_sese_info (sese_info_p region)
{
  region->params.release ();
  region->loop_nest.release ();

  for (rename_map_t::iterator it = region->rename_map->begin ();
       it != region->rename_map->end (); ++it)
    (*it).second.release ();

  for (bb_map_t::iterator it = region->copied_bb_map->begin ();
       it != region->copied_bb_map->end (); ++it)
    (*it).second.release ();

  delete region->rename_map;
  delete region->parameter_rename_map;
  delete region->copied_bb_map;

  region->rename_map = NULL;
  region->parameter_rename_map = NULL;
  region->copied_bb_map = NULL;

  region->bbs.release ();
  region->incomplete_phis.release ();

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
  unsigned i;
  bitmap_iterator bi;
  bitmap liveouts = BITMAP_ALLOC (NULL);

  sese_build_liveouts (region, liveouts);

  EXECUTE_IF_SET_IN_BITMAP (liveouts, 0, i, bi)
    if (!virtual_operand_p (ssa_name (i)))
      sese_add_exit_phis_edge (bb, ssa_name (i), false_e, true_e);

  BITMAP_FREE (liveouts);
}

/* Returns the outermost loop in SCOP that contains BB.  */

struct loop *
outermost_loop_in_sese_1 (sese_l &region, basic_block bb)
{
  struct loop *nest;

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

/* Sets the false region of an IF_REGION to REGION.  */

void
if_region_set_false_region (ifsese if_region, sese_info_p region)
{
  basic_block condition = if_region_get_condition_block (if_region);
  edge false_edge = get_false_edge_from_guard_bb (condition);
  basic_block dummy = false_edge->dest;
  edge entry_region = region->region.entry;
  edge exit_region = region->region.exit;
  basic_block before_region = entry_region->src;
  basic_block last_in_region = exit_region->src;
  hashval_t hash = htab_hash_pointer (exit_region);
  loop_exit **slot
    = current_loops->exits->find_slot_with_hash (exit_region, hash, NO_INSERT);

  entry_region->flags = false_edge->flags;
  false_edge->flags = exit_region->flags;

  redirect_edge_pred (entry_region, condition);
  redirect_edge_pred (exit_region, before_region);
  redirect_edge_pred (false_edge, last_in_region);
  redirect_edge_succ (false_edge, single_succ (dummy));
  delete_basic_block (dummy);

  exit_region->flags = EDGE_FALLTHRU;
  recompute_all_dominators ();

  region->region.exit = false_edge;

  free (if_region->false_region);
  if_region->false_region = region;

  if (slot)
    {
      struct loop_exit *loop_exit = ggc_cleared_alloc<struct loop_exit> ();

      memcpy (loop_exit, *((struct loop_exit **) slot),
	      sizeof (struct loop_exit));
      current_loops->exits->clear_slot (slot);

      hashval_t hash = htab_hash_pointer (false_edge);
      slot = current_loops->exits->find_slot_with_hash (false_edge, hash,
							INSERT);
      loop_exit->e = false_edge;
      *slot = loop_exit;
      false_edge->src->loop_father->exits->next = loop_exit;
    }
}

/* Creates an IFSESE with CONDITION on edge ENTRY.  */

static ifsese
create_if_region_on_edge (edge entry, tree condition)
{
  edge e;
  edge_iterator ei;
  sese_info_p sese_region = XNEW (struct sese_info_t);
  sese_info_p true_region = XNEW (struct sese_info_t);
  sese_info_p false_region = XNEW (struct sese_info_t);
  ifsese if_region = XNEW (struct ifsese_s);
  edge exit = create_empty_if_region_on_edge (entry, condition);

  if_region->region = sese_region;
  if_region->region->region.entry = entry;
  if_region->region->region.exit = exit;

  FOR_EACH_EDGE (e, ei, entry->dest->succs)
    {
      if (e->flags & EDGE_TRUE_VALUE)
	{
	  true_region->region.entry = e;
	  true_region->region.exit = single_succ_edge (e->dest);
	  if_region->true_region = true_region;
	}
      else if (e->flags & EDGE_FALSE_VALUE)
	{
	  false_region->region.entry = e;
	  false_region->region.exit = single_succ_edge (e->dest);
	  if_region->false_region = false_region;
	}
    }

  return if_region;
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
  basic_block pred_block = split_edge (region->region.entry);
  ifsese if_region;

  region->region.entry = single_succ_edge (pred_block);
  if_region = create_if_region_on_edge (single_pred_edge (pred_block),
					integer_one_node);
  if_region_set_false_region (if_region, region);

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

  return !chrec_contains_undetermined (scev)
    && (TREE_CODE (scev) != SSA_NAME
	|| !defined_in_sese_p (scev, region))
    && (tree_does_not_contain_chrecs (scev)
	|| evolution_function_is_affine_p (scev));
}

/* Returns the scalar evolution of T in REGION.  Every variable that
   is not defined in the REGION is considered a parameter.  */

tree
scalar_evolution_in_region (const sese_l &region, loop_p loop, tree t)
{
  gimple *def;
  struct loop *def_loop;
  basic_block before = region.entry->src;

  /* SCOP parameters.  */
  if (TREE_CODE (t) == SSA_NAME
      && !defined_in_sese_p (t, region))
    return t;

  if (TREE_CODE (t) != SSA_NAME
      || loop_in_sese_p (loop, region))
    /* FIXME: we would need instantiate SCEV to work on a region, and be more
       flexible wrt. memory loads that may be invariant in the region.  */
    return instantiate_scev (before, loop,
			     analyze_scalar_evolution (loop, t));

  def = SSA_NAME_DEF_STMT (t);
  def_loop = loop_containing_stmt (def);

  if (loop_in_sese_p (def_loop, region))
    {
      t = analyze_scalar_evolution (def_loop, t);
      def_loop = superloop_at_depth (def_loop, loop_depth (loop) + 1);
      t = compute_overall_effect_of_inner_loop (def_loop, t);
      return t;
    }

  bool has_vdefs = false;
  if (invariant_in_sese_p_rec (t, region, &has_vdefs))
    return t;

  /* T variates in REGION.  */
  if (has_vdefs)
    return chrec_dont_know;

  return instantiate_scev (before, loop, t);
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
