/* Single entry single exit control flow regions.
   Copyright (C) 2008-2015 Free Software Foundation, Inc.
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
#include "cfganal.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "tree-eh.h"
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
#include "value-prof.h"
#include "sese.h"
#include "tree-ssa-propagate.h"

/* Record LOOP as occurring in REGION.  */

static void
sese_record_loop (sese_info_p region, loop_p loop)
{
  if (sese_contains_loop (region, loop))
    return;

  bitmap_set_bit (region->loops, loop->num);
  region->loop_nest.safe_push (loop);
}

/* Build the loop nests contained in REGION.  Returns true when the
   operation was successful.  */

void
build_sese_loop_nests (sese_info_p region)
{
  unsigned i;
  basic_block bb;
  struct loop *loop0, *loop1;

  FOR_EACH_BB_FN (bb, cfun)
    if (bb_in_sese_p (bb, region->region))
      {
	struct loop *loop = bb->loop_father;

	/* Only add loops if they are completely contained in the SCoP.  */
	if (loop->header == bb
	    && bb_in_sese_p (loop->latch, region->region))
	  sese_record_loop (region, loop);
      }

  /* Make sure that the loops in the SESE_LOOP_NEST are ordered.  It
     can be the case that an inner loop is inserted before an outer
     loop.  To avoid this, semi-sort once.  */
  FOR_EACH_VEC_ELT (region->loop_nest, i, loop0)
    {
      if (region->loop_nest.length () == i + 1)
	break;

      loop1 = region->loop_nest[i + 1];
      if (loop0->num > loop1->num)
	{
	  region->loop_nest[i] = loop1;
	  region->loop_nest[i + 1] = loop0;
	}
    }
}

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
sese_reset_debug_liveouts_bb (sese_info_p region, bitmap liveouts, basic_block bb)
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
  region->loops = BITMAP_ALLOC (NULL);
  region->loop_nest.create (3);
  region->params.create (3);
  region->rename_map = new rename_map_t;
  region->copied_bb_map = new bb_map_t;
  region->bbs.create (3);
  region->incomplete_phis.create (3);

  return region;
}

/* Deletes REGION.  */

void
free_sese_info (sese_info_p region)
{
  if (region->loops)
    region->loops = BITMAP_ALLOC (NULL);

  region->params.release ();
  region->loop_nest.release ();

  for (rename_map_t::iterator it = region->rename_map->begin ();
       it != region->rename_map->begin (); ++it)
    (*it).second.release ();

  for (bb_map_t::iterator it = region->copied_bb_map->begin ();
       it != region->copied_bb_map->begin (); ++it)
    (*it).second.release ();

  delete region->rename_map;
  delete region->copied_bb_map;

  region->rename_map = NULL;
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

  update_ssa (TODO_update_ssa);

  sese_build_liveouts (region, liveouts);

  EXECUTE_IF_SET_IN_BITMAP (liveouts, 0, i, bi)
    if (!virtual_operand_p (ssa_name (i)))
      sese_add_exit_phis_edge (bb, ssa_name (i), false_e, true_e);

  BITMAP_FREE (liveouts);

  update_ssa (TODO_update_ssa);
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

/* Check if USE is defined in a basic block from where the definition of USE can
 propagate from all the paths.  */

static bool
is_loop_closed_ssa_use (basic_block bb, tree use)
{
  if (TREE_CODE (use) != SSA_NAME)
    return true;

  /* We should not have a rename for virtual operands.  */
  gcc_assert (!virtual_operand_p (use));

  /* For close-phi nodes def always comes from a loop which has a back-edge.  */
  if (bb_contains_loop_close_phi_nodes (bb))
    return true;

  gimple *def = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = gimple_bb (def);
  return (!def_bb
	  || flow_bb_inside_loop_p (def_bb->loop_father, bb));
}

/* Return the number of phi nodes in BB.  */

static int
number_of_phi_nodes (basic_block bb)
{
  int num_phis = 0;
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    num_phis++;
  return num_phis;
}

/* Return true when BB contains loop close phi nodes.  */

bool
bb_contains_loop_close_phi_nodes (basic_block bb)
{
  return single_pred_p (bb)
    && bb->loop_father != single_pred_edge (bb)->src->loop_father;
}

/* Return true when BB contains loop phi nodes.  */

bool
bb_contains_loop_phi_nodes (basic_block bb)
{
  gcc_assert (EDGE_COUNT (bb->preds) <= 2);

  if (bb->preds->length () == 1)
    return false;

  unsigned depth = loop_depth (bb->loop_father);

  edge preds[2] = { (*bb->preds)[0], (*bb->preds)[1] };

  if (depth > loop_depth (preds[0]->src->loop_father)
      || depth > loop_depth (preds[1]->src->loop_father))
    return true;

  /* When one of the edges correspond to the same loop father and other
     doesn't.  */
  if (bb->loop_father != preds[0]->src->loop_father
      && bb->loop_father == preds[1]->src->loop_father)
    return true;

  if (bb->loop_father != preds[1]->src->loop_father
      && bb->loop_father == preds[0]->src->loop_father)
    return true;

  return false;
}

/* Returns true if BB uses name in one of its PHIs.  */

static bool
phi_uses_name (basic_block bb, tree name)
{
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree use_arg = gimple_phi_arg_def (phi, i);
	  if (use_arg == name)
	    return true;
	}
    }
  return false;
}

/* Return true if RENAME (defined in BB) is a valid use in NEW_BB.  The
definition should flow into use, and the use should respect the loop-closed SSA
form.  */

static bool
is_valid_rename (tree rename, basic_block def_bb,
		 basic_block use_bb, bool loop_phi,
		 tree old_name, basic_block old_bb)
{
  /* The def of the rename must either dominate the uses or come from a
     back-edge.  Also the def must respect the loop closed ssa form.  */
  if (!is_loop_closed_ssa_use (use_bb, rename))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] rename not in loop closed ssa:");
	  print_generic_expr (dump_file, rename, 0);
	}
      return false;
    }

  if (dominated_by_p (CDI_DOMINATORS, use_bb, def_bb))
    return true;

  if (bb_contains_loop_phi_nodes (use_bb) && loop_phi)
    {
      /* The loop-header dominates the loop-body.  */
      if (!dominated_by_p (CDI_DOMINATORS, def_bb, use_bb))
	return false;

      /* RENAME would be used in loop-phi.  */
      gcc_assert (number_of_phi_nodes (use_bb));

      /* For definitions coming from back edges, we should check that
	 old_name is used in a loop PHI node.  */
      if (phi_uses_name (old_bb, old_name))
	return true;
    }
  return false;
}

/* Returns the expression associated to OLD_NAME (which is used in OLD_BB), in
   NEW_BB from RENAME_MAP.  LOOP_PHI is true when we want to rename OLD_NAME
   within a loop PHI instruction.  */

static tree
get_rename (rename_map_t *rename_map, basic_block new_bb, tree old_name,
	    basic_block old_bb, bool loop_phi)
{
  gcc_assert (TREE_CODE (old_name) == SSA_NAME);
  vec <tree> *renames = rename_map->get (old_name);

  if (!renames || renames->is_empty ())
    return NULL_TREE;

  if (1 == renames->length ())
    {
      tree rename = (*renames)[0];
      basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (rename));
      if (is_valid_rename (rename, bb, new_bb, loop_phi, old_name, old_bb))
	return rename;
      return NULL_TREE;
    }

  /* More than one renames corresponding to the old_name.  Find the rename for
     which the definition flows into usage at new_bb.  */
  int i;
  tree t1 = NULL_TREE, t2;
  basic_block t1_bb = NULL;
  FOR_EACH_VEC_ELT (*renames, i, t2)
    {
      basic_block t2_bb = gimple_bb (SSA_NAME_DEF_STMT (t2));

      /* Defined in the same basic block as used.  */
      if (t2_bb == new_bb)
	return t2;

      /* NEW_BB and T2_BB are in two unrelated if-clauses.  */
      if (!dominated_by_p (CDI_DOMINATORS, new_bb, t2_bb))
	continue;

      /* Compute the nearest dominator.  */
      if (!t1 || dominated_by_p (CDI_DOMINATORS, t2_bb, t1_bb))
	{
	  t1_bb = t2_bb;
	  t1 = t2;
	}
      //if (is_valid_rename (rename, bb, new_bb, loop_phi, old_name, old_bb))
      //return rename;
    }

  return t1;
}

/* Register in RENAME_MAP the rename tuple (OLD_NAME, EXPR).
   When OLD_NAME and EXPR are the same we assert.  */

static void
set_rename (tree old_name, tree expr, sese_info_p region)
{
  if (dump_file)
    {
      fprintf (dump_file, "\n[codegen] setting rename: old_name = ");
      print_generic_expr (dump_file, old_name, 0);
      fprintf (dump_file, ", new_name = ");
      print_generic_expr (dump_file, expr, 0);
    }

  if (old_name == expr)
    return;

  vec <tree> *renames = region->rename_map->get (old_name);

  if (renames)
    renames->safe_push (expr);
  else
    {
      vec<tree> r;
      r.create (2);
      r.safe_push (expr);
      region->rename_map->put (old_name, r);
    }
}

/* Return an iterator to the instructions comes
   last in the execution order.  Either GSI1 and GSI2 should belong
   to the same basic block or one of their respective basic blocks
   should dominate the other.  */

gimple_stmt_iterator
later_of_the_two (gimple_stmt_iterator gsi1, gimple_stmt_iterator gsi2)
{
  basic_block bb1 = gsi_bb (gsi1);
  basic_block bb2 = gsi_bb (gsi2);

  /* Find the iterator which is the latest.  */
  if (bb1 == bb2)
    {
      /* For empty basic blocks gsis point to the end of the sequence.  Since
	 there is no operator== defined for gimple_stmt_iterator and for gsis
	 not pointing to a valid statement gsi_next would assert.  */
      gimple_stmt_iterator gsi = gsi1;
      do {
	if (gsi_stmt (gsi) == gsi_stmt (gsi2))
	  return gsi2;
	gsi_next (&gsi);
      } while (!gsi_end_p (gsi));

      return gsi1;
    }

  /* Find the basic block closest to the basic block which defines stmt.  */
  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
    return gsi1;

  gcc_assert (dominated_by_p (CDI_DOMINATORS, bb2, bb1));
  return gsi2;
}

/* Insert each statement from SEQ at its earliest insertion p.  */

static void
gsi_insert_earliest (gimple_seq seq, sese_info_p region)
{
  update_modified_stmts (seq);
  sese_l &codegen_region = region->if_region->true_region->region;
  basic_block begin_bb = get_entry_bb (codegen_region);

  /* Inserting the gimple statements in a vector because gimple_seq behave
     in strage ways when inserting the stmts from it into different basic
     blocks one at a time.  */
  auto_vec<gimple *, 3> stmts;
  for (gimple_stmt_iterator gsi = gsi_start (seq); !gsi_end_p (gsi);
       gsi_next (&gsi))
    stmts.safe_push (gsi_stmt (gsi));

  int i;
  gimple *use_stmt;
  FOR_EACH_VEC_ELT (stmts, i, use_stmt)
    {
      gcc_assert (gimple_code (use_stmt) != GIMPLE_PHI);
      gimple_stmt_iterator gsi_def_stmt = gsi_start_bb_nondebug (begin_bb);

      use_operand_p use_p;
      ssa_op_iter op_iter;
      FOR_EACH_SSA_USE_OPERAND (use_p, use_stmt, op_iter, SSA_OP_USE)
	{
	  /* Iterator to the current def of use_p.  For function parameters or
	     anything where def is not found, insert at the beginning of the
	     generated region.  */
	  gimple_stmt_iterator gsi_stmt = gsi_def_stmt;

	  tree op = USE_FROM_PTR (use_p);
	  gimple *stmt = SSA_NAME_DEF_STMT (op);
	  if (stmt && (gimple_code (stmt) != GIMPLE_NOP))
	    gsi_stmt = gsi_for_stmt (stmt);

	  /* For region parameters, insert at the beginning of the generated
	     region.  */
	  if (!bb_in_sese_p (gsi_bb (gsi_stmt), codegen_region))
	    {
	      /* The parameter should have been inserted in the parameter
		 map or it must have a scev.  */
	      gsi_stmt = gsi_def_stmt;
	    }

	  gsi_def_stmt = later_of_the_two (gsi_stmt, gsi_def_stmt);
	}

      if (!gsi_stmt (gsi_def_stmt))
	{
	  gimple_stmt_iterator gsi = gsi_after_labels (gsi_bb (gsi_def_stmt));
	  gsi_insert_before (&gsi, use_stmt, GSI_NEW_STMT);
	}
      else if (gimple_code (gsi_stmt (gsi_def_stmt)) == GIMPLE_PHI)
	{
	  gimple_stmt_iterator bsi
	    = gsi_start_bb_nondebug (gsi_bb (gsi_def_stmt));
	  /* Insert right after the PHI statements.  */
	  gsi_insert_before (&bsi, use_stmt, GSI_NEW_STMT);
	}
      else
	gsi_insert_after (&gsi_def_stmt, use_stmt, GSI_NEW_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] inserting statement: ");
	  print_gimple_stmt (dump_file, use_stmt, 0, TDF_VOPS | TDF_MEMSYMS);
	  print_loops_bb (dump_file, gimple_bb (use_stmt), 0, 3);
	}
    }
}

/* Collect all the operands of NEW_EXPR by recursively visiting each
   operand.  */

static void
collect_all_ssa_names (tree new_expr, vec<tree> *vec_ssa, sese_info_p region)
{

  /* Rename all uses in new_expr.  */
  if (TREE_CODE (new_expr) == SSA_NAME)
    {
      vec_ssa->safe_push (new_expr);
      return;
    }

  /* Iterate over SSA_NAMES in NEW_EXPR.  */
  for (int i = 0; i < (TREE_CODE_LENGTH (TREE_CODE (new_expr))); i++)
    {
      tree op = TREE_OPERAND (new_expr, i);
      collect_all_ssa_names (op, vec_ssa, region);
    }
}

static tree
substitute_ssa_name (tree exp, tree f, tree r)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;
  tree new_tree;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      op0 = substitute_ssa_name (TREE_CHAIN (exp), f, r);
      op1 = substitute_ssa_name (TREE_VALUE (exp), f, r);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else if (code == COMPONENT_REF)
    {
      tree inner;

      /* If this expression is getting a value from a PLACEHOLDER_EXPR
	 and it is the right field, replace it with R.  */
      for (inner = TREE_OPERAND (exp, 0);
	   REFERENCE_CLASS_P (inner);
	   inner = TREE_OPERAND (inner, 0))
	;

      /* The field.  */
      op1 = TREE_OPERAND (exp, 1);

      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && op1 == f)
	return r;

      /* If this expression hasn't been completed let, leave it alone.  */
      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && !TREE_TYPE (inner))
	return exp;

      op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      new_tree
	= fold_build3 (COMPONENT_REF, TREE_TYPE (exp), op0, op1, NULL_TREE);
   }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
	return exp;

      case tcc_declaration:
	if (exp == f)
	  return r;
	else
	  return exp;

      case tcc_expression:
	if (exp == f)
	  return r;

	/* Fall through...  */

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_reference:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    if (exp == f)
	      return r;
	    return exp;

	  case 1:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new_tree = fold_build1 (code, TREE_TYPE (exp), op0);
	    break;

	  case 2:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new_tree = fold_build2 (code, TREE_TYPE (exp), op0, op1);
	    break;

	  case 3:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);
	    op2 = substitute_ssa_name (TREE_OPERAND (exp, 2), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new_tree = fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);
	    break;

	  case 4:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);
	    op2 = substitute_ssa_name (TREE_OPERAND (exp, 2), f, r);
	    op3 = substitute_ssa_name (TREE_OPERAND (exp, 3), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new_tree
	      = fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      case tcc_vl_exp:
      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new_tree) |= TREE_READONLY (exp);

  if (code == INDIRECT_REF || code == ARRAY_REF || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (new_tree) |= TREE_THIS_NOTRAP (exp);

  return new_tree;
}

/* Rename all the operands of NEW_EXPR by recursively visiting each operand.  */

static tree
rename_all_uses (tree new_expr, basic_block new_bb, basic_block old_bb,
		 sese_info_p region)
{
  vec<tree> ssa_names;
  ssa_names.create (2);
  collect_all_ssa_names (new_expr, &ssa_names, region);
  tree t;
  int i;
  FOR_EACH_VEC_ELT (ssa_names, i, t)
    {
      if (tree r = get_rename (region->rename_map, new_bb, t, old_bb, false))
	new_expr = substitute_ssa_name (new_expr, t, r);
      /* else
	 return NULL_TREE;*/
    }

  return new_expr;
}

static tree
get_rename_from_scev (tree old_name, gimple_seq *stmts, loop_p loop,
		      basic_block new_bb, basic_block old_bb,
		      vec<tree> iv_map, sese_info_p region, bool *gloog_error)
{
  tree scev = scalar_evolution_in_region (region->region, loop, old_name);

  /* At this point we should know the exact scev for each
     scalar SSA_NAME used in the scop: all the other scalar
     SSA_NAMEs should have been translated out of SSA using
     arrays with one element.  */
  tree new_expr;
  if (chrec_contains_undetermined (scev))
    {
      *gloog_error = true;
      return build_zero_cst (TREE_TYPE (old_name));
    }

    new_expr = chrec_apply_map (scev, iv_map);

  /* The apply should produce an expression tree containing
     the uses of the new induction variables.  We should be
     able to use new_expr instead of the old_name in the newly
     generated loop nest.  */
  if (chrec_contains_undetermined (new_expr)
      || tree_contains_chrecs (new_expr, NULL))
    {
      *gloog_error = true;
      return build_zero_cst (TREE_TYPE (old_name));
    }

  new_expr = rename_all_uses (new_expr, new_bb, old_bb, region);

  /* Replace the old_name with the new_expr.  */
  return force_gimple_operand (unshare_expr (new_expr), stmts,
			       true, NULL_TREE);
}

/* Renames the scalar uses of the statement COPY, using the
   substitution map RENAME_MAP, inserting the gimplification code at
   GSI_TGT, for the translation REGION, with the original copied
   statement in LOOP, and using the induction variable renaming map
   IV_MAP.  Returns true when something has been renamed.  GLOOG_ERROR
   is set when the code generation cannot continue.  */

static bool
rename_uses (gimple *copy, gimple_stmt_iterator *gsi_tgt,
	     basic_block old_bb, sese_info_p region,
	     loop_p loop, vec<tree> iv_map, bool *gloog_error)
{
  bool changed = false;

  if (is_gimple_debug (copy))
    {
      if (gimple_debug_bind_p (copy))
	gimple_debug_bind_reset_value (copy);
      else if (gimple_debug_source_bind_p (copy))
	return false;
      else
	gcc_unreachable ();

      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n[codegen] renaming uses of stmt: ");
      print_gimple_stmt (dump_file, copy, 0, 0);
    }

  use_operand_p use_p;
  ssa_op_iter op_iter;
  FOR_EACH_SSA_USE_OPERAND (use_p, copy, op_iter, SSA_OP_USE)
    {
      tree old_name = USE_FROM_PTR (use_p);

      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] renaming old_name = ");
	  print_generic_expr (dump_file, old_name, 0);
	}

      if (TREE_CODE (old_name) != SSA_NAME
	  || SSA_NAME_IS_DEFAULT_DEF (old_name))
	continue;

      changed = true;
      tree new_expr = get_rename (region->rename_map, gsi_tgt->bb, old_name,
				  old_bb, false);

      if (new_expr)
	{
	  tree type_old_name = TREE_TYPE (old_name);
	  tree type_new_expr = TREE_TYPE (new_expr);

	  if (dump_file)
	    {
	      fprintf (dump_file, "\n[codegen] from rename_map: new_name = ");
	      print_generic_expr (dump_file, new_expr, 0);
	    }

	  if (type_old_name != type_new_expr
	      || TREE_CODE (new_expr) != SSA_NAME)
	    {
	      tree var = create_tmp_var (type_old_name, "var");

	      if (!useless_type_conversion_p (type_old_name, type_new_expr))
		new_expr = fold_convert (type_old_name, new_expr);

	      gimple_seq stmts;
	      new_expr = force_gimple_operand (new_expr, &stmts, true, var);
	      gsi_insert_earliest (stmts, region);
	    }

	  replace_exp (use_p, new_expr);
	  continue;
	}

      gimple_seq stmts;
      new_expr = get_rename_from_scev (old_name, &stmts, loop, gimple_bb (copy),
				       old_bb, iv_map, region, gloog_error);
      if (!new_expr || *gloog_error)
	return false;

      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] not in rename map, scev: ");
	  print_generic_expr (dump_file, new_expr, 0);
	}

      gsi_insert_earliest (stmts, region);
      replace_exp (use_p, new_expr);

      if (TREE_CODE (new_expr) == INTEGER_CST
	  && is_gimple_assign (copy))
	{
	  tree rhs = gimple_assign_rhs1 (copy);

	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (rhs);
	}

      set_rename (old_name, new_expr, region);
    }

  return changed;
}

/* Returns a basic block that could correspond to where a constant was defined
   in the original code.  In the original code OLD_BB had the definition, we
   need to find which basic block out of the copies of old_bb, in the new
   region, should a definition correspond to if it has to reach BB.  */

static basic_block
get_def_bb_for_const (sese_info_p region, basic_block bb, basic_block old_bb)
{
  vec <basic_block> *bbs = region->copied_bb_map->get (old_bb);

  if (!bbs || bbs->is_empty ())
    return NULL;

  if (1 == bbs->length ())
    return (*bbs)[0];

  int i;
  basic_block b1 = NULL, b2;
  FOR_EACH_VEC_ELT (*bbs, i, b2)
    {
      if (b2 == bb)
	return bb;

      /* BB and B2 are in two unrelated if-clauses.  */
      if (!dominated_by_p (CDI_DOMINATORS, bb, b2))
	continue;

      /* Compute the nearest dominator.  */
      if (!b1 || dominated_by_p (CDI_DOMINATORS, b2, b1))
	b1 = b2;
    }

  gcc_assert (b1);
  return b1;
}

/* LOOP_PHI is true when we want to rename an OP within a loop PHI
   instruction.  */

static tree
get_new_name (sese_info_p region, basic_block new_bb, tree op,
	      basic_block old_bb, bool loop_phi)
{
  if (TREE_CODE (op) == INTEGER_CST
      || TREE_CODE (op) == REAL_CST
      || TREE_CODE (op) == COMPLEX_CST
      || TREE_CODE (op) == VECTOR_CST)
    return op;

  return get_rename (region->rename_map, new_bb, op, old_bb, loop_phi);
}

/* Return a debug location for OP.  */

static location_t
get_loc (tree op)
{
  location_t loc = UNKNOWN_LOCATION;

  if (TREE_CODE (op) == SSA_NAME)
    loc = gimple_location (SSA_NAME_DEF_STMT (op));
  return loc;
}

/* Returns the incoming edges of basic_block BB in the pair.  The first edge is
   the init edge (from outside the loop) and the second one is the back edge
   from the same loop.  */

std::pair<edge, edge>
get_edges (basic_block bb)
{
  std::pair<edge, edge> edges;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (bb->loop_father != e->src->loop_father)
      edges.first = e;
    else
      edges.second = e;
  return edges;
}

/* Copy the PHI arguments from OLD_PHI to the NEW_PHI.  The arguments to NEW_PHI
   must be found unless they can be POSTPONEd for later.  */

void
copy_loop_phi_args (gphi *old_phi, init_back_edge_pair_t &ibp_old_bb,
		    gphi *new_phi, init_back_edge_pair_t &ibp_new_bb,
		    sese_info_p region, bool postpone)
{
  gcc_assert (gimple_phi_num_args (old_phi) == gimple_phi_num_args (new_phi));

  basic_block new_bb = gimple_bb (new_phi);
  for (unsigned i = 0; i < gimple_phi_num_args (old_phi); i++)
    {
      edge e;
      if (gimple_phi_arg_edge (old_phi, i) == ibp_old_bb.first)
	e = ibp_new_bb.first;
      else
	e = ibp_new_bb.second;

      tree old_name = gimple_phi_arg_def (old_phi, i);
      tree new_name = get_new_name (region, new_bb, old_name,
				    gimple_bb (old_phi), true);
      if (new_name)
	{
	  add_phi_arg (new_phi, new_name, e, get_loc (old_name));
	  continue;
	}

      gimple *old_def_stmt = SSA_NAME_DEF_STMT (old_name);
      if (!old_def_stmt || gimple_code (old_def_stmt) == GIMPLE_NOP)
      /* If the phi arg was a function arg, or wasn't defined, just use the old
	 name.  */
	add_phi_arg (new_phi, old_name, e, get_loc (old_name));
      else if (postpone)
	{
	  /* Postpone code gen for later for those back-edges we don't have the
	     names yet.  */
	  region->incomplete_phis.safe_push (std::make_pair (old_phi, new_phi));
	  if (dump_file)
	    fprintf (dump_file, "\n[codegen] postpone loop phi nodes: ");
	}
      else
	/* Either we should add the arg to phi or, we should postpone.  */
	gcc_unreachable ();
    }
}

/* Copy loop phi nodes from BB to NEW_BB.  */

static bool
copy_loop_phi_nodes (basic_block bb, basic_block new_bb, sese_info_p region)
{
  if (dump_file)
    fprintf (dump_file, "\n[codegen] copying loop phi nodes in bb_%d.",
	     new_bb->index);

  /* Loop phi nodes should have only two arguments.  */
  gcc_assert (2 == EDGE_COUNT (bb->preds));

  /* First edge is the init edge and second is the back edge.  */
  init_back_edge_pair_t ibp_old_bb = get_edges (bb);

  /* First edge is the init edge and second is the back edge.  */
  init_back_edge_pair_t ibp_new_bb = get_edges (new_bb);

  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;
      if (is_gimple_reg (res) && scev_analyzable_p (res, region->region))
	continue;

      gphi *new_phi = create_phi_node (SSA_NAME_VAR (res), new_bb);
      tree new_res = create_new_def_for (res, new_phi,
					 gimple_phi_result_ptr (new_phi));
      set_rename (res, new_res, region);
      copy_loop_phi_args (phi, ibp_old_bb, new_phi, ibp_new_bb, region, true);
      update_stmt (new_phi);
    }

  return true;
}

/* Return the init value of PHI, the value coming from outside the loop.  */

static tree
get_loop_init_value (gphi *phi)
{

  loop_p loop = gimple_bb (phi)->loop_father;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    if (e->src->loop_father != loop)
      return gimple_phi_arg_def (phi, e->dest_idx);

  return NULL_TREE;
}

/* Find the init value (the value which comes from outside the loop), of one of
   the operands of DEF which is defined by a loop phi.  */

static tree
find_init_value (gimple *def)
{
  if (gimple_code (def) == GIMPLE_PHI)
    return get_loop_init_value (as_a <gphi*> (def));

  if (gimple_vuse (def))
    return NULL_TREE;

  ssa_op_iter iter;
  use_operand_p use_p;
  FOR_EACH_SSA_USE_OPERAND (use_p, def, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      if (TREE_CODE (use) == SSA_NAME)
	{
	  if (tree res = find_init_value (SSA_NAME_DEF_STMT (use)))
	    return res;
	}
    }

  return NULL_TREE;
}

/* Return the init value, the value coming from outside the loop.  */

static tree
find_init_value_close_phi (gphi *phi)
{
  gcc_assert (gimple_phi_num_args (phi) == 1);
  tree use_arg = gimple_phi_arg_def (phi, 0);
  gimple *def = SSA_NAME_DEF_STMT (use_arg);
  return find_init_value (def);
}

/* Copy all the loop-close phi args from BB to NEW_BB.  */

bool
copy_loop_close_phi_args (basic_block old_bb, basic_block new_bb,
			  sese_info_p region, bool postpone)
{
  /* The successor of bb having close phi should be a merge of the diamond
     inserted to guard the loop during codegen.  */
  basic_block close_phi_merge_bb = single_succ (new_bb);

  for (gphi_iterator psi = gsi_start_phis (old_bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;

      if (is_gimple_reg (res) && scev_analyzable_p (res, region->region))
	/* Loop close phi nodes should not be scev_analyzable_p.  */
	gcc_unreachable ();

      gphi *new_phi = create_phi_node (SSA_NAME_VAR (res), new_bb);
      tree new_res = create_new_def_for (res, new_phi,
					 gimple_phi_result_ptr (new_phi));
      set_rename (res, new_res, region);

      tree old_name = gimple_phi_arg_def (phi, 0);
      tree new_name = get_new_name (region, new_bb, old_name, old_bb, false);

      /* Predecessor basic blocks of a loop close phi should have been code
	 generated before.  FIXME: This is fixable by merging PHIs from inner
	 loops as well.  When we are looking at close-phi of an outer loop, and
	 arguments flowing out of inner loop as not been collected by the
	 outer-loop close phi, we will hit this situation.  For now we just bail
	 out.  See: gfortran.dg/graphite/interchange-3.f90.  */
      if (!new_name)
	return false;

      add_phi_arg (new_phi, new_name, single_pred_edge (new_bb),
		   get_loc (old_name));
      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] Adding loop-closed phi: ");
	  print_gimple_stmt (dump_file, new_phi, 0, 0);
	}

      update_stmt (new_phi);

      /* When there is no loop guard around this codegenerated loop, there is no
	 need to collect the close-phi arg.  */
      if (2 != EDGE_COUNT (close_phi_merge_bb->preds))
	continue;

      /* Add a PHI in the close_phi_merge_bb for each close phi of the loop.  */
      tree init = find_init_value_close_phi (new_phi);

      /* A close phi must come from a loop-phi having an init value.  */
      if (!init)
	{
	  gcc_assert (postpone);
	  region->incomplete_phis.safe_push (std::make_pair (phi, new_phi));
	  if (dump_file)
	    {
	      fprintf (dump_file, "\n[codegen] postpone close phi nodes: ");
	      print_gimple_stmt (dump_file, new_phi, 0, 0);
	    }
	  continue;
	}

      gphi *merge_phi = create_phi_node (SSA_NAME_VAR (res),
					 close_phi_merge_bb);
      tree merge_res = create_new_def_for (res, merge_phi,
					   gimple_phi_result_ptr (merge_phi));
      set_rename (res, merge_res, region);

      edge from_loop = single_succ_edge (new_bb);
      add_phi_arg (merge_phi, new_res, from_loop, get_loc (old_name));

      /* The edge coming from loop guard.  */
      edge other = from_loop == (*close_phi_merge_bb->preds)[0]
	? (*close_phi_merge_bb->preds)[1] : (*close_phi_merge_bb->preds)[0];

      add_phi_arg (merge_phi, init, other, get_loc (old_name));
      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] Adding guard-phi: ");
	  print_gimple_stmt (dump_file, merge_phi, 0, 0);
	}

      update_stmt (new_phi);
    }

  return true;
}

/* Copy loop close phi nodes from BB to NEW_BB.  */

static bool
copy_loop_close_phi_nodes (basic_block old_bb, basic_block new_bb,
			   sese_info_p region)
{
  if (dump_file)
    fprintf (dump_file, "\n[codegen] copying loop closed phi nodes in bb_%d.",
	     new_bb->index);
  /* Loop close phi nodes should have only one argument.  */
  gcc_assert (1 == EDGE_COUNT (old_bb->preds));

  return copy_loop_close_phi_args (old_bb, new_bb, region, true);
}


/* Add NEW_NAME as the ARGNUM-th arg of NEW_PHI which is in NEW_BB.
   DOMINATING_PRED is the predecessor basic block of OLD_BB which dominates the
   other pred of OLD_BB as well.  If no such basic block exists then it is NULL.
   NON_DOMINATING_PRED is a pred which does not dominate OLD_BB, it cannot be
   NULL.

   Case1: OLD_BB->preds {BB1, BB2} and BB1 does not dominate BB2 and vice versa.
   In this case DOMINATING_PRED = NULL.

   Case2: OLD_BB->preds {BB1, BB2} and BB1 dominates BB2.

   Returns true on successful copy of the args, false otherwise.  */

static bool
add_phi_arg_for_new_expr (tree old_phi_args[2], tree new_phi_args[2],
			  edge old_bb_dominating_edge,
			  edge old_bb_non_dominating_edge,
			  gphi *phi, gphi *new_phi,
			  basic_block new_bb, sese_info_p region)
{
  basic_block def_pred[2];
  int not_found_bb_index = -1;
  for (int i = 0; i < 2; i++)
    {
      /* If the corresponding def_bb could not be found the entry will be
	 NULL.  */
      if (TREE_CODE (old_phi_args[i]) == INTEGER_CST)
	def_pred[i] = get_def_bb_for_const (region, new_bb,
					 gimple_phi_arg_edge (phi, i)->src);
      else
	def_pred[i] = gimple_bb (SSA_NAME_DEF_STMT (new_phi_args[i]));
      if (!def_pred[i])
	{
	  gcc_assert (not_found_bb_index == -1);
	  not_found_bb_index = i;
	}
    }

  /* Here we are pattern matching on the structure of CFG w.r.t. old one.  */
  if (old_bb_dominating_edge)
    {
      return false;
      basic_block new_pred1 = (*new_bb->preds)[0]->src;
      basic_block new_pred2 = (*new_bb->preds)[1]->src;
      vec <basic_block> *bbs
	= region->copied_bb_map->get (old_bb_non_dominating_edge->src);
      gcc_assert (bbs);
      basic_block new_pred = NULL;
      basic_block b;
      int i;
      FOR_EACH_VEC_ELT (*bbs, i, b)
	if (new_pred1 == b || new_pred2 == b)
	  {
	    gcc_assert (!new_pred);
	    new_pred = b;
	  }

      gcc_assert (new_pred);

      edge new_non_dominating_edge = find_edge (new_pred, new_bb);
      /* By the process of elimination we first insert insert phi-edge for
	 non-dominating pred which is computed above and then we insert the
	 remaining one.  */
      int inserted_edge = 0;
      for (; inserted_edge < 2; inserted_edge++)
	{
	  edge new_bb_pred_edge = gimple_phi_arg_edge (phi, inserted_edge);
	  if (new_non_dominating_edge == new_bb_pred_edge)
	    {
	      add_phi_arg (new_phi, new_phi_args[inserted_edge],
			   new_non_dominating_edge,
			   get_loc (old_phi_args[inserted_edge]));
	      break;
	    }
	}

      int edge_dominating = 0;
      if (inserted_edge == 0)
	edge_dominating = 1;

      edge new_dominating_edge = NULL;
      for (int i; i < 2; i++)
	{
	  edge e = gimple_phi_arg_edge (new_phi, i);
	  if (e != new_non_dominating_edge)
	    new_dominating_edge = e;
	}

      add_phi_arg (new_phi, new_phi_args[edge_dominating], new_dominating_edge,
		   get_loc (old_phi_args[inserted_edge]));
    }
  else
    {
      /* Classic diamond structure: both edges are non-dominating.  We need to
	 find one unique edge then the other can be found be elimination.  If
	 any definition (def_pred) dominates both the preds of new_bb then we
	 bail out.  Entries of def_pred maybe NULL, in that case we must
	 uniquely find pred with help of only one entry.  */
      edge new_e[2] = { NULL, NULL };
      for (int i = 0; i < 2; i++)
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, new_bb->preds)
	    if (def_pred[i]
		&& dominated_by_p (CDI_DOMINATORS, e->src, def_pred[i]))
	      {
		if (new_e[i])
		  /* We do not know how to handle the case when def_pred
		     dominates more than a predecessor.  */
		  return false;
		new_e[i] = e;
	      }
	}

      gcc_assert (new_e[0] || new_e[1]);

      /* Find the other edge by process of elimination.  */
      if (not_found_bb_index != -1)
	{
	  gcc_assert (!new_e[not_found_bb_index]);
	  int found_bb_index = not_found_bb_index == 1 ? 0 : 1;
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, new_bb->preds)
	    {
	      if (new_e[found_bb_index] == e)
		continue;
	      new_e[not_found_bb_index] = e;
	    }
	}

      /* Add edges to phi args.  */
      for (int i = 0; i < 2; i++)
	add_phi_arg (new_phi, new_phi_args[i], new_e[i],
		     get_loc (old_phi_args[i]));
    }

  return true;
}

/* Copy the arguments of cond-phi node PHI, to NEW_PHI in the codegenerated
   region.  If postpone is true and it isn't possible to copy any arg of PHI,
   the PHI is added to the REGION->INCOMPLETE_PHIS to be codegenerated
   later.  Returns false if the copying was unsuccessful.  */

bool
copy_cond_phi_args (gphi *phi, gphi *new_phi, vec<tree> iv_map,
		    sese_info_p region, bool postpone)
{
  if (dump_file)
    fprintf (dump_file, "\n[codegen] copying cond phi args: ");
  gcc_assert (2 == gimple_phi_num_args (phi));

  basic_block new_bb = gimple_bb (new_phi);
  loop_p loop = gimple_bb (phi)->loop_father;

  basic_block old_bb = gimple_bb (phi);
  edge old_bb_non_dominating_edge = NULL, old_bb_dominating_edge = NULL;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, old_bb->preds)
    if (!dominated_by_p (CDI_DOMINATORS, old_bb, e->src))
      old_bb_non_dominating_edge = e;
    else
      old_bb_dominating_edge = e;

  gcc_assert (!dominated_by_p (CDI_DOMINATORS, old_bb,
			       old_bb_non_dominating_edge->src));

  tree new_phi_args[2];
  tree old_phi_args[2];

  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree old_name = gimple_phi_arg_def (phi, i);
      tree new_name = get_new_name (region, new_bb, old_name, old_bb, false);
      old_phi_args[i] = old_name;
      if (new_name)
	{
	  new_phi_args [i] = new_name;
	  continue;
	}

      if (vec_find (region->params, old_name))
	{
	  new_phi_args [i] = old_name;
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "\n[codegen] parameter argument to phi, new_expr: ");
	      print_gimple_stmt (dump_file, new_phi, 0, 0);
	    }
	  continue;
	}

      /* If the phi-arg is scev-analyzeable but only in the first stage.  */
      if (postpone && is_gimple_reg (old_name)
	  && scev_analyzable_p (old_name, region->region))
	{
	  gimple_seq stmts;
	  bool gloog_error = false;
	  tree new_expr
	    = get_rename_from_scev (old_name, &stmts, loop, new_bb,
				    old_bb, iv_map, region, &gloog_error);
	  if (gloog_error)
	    return false;

	  gcc_assert (new_expr);
	  if (dump_file)
	    {
	      fprintf (dump_file, "\n[codegen] scev analyzeable, new_expr: ");
	      print_generic_expr (dump_file, new_expr, 0);
	    }
	  gsi_insert_earliest (stmts, region);
	  new_phi_args [i] = new_name;
	  continue;
	}

      gimple *old_def_stmt = SSA_NAME_DEF_STMT (old_name);
      if (!old_def_stmt || gimple_code (old_def_stmt) == GIMPLE_NOP)
	/* If the phi arg was a function arg, or wasn't defined, just use the
	   old name.  */
	gcc_unreachable ();
      else if (postpone)
	{
	  /* Postpone code gen for later for back-edges.  */
	  region->incomplete_phis.safe_push (std::make_pair (phi, new_phi));

	  if (dump_file)
	    {
	      fprintf (dump_file, "\n[codegen] postpone cond phi nodes: ");
	      print_gimple_stmt (dump_file, new_phi, 0, 0);
	    }

	  new_phi_args [i] = NULL_TREE;
	  continue;
	}
      else
	gcc_unreachable ();
    }

  return add_phi_arg_for_new_expr (old_phi_args, new_phi_args,
				   old_bb_dominating_edge,
				   old_bb_non_dominating_edge,
				   phi, new_phi, new_bb, region);
}

/* Copy cond phi nodes from BB to NEW_BB.  */

static bool
copy_cond_phi_nodes (basic_block bb, basic_block new_bb, vec<tree> iv_map,
		     sese_info_p region)
{

  gcc_assert (!bb_contains_loop_close_phi_nodes (bb));

  if (dump_file)
    fprintf (dump_file, "\n[codegen] copying cond phi nodes in bb_%d:",
	     new_bb->index);

  /* Cond phi nodes should have exactly two arguments.  */
  gcc_assert (2 == EDGE_COUNT (bb->preds));

  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;
      if (is_gimple_reg (res) && scev_analyzable_p (res, region->region))
	/* Cond phi nodes should not be scev_analyzable_p.  */
	gcc_unreachable ();

      gphi *new_phi = create_phi_node (SSA_NAME_VAR (res), new_bb);
      tree new_res = create_new_def_for (res, new_phi,
					 gimple_phi_result_ptr (new_phi));
      set_rename (res, new_res, region);

      if (!copy_cond_phi_args (phi, new_phi, iv_map, region, true))
	return false;

      update_stmt (new_phi);
    }

  return true;
}

/* Return true if STMT should be copied from region to the
   new code-generated region.  LABELs, CONDITIONS, induction-variables
   and region parameters need not be copied.  */

static bool
should_copy_to_new_region (gimple *stmt, sese_info_p region)
{
  /* Do not copy labels or conditions.  */
  if (gimple_code (stmt) == GIMPLE_LABEL
      || gimple_code (stmt) == GIMPLE_COND)
    return false;

  tree lhs;
  /* Do not copy induction variables.  */
  if (is_gimple_assign (stmt)
      && (lhs = gimple_assign_lhs (stmt))
      && TREE_CODE (lhs) == SSA_NAME
      && is_gimple_reg (lhs)
      && scev_analyzable_p (lhs, region->region))
    return false;

  return true;
}

/* Create new names for all the definitions created by COPY and
   add replacement mappings for each new name.  */

static void
set_rename_for_each_def (gimple *stmt, sese_info_p region)
{
  def_operand_p def_p;
  ssa_op_iter op_iter;
  FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, op_iter, SSA_OP_ALL_DEFS)
    {
      tree old_name = DEF_FROM_PTR (def_p);
      tree new_name = create_new_def_for (old_name, stmt, def_p);
      set_rename (old_name, new_name, region);
    }
}

/* Duplicates the statements of basic block BB into basic block NEW_BB
   and compute the new induction variables according to the IV_MAP.
   GLOOG_ERROR is set when the code generation cannot continue.  */
static bool
graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb,
				vec<tree> iv_map, sese_info_p region,
				bool *gloog_error)
{
  /* Iterator poining to the place where new statement (s) will be inserted.  */
  gimple_stmt_iterator gsi_tgt = gsi_last_bb (new_bb);

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!should_copy_to_new_region (stmt, region))
	continue;

      /* Create a new copy of STMT and duplicate STMT's virtual
	 operands.  */
      gimple *copy = gimple_copy (stmt);
      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "\n[codegen] inserting statement: ");
	  print_gimple_stmt (dump_file, copy, 0, 0);
	}

      maybe_duplicate_eh_stmt (copy, stmt);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* Crete new names for each def in the copied stmt.  */
      set_rename_for_each_def (copy, region);

      loop_p loop = bb->loop_father;
      if (rename_uses (copy, &gsi_tgt, bb, region, loop, iv_map, gloog_error))
	{
	  fold_stmt_inplace (&gsi_tgt);
	  gcc_assert (gsi_stmt (gsi_tgt) == copy);
	}

      if (*gloog_error)
	return false;

      update_stmt (copy);
    }

  return true;
}

/* Copies BB and includes in the copied BB all the statements that can
   be reached following the use-def chains from the memory accesses,
   and returns the next edge following this new block.  GLOOG_ERROR is
   set when the code generation cannot continue.  */

edge
copy_bb_and_scalar_dependences (basic_block bb, sese_info_p region,
				edge next_e, vec<tree> iv_map,
				bool *codegen_err)
{
  int num_phis = number_of_phi_nodes (bb);

  if (region->copied_bb_map->get (bb))
    {
      /* FIXME: We do not handle inner loop unrolling when the inner loop has
	 phi-nodes.  In that case inner loop will be copied multiple times
	 outside the region.  */
      if (num_phis)
	{
	  *codegen_err = true;
	  return NULL;
	}
    }

  basic_block new_bb = split_edge (next_e);
  if (num_phis > 0 && bb_contains_loop_phi_nodes (bb))
    {
      basic_block phi_bb = next_e->dest->loop_father->header;

      /* At this point we are unable to codegenerate by still preserving the SSA
	 structure because maybe the loop is completely unrolled and the PHIs
	 and cross-bb scalar dependencies are untrackable w.r.t. the original
	 code.  See gfortran.dg/graphite/pr29832.f90.  */
      if (EDGE_COUNT (bb->preds) != EDGE_COUNT (phi_bb->preds))
	{
	  *codegen_err = true;
	  return NULL;
	}

      if (dump_file)
	fprintf (dump_file, "\n[codegen] bb_%d contains loop phi nodes",
		 bb->index);
      if (!copy_loop_phi_nodes (bb, phi_bb, region))
	{
	  *codegen_err = true;
	  return NULL;
	}
    }
  else if (bb_contains_loop_close_phi_nodes (bb))
    {
      if (dump_file)
	fprintf (dump_file, "\n[codegen] bb_%d contains close phi nodes",
		 bb->index);

      /* Make sure that NEW_BB is the loop->exit->dest.  */
      edge e = single_pred_edge (new_bb);
      basic_block phi_bb = new_bb;
      if (e->src->loop_father == e->dest->loop_father)
	{
	  /* This is one of the places which shows preserving original structure
	     is not always possible, as we may need to insert close PHI for a
	     loop where the latch does not have any mapping, or the mapping is
	     ambiguous.  */
	  basic_block old_loop_bb = single_pred_edge (bb)->src;
	  vec <basic_block> *bbs = region->copied_bb_map->get (old_loop_bb);
	  if (!bbs || bbs->length () != 1)
	    {
	      *codegen_err = true;
	      return NULL;
	    }

	  basic_block new_loop_bb = (*bbs)[0];
	  loop_p new_loop = new_loop_bb->loop_father;
	  phi_bb = single_exit (new_loop)->dest;
	  e = single_pred_edge (phi_bb);
	}

      gcc_assert (e->src->loop_father != e->dest->loop_father);

      if (!copy_loop_close_phi_nodes (bb, phi_bb, region))
	{
	  *codegen_err = true;
	  return NULL;
	}
    }
  else if (num_phis > 0)
    {
      if (dump_file)
	fprintf (dump_file, "\n[codegen] bb_%d contains cond phi nodes",
		 bb->index);

      basic_block phi_bb = single_pred (new_bb);
      loop_p loop_father = new_bb->loop_father;

      /* Move back until we find the block with two predecessors.  */
      while (single_pred_p (phi_bb))
	phi_bb = single_pred_edge (phi_bb)->src;

      /* If a corresponding merge-point was not found, then abort codegen.  */
      if (phi_bb->loop_father != loop_father
	  || !copy_cond_phi_nodes (bb, phi_bb, iv_map, region))
	{
	  *codegen_err = true;
	  return NULL;
	}
    }

  if (dump_file)
    fprintf (dump_file, "\n[codegen] copying from bb_%d to bb_%d",
	     bb->index, new_bb->index);

  vec <basic_block> *copied_bbs = region->copied_bb_map->get (bb);
  if (copied_bbs)
    copied_bbs->safe_push (new_bb);
  else
    {
      vec<basic_block> bbs;
      bbs.create (2);
      bbs.safe_push (new_bb);
      region->copied_bb_map->put (bb, bbs);
    }

  if (!graphite_copy_stmts_from_block (bb, new_bb, iv_map, region, codegen_err))
    {
      *codegen_err = true;
      return NULL;
    }

  return single_succ_edge (new_bb);
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

      memcpy (loop_exit, *((struct loop_exit **) slot), sizeof (struct loop_exit));
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
invariant_in_sese_p_rec (tree t, sese_l &region, bool *has_vdefs)
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

/* Returns the scalar evolution of T in REGION.  Every variable that
   is not defined in the REGION is considered a parameter.  */

tree
scalar_evolution_in_region (sese_l &region, loop_p loop, tree t)
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
