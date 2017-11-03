/* High-level loop manipulation functions.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

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
#include "tree-pass.h"	/* ??? for TODO_update_ssa but this isn't a pass.  */
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-inline.h"

/* All bitmaps for rewriting into loop-closed SSA go on this obstack,
   so that we can free them all at once.  */
static bitmap_obstack loop_renamer_obstack;

/* Creates an induction variable with value BASE + STEP * iteration in LOOP.
   It is expected that neither BASE nor STEP are shared with other expressions
   (unless the sharing rules allow this).  Use VAR as a base var_decl for it
   (if NULL, a new temporary will be created).  The increment will occur at
   INCR_POS (after it if AFTER is true, before it otherwise).  INCR_POS and
   AFTER can be computed using standard_iv_increment_position.  The ssa versions
   of the variable before and after increment will be stored in VAR_BEFORE and
   VAR_AFTER (unless they are NULL).  */

void
create_iv (tree base, tree step, tree var, struct loop *loop,
	   gimple_stmt_iterator *incr_pos, bool after,
	   tree *var_before, tree *var_after)
{
  gassign *stmt;
  gphi *phi;
  tree initial, step1;
  gimple_seq stmts;
  tree vb, va;
  enum tree_code incr_op = PLUS_EXPR;
  edge pe = loop_preheader_edge (loop);

  if (var != NULL_TREE)
    {
      vb = make_ssa_name (var);
      va = make_ssa_name (var);
    }
  else
    {
      vb = make_temp_ssa_name (TREE_TYPE (base), NULL, "ivtmp");
      va = make_temp_ssa_name (TREE_TYPE (base), NULL, "ivtmp");
    }
  if (var_before)
    *var_before = vb;
  if (var_after)
    *var_after = va;

  /* For easier readability of the created code, produce MINUS_EXPRs
     when suitable.  */
  if (TREE_CODE (step) == INTEGER_CST)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (step)))
	{
	  step1 = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	  if (tree_int_cst_lt (step1, step))
	    {
	      incr_op = MINUS_EXPR;
	      step = step1;
	    }
	}
      else
	{
	  bool ovf;

	  if (!tree_expr_nonnegative_warnv_p (step, &ovf)
	      && may_negate_without_overflow_p (step))
	    {
	      incr_op = MINUS_EXPR;
	      step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	    }
	}
    }
  if (POINTER_TYPE_P (TREE_TYPE (base)))
    {
      if (TREE_CODE (base) == ADDR_EXPR)
	mark_addressable (TREE_OPERAND (base, 0));
      step = convert_to_ptrofftype (step);
      if (incr_op == MINUS_EXPR)
	step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
      incr_op = POINTER_PLUS_EXPR;
    }
  /* Gimplify the step if necessary.  We put the computations in front of the
     loop (i.e. the step should be loop invariant).  */
  step = force_gimple_operand (step, &stmts, true, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (pe, stmts);

  stmt = gimple_build_assign (va, incr_op, vb, step);
  if (after)
    gsi_insert_after (incr_pos, stmt, GSI_NEW_STMT);
  else
    gsi_insert_before (incr_pos, stmt, GSI_NEW_STMT);

  initial = force_gimple_operand (base, &stmts, true, var);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (pe, stmts);

  phi = create_phi_node (vb, loop->header);
  add_phi_arg (phi, initial, loop_preheader_edge (loop), UNKNOWN_LOCATION);
  add_phi_arg (phi, va, loop_latch_edge (loop), UNKNOWN_LOCATION);
}

/* Return the innermost superloop LOOP of USE_LOOP that is a superloop of
   both DEF_LOOP and USE_LOOP.  */

static inline struct loop *
find_sibling_superloop (struct loop *use_loop, struct loop *def_loop)
{
  unsigned ud = loop_depth (use_loop);
  unsigned dd = loop_depth (def_loop);
  gcc_assert (ud > 0 && dd > 0);
  if (ud > dd)
    use_loop = superloop_at_depth (use_loop, dd);
  if (ud < dd)
    def_loop = superloop_at_depth (def_loop, ud);
  while (loop_outer (use_loop) != loop_outer (def_loop))
    {
      use_loop = loop_outer (use_loop);
      def_loop = loop_outer (def_loop);
      gcc_assert (use_loop && def_loop);
    }
  return use_loop;
}

/* DEF_BB is a basic block containing a DEF that needs rewriting into
   loop-closed SSA form.  USE_BLOCKS is the set of basic blocks containing
   uses of DEF that "escape" from the loop containing DEF_BB (i.e. blocks in
   USE_BLOCKS are dominated by DEF_BB but not in the loop father of DEF_B).
   ALL_EXITS[I] is the set of all basic blocks that exit loop I.

   Compute the subset of LOOP_EXITS that exit the loop containing DEF_BB
   or one of its loop fathers, in which DEF is live.  This set is returned
   in the bitmap LIVE_EXITS.

   Instead of computing the complete livein set of the def, we use the loop
   nesting tree as a form of poor man's structure analysis.  This greatly
   speeds up the analysis, which is important because this function may be
   called on all SSA names that need rewriting, one at a time.  */

static void
compute_live_loop_exits (bitmap live_exits, bitmap use_blocks,
			 bitmap *loop_exits, basic_block def_bb)
{
  unsigned i;
  bitmap_iterator bi;
  struct loop *def_loop = def_bb->loop_father;
  unsigned def_loop_depth = loop_depth (def_loop);
  bitmap def_loop_exits;

  /* Normally the work list size is bounded by the number of basic
     blocks in the largest loop.  We don't know this number, but we
     can be fairly sure that it will be relatively small.  */
  auto_vec<basic_block> worklist (MAX (8, n_basic_blocks_for_fn (cfun) / 128));

  EXECUTE_IF_SET_IN_BITMAP (use_blocks, 0, i, bi)
    {
      basic_block use_bb = BASIC_BLOCK_FOR_FN (cfun, i);
      struct loop *use_loop = use_bb->loop_father;
      gcc_checking_assert (def_loop != use_loop
			   && ! flow_loop_nested_p (def_loop, use_loop));
      if (! flow_loop_nested_p (use_loop, def_loop))
	use_bb = find_sibling_superloop (use_loop, def_loop)->header;
      if (bitmap_set_bit (live_exits, use_bb->index))
	worklist.safe_push (use_bb);
    }

  /* Iterate until the worklist is empty.  */
  while (! worklist.is_empty ())
    {
      edge e;
      edge_iterator ei;

      /* Pull a block off the worklist.  */
      basic_block bb = worklist.pop ();

      /* Make sure we have at least enough room in the work list
	 for all predecessors of this block.  */
      worklist.reserve (EDGE_COUNT (bb->preds));

      /* For each predecessor block.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  basic_block pred = e->src;
	  struct loop *pred_loop = pred->loop_father;
	  unsigned pred_loop_depth = loop_depth (pred_loop);
	  bool pred_visited;

	  /* We should have met DEF_BB along the way.  */
	  gcc_assert (pred != ENTRY_BLOCK_PTR_FOR_FN (cfun));

	  if (pred_loop_depth >= def_loop_depth)
	    {
	      if (pred_loop_depth > def_loop_depth)
		pred_loop = superloop_at_depth (pred_loop, def_loop_depth);
	      /* If we've reached DEF_LOOP, our train ends here.  */
	      if (pred_loop == def_loop)
		continue;
	    }
	  else if (! flow_loop_nested_p (pred_loop, def_loop))
	    pred = find_sibling_superloop (pred_loop, def_loop)->header;

	  /* Add PRED to the LIVEIN set.  PRED_VISITED is true if
	     we had already added PRED to LIVEIN before.  */
	  pred_visited = !bitmap_set_bit (live_exits, pred->index);

	  /* If we have visited PRED before, don't add it to the worklist.
	     If BB dominates PRED, then we're probably looking at a loop.
	     We're only interested in looking up in the dominance tree
	     because DEF_BB dominates all the uses.  */
	  if (pred_visited || dominated_by_p (CDI_DOMINATORS, pred, bb))
	    continue;

	  worklist.quick_push (pred);
	}
    }

  def_loop_exits = BITMAP_ALLOC (&loop_renamer_obstack);
  for (struct loop *loop = def_loop;
       loop != current_loops->tree_root;
       loop = loop_outer (loop))
    bitmap_ior_into (def_loop_exits, loop_exits[loop->num]);
  bitmap_and_into (live_exits, def_loop_exits);
  BITMAP_FREE (def_loop_exits);
}

/* Add a loop-closing PHI for VAR in basic block EXIT.  */

static void
add_exit_phi (basic_block exit, tree var)
{
  gphi *phi;
  edge e;
  edge_iterator ei;

  /* Check that at least one of the edges entering the EXIT block exits
     the loop, or a superloop of that loop, that VAR is defined in.  */
  if (flag_checking)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (var);
      basic_block def_bb = gimple_bb (def_stmt);
      FOR_EACH_EDGE (e, ei, exit->preds)
	{
	  struct loop *aloop = find_common_loop (def_bb->loop_father,
						 e->src->loop_father);
	  if (!flow_bb_inside_loop_p (aloop, e->dest))
	    break;
	}
      gcc_assert (e);
    }

  phi = create_phi_node (NULL_TREE, exit);
  create_new_def_for (var, phi, gimple_phi_result_ptr (phi));
  FOR_EACH_EDGE (e, ei, exit->preds)
    add_phi_arg (phi, var, e, UNKNOWN_LOCATION);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, ";; Created LCSSA PHI: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
    }
}

/* Add exit phis for VAR that is used in LIVEIN.
   Exits of the loops are stored in LOOP_EXITS.  */

static void
add_exit_phis_var (tree var, bitmap use_blocks, bitmap *loop_exits)
{
  unsigned index;
  bitmap_iterator bi;
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
  bitmap live_exits = BITMAP_ALLOC (&loop_renamer_obstack);

  gcc_checking_assert (! bitmap_bit_p (use_blocks, def_bb->index));

  compute_live_loop_exits (live_exits, use_blocks, loop_exits, def_bb);

  EXECUTE_IF_SET_IN_BITMAP (live_exits, 0, index, bi)
    {
      add_exit_phi (BASIC_BLOCK_FOR_FN (cfun, index), var);
    }

  BITMAP_FREE (live_exits);
}

/* Add exit phis for the names marked in NAMES_TO_RENAME.
   Exits of the loops are stored in EXITS.  Sets of blocks where the ssa
   names are used are stored in USE_BLOCKS.  */

static void
add_exit_phis (bitmap names_to_rename, bitmap *use_blocks, bitmap *loop_exits)
{
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (names_to_rename, 0, i, bi)
    {
      add_exit_phis_var (ssa_name (i), use_blocks[i], loop_exits);
    }
}

/* Fill the array of bitmaps LOOP_EXITS with all loop exit edge targets.  */

static void
get_loops_exits (bitmap *loop_exits)
{
  struct loop *loop;
  unsigned j;
  edge e;

  FOR_EACH_LOOP (loop, 0)
    {
      vec<edge> exit_edges = get_loop_exit_edges (loop);
      loop_exits[loop->num] = BITMAP_ALLOC (&loop_renamer_obstack);
      FOR_EACH_VEC_ELT (exit_edges, j, e)
        bitmap_set_bit (loop_exits[loop->num], e->dest->index);
      exit_edges.release ();
    }
}

/* For USE in BB, if it is used outside of the loop it is defined in,
   mark it for rewrite.  Record basic block BB where it is used
   to USE_BLOCKS.  Record the ssa name index to NEED_PHIS bitmap.
   Note that for USEs in phis, BB should be the src of the edge corresponding to
   the use, rather than the bb containing the phi.  */

static void
find_uses_to_rename_use (basic_block bb, tree use, bitmap *use_blocks,
			 bitmap need_phis)
{
  unsigned ver;
  basic_block def_bb;
  struct loop *def_loop;

  if (TREE_CODE (use) != SSA_NAME)
    return;

  ver = SSA_NAME_VERSION (use);
  def_bb = gimple_bb (SSA_NAME_DEF_STMT (use));
  if (!def_bb)
    return;
  def_loop = def_bb->loop_father;

  /* If the definition is not inside a loop, it is not interesting.  */
  if (!loop_outer (def_loop))
    return;

  /* If the use is not outside of the loop it is defined in, it is not
     interesting.  */
  if (flow_bb_inside_loop_p (def_loop, bb))
    return;

  /* If we're seeing VER for the first time, we still have to allocate
     a bitmap for its uses.  */
  if (bitmap_set_bit (need_phis, ver))
    use_blocks[ver] = BITMAP_ALLOC (&loop_renamer_obstack);
  bitmap_set_bit (use_blocks[ver], bb->index);
}

/* For uses matching USE_FLAGS in STMT, mark names that are used outside of the
   loop they are defined to rewrite.  Record the set of blocks in which the ssa
   names are used to USE_BLOCKS, and the ssa names themselves to NEED_PHIS.  */

static void
find_uses_to_rename_stmt (gimple *stmt, bitmap *use_blocks, bitmap need_phis,
			  int use_flags)
{
  ssa_op_iter iter;
  tree var;
  basic_block bb = gimple_bb (stmt);

  if (is_gimple_debug (stmt))
    return;

  /* FOR_EACH_SSA_TREE_OPERAND iterator does not allows SSA_OP_VIRTUAL_USES
     only.  */
  if (use_flags == SSA_OP_VIRTUAL_USES)
    {
      tree vuse = gimple_vuse (stmt);
      if (vuse != NULL_TREE)
	find_uses_to_rename_use (bb, gimple_vuse (stmt), use_blocks, need_phis);
    }
  else
    FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, use_flags)
      find_uses_to_rename_use (bb, var, use_blocks, need_phis);
}

/* Marks names matching USE_FLAGS that are used in BB and outside of the loop
   they are defined in for rewrite.  Records the set of blocks in which the ssa
   names are used to USE_BLOCKS.  Record the SSA names that will
   need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_bb (basic_block bb, bitmap *use_blocks, bitmap need_phis,
			int use_flags)
{
  edge e;
  edge_iterator ei;
  bool do_virtuals = (use_flags & SSA_OP_VIRTUAL_USES) != 0;
  bool do_nonvirtuals = (use_flags & SSA_OP_USE) != 0;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (gphi_iterator bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi);
	 gsi_next (&bsi))
      {
        gphi *phi = bsi.phi ();
	bool virtual_p = virtual_operand_p (gimple_phi_result (phi));
	if ((virtual_p && do_virtuals)
	    || (!virtual_p && do_nonvirtuals))
	  find_uses_to_rename_use (bb, PHI_ARG_DEF_FROM_EDGE (phi, e),
				   use_blocks, need_phis);
      }

  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    find_uses_to_rename_stmt (gsi_stmt (bsi), use_blocks, need_phis,
			      use_flags);
}

/* Marks names matching USE_FLAGS that are used outside of the loop they are
   defined in for rewrite.  Records the set of blocks in which the ssa names are
   used to USE_BLOCKS.  Record the SSA names that will need exit PHIs in
   NEED_PHIS.  If CHANGED_BBS is not NULL, scan only blocks in this set.  */

static void
find_uses_to_rename (bitmap changed_bbs, bitmap *use_blocks, bitmap need_phis,
		     int use_flags)
{
  basic_block bb;
  unsigned index;
  bitmap_iterator bi;

  if (changed_bbs)
    EXECUTE_IF_SET_IN_BITMAP (changed_bbs, 0, index, bi)
      {
	bb = BASIC_BLOCK_FOR_FN (cfun, index);
	if (bb)
	  find_uses_to_rename_bb (bb, use_blocks, need_phis, use_flags);
      }
  else
    FOR_EACH_BB_FN (bb, cfun)
      find_uses_to_rename_bb (bb, use_blocks, need_phis, use_flags);
}

/* Mark uses of DEF that are used outside of the loop they are defined in for
   rewrite.  Record the set of blocks in which the ssa names are used to
   USE_BLOCKS.  Record the SSA names that will need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_def (tree def, bitmap *use_blocks, bitmap need_phis)
{
  gimple *use_stmt;
  imm_use_iterator imm_iter;

  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, def)
    {
      if (is_gimple_debug (use_stmt))
	continue;

      basic_block use_bb = gimple_bb (use_stmt);

      use_operand_p use_p;
      FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
	{
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      edge e = gimple_phi_arg_edge (as_a <gphi *> (use_stmt),
					    PHI_ARG_INDEX_FROM_USE (use_p));
	      use_bb = e->src;
	    }
	  find_uses_to_rename_use (use_bb, USE_FROM_PTR (use_p), use_blocks,
				   need_phis);
	}
    }
}

/* Marks names matching USE_FLAGS that are defined in LOOP and used outside of
   it for rewrite.  Records the set of blocks in which the ssa names are used to
   USE_BLOCKS.  Record the SSA names that will need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_in_loop (struct loop *loop, bitmap *use_blocks,
			     bitmap need_phis, int use_flags)
{
  bool do_virtuals = (use_flags & SSA_OP_VIRTUAL_USES) != 0;
  bool do_nonvirtuals = (use_flags & SSA_OP_USE) != 0;
  int def_flags = ((do_virtuals ? SSA_OP_VIRTUAL_DEFS : 0)
		   | (do_nonvirtuals ? SSA_OP_DEF : 0));


  basic_block *bbs = get_loop_body (loop);

  for (unsigned int i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  gphi *phi = bsi.phi ();
	  tree res = gimple_phi_result (phi);
	  bool virtual_p = virtual_operand_p (res);
	  if ((virtual_p && do_virtuals)
	      || (!virtual_p && do_nonvirtuals))
	    find_uses_to_rename_def (res, use_blocks, need_phis);
      }

      for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  /* FOR_EACH_SSA_TREE_OPERAND iterator does not allows
	     SSA_OP_VIRTUAL_DEFS only.  */
	  if (def_flags == SSA_OP_VIRTUAL_DEFS)
	    {
	      tree vdef = gimple_vdef (stmt);
	      if (vdef != NULL)
		find_uses_to_rename_def (vdef, use_blocks, need_phis);
	    }
	  else
	    {
	      tree var;
	      ssa_op_iter iter;
	      FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, def_flags)
		find_uses_to_rename_def (var, use_blocks, need_phis);
	    }
	}
    }

  XDELETEVEC (bbs);
}

/* Rewrites the program into a loop closed ssa form -- i.e. inserts extra
   phi nodes to ensure that no variable is used outside the loop it is
   defined in.

   This strengthening of the basic ssa form has several advantages:

   1) Updating it during unrolling/peeling/versioning is trivial, since
      we do not need to care about the uses outside of the loop.
      The same applies to virtual operands which are also rewritten into
      loop closed SSA form.  Note that virtual operands are always live
      until function exit.
   2) The behavior of all uses of an induction variable is the same.
      Without this, you need to distinguish the case when the variable
      is used outside of the loop it is defined in, for example

      for (i = 0; i < 100; i++)
	{
	  for (j = 0; j < 100; j++)
	    {
	      k = i + j;
	      use1 (k);
	    }
	  use2 (k);
	}

      Looking from the outer loop with the normal SSA form, the first use of k
      is not well-behaved, while the second one is an induction variable with
      base 99 and step 1.

      If LOOP is non-null, only rewrite uses that have defs in LOOP.  Otherwise,
      if CHANGED_BBS is not NULL, we look for uses outside loops only in the
      basic blocks in this set.

      USE_FLAGS allows us to specify whether we want virtual, non-virtual or
      both variables rewritten.

      UPDATE_FLAG is used in the call to update_ssa.  See
      TODO_update_ssa* for documentation.  */

void
rewrite_into_loop_closed_ssa_1 (bitmap changed_bbs, unsigned update_flag,
				int use_flags, struct loop *loop)
{
  bitmap *use_blocks;
  bitmap names_to_rename;

  loops_state_set (LOOP_CLOSED_SSA);
  if (number_of_loops (cfun) <= 1)
    return;

  /* If the pass has caused the SSA form to be out-of-date, update it
     now.  */
  if (update_flag != 0)
    update_ssa (update_flag);
  else if (flag_checking)
    verify_ssa (true, true);

  bitmap_obstack_initialize (&loop_renamer_obstack);

  names_to_rename = BITMAP_ALLOC (&loop_renamer_obstack);

  /* Uses of names to rename.  We don't have to initialize this array,
     because we know that we will only have entries for the SSA names
     in NAMES_TO_RENAME.  */
  use_blocks = XNEWVEC (bitmap, num_ssa_names);

  if (loop != NULL)
    {
      gcc_assert (changed_bbs == NULL);
      find_uses_to_rename_in_loop (loop, use_blocks, names_to_rename,
				   use_flags);
    }
  else
    {
      gcc_assert (loop == NULL);
      find_uses_to_rename (changed_bbs, use_blocks, names_to_rename, use_flags);
    }

  if (!bitmap_empty_p (names_to_rename))
    {
      /* An array of bitmaps where LOOP_EXITS[I] is the set of basic blocks
	 that are the destination of an edge exiting loop number I.  */
      bitmap *loop_exits = XNEWVEC (bitmap, number_of_loops (cfun));
      get_loops_exits (loop_exits);

      /* Add the PHI nodes on exits of the loops for the names we need to
	 rewrite.  */
      add_exit_phis (names_to_rename, use_blocks, loop_exits);

      free (loop_exits);

      /* Fix up all the names found to be used outside their original
	 loops.  */
      update_ssa (TODO_update_ssa);
    }

  bitmap_obstack_release (&loop_renamer_obstack);
  free (use_blocks);
}

/* Rewrites the non-virtual defs and uses into a loop closed ssa form.  If
   CHANGED_BBS is not NULL, we look for uses outside loops only in the basic
   blocks in this set.  UPDATE_FLAG is used in the call to update_ssa.  See
   TODO_update_ssa* for documentation.  */

void
rewrite_into_loop_closed_ssa (bitmap changed_bbs, unsigned update_flag)
{
  rewrite_into_loop_closed_ssa_1 (changed_bbs, update_flag, SSA_OP_USE, NULL);
}

/* Rewrites virtual defs and uses with def in LOOP into loop closed ssa
   form.  */

void
rewrite_virtuals_into_loop_closed_ssa (struct loop *loop)
{
  rewrite_into_loop_closed_ssa_1 (NULL, 0, SSA_OP_VIRTUAL_USES, loop);
}

/* Check invariants of the loop closed ssa form for the def in DEF_BB.  */

static void
check_loop_closed_ssa_def (basic_block def_bb, tree def)
{
  use_operand_p use_p;
  imm_use_iterator iterator;
  FOR_EACH_IMM_USE_FAST (use_p, iterator, def)
    {
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;

      basic_block use_bb = gimple_bb (USE_STMT (use_p));
      if (is_a <gphi *> (USE_STMT (use_p)))
	use_bb = EDGE_PRED (use_bb, PHI_ARG_INDEX_FROM_USE (use_p))->src;

      gcc_assert (flow_bb_inside_loop_p (def_bb->loop_father, use_bb));
    }
}

/* Checks invariants of loop closed ssa form in BB.  */

static void
check_loop_closed_ssa_bb (basic_block bb)
{
  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    {
      gphi *phi = bsi.phi ();

      if (!virtual_operand_p (PHI_RESULT (phi)))
	check_loop_closed_ssa_def (bb, PHI_RESULT (phi));
    }

  for (gimple_stmt_iterator bsi = gsi_start_nondebug_bb (bb); !gsi_end_p (bsi);
       gsi_next_nondebug (&bsi))
    {
      ssa_op_iter iter;
      tree var;
      gimple *stmt = gsi_stmt (bsi);

      FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_DEF)
	check_loop_closed_ssa_def (bb, var);
    }
}

/* Checks that invariants of the loop closed ssa form are preserved.
   Call verify_ssa when VERIFY_SSA_P is true.  Note all loops are checked
   if LOOP is NULL, otherwise, only LOOP is checked.  */

DEBUG_FUNCTION void
verify_loop_closed_ssa (bool verify_ssa_p, struct loop *loop)
{
  if (number_of_loops (cfun) <= 1)
    return;

  if (verify_ssa_p)
    verify_ssa (false, true);

  timevar_push (TV_VERIFY_LOOP_CLOSED);

  if (loop == NULL)
    {
      basic_block bb;

      FOR_EACH_BB_FN (bb, cfun)
	if (bb->loop_father && bb->loop_father->num > 0)
	  check_loop_closed_ssa_bb (bb);
    }
  else
    {
      basic_block *bbs = get_loop_body (loop);

      for (unsigned i = 0; i < loop->num_nodes; ++i)
	check_loop_closed_ssa_bb (bbs[i]);

      free (bbs);
    }

  timevar_pop (TV_VERIFY_LOOP_CLOSED);
}

/* Split loop exit edge EXIT.  The things are a bit complicated by a need to
   preserve the loop closed ssa form.  The newly created block is returned.  */

basic_block
split_loop_exit_edge (edge exit)
{
  basic_block dest = exit->dest;
  basic_block bb = split_edge (exit);
  gphi *phi, *new_phi;
  tree new_name, name;
  use_operand_p op_p;
  gphi_iterator psi;
  source_location locus;

  for (psi = gsi_start_phis (dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      op_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, single_succ_edge (bb));
      locus = gimple_phi_arg_location_from_edge (phi, single_succ_edge (bb));

      name = USE_FROM_PTR (op_p);

      /* If the argument of the PHI node is a constant, we do not need
	 to keep it inside loop.  */
      if (TREE_CODE (name) != SSA_NAME)
	continue;

      /* Otherwise create an auxiliary phi node that will copy the value
	 of the SSA name out of the loop.  */
      new_name = duplicate_ssa_name (name, NULL);
      new_phi = create_phi_node (new_name, bb);
      add_phi_arg (new_phi, name, exit, locus);
      SET_USE (op_p, new_name);
    }

  return bb;
}

/* Returns the basic block in that statements should be emitted for induction
   variables incremented at the end of the LOOP.  */

basic_block
ip_end_pos (struct loop *loop)
{
  return loop->latch;
}

/* Returns the basic block in that statements should be emitted for induction
   variables incremented just before exit condition of a LOOP.  */

basic_block
ip_normal_pos (struct loop *loop)
{
  gimple *last;
  basic_block bb;
  edge exit;

  if (!single_pred_p (loop->latch))
    return NULL;

  bb = single_pred (loop->latch);
  last = last_stmt (bb);
  if (!last
      || gimple_code (last) != GIMPLE_COND)
    return NULL;

  exit = EDGE_SUCC (bb, 0);
  if (exit->dest == loop->latch)
    exit = EDGE_SUCC (bb, 1);

  if (flow_bb_inside_loop_p (loop, exit->dest))
    return NULL;

  return bb;
}

/* Stores the standard position for induction variable increment in LOOP
   (just before the exit condition if it is available and latch block is empty,
   end of the latch block otherwise) to BSI.  INSERT_AFTER is set to true if
   the increment should be inserted after *BSI.  */

void
standard_iv_increment_position (struct loop *loop, gimple_stmt_iterator *bsi,
				bool *insert_after)
{
  basic_block bb = ip_normal_pos (loop), latch = ip_end_pos (loop);
  gimple *last = last_stmt (latch);

  if (!bb
      || (last && gimple_code (last) != GIMPLE_LABEL))
    {
      *bsi = gsi_last_bb (latch);
      *insert_after = true;
    }
  else
    {
      *bsi = gsi_last_bb (bb);
      *insert_after = false;
    }
}

/* Copies phi node arguments for duplicated blocks.  The index of the first
   duplicated block is FIRST_NEW_BLOCK.  */

static void
copy_phi_node_args (unsigned first_new_block)
{
  unsigned i;

  for (i = first_new_block; i < (unsigned) last_basic_block_for_fn (cfun); i++)
    BASIC_BLOCK_FOR_FN (cfun, i)->flags |= BB_DUPLICATED;

  for (i = first_new_block; i < (unsigned) last_basic_block_for_fn (cfun); i++)
    add_phi_args_after_copy_bb (BASIC_BLOCK_FOR_FN (cfun, i));

  for (i = first_new_block; i < (unsigned) last_basic_block_for_fn (cfun); i++)
    BASIC_BLOCK_FOR_FN (cfun, i)->flags &= ~BB_DUPLICATED;
}


/* The same as cfgloopmanip.c:duplicate_loop_to_header_edge, but also
   updates the PHI nodes at start of the copied region.  In order to
   achieve this, only loops whose exits all lead to the same location
   are handled.

   Notice that we do not completely update the SSA web after
   duplication.  The caller is responsible for calling update_ssa
   after the loop has been duplicated.  */

bool
gimple_duplicate_loop_to_header_edge (struct loop *loop, edge e,
				    unsigned int ndupl, sbitmap wont_exit,
				    edge orig, vec<edge> *to_remove,
				    int flags)
{
  unsigned first_new_block;

  if (!loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES))
    return false;
  if (!loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS))
    return false;

  first_new_block = last_basic_block_for_fn (cfun);
  if (!duplicate_loop_to_header_edge (loop, e, ndupl, wont_exit,
				      orig, to_remove, flags))
    return false;

  /* Readd the removed phi args for e.  */
  flush_pending_stmts (e);

  /* Copy the phi node arguments.  */
  copy_phi_node_args (first_new_block);

  scev_reset ();

  return true;
}

/* Returns true if we can unroll LOOP FACTOR times.  Number
   of iterations of the loop is returned in NITER.  */

bool
can_unroll_loop_p (struct loop *loop, unsigned factor,
		   struct tree_niter_desc *niter)
{
  edge exit;

  /* Check whether unrolling is possible.  We only want to unroll loops
     for that we are able to determine number of iterations.  We also
     want to split the extra iterations of the loop from its end,
     therefore we require that the loop has precisely one
     exit.  */

  exit = single_dom_exit (loop);
  if (!exit)
    return false;

  if (!number_of_iterations_exit (loop, exit, niter, false)
      || niter->cmp == ERROR_MARK
      /* Scalar evolutions analysis might have copy propagated
	 the abnormal ssa names into these expressions, hence
	 emitting the computations based on them during loop
	 unrolling might create overlapping life ranges for
	 them, and failures in out-of-ssa.  */
      || contains_abnormal_ssa_name_p (niter->may_be_zero)
      || contains_abnormal_ssa_name_p (niter->control.base)
      || contains_abnormal_ssa_name_p (niter->control.step)
      || contains_abnormal_ssa_name_p (niter->bound))
    return false;

  /* And of course, we must be able to duplicate the loop.  */
  if (!can_duplicate_loop_p (loop))
    return false;

  /* The final loop should be small enough.  */
  if (tree_num_loop_insns (loop, &eni_size_weights) * factor
      > (unsigned) PARAM_VALUE (PARAM_MAX_UNROLLED_INSNS))
    return false;

  return true;
}

/* Determines the conditions that control execution of LOOP unrolled FACTOR
   times.  DESC is number of iterations of LOOP.  ENTER_COND is set to
   condition that must be true if the main loop can be entered.
   EXIT_BASE, EXIT_STEP, EXIT_CMP and EXIT_BOUND are set to values describing
   how the exit from the unrolled loop should be controlled.  */

static void
determine_exit_conditions (struct loop *loop, struct tree_niter_desc *desc,
			   unsigned factor, tree *enter_cond,
			   tree *exit_base, tree *exit_step,
			   enum tree_code *exit_cmp, tree *exit_bound)
{
  gimple_seq stmts;
  tree base = desc->control.base;
  tree step = desc->control.step;
  tree bound = desc->bound;
  tree type = TREE_TYPE (step);
  tree bigstep, delta;
  tree min = lower_bound_in_type (type, type);
  tree max = upper_bound_in_type (type, type);
  enum tree_code cmp = desc->cmp;
  tree cond = boolean_true_node, assum;

  /* For pointers, do the arithmetics in the type of step.  */
  base = fold_convert (type, base);
  bound = fold_convert (type, bound);

  *enter_cond = boolean_false_node;
  *exit_base = NULL_TREE;
  *exit_step = NULL_TREE;
  *exit_cmp = ERROR_MARK;
  *exit_bound = NULL_TREE;
  gcc_assert (cmp != ERROR_MARK);

  /* We only need to be correct when we answer question
     "Do at least FACTOR more iterations remain?" in the unrolled loop.
     Thus, transforming BASE + STEP * i <> BOUND to
     BASE + STEP * i < BOUND is ok.  */
  if (cmp == NE_EXPR)
    {
      if (tree_int_cst_sign_bit (step))
	cmp = GT_EXPR;
      else
	cmp = LT_EXPR;
    }
  else if (cmp == LT_EXPR)
    {
      gcc_assert (!tree_int_cst_sign_bit (step));
    }
  else if (cmp == GT_EXPR)
    {
      gcc_assert (tree_int_cst_sign_bit (step));
    }
  else
    gcc_unreachable ();

  /* The main body of the loop may be entered iff:

     1) desc->may_be_zero is false.
     2) it is possible to check that there are at least FACTOR iterations
	of the loop, i.e., BOUND - step * FACTOR does not overflow.
     3) # of iterations is at least FACTOR  */

  if (!integer_zerop (desc->may_be_zero))
    cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			invert_truthvalue (desc->may_be_zero),
			cond);

  bigstep = fold_build2 (MULT_EXPR, type, step,
			 build_int_cst_type (type, factor));
  delta = fold_build2 (MINUS_EXPR, type, bigstep, step);
  if (cmp == LT_EXPR)
    assum = fold_build2 (GE_EXPR, boolean_type_node,
			 bound,
			 fold_build2 (PLUS_EXPR, type, min, delta));
  else
    assum = fold_build2 (LE_EXPR, boolean_type_node,
			 bound,
			 fold_build2 (PLUS_EXPR, type, max, delta));
  cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, assum, cond);

  bound = fold_build2 (MINUS_EXPR, type, bound, delta);
  assum = fold_build2 (cmp, boolean_type_node, base, bound);
  cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, assum, cond);

  cond = force_gimple_operand (unshare_expr (cond), &stmts, false, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
  /* cond now may be a gimple comparison, which would be OK, but also any
     other gimple rhs (say a && b).  In this case we need to force it to
     operand.  */
  if (!is_gimple_condexpr (cond))
    {
      cond = force_gimple_operand (cond, &stmts, true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
    }
  *enter_cond = cond;

  base = force_gimple_operand (unshare_expr (base), &stmts, true, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
  bound = force_gimple_operand (unshare_expr (bound), &stmts, true, NULL_TREE);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);

  *exit_base = base;
  *exit_step = bigstep;
  *exit_cmp = cmp;
  *exit_bound = bound;
}

/* Scales the frequencies of all basic blocks in LOOP that are strictly
   dominated by BB by NUM/DEN.  */

static void
scale_dominated_blocks_in_loop (struct loop *loop, basic_block bb,
				int num, int den)
{
  basic_block son;

  if (den == 0)
    return;

  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      if (!flow_bb_inside_loop_p (loop, son))
	continue;
      scale_bbs_frequencies_int (&son, 1, num, den);
      scale_dominated_blocks_in_loop (loop, son, num, den);
    }
}

/* Return estimated niter for LOOP after unrolling by FACTOR times.  */

gcov_type
niter_for_unrolled_loop (struct loop *loop, unsigned factor)
{
  gcc_assert (factor != 0);
  bool profile_p = false;
  gcov_type est_niter = expected_loop_iterations_unbounded (loop, &profile_p);
  /* Note that this is really CEIL (est_niter + 1, factor) - 1, where the
     "+ 1" converts latch iterations to loop iterations and the "- 1"
     converts back.  */
  gcov_type new_est_niter = est_niter / factor;

  if (est_niter == -1)
    return -1;

  /* Without profile feedback, loops for which we do not know a better estimate
     are assumed to roll 10 times.  When we unroll such loop, it appears to
     roll too little, and it may even seem to be cold.  To avoid this, we
     ensure that the created loop appears to roll at least 5 times (but at
     most as many times as before unrolling).  Don't do adjustment if profile
     feedback is present.  */
  if (new_est_niter < 5 && !profile_p)
    {
      if (est_niter < 5)
	new_est_niter = est_niter;
      else
	new_est_niter = 5;
    }

  if (loop->any_upper_bound)
    {
      /* As above, this is really CEIL (upper_bound + 1, factor) - 1.  */
      widest_int bound = wi::udiv_floor (loop->nb_iterations_upper_bound,
					 factor);
      if (wi::ltu_p (bound, new_est_niter))
	new_est_niter = bound.to_uhwi ();
    }

  return new_est_niter;
}

/* Unroll LOOP FACTOR times.  DESC describes number of iterations of LOOP.
   EXIT is the exit of the loop to that DESC corresponds.

   If N is number of iterations of the loop and MAY_BE_ZERO is the condition
   under that loop exits in the first iteration even if N != 0,

   while (1)
     {
       x = phi (init, next);

       pre;
       if (st)
         break;
       post;
     }

   becomes (with possibly the exit conditions formulated a bit differently,
   avoiding the need to create a new iv):

   if (MAY_BE_ZERO || N < FACTOR)
     goto rest;

   do
     {
       x = phi (init, next);

       pre;
       post;
       pre;
       post;
       ...
       pre;
       post;
       N -= FACTOR;

     } while (N >= FACTOR);

   rest:
     init' = phi (init, x);

   while (1)
     {
       x = phi (init', next);

       pre;
       if (st)
         break;
       post;
     }

   Before the loop is unrolled, TRANSFORM is called for it (only for the
   unrolled loop, but not for its versioned copy).  DATA is passed to
   TRANSFORM.  */

/* Probability in % that the unrolled loop is entered.  Just a guess.  */
#define PROB_UNROLLED_LOOP_ENTERED 90

void
tree_transform_and_unroll_loop (struct loop *loop, unsigned factor,
				edge exit, struct tree_niter_desc *desc,
				transform_callback transform,
				void *data)
{
  gcond *exit_if;
  tree ctr_before, ctr_after;
  tree enter_main_cond, exit_base, exit_step, exit_bound;
  enum tree_code exit_cmp;
  gphi *phi_old_loop, *phi_new_loop, *phi_rest;
  gphi_iterator psi_old_loop, psi_new_loop;
  tree init, next, new_init;
  struct loop *new_loop;
  basic_block rest, exit_bb;
  edge old_entry, new_entry, old_latch, precond_edge, new_exit;
  edge new_nonexit, e;
  gimple_stmt_iterator bsi;
  use_operand_p op;
  bool ok;
  unsigned i;
  profile_probability prob, prob_entry, scale_unrolled;
  profile_count freq_e, freq_h;
  gcov_type new_est_niter = niter_for_unrolled_loop (loop, factor);
  unsigned irr = loop_preheader_edge (loop)->flags & EDGE_IRREDUCIBLE_LOOP;
  auto_vec<edge> to_remove;

  determine_exit_conditions (loop, desc, factor,
			     &enter_main_cond, &exit_base, &exit_step,
			     &exit_cmp, &exit_bound);

  /* Let us assume that the unrolled loop is quite likely to be entered.  */
  if (integer_nonzerop (enter_main_cond))
    prob_entry = profile_probability::always ();
  else
    prob_entry = profile_probability::guessed_always ()
			.apply_scale (PROB_UNROLLED_LOOP_ENTERED, 100);

  /* The values for scales should keep profile consistent, and somewhat close
     to correct.

     TODO: The current value of SCALE_REST makes it appear that the loop that
     is created by splitting the remaining iterations of the unrolled loop is
     executed the same number of times as the original loop, and with the same
     frequencies, which is obviously wrong.  This does not appear to cause
     problems, so we do not bother with fixing it for now.  To make the profile
     correct, we would need to change the probability of the exit edge of the
     loop, and recompute the distribution of frequencies in its body because
     of this change (scale the frequencies of blocks before and after the exit
     by appropriate factors).  */
  scale_unrolled = prob_entry;

  new_loop = loop_version (loop, enter_main_cond, NULL, prob_entry,
			   prob_entry.invert (), scale_unrolled,
			   profile_probability::guessed_always (),
			   true);
  gcc_assert (new_loop != NULL);
  update_ssa (TODO_update_ssa);

  /* Prepare the cfg and update the phi nodes.  Move the loop exit to the
     loop latch (and make its condition dummy, for the moment).  */
  rest = loop_preheader_edge (new_loop)->src;
  precond_edge = single_pred_edge (rest);
  split_edge (loop_latch_edge (loop));
  exit_bb = single_pred (loop->latch);

  /* Since the exit edge will be removed, the frequency of all the blocks
     in the loop that are dominated by it must be scaled by
     1 / (1 - exit->probability).  */
  if (exit->probability.initialized_p ())
    scale_dominated_blocks_in_loop (loop, exit->src,
				    /* We are scaling up here so probability
				       does not fit.  */
				    REG_BR_PROB_BASE,
				    REG_BR_PROB_BASE
				    - exit->probability.to_reg_br_prob_base ());

  bsi = gsi_last_bb (exit_bb);
  exit_if = gimple_build_cond (EQ_EXPR, integer_zero_node,
			       integer_zero_node,
			       NULL_TREE, NULL_TREE);

  gsi_insert_after (&bsi, exit_if, GSI_NEW_STMT);
  new_exit = make_edge (exit_bb, rest, EDGE_FALSE_VALUE | irr);
  rescan_loop_exit (new_exit, true, false);

  /* Set the probability of new exit to the same of the old one.  Fix
     the frequency of the latch block, by scaling it back by
     1 - exit->probability.  */
  new_exit->probability = exit->probability;
  new_nonexit = single_pred_edge (loop->latch);
  new_nonexit->probability = exit->probability.invert ();
  new_nonexit->flags = EDGE_TRUE_VALUE;
  if (new_nonexit->probability.initialized_p ())
    scale_bbs_frequencies (&loop->latch, 1, new_nonexit->probability);

  old_entry = loop_preheader_edge (loop);
  new_entry = loop_preheader_edge (new_loop);
  old_latch = loop_latch_edge (loop);
  for (psi_old_loop = gsi_start_phis (loop->header),
       psi_new_loop = gsi_start_phis (new_loop->header);
       !gsi_end_p (psi_old_loop);
       gsi_next (&psi_old_loop), gsi_next (&psi_new_loop))
    {
      phi_old_loop = psi_old_loop.phi ();
      phi_new_loop = psi_new_loop.phi ();

      init = PHI_ARG_DEF_FROM_EDGE (phi_old_loop, old_entry);
      op = PHI_ARG_DEF_PTR_FROM_EDGE (phi_new_loop, new_entry);
      gcc_assert (operand_equal_for_phi_arg_p (init, USE_FROM_PTR (op)));
      next = PHI_ARG_DEF_FROM_EDGE (phi_old_loop, old_latch);

      /* Prefer using original variable as a base for the new ssa name.
	 This is necessary for virtual ops, and useful in order to avoid
	 losing debug info for real ops.  */
      if (TREE_CODE (next) == SSA_NAME
	  && useless_type_conversion_p (TREE_TYPE (next),
					TREE_TYPE (init)))
	new_init = copy_ssa_name (next);
      else if (TREE_CODE (init) == SSA_NAME
	       && useless_type_conversion_p (TREE_TYPE (init),
					     TREE_TYPE (next)))
	new_init = copy_ssa_name (init);
      else if (useless_type_conversion_p (TREE_TYPE (next), TREE_TYPE (init)))
	new_init = make_temp_ssa_name (TREE_TYPE (next), NULL, "unrinittmp");
      else
	new_init = make_temp_ssa_name (TREE_TYPE (init), NULL, "unrinittmp");

      phi_rest = create_phi_node (new_init, rest);

      add_phi_arg (phi_rest, init, precond_edge, UNKNOWN_LOCATION);
      add_phi_arg (phi_rest, next, new_exit, UNKNOWN_LOCATION);
      SET_USE (op, new_init);
    }

  remove_path (exit);

  /* Transform the loop.  */
  if (transform)
    (*transform) (loop, data);

  /* Unroll the loop and remove the exits in all iterations except for the
     last one.  */
  auto_sbitmap wont_exit (factor);
  bitmap_ones (wont_exit);
  bitmap_clear_bit (wont_exit, factor - 1);

  ok = gimple_duplicate_loop_to_header_edge
	  (loop, loop_latch_edge (loop), factor - 1,
	   wont_exit, new_exit, &to_remove, DLTHE_FLAG_UPDATE_FREQ);
  gcc_assert (ok);

  FOR_EACH_VEC_ELT (to_remove, i, e)
    {
      ok = remove_path (e);
      gcc_assert (ok);
    }
  update_ssa (TODO_update_ssa);

  /* Ensure that the frequencies in the loop match the new estimated
     number of iterations, and change the probability of the new
     exit edge.  */

  freq_h = loop->header->count;
  freq_e = (loop_preheader_edge (loop))->count ();
  if (freq_h.nonzero_p ())
    {
      /* Avoid dropping loop body profile counter to 0 because of zero count
	 in loop's preheader.  */
      if (freq_e == profile_count::zero ())
        freq_e = profile_count::from_gcov_type (1);
      scale_loop_frequencies (loop, freq_e.probability_in (freq_h));
    }

  exit_bb = single_pred (loop->latch);
  new_exit = find_edge (exit_bb, rest);
  new_exit->probability = profile_probability::always ()
				.apply_scale (1, new_est_niter + 1);

  rest->count += new_exit->count ();

  new_nonexit = single_pred_edge (loop->latch);
  prob = new_nonexit->probability;
  new_nonexit->probability = new_exit->probability.invert ();
  prob = new_nonexit->probability / prob;
  if (prob.initialized_p ())
    scale_bbs_frequencies (&loop->latch, 1, prob);

  /* Finally create the new counter for number of iterations and add the new
     exit instruction.  */
  bsi = gsi_last_nondebug_bb (exit_bb);
  exit_if = as_a <gcond *> (gsi_stmt (bsi));
  create_iv (exit_base, exit_step, NULL_TREE, loop,
	     &bsi, false, &ctr_before, &ctr_after);
  gimple_cond_set_code (exit_if, exit_cmp);
  gimple_cond_set_lhs (exit_if, ctr_after);
  gimple_cond_set_rhs (exit_if, exit_bound);
  update_stmt (exit_if);

  checking_verify_flow_info ();
  checking_verify_loop_structure ();
  checking_verify_loop_closed_ssa (true, loop);
  checking_verify_loop_closed_ssa (true, new_loop);
}

/* Wrapper over tree_transform_and_unroll_loop for case we do not
   want to transform the loop before unrolling.  The meaning
   of the arguments is the same as for tree_transform_and_unroll_loop.  */

void
tree_unroll_loop (struct loop *loop, unsigned factor,
		  edge exit, struct tree_niter_desc *desc)
{
  tree_transform_and_unroll_loop (loop, factor, exit, desc,
				  NULL, NULL);
}

/* Rewrite the phi node at position PSI in function of the main
   induction variable MAIN_IV and insert the generated code at GSI.  */

static void
rewrite_phi_with_iv (loop_p loop,
		     gphi_iterator *psi,
		     gimple_stmt_iterator *gsi,
		     tree main_iv)
{
  affine_iv iv;
  gassign *stmt;
  gphi *phi = psi->phi ();
  tree atype, mtype, val, res = PHI_RESULT (phi);

  if (virtual_operand_p (res) || res == main_iv)
    {
      gsi_next (psi);
      return;
    }

  if (!simple_iv (loop, loop, res, &iv, true))
    {
      gsi_next (psi);
      return;
    }

  remove_phi_node (psi, false);

  atype = TREE_TYPE (res);
  mtype = POINTER_TYPE_P (atype) ? sizetype : atype;
  val = fold_build2 (MULT_EXPR, mtype, unshare_expr (iv.step),
		     fold_convert (mtype, main_iv));
  val = fold_build2 (POINTER_TYPE_P (atype)
		     ? POINTER_PLUS_EXPR : PLUS_EXPR,
		     atype, unshare_expr (iv.base), val);
  val = force_gimple_operand_gsi (gsi, val, false, NULL_TREE, true,
				  GSI_SAME_STMT);
  stmt = gimple_build_assign (res, val);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
}

/* Rewrite all the phi nodes of LOOP in function of the main induction
   variable MAIN_IV.  */

static void
rewrite_all_phi_nodes_with_iv (loop_p loop, tree main_iv)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  gphi_iterator psi;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator gsi = gsi_after_labels (bb);

      if (bb->loop_father != loop)
	continue;

      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); )
	rewrite_phi_with_iv (loop, &psi, &gsi, main_iv);
    }

  free (bbs);
}

/* Bases all the induction variables in LOOP on a single induction variable
   (with base 0 and step 1), whose final value is compared with *NIT.  When the
   IV type precision has to be larger than *NIT type precision, *NIT is
   converted to the larger type, the conversion code is inserted before the
   loop, and *NIT is updated to the new definition.  When BUMP_IN_LATCH is true,
   the induction variable is incremented in the loop latch, otherwise it is
   incremented in the loop header.  Return the induction variable that was
   created.  */

tree
canonicalize_loop_ivs (struct loop *loop, tree *nit, bool bump_in_latch)
{
  unsigned precision = TYPE_PRECISION (TREE_TYPE (*nit));
  unsigned original_precision = precision;
  tree type, var_before;
  gimple_stmt_iterator gsi;
  gphi_iterator psi;
  gcond *stmt;
  edge exit = single_dom_exit (loop);
  gimple_seq stmts;
  bool unsigned_p = false;

  for (psi = gsi_start_phis (loop->header);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = PHI_RESULT (phi);
      bool uns;

      type = TREE_TYPE (res);
      if (virtual_operand_p (res)
	  || (!INTEGRAL_TYPE_P (type)
	      && !POINTER_TYPE_P (type))
	  || TYPE_PRECISION (type) < precision)
	continue;

      uns = POINTER_TYPE_P (type) | TYPE_UNSIGNED (type);

      if (TYPE_PRECISION (type) > precision)
	unsigned_p = uns;
      else
	unsigned_p |= uns;

      precision = TYPE_PRECISION (type);
    }

  scalar_int_mode mode = smallest_int_mode_for_size (precision);
  precision = GET_MODE_PRECISION (mode);
  type = build_nonstandard_integer_type (precision, unsigned_p);

  if (original_precision != precision)
    {
      *nit = fold_convert (type, *nit);
      *nit = force_gimple_operand (*nit, &stmts, true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (loop_preheader_edge (loop), stmts);
    }

  if (bump_in_latch)
    gsi = gsi_last_bb (loop->latch);
  else
    gsi = gsi_last_nondebug_bb (loop->header);
  create_iv (build_int_cst_type (type, 0), build_int_cst (type, 1), NULL_TREE,
	     loop, &gsi, bump_in_latch, &var_before, NULL);

  rewrite_all_phi_nodes_with_iv (loop, var_before);

  stmt = as_a <gcond *> (last_stmt (exit->src));
  /* Make the loop exit if the control condition is not satisfied.  */
  if (exit->flags & EDGE_TRUE_VALUE)
    {
      edge te, fe;

      extract_true_false_edges_from_block (exit->src, &te, &fe);
      te->flags = EDGE_FALSE_VALUE;
      fe->flags = EDGE_TRUE_VALUE;
    }
  gimple_cond_set_code (stmt, LT_EXPR);
  gimple_cond_set_lhs (stmt, var_before);
  gimple_cond_set_rhs (stmt, *nit);
  update_stmt (stmt);

  return var_before;
}
