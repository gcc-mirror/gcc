/* High-level loop manipulation functions.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "cfglayout.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-inline.h"
#include "langhooks.h"

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
  gimple stmt;
  tree initial, step1;
  gimple_seq stmts;
  tree vb, va;
  enum tree_code incr_op = PLUS_EXPR;
  edge pe = loop_preheader_edge (loop);

  if (!var)
    {
      var = create_tmp_var (TREE_TYPE (base), "ivtmp");
      add_referenced_var (var);
    }

  vb = make_ssa_name (var, NULL);
  if (var_before)
    *var_before = vb;
  va = make_ssa_name (var, NULL);
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

  stmt = gimple_build_assign_with_ops (incr_op, va, vb, step);
  if (after)
    gsi_insert_after (incr_pos, stmt, GSI_NEW_STMT);
  else
    gsi_insert_before (incr_pos, stmt, GSI_NEW_STMT);

  initial = force_gimple_operand (base, &stmts, true, var);
  if (stmts)
    gsi_insert_seq_on_edge_immediate (pe, stmts);

  stmt = create_phi_node (vb, loop->header);
  SSA_NAME_DEF_STMT (vb) = stmt;
  add_phi_arg (stmt, initial, loop_preheader_edge (loop), UNKNOWN_LOCATION);
  add_phi_arg (stmt, va, loop_latch_edge (loop), UNKNOWN_LOCATION);
}

/* Add exit phis for the USE on EXIT.  */

static void
add_exit_phis_edge (basic_block exit, tree use)
{
  gimple phi, def_stmt = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = gimple_bb (def_stmt);
  struct loop *def_loop;
  edge e;
  edge_iterator ei;

  /* Check that some of the edges entering the EXIT block exits a loop in
     that USE is defined.  */
  FOR_EACH_EDGE (e, ei, exit->preds)
    {
      def_loop = find_common_loop (def_bb->loop_father, e->src->loop_father);
      if (!flow_bb_inside_loop_p (def_loop, e->dest))
	break;
    }

  if (!e)
    return;

  phi = create_phi_node (use, exit);
  create_new_def_for (gimple_phi_result (phi), phi,
		      gimple_phi_result_ptr (phi));
  FOR_EACH_EDGE (e, ei, exit->preds)
    add_phi_arg (phi, use, e, UNKNOWN_LOCATION);
}

/* Add exit phis for VAR that is used in LIVEIN.
   Exits of the loops are stored in EXITS.  */

static void
add_exit_phis_var (tree var, bitmap livein, bitmap exits)
{
  bitmap def;
  unsigned index;
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
  bitmap_iterator bi;

  if (is_gimple_reg (var))
    bitmap_clear_bit (livein, def_bb->index);
  else
    bitmap_set_bit (livein, def_bb->index);

  def = BITMAP_ALLOC (NULL);
  bitmap_set_bit (def, def_bb->index);
  compute_global_livein (livein, def);
  BITMAP_FREE (def);

  EXECUTE_IF_AND_IN_BITMAP (exits, livein, 0, index, bi)
    {
      add_exit_phis_edge (BASIC_BLOCK (index), var);
    }
}

/* Add exit phis for the names marked in NAMES_TO_RENAME.
   Exits of the loops are stored in EXITS.  Sets of blocks where the ssa
   names are used are stored in USE_BLOCKS.  */

static void
add_exit_phis (bitmap names_to_rename, bitmap *use_blocks, bitmap loop_exits)
{
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (names_to_rename, 0, i, bi)
    {
      add_exit_phis_var (ssa_name (i), use_blocks[i], loop_exits);
    }
}

/* Returns a bitmap of all loop exit edge targets.  */

static bitmap
get_loops_exits (void)
{
  bitmap exits = BITMAP_ALLOC (NULL);
  basic_block bb;
  edge e;
  edge_iterator ei;

  FOR_EACH_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src != ENTRY_BLOCK_PTR
	    && !flow_bb_inside_loop_p (e->src->loop_father, bb))
	  {
	    bitmap_set_bit (exits, bb->index);
	    break;
	  }
    }

  return exits;
}

/* For USE in BB, if it is used outside of the loop it is defined in,
   mark it for rewrite.  Record basic block BB where it is used
   to USE_BLOCKS.  Record the ssa name index to NEED_PHIS bitmap.  */

static void
find_uses_to_rename_use (basic_block bb, tree use, bitmap *use_blocks,
			 bitmap need_phis)
{
  unsigned ver;
  basic_block def_bb;
  struct loop *def_loop;

  if (TREE_CODE (use) != SSA_NAME)
    return;

  /* We don't need to keep virtual operands in loop-closed form.  */
  if (!is_gimple_reg (use))
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

  if (!use_blocks[ver])
    use_blocks[ver] = BITMAP_ALLOC (NULL);
  bitmap_set_bit (use_blocks[ver], bb->index);

  bitmap_set_bit (need_phis, ver);
}

/* For uses in STMT, mark names that are used outside of the loop they are
   defined to rewrite.  Record the set of blocks in that the ssa
   names are defined to USE_BLOCKS and the ssa names themselves to
   NEED_PHIS.  */

static void
find_uses_to_rename_stmt (gimple stmt, bitmap *use_blocks, bitmap need_phis)
{
  ssa_op_iter iter;
  tree var;
  basic_block bb = gimple_bb (stmt);

  if (is_gimple_debug (stmt))
    return;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES)
    find_uses_to_rename_use (bb, var, use_blocks, need_phis);
}

/* Marks names that are used in BB and outside of the loop they are
   defined in for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  Record the SSA names that will
   need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_bb (basic_block bb, bitmap *use_blocks, bitmap need_phis)
{
  gimple_stmt_iterator bsi;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (bsi = gsi_start_phis (e->dest); !gsi_end_p (bsi); gsi_next (&bsi))
      find_uses_to_rename_use (bb, PHI_ARG_DEF_FROM_EDGE (gsi_stmt (bsi), e),
			       use_blocks, need_phis);

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    find_uses_to_rename_stmt (gsi_stmt (bsi), use_blocks, need_phis);
}

/* Marks names that are used outside of the loop they are defined in
   for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  If CHANGED_BBS is not NULL,
   scan only blocks in this set.  */

static void
find_uses_to_rename (bitmap changed_bbs, bitmap *use_blocks, bitmap need_phis)
{
  basic_block bb;
  unsigned index;
  bitmap_iterator bi;

  if (changed_bbs && !bitmap_empty_p (changed_bbs))
    {
      EXECUTE_IF_SET_IN_BITMAP (changed_bbs, 0, index, bi)
	{
	  find_uses_to_rename_bb (BASIC_BLOCK (index), use_blocks, need_phis);
	}
    }
  else
    {
      FOR_EACH_BB (bb)
	{
	  find_uses_to_rename_bb (bb, use_blocks, need_phis);
	}
    }
}

/* Rewrites the program into a loop closed ssa form -- i.e. inserts extra
   phi nodes to ensure that no variable is used outside the loop it is
   defined in.

   This strengthening of the basic ssa form has several advantages:

   1) Updating it during unrolling/peeling/versioning is trivial, since
      we do not need to care about the uses outside of the loop.
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

      If CHANGED_BBS is not NULL, we look for uses outside loops only in
      the basic blocks in this set.

      UPDATE_FLAG is used in the call to update_ssa.  See
      TODO_update_ssa* for documentation.  */

void
rewrite_into_loop_closed_ssa (bitmap changed_bbs, unsigned update_flag)
{
  bitmap loop_exits;
  bitmap *use_blocks;
  unsigned i, old_num_ssa_names;
  bitmap names_to_rename;

  loops_state_set (LOOP_CLOSED_SSA);
  if (number_of_loops () <= 1)
    return;

  loop_exits = get_loops_exits ();
  names_to_rename = BITMAP_ALLOC (NULL);

  /* If the pass has caused the SSA form to be out-of-date, update it
     now.  */
  update_ssa (update_flag);

  old_num_ssa_names = num_ssa_names;
  use_blocks = XCNEWVEC (bitmap, old_num_ssa_names);

  /* Find the uses outside loops.  */
  find_uses_to_rename (changed_bbs, use_blocks, names_to_rename);

  /* Add the PHI nodes on exits of the loops for the names we need to
     rewrite.  */
  add_exit_phis (names_to_rename, use_blocks, loop_exits);

  for (i = 0; i < old_num_ssa_names; i++)
    BITMAP_FREE (use_blocks[i]);
  free (use_blocks);
  BITMAP_FREE (loop_exits);
  BITMAP_FREE (names_to_rename);

  /* Fix up all the names found to be used outside their original
     loops.  */
  update_ssa (TODO_update_ssa);
}

/* Check invariants of the loop closed ssa form for the USE in BB.  */

static void
check_loop_closed_ssa_use (basic_block bb, tree use)
{
  gimple def;
  basic_block def_bb;

  if (TREE_CODE (use) != SSA_NAME || !is_gimple_reg (use))
    return;

  def = SSA_NAME_DEF_STMT (use);
  def_bb = gimple_bb (def);
  gcc_assert (!def_bb
	      || flow_bb_inside_loop_p (def_bb->loop_father, bb));
}

/* Checks invariants of loop closed ssa form in statement STMT in BB.  */

static void
check_loop_closed_ssa_stmt (basic_block bb, gimple stmt)
{
  ssa_op_iter iter;
  tree var;

  if (is_gimple_debug (stmt))
    return;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES)
    check_loop_closed_ssa_use (bb, var);
}

/* Checks that invariants of the loop closed ssa form are preserved.
   Call verify_ssa when VERIFY_SSA_P is true.  */

DEBUG_FUNCTION void
verify_loop_closed_ssa (bool verify_ssa_p)
{
  basic_block bb;
  gimple_stmt_iterator bsi;
  gimple phi;
  edge e;
  edge_iterator ei;

  if (number_of_loops () <= 1)
    return;

  if (verify_ssa_p)
    verify_ssa (false);

  timevar_push (TV_VERIFY_LOOP_CLOSED);

  FOR_EACH_BB (bb)
    {
      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  phi = gsi_stmt (bsi);
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    check_loop_closed_ssa_use (e->src,
				       PHI_ARG_DEF_FROM_EDGE (phi, e));
	}

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	check_loop_closed_ssa_stmt (bb, gsi_stmt (bsi));
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
  gimple phi, new_phi;
  tree new_name, name;
  use_operand_p op_p;
  gimple_stmt_iterator psi;
  source_location locus;

  for (psi = gsi_start_phis (dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = gsi_stmt (psi);
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
      SSA_NAME_DEF_STMT (new_name) = new_phi;
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
  gimple last;
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
  gimple last = last_stmt (latch);

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

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    BASIC_BLOCK (i)->flags |= BB_DUPLICATED;

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    add_phi_args_after_copy_bb (BASIC_BLOCK (i));

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    BASIC_BLOCK (i)->flags &= ~BB_DUPLICATED;
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
				    edge orig, VEC (edge, heap) **to_remove,
				    int flags)
{
  unsigned first_new_block;

  if (!loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES))
    return false;
  if (!loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS))
    return false;

#ifdef ENABLE_CHECKING
  if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
    verify_loop_closed_ssa (true);
#endif

  first_new_block = last_basic_block;
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
  gimple exit_if;
  tree ctr_before, ctr_after;
  tree enter_main_cond, exit_base, exit_step, exit_bound;
  enum tree_code exit_cmp;
  gimple phi_old_loop, phi_new_loop, phi_rest;
  gimple_stmt_iterator psi_old_loop, psi_new_loop;
  tree init, next, new_init, var;
  struct loop *new_loop;
  basic_block rest, exit_bb;
  edge old_entry, new_entry, old_latch, precond_edge, new_exit;
  edge new_nonexit, e;
  gimple_stmt_iterator bsi;
  use_operand_p op;
  bool ok;
  unsigned est_niter, prob_entry, scale_unrolled, scale_rest, freq_e, freq_h;
  unsigned new_est_niter, i, prob;
  unsigned irr = loop_preheader_edge (loop)->flags & EDGE_IRREDUCIBLE_LOOP;
  sbitmap wont_exit;
  VEC (edge, heap) *to_remove = NULL;

  est_niter = expected_loop_iterations (loop);
  determine_exit_conditions (loop, desc, factor,
			     &enter_main_cond, &exit_base, &exit_step,
			     &exit_cmp, &exit_bound);

  /* Let us assume that the unrolled loop is quite likely to be entered.  */
  if (integer_nonzerop (enter_main_cond))
    prob_entry = REG_BR_PROB_BASE;
  else
    prob_entry = PROB_UNROLLED_LOOP_ENTERED * REG_BR_PROB_BASE / 100;

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
  scale_rest = REG_BR_PROB_BASE;

  new_loop = loop_version (loop, enter_main_cond, NULL,
			   prob_entry, scale_unrolled, scale_rest, true);
  gcc_assert (new_loop != NULL);
  update_ssa (TODO_update_ssa);

  /* Determine the probability of the exit edge of the unrolled loop.  */
  new_est_niter = est_niter / factor;

  /* Without profile feedback, loops for that we do not know a better estimate
     are assumed to roll 10 times.  When we unroll such loop, it appears to
     roll too little, and it may even seem to be cold.  To avoid this, we
     ensure that the created loop appears to roll at least 5 times (but at
     most as many times as before unrolling).  */
  if (new_est_niter < 5)
    {
      if (est_niter < 5)
	new_est_niter = est_niter;
      else
	new_est_niter = 5;
    }

  /* Prepare the cfg and update the phi nodes.  Move the loop exit to the
     loop latch (and make its condition dummy, for the moment).  */
  rest = loop_preheader_edge (new_loop)->src;
  precond_edge = single_pred_edge (rest);
  split_edge (loop_latch_edge (loop));
  exit_bb = single_pred (loop->latch);

  /* Since the exit edge will be removed, the frequency of all the blocks
     in the loop that are dominated by it must be scaled by
     1 / (1 - exit->probability).  */
  scale_dominated_blocks_in_loop (loop, exit->src,
				  REG_BR_PROB_BASE,
				  REG_BR_PROB_BASE - exit->probability);

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
  new_exit->count = exit->count;
  new_exit->probability = exit->probability;
  new_nonexit = single_pred_edge (loop->latch);
  new_nonexit->probability = REG_BR_PROB_BASE - exit->probability;
  new_nonexit->flags = EDGE_TRUE_VALUE;
  new_nonexit->count -= exit->count;
  if (new_nonexit->count < 0)
    new_nonexit->count = 0;
  scale_bbs_frequencies_int (&loop->latch, 1, new_nonexit->probability,
			     REG_BR_PROB_BASE);

  old_entry = loop_preheader_edge (loop);
  new_entry = loop_preheader_edge (new_loop);
  old_latch = loop_latch_edge (loop);
  for (psi_old_loop = gsi_start_phis (loop->header),
       psi_new_loop = gsi_start_phis (new_loop->header);
       !gsi_end_p (psi_old_loop);
       gsi_next (&psi_old_loop), gsi_next (&psi_new_loop))
    {
      phi_old_loop = gsi_stmt (psi_old_loop);
      phi_new_loop = gsi_stmt (psi_new_loop);

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
	var = SSA_NAME_VAR (next);
      else if (TREE_CODE (init) == SSA_NAME
	       && useless_type_conversion_p (TREE_TYPE (init),
					     TREE_TYPE (next)))
	var = SSA_NAME_VAR (init);
      else if (useless_type_conversion_p (TREE_TYPE (next), TREE_TYPE (init)))
	{
	  var = create_tmp_var (TREE_TYPE (next), "unrinittmp");
	  add_referenced_var (var);
	}
      else
	{
	  var = create_tmp_var (TREE_TYPE (init), "unrinittmp");
	  add_referenced_var (var);
	}

      new_init = make_ssa_name (var, NULL);
      phi_rest = create_phi_node (new_init, rest);
      SSA_NAME_DEF_STMT (new_init) = phi_rest;

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
  wont_exit = sbitmap_alloc (factor);
  sbitmap_ones (wont_exit);
  RESET_BIT (wont_exit, factor - 1);

  ok = gimple_duplicate_loop_to_header_edge
	  (loop, loop_latch_edge (loop), factor - 1,
	   wont_exit, new_exit, &to_remove, DLTHE_FLAG_UPDATE_FREQ);
  free (wont_exit);
  gcc_assert (ok);

  FOR_EACH_VEC_ELT (edge, to_remove, i, e)
    {
      ok = remove_path (e);
      gcc_assert (ok);
    }
  VEC_free (edge, heap, to_remove);
  update_ssa (TODO_update_ssa);

  /* Ensure that the frequencies in the loop match the new estimated
     number of iterations, and change the probability of the new
     exit edge.  */
  freq_h = loop->header->frequency;
  freq_e = EDGE_FREQUENCY (loop_preheader_edge (loop));
  if (freq_h != 0)
    scale_loop_frequencies (loop, freq_e * (new_est_niter + 1), freq_h);

  exit_bb = single_pred (loop->latch);
  new_exit = find_edge (exit_bb, rest);
  new_exit->count = loop_preheader_edge (loop)->count;
  new_exit->probability = REG_BR_PROB_BASE / (new_est_niter + 1);

  rest->count += new_exit->count;
  rest->frequency += EDGE_FREQUENCY (new_exit);

  new_nonexit = single_pred_edge (loop->latch);
  prob = new_nonexit->probability;
  new_nonexit->probability = REG_BR_PROB_BASE - new_exit->probability;
  new_nonexit->count = exit_bb->count - new_exit->count;
  if (new_nonexit->count < 0)
    new_nonexit->count = 0;
  if (prob > 0)
    scale_bbs_frequencies_int (&loop->latch, 1, new_nonexit->probability,
			       prob);

  /* Finally create the new counter for number of iterations and add the new
     exit instruction.  */
  bsi = gsi_last_nondebug_bb (exit_bb);
  exit_if = gsi_stmt (bsi);
  create_iv (exit_base, exit_step, NULL_TREE, loop,
	     &bsi, false, &ctr_before, &ctr_after);
  gimple_cond_set_code (exit_if, exit_cmp);
  gimple_cond_set_lhs (exit_if, ctr_after);
  gimple_cond_set_rhs (exit_if, exit_bound);
  update_stmt (exit_if);

#ifdef ENABLE_CHECKING
  verify_flow_info ();
  verify_dominators (CDI_DOMINATORS);
  verify_loop_structure ();
  verify_loop_closed_ssa (true);
#endif
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
		     gimple_stmt_iterator *psi,
		     gimple_stmt_iterator *gsi,
		     tree main_iv)
{
  affine_iv iv;
  gimple stmt, phi = gsi_stmt (*psi);
  tree atype, mtype, val, res = PHI_RESULT (phi);

  if (!is_gimple_reg (res) || res == main_iv)
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
  SSA_NAME_DEF_STMT (res) = stmt;
}

/* Rewrite all the phi nodes of LOOP in function of the main induction
   variable MAIN_IV.  */

static void
rewrite_all_phi_nodes_with_iv (loop_p loop, tree main_iv)
{
  unsigned i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);
  gimple_stmt_iterator psi;

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

/* Bases all the induction variables in LOOP on a single induction
   variable (unsigned with base 0 and step 1), whose final value is
   compared with *NIT.  When the IV type precision has to be larger
   than *NIT type precision, *NIT is converted to the larger type, the
   conversion code is inserted before the loop, and *NIT is updated to
   the new definition.  When BUMP_IN_LATCH is true, the induction
   variable is incremented in the loop latch, otherwise it is
   incremented in the loop header.  Return the induction variable that
   was created.  */

tree
canonicalize_loop_ivs (struct loop *loop, tree *nit, bool bump_in_latch)
{
  unsigned precision = TYPE_PRECISION (TREE_TYPE (*nit));
  unsigned original_precision = precision;
  tree type, var_before;
  gimple_stmt_iterator gsi, psi;
  gimple stmt;
  edge exit = single_dom_exit (loop);
  gimple_seq stmts;
  enum machine_mode mode;
  bool unsigned_p = false;

  for (psi = gsi_start_phis (loop->header);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gimple phi = gsi_stmt (psi);
      tree res = PHI_RESULT (phi);
      bool uns;

      type = TREE_TYPE (res);
      if (!is_gimple_reg (res)
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

  mode = smallest_mode_for_size (precision, MODE_INT);
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

  stmt = last_stmt (exit->src);
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
