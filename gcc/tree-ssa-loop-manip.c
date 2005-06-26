/* High-level loop manipulation functions.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "cfglayout.h"
#include "tree-scalar-evolution.h"

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
	   block_stmt_iterator *incr_pos, bool after,
	   tree *var_before, tree *var_after)
{
  tree stmt, initial, step1, stmts;
  tree vb, va;
  enum tree_code incr_op = PLUS_EXPR;
  edge pe = loop_preheader_edge (loop);

  if (!var)
    {
      var = create_tmp_var (TREE_TYPE (base), "ivtmp");
      add_referenced_tmp_var (var);
    }

  vb = make_ssa_name (var, NULL_TREE);
  if (var_before)
    *var_before = vb;
  va = make_ssa_name (var, NULL_TREE);
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
	  if (!tree_expr_nonnegative_p (step)
	      && may_negate_without_overflow_p (step))
	    {
	      incr_op = MINUS_EXPR;
	      step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	    }
	}
    }

  /* Gimplify the step if necessary.  We put the computations in front of the
     loop (i.e. the step should be loop invariant).  */
  step = force_gimple_operand (step, &stmts, true, var);
  if (stmts)
    bsi_insert_on_edge_immediate_loop (pe, stmts);

  stmt = build2 (MODIFY_EXPR, void_type_node, va,
		 build2 (incr_op, TREE_TYPE (base),
			 vb, step));
  SSA_NAME_DEF_STMT (va) = stmt;
  if (after)
    bsi_insert_after (incr_pos, stmt, BSI_NEW_STMT);
  else
    bsi_insert_before (incr_pos, stmt, BSI_NEW_STMT);

  initial = force_gimple_operand (base, &stmts, true, var);
  if (stmts)
    bsi_insert_on_edge_immediate_loop (pe, stmts);

  stmt = create_phi_node (vb, loop->header);
  SSA_NAME_DEF_STMT (vb) = stmt;
  add_phi_arg (stmt, initial, loop_preheader_edge (loop));
  add_phi_arg (stmt, va, loop_latch_edge (loop));
}

/* Add exit phis for the USE on EXIT.  */

static void
add_exit_phis_edge (basic_block exit, tree use)
{
  tree phi, def_stmt = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = bb_for_stmt (def_stmt);
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
  create_new_def_for (PHI_RESULT (phi), phi, PHI_RESULT_PTR (phi));
  FOR_EACH_EDGE (e, ei, exit->preds)
    add_phi_arg (phi, use, e);
}

/* Add exit phis for VAR that is used in LIVEIN.
   Exits of the loops are stored in EXITS.  */

static void
add_exit_phis_var (tree var, bitmap livein, bitmap exits)
{
  bitmap def;
  unsigned index;
  basic_block def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (var));
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
  def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (use));
  if (!def_bb)
    return;
  def_loop = def_bb->loop_father;

  /* If the definition is not inside loop, it is not interesting.  */
  if (!def_loop->outer)
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
find_uses_to_rename_stmt (tree stmt, bitmap *use_blocks, bitmap need_phis)
{
  ssa_op_iter iter;
  tree var;
  basic_block bb = bb_for_stmt (stmt);

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
    find_uses_to_rename_use (bb, var, use_blocks, need_phis);
}

/* Marks names that are used in BB and outside of the loop they are
   defined in for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  Record the SSA names that will
   need exit PHIs in NEED_PHIS.  */

static void
find_uses_to_rename_bb (basic_block bb, bitmap *use_blocks, bitmap need_phis)
{
  block_stmt_iterator bsi;
  edge e;
  edge_iterator ei;
  tree phi;

  FOR_EACH_EDGE (e, ei, bb->succs)
    for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
      find_uses_to_rename_use (bb, PHI_ARG_DEF_FROM_EDGE (phi, e),
			       use_blocks, need_phis);
 
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    find_uses_to_rename_stmt (bsi_stmt (bsi), use_blocks, need_phis);
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
  bitmap loop_exits = get_loops_exits ();
  bitmap *use_blocks;
  unsigned i, old_num_ssa_names;
  bitmap names_to_rename = BITMAP_ALLOC (NULL);

  /* If the pass has caused the SSA form to be out-of-date, update it
     now.  */
  update_ssa (update_flag);

  old_num_ssa_names = num_ssa_names;
  use_blocks = xcalloc (old_num_ssa_names, sizeof (bitmap));

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
  tree def;
  basic_block def_bb;
  
  if (TREE_CODE (use) != SSA_NAME || !is_gimple_reg (use))
    return;

  def = SSA_NAME_DEF_STMT (use);
  def_bb = bb_for_stmt (def);
  gcc_assert (!def_bb
	      || flow_bb_inside_loop_p (def_bb->loop_father, bb));
}

/* Checks invariants of loop closed ssa form in statement STMT in BB.  */

static void
check_loop_closed_ssa_stmt (basic_block bb, tree stmt)
{
  ssa_op_iter iter;
  tree var;

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
    check_loop_closed_ssa_use (bb, var);
}

/* Checks that invariants of the loop closed ssa form are preserved.  */

void
verify_loop_closed_ssa (void)
{
  basic_block bb;
  block_stmt_iterator bsi;
  tree phi;
  unsigned i;

  if (current_loops == NULL)
    return;

  verify_ssa (false);

  FOR_EACH_BB (bb)
    {
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
	  check_loop_closed_ssa_use (PHI_ARG_EDGE (phi, i)->src,
				     PHI_ARG_DEF (phi, i));

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	check_loop_closed_ssa_stmt (bb, bsi_stmt (bsi));
    }
}

/* Split loop exit edge EXIT.  The things are a bit complicated by a need to
   preserve the loop closed ssa form.  */

void
split_loop_exit_edge (edge exit)
{
  basic_block dest = exit->dest;
  basic_block bb = loop_split_edge_with (exit, NULL);
  tree phi, new_phi, new_name, name;
  use_operand_p op_p;

  for (phi = phi_nodes (dest); phi; phi = PHI_CHAIN (phi))
    {
      op_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, single_succ_edge (bb));

      name = USE_FROM_PTR (op_p);

      /* If the argument of the phi node is a constant, we do not need
	 to keep it inside loop.  */
      if (TREE_CODE (name) != SSA_NAME)
	continue;

      /* Otherwise create an auxiliary phi node that will copy the value
	 of the ssa name out of the loop.  */
      new_name = duplicate_ssa_name (name, NULL);
      new_phi = create_phi_node (new_name, bb);
      SSA_NAME_DEF_STMT (new_name) = new_phi;
      add_phi_arg (new_phi, name, exit);
      SET_USE (op_p, new_name);
    }
}

/* Insert statement STMT to the edge E and update the loop structures.
   Returns the newly created block (if any).  */

basic_block
bsi_insert_on_edge_immediate_loop (edge e, tree stmt)
{
  basic_block src, dest, new_bb;
  struct loop *loop_c;

  src = e->src;
  dest = e->dest;

  loop_c = find_common_loop (src->loop_father, dest->loop_father);

  new_bb = bsi_insert_on_edge_immediate (e, stmt);

  if (!new_bb)
    return NULL;

  add_bb_to_loop (new_bb, loop_c);
  if (dest->loop_father->latch == src)
    dest->loop_father->latch = new_bb;

  return new_bb;
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
  tree last;
  basic_block bb;
  edge exit;

  if (!single_pred_p (loop->latch))
    return NULL;

  bb = single_pred (loop->latch);
  last = last_stmt (bb);
  if (TREE_CODE (last) != COND_EXPR)
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
standard_iv_increment_position (struct loop *loop, block_stmt_iterator *bsi,
				bool *insert_after)
{
  basic_block bb = ip_normal_pos (loop), latch = ip_end_pos (loop);
  tree last = last_stmt (latch);

  if (!bb
      || (last && TREE_CODE (last) != LABEL_EXPR))
    {
      *bsi = bsi_last (latch);
      *insert_after = true;
    }
  else
    {
      *bsi = bsi_last (bb);
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
tree_duplicate_loop_to_header_edge (struct loop *loop, edge e,
				    struct loops *loops,
				    unsigned int ndupl, sbitmap wont_exit,
				    edge orig, edge *to_remove,
				    unsigned int *n_to_remove, int flags)
{
  unsigned first_new_block;

  if (!(loops->state & LOOPS_HAVE_SIMPLE_LATCHES))
    return false;
  if (!(loops->state & LOOPS_HAVE_PREHEADERS))
    return false;

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif

  first_new_block = last_basic_block;
  if (!duplicate_loop_to_header_edge (loop, e, loops, ndupl, wont_exit,
				      orig, to_remove, n_to_remove, flags))
    return false;

  /* Readd the removed phi args for e.  */
  flush_pending_stmts (e);

  /* Copy the phi node arguments.  */
  copy_phi_node_args (first_new_block);

  scev_reset ();

  return true;
}
