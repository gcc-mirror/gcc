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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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
	  step1 = fold (build1 (NEGATE_EXPR, TREE_TYPE (step), step));
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
	      step = fold (build1 (NEGATE_EXPR, TREE_TYPE (step), step));
	    }
	}
    }

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
    {
      edge pe = loop_preheader_edge (loop);

      bsi_insert_on_edge_immediate_loop (pe, stmts);
    }

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

  FOR_EACH_EDGE (e, ei, exit->preds)
    add_phi_arg (phi, use, e);

  SSA_NAME_DEF_STMT (use) = def_stmt;
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

  bitmap_clear_bit (livein, def_bb->index);

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
   to USE_BLOCKS.  */

static void
find_uses_to_rename_use (basic_block bb, tree use, bitmap *use_blocks)
{
  unsigned ver;
  basic_block def_bb;
  struct loop *def_loop;

  if (TREE_CODE (use) != SSA_NAME)
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

  if (!flow_bb_inside_loop_p (def_loop, bb))
    mark_for_rewrite (use);
}

/* For uses in STMT, mark names that are used outside of the loop they are
   defined to rewrite.  Record the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  */

static void
find_uses_to_rename_stmt (tree stmt, bitmap *use_blocks)
{
  ssa_op_iter iter;
  tree var;
  basic_block bb = bb_for_stmt (stmt);

  get_stmt_operands (stmt);

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES | SSA_OP_ALL_KILLS)
    find_uses_to_rename_use (bb, var, use_blocks);
}

/* Marks names that are used outside of the loop they are defined in
   for rewrite.  Records the set of blocks in that the ssa
   names are defined to USE_BLOCKS.  */

static void
find_uses_to_rename (bitmap *use_blocks)
{
  basic_block bb;
  block_stmt_iterator bsi;
  tree phi;
  unsigned i;

  FOR_EACH_BB (bb)
    {
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
	  find_uses_to_rename_use (EDGE_PRED (bb, i)->src,
				   PHI_ARG_DEF (phi, i), use_blocks);

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	find_uses_to_rename_stmt (bsi_stmt (bsi), use_blocks);
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
      base 99 and step 1.  */

void
rewrite_into_loop_closed_ssa (void)
{
  bitmap loop_exits = get_loops_exits ();
  bitmap *use_blocks;
  unsigned i;
  bitmap names_to_rename;

  gcc_assert (!any_marked_for_rewrite_p ());

  use_blocks = xcalloc (num_ssa_names, sizeof (bitmap));

  /* Find the uses outside loops.  */
  find_uses_to_rename (use_blocks);

  /* Add the phi nodes on exits of the loops for the names we need to
     rewrite.  */
  names_to_rename = marked_ssa_names ();
  add_exit_phis (names_to_rename, use_blocks, loop_exits);

  for (i = 0; i < num_ssa_names; i++)
    BITMAP_FREE (use_blocks[i]);
  free (use_blocks);
  BITMAP_FREE (loop_exits);
  BITMAP_FREE (names_to_rename);

  /* Do the rewriting.  */
  rewrite_ssa_into_ssa ();
}

/* Check invariants of the loop closed ssa form for the USE in BB.  */

static void
check_loop_closed_ssa_use (basic_block bb, tree use)
{
  tree def;
  basic_block def_bb;
  
  if (TREE_CODE (use) != SSA_NAME)
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

  get_stmt_operands (stmt);

  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_ALL_USES)
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

  verify_ssa ();

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
      op_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, EDGE_SUCC (bb, 0));

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

  if (EDGE_COUNT (loop->latch->preds) > 1)
    return NULL;

  bb = EDGE_PRED (loop->latch, 0)->src;
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
    BASIC_BLOCK (i)->rbi->duplicated = 1;

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    add_phi_args_after_copy_bb (BASIC_BLOCK (i));

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    BASIC_BLOCK (i)->rbi->duplicated = 0;
}

/* Renames variables in the area copied by tree_duplicate_loop_to_header_edge.
   FIRST_NEW_BLOCK is the first block in the copied area.   DEFINITIONS is
   a bitmap of all ssa names defined inside the loop.  */

static void
rename_variables (unsigned first_new_block, bitmap definitions)
{
  unsigned i, copy_number = 0;
  basic_block bb;
  htab_t ssa_name_map = NULL;

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    {
      bb = BASIC_BLOCK (i);

      /* We assume that first come all blocks from the first copy, then all
	 blocks from the second copy, etc.  */
      if (copy_number != (unsigned) bb->rbi->copy_number)
	{
	  allocate_ssa_names (definitions, &ssa_name_map);
	  copy_number = bb->rbi->copy_number;
	}

      rewrite_to_new_ssa_names_bb (bb, ssa_name_map);
    }

  htab_delete (ssa_name_map);
}

/* Sets SSA_NAME_DEF_STMT for results of all phi nodes in BB.  */

static void
set_phi_def_stmts (basic_block bb)
{
  tree phi;

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    SSA_NAME_DEF_STMT (PHI_RESULT (phi)) = phi;
}

/* The same as cfgloopmanip.c:duplicate_loop_to_header_edge, but also updates
   ssa.  In order to achieve this, only loops whose exits all lead to the same
   location are handled.
   
   FIXME: we create some degenerate phi nodes that could be avoided by copy
   propagating them instead.  Unfortunately this is not completely
   straightforward due to problems with constant folding.  */

bool
tree_duplicate_loop_to_header_edge (struct loop *loop, edge e,
				    struct loops *loops,
				    unsigned int ndupl, sbitmap wont_exit,
				    edge orig, edge *to_remove,
				    unsigned int *n_to_remove, int flags)
{
  unsigned first_new_block;
  basic_block bb;
  unsigned i;
  bitmap definitions;

  if (!(loops->state & LOOPS_HAVE_SIMPLE_LATCHES))
    return false;
  if (!(loops->state & LOOPS_HAVE_PREHEADERS))
    return false;

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif

  gcc_assert (!any_marked_for_rewrite_p ());

  first_new_block = last_basic_block;
  if (!duplicate_loop_to_header_edge (loop, e, loops, ndupl, wont_exit,
				      orig, to_remove, n_to_remove, flags))
    return false;

  /* Readd the removed phi args for e.  */
  flush_pending_stmts (e);

  /* Copy the phi node arguments.  */
  copy_phi_node_args (first_new_block);

  /* Rename the variables.  */
  definitions = marked_ssa_names ();
  rename_variables (first_new_block, definitions);
  unmark_all_for_rewrite ();
  BITMAP_FREE (definitions);

  /* For some time we have the identical ssa names as results in multiple phi
     nodes.  When phi node is resized, it sets SSA_NAME_DEF_STMT of its result
     to the new copy.  This means that we cannot easily ensure that the ssa
     names defined in those phis are pointing to the right one -- so just
     recompute SSA_NAME_DEF_STMT for them.  */ 

  for (i = first_new_block; i < (unsigned) last_basic_block; i++)
    {
      bb = BASIC_BLOCK (i);
      set_phi_def_stmts (bb);
      if (bb->rbi->copy_number == 1)
  	set_phi_def_stmts (bb->rbi->original);
    }

  scev_reset ();
#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
#endif

  return true;
}

/*---------------------------------------------------------------------------
  Loop versioning
  ---------------------------------------------------------------------------*/
 
/* Adjust phi nodes for 'first' basic block.  'second' basic block is a copy
   of 'first'. Both of them are dominated by 'new_head' basic block. When
   'new_head' was created by 'second's incoming edge it received phi arguments
   on the edge by split_edge(). Later, additional edge 'e' was created to
   connect 'new_head' and 'first'. Now this routine adds phi args on this 
   additional edge 'e' that new_head to second edge received as part of edge 
   splitting.
*/

static void
lv_adjust_loop_header_phi (basic_block first, basic_block second,
			   basic_block new_head, edge e)
{
  tree phi1, phi2;

  /* Browse all 'second' basic block phi nodes and add phi args to
     edge 'e' for 'first' head. PHI args are always in correct order.  */

  for (phi2 = phi_nodes (second), phi1 = phi_nodes (first); 
       phi2 && phi1; 
       phi2 = PHI_CHAIN (phi2),  phi1 = PHI_CHAIN (phi1))
    {
      edge e2 = find_edge (new_head, second);

      if (e2)
	{
	  tree def = PHI_ARG_DEF (phi2, e2->dest_idx);
	  add_phi_arg (phi1, def, e);
	}
    }
}

/* Adjust entry edge for lv.
   
  e is an incoming edge. 

  --- edge e ---- > [second_head]

  Split it and insert new conditional expression and adjust edges.
   
   --- edge e ---> [cond expr] ---> [first_head]
                        |
                        +---------> [second_head]

*/
   
static basic_block
lv_adjust_loop_entry_edge (basic_block first_head,
			   basic_block second_head,
			   edge e,
			   tree cond_expr)
{ 
  block_stmt_iterator bsi;
  basic_block new_head = NULL;
  tree goto1 = NULL_TREE;
  tree goto2 = NULL_TREE;
  tree new_cond_expr = NULL_TREE;
  edge e0, e1;

  gcc_assert (e->dest == second_head);

  /* Split edge 'e'. This will create a new basic block, where we can
     insert conditional expr.  */
  new_head = split_edge (e);

  /* Build new conditional expr */
  goto1 = build1 (GOTO_EXPR, void_type_node, tree_block_label (first_head));
  goto2 = build1 (GOTO_EXPR, void_type_node, tree_block_label (second_head));
  new_cond_expr = build3 (COND_EXPR, void_type_node, cond_expr, goto1, goto2);

  /* Add new cond. in new head.  */ 
  bsi = bsi_start (new_head); 
  bsi_insert_after (&bsi, new_cond_expr, BSI_NEW_STMT);

  /* Adjust edges appropriately to connect new head with first head
     as well as second head.  */
  e0 = EDGE_SUCC (new_head, 0);
  e0->flags &= ~EDGE_FALLTHRU;
  e0->flags |= EDGE_FALSE_VALUE;
  e1 = make_edge (new_head, first_head, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, first_head, new_head);
  set_immediate_dominator (CDI_DOMINATORS, second_head, new_head);

  /* Adjust loop header phi nodes.  */
  lv_adjust_loop_header_phi (first_head, second_head, new_head, e1);

  return new_head;
}

/* Main entry point for Loop Versioning transformation.
   
This transformation given a condition and a loop, creates
-if (condition) { loop_copy1 } else { loop_copy2 },
where loop_copy1 is the loop transformed in one way, and loop_copy2
is the loop transformed in another way (or unchanged). 'condition'
may be a run time test for things that were not resolved by static
analysis (overlapping ranges (anti-aliasing), alignment, etc.).  */

struct loop *
tree_ssa_loop_version (struct loops *loops, struct loop * loop, 
		       tree cond_expr, basic_block *condition_bb)
{
  edge entry, latch_edge, exit, true_edge, false_edge;
  basic_block first_head, second_head;
  int irred_flag;
  struct loop *nloop;

  /* CHECKME: Loop versioning does not handle nested loop at this point.  */
  if (loop->inner)
    return NULL;

  /* Record entry and latch edges for the loop */
  entry = loop_preheader_edge (loop);

  /* Note down head of loop as first_head.  */
  first_head = entry->dest;

  /* Duplicate loop.  */
  irred_flag = entry->flags & EDGE_IRREDUCIBLE_LOOP;
  entry->flags &= ~EDGE_IRREDUCIBLE_LOOP;
  if (!tree_duplicate_loop_to_header_edge (loop, entry, loops, 1,
					   NULL, NULL, NULL, NULL, 0))
    {
      entry->flags |= irred_flag;
      return NULL;
    }

  /* After duplication entry edge now points to new loop head block.
     Note down new head as second_head.  */
  second_head = entry->dest;

  /* Split loop entry edge and insert new block with cond expr.  */
  *condition_bb = lv_adjust_loop_entry_edge (first_head, second_head, entry, 
					    cond_expr); 

  latch_edge = EDGE_SUCC (loop->latch->rbi->copy, 0);
  
  extract_true_false_edges_from_block (*condition_bb, &true_edge, &false_edge);
  nloop = loopify (loops, 
		   latch_edge,
		   EDGE_PRED (loop->header->rbi->copy, 0),
		   *condition_bb, true_edge, false_edge,
		   false /* Do not redirect all edges.  */);

  exit = loop->single_exit;
  if (exit)
    nloop->single_exit = find_edge (exit->src->rbi->copy, exit->dest);

  /* loopify redirected latch_edge. Update its PENDING_STMTS.  */ 
  flush_pending_stmts (latch_edge);

  /* loopify redirected condition_bb's succ edge. Update its PENDING_STMTS.  */ 
  extract_true_false_edges_from_block (*condition_bb, &true_edge, &false_edge);
  flush_pending_stmts (false_edge);

  /* Adjust irreducible flag.  */
  if (irred_flag)
    {
      (*condition_bb)->flags |= BB_IRREDUCIBLE_LOOP;
      loop_preheader_edge (loop)->flags |= EDGE_IRREDUCIBLE_LOOP;
      loop_preheader_edge (nloop)->flags |= EDGE_IRREDUCIBLE_LOOP;
      EDGE_PRED ((*condition_bb), 0)->flags |= EDGE_IRREDUCIBLE_LOOP;
    }

  /* At this point condition_bb is loop predheader with two successors, 
     first_head and second_head.   Make sure that loop predheader has only 
     one successor.  */
  loop_split_edge_with (loop_preheader_edge (loop), NULL);
  loop_split_edge_with (loop_preheader_edge (nloop), NULL);

  return nloop;
}
