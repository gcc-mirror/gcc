/* High-level loop manipulation functions.
   Copyright (C) 2004 Free Software Foundation, Inc.
   
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

/* Add exit phis for the USE on EXIT.  */

static void
add_exit_phis_edge (basic_block exit, tree use)
{
  tree phi, def_stmt = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = bb_for_stmt (def_stmt);
  struct loop *def_loop;
  edge e;

  /* Check that some of the edges entering the EXIT block exits a loop in
     that USE is defined.  */
  for (e = exit->pred; e; e = e->pred_next)
    {
      def_loop = find_common_loop (def_bb->loop_father, e->src->loop_father);
      if (!flow_bb_inside_loop_p (def_loop, e->dest))
	break;
    }

  if (!e)
    return;

  phi = create_phi_node (use, exit);

  for (e = exit->pred; e; e = e->pred_next)
    add_phi_arg (&phi, use, e);

  SSA_NAME_DEF_STMT (use) = def_stmt;
}

/* Add exit phis for VAR that is used in LIVEIN.
   Exits of the loops are stored in EXITS.  */

static void
add_exit_phis_var (tree var, bitmap livein, bitmap exits)
{
  bitmap def;
  int index;
  basic_block def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (var));

  bitmap_clear_bit (livein, def_bb->index);

  def = BITMAP_XMALLOC ();
  bitmap_set_bit (def, def_bb->index);
  compute_global_livein (livein, def);
  BITMAP_XFREE (def);

  EXECUTE_IF_AND_IN_BITMAP (exits, livein, 0, index,
			    add_exit_phis_edge (BASIC_BLOCK (index), var));
}

/* Add exit phis for the names marked in NAMES_TO_RENAME.
   Exits of the loops are stored in EXITS.  Sets of blocks where the ssa
   names are used are stored in USE_BLOCKS.  */

static void
add_exit_phis (bitmap names_to_rename, bitmap *use_blocks, bitmap loop_exits)
{
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (names_to_rename, 0, i,
    {
      add_exit_phis_var (ssa_name (i), use_blocks[i], loop_exits);
    });
}

/* Returns a bitmap of all loop exit edge targets.  */

static bitmap
get_loops_exits (void)
{
  bitmap exits = BITMAP_XMALLOC ();
  basic_block bb;
  edge e;

  FOR_EACH_BB (bb)
    {
      for (e = bb->pred; e; e = e->pred_next)
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
    use_blocks[ver] = BITMAP_XMALLOC ();
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
  use_optype uses;
  vuse_optype vuses;
  v_may_def_optype v_may_defs;
  stmt_ann_t ann;
  unsigned i;
  basic_block bb = bb_for_stmt (stmt);

  get_stmt_operands (stmt);
  ann = stmt_ann (stmt);

  uses = USE_OPS (ann);
  for (i = 0; i < NUM_USES (uses); i++)
    find_uses_to_rename_use (bb, USE_OP (uses, i), use_blocks);

  vuses = VUSE_OPS (ann);
  for (i = 0; i < NUM_VUSES (vuses); i++)
    find_uses_to_rename_use (bb, VUSE_OP (vuses, i),use_blocks);

  v_may_defs = V_MAY_DEF_OPS (ann);
  for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
    find_uses_to_rename_use (bb, V_MAY_DEF_OP (v_may_defs, i), use_blocks);
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
      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
	  find_uses_to_rename_use (PHI_ARG_EDGE (phi, i)->src,
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

  if (any_marked_for_rewrite_p ())
    abort ();

  use_blocks = xcalloc (num_ssa_names, sizeof (bitmap));

  /* Find the uses outside loops.  */
  find_uses_to_rename (use_blocks);

  /* Add the phi nodes on exits of the loops for the names we need to
     rewrite.  */
  names_to_rename = marked_ssa_names ();
  add_exit_phis (names_to_rename, use_blocks, loop_exits);

  for (i = 0; i < num_ssa_names; i++)
    BITMAP_XFREE (use_blocks[i]);
  free (use_blocks);
  BITMAP_XFREE (loop_exits);
  BITMAP_XFREE (names_to_rename);

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
  if (def_bb
      && !flow_bb_inside_loop_p (def_bb->loop_father, bb))
    abort ();
}

/* Checks invariants of loop closed ssa form in statement STMT in BB.  */

static void
check_loop_closed_ssa_stmt (basic_block bb, tree stmt)
{
  use_optype uses;
  vuse_optype vuses;
  v_may_def_optype v_may_defs;
  stmt_ann_t ann;
  unsigned i;

  get_stmt_operands (stmt);
  ann = stmt_ann (stmt);

  uses = USE_OPS (ann);
  for (i = 0; i < NUM_USES (uses); i++)
    check_loop_closed_ssa_use (bb, USE_OP (uses, i));

  vuses = VUSE_OPS (ann);
  for (i = 0; i < NUM_VUSES (vuses); i++)
    check_loop_closed_ssa_use (bb, VUSE_OP (vuses, i));

  v_may_defs = V_MAY_DEF_OPS (ann);
  for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
    check_loop_closed_ssa_use (bb, V_MAY_DEF_OP (v_may_defs, i));
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
      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
	  check_loop_closed_ssa_use (PHI_ARG_EDGE (phi, i)->src,
				     PHI_ARG_DEF (phi, i));

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	check_loop_closed_ssa_stmt (bb, bsi_stmt (bsi));
    }
}
