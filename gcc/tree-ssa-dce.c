/* Dead code elimination pass for the GNU compiler.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Ben Elliston <bje@redhat.com>
   and Andrew MacLeod <amacleod@redhat.com>
   Adapted to use control dependence by Steven Bosscher, SUSE Labs.
 
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

/* Dead code elimination.

   References:

     Building an Optimizing Compiler,
     Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     Advanced Compiler Design and Implementation,
     Steven Muchnick, Morgan Kaufmann, 1997, Section 18.10.

   Dead-code elimination is the removal of statements which have no
   impact on the program's output.  "Dead statements" have no impact
   on the program's output, while "necessary statements" may have
   impact on the output.

   The algorithm consists of three phases:
   1. Marking as necessary all statements known to be necessary,
      e.g. most function calls, writing a value to memory, etc;
   2. Propagating necessary statements, e.g., the statements
      giving values to operands in necessary statements; and
   3. Removing dead statements.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"

#include "tree.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "flags.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"

static struct stmt_stats
{
  int total;
  int total_phis;
  int removed;
  int removed_phis;
} stats;

#define STMT_NECESSARY GF_PLF_1

static VEC(gimple,heap) *worklist;

/* Vector indicating an SSA name has already been processed and marked
   as necessary.  */
static sbitmap processed;

/* Vector indicating that last_stmt if a basic block has already been
   marked as necessary.  */
static sbitmap last_stmt_necessary;

/* Before we can determine whether a control branch is dead, we need to
   compute which blocks are control dependent on which edges.

   We expect each block to be control dependent on very few edges so we
   use a bitmap for each block recording its edges.  An array holds the
   bitmap.  The Ith bit in the bitmap is set if that block is dependent
   on the Ith edge.  */
static bitmap *control_dependence_map;

/* Vector indicating that a basic block has already had all the edges
   processed that it is control dependent on.  */
static sbitmap visited_control_parents;

/* TRUE if this pass alters the CFG (by removing control statements).
   FALSE otherwise.

   If this pass alters the CFG, then it will arrange for the dominators
   to be recomputed.  */
static bool cfg_altered;

/* Execute code that follows the macro for each edge (given number
   EDGE_NUMBER within the CODE) for which the block with index N is
   control dependent.  */
#define EXECUTE_IF_CONTROL_DEPENDENT(BI, N, EDGE_NUMBER)	\
  EXECUTE_IF_SET_IN_BITMAP (control_dependence_map[(N)], 0,	\
			    (EDGE_NUMBER), (BI))


/* Indicate block BB is control dependent on an edge with index EDGE_INDEX.  */
static inline void
set_control_dependence_map_bit (basic_block bb, int edge_index)
{
  if (bb == ENTRY_BLOCK_PTR)
    return;
  gcc_assert (bb != EXIT_BLOCK_PTR);
  bitmap_set_bit (control_dependence_map[bb->index], edge_index);
}

/* Clear all control dependences for block BB.  */
static inline void
clear_control_dependence_bitmap (basic_block bb)
{
  bitmap_clear (control_dependence_map[bb->index]);
}


/* Find the immediate postdominator PDOM of the specified basic block BLOCK.
   This function is necessary because some blocks have negative numbers.  */

static inline basic_block
find_pdom (basic_block block)
{
  gcc_assert (block != ENTRY_BLOCK_PTR);

  if (block == EXIT_BLOCK_PTR)
    return EXIT_BLOCK_PTR;
  else
    {
      basic_block bb = get_immediate_dominator (CDI_POST_DOMINATORS, block);
      if (! bb)
	return EXIT_BLOCK_PTR;
      return bb;
    }
}


/* Determine all blocks' control dependences on the given edge with edge_list
   EL index EDGE_INDEX, ala Morgan, Section 3.6.  */

static void
find_control_dependence (struct edge_list *el, int edge_index)
{
  basic_block current_block;
  basic_block ending_block;

  gcc_assert (INDEX_EDGE_PRED_BB (el, edge_index) != EXIT_BLOCK_PTR);

  if (INDEX_EDGE_PRED_BB (el, edge_index) == ENTRY_BLOCK_PTR)
    ending_block = single_succ (ENTRY_BLOCK_PTR);
  else
    ending_block = find_pdom (INDEX_EDGE_PRED_BB (el, edge_index));

  for (current_block = INDEX_EDGE_SUCC_BB (el, edge_index);
       current_block != ending_block && current_block != EXIT_BLOCK_PTR;
       current_block = find_pdom (current_block))
    {
      edge e = INDEX_EDGE (el, edge_index);

      /* For abnormal edges, we don't make current_block control
	 dependent because instructions that throw are always necessary
	 anyway.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      set_control_dependence_map_bit (current_block, edge_index);
    }
}


/* Record all blocks' control dependences on all edges in the edge
   list EL, ala Morgan, Section 3.6.  */

static void
find_all_control_dependences (struct edge_list *el)
{
  int i;

  for (i = 0; i < NUM_EDGES (el); ++i)
    find_control_dependence (el, i);
}

/* If STMT is not already marked necessary, mark it, and add it to the
   worklist if ADD_TO_WORKLIST is true.  */
static inline void
mark_stmt_necessary (gimple stmt, bool add_to_worklist)
{
  gcc_assert (stmt);

  if (gimple_plf (stmt, STMT_NECESSARY))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Marking useful stmt: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  gimple_set_plf (stmt, STMT_NECESSARY, true);
  if (add_to_worklist)
    VEC_safe_push (gimple, heap, worklist, stmt);
}


/* Mark the statement defining operand OP as necessary.  */

static inline void
mark_operand_necessary (tree op)
{
  gimple stmt;
  int ver;

  gcc_assert (op);

  ver = SSA_NAME_VERSION (op);
  if (TEST_BIT (processed, ver))
    {
      stmt = SSA_NAME_DEF_STMT (op);
      gcc_assert (gimple_nop_p (stmt)
		  || gimple_plf (stmt, STMT_NECESSARY));
      return;
    }
  SET_BIT (processed, ver);

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (stmt);

  if (gimple_plf (stmt, STMT_NECESSARY) || gimple_nop_p (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "marking necessary through ");
      print_generic_expr (dump_file, op, 0);
      fprintf (dump_file, " stmt ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  gimple_set_plf (stmt, STMT_NECESSARY, true);
  VEC_safe_push (gimple, heap, worklist, stmt);
}


/* Mark STMT as necessary if it obviously is.  Add it to the worklist if
   it can make other statements necessary.

   If AGGRESSIVE is false, control statements are conservatively marked as
   necessary.  */

static void
mark_stmt_if_obviously_necessary (gimple stmt, bool aggressive)
{
  tree lhs = NULL_TREE;
  /* With non-call exceptions, we have to assume that all statements could
     throw.  If a statement may throw, it is inherently necessary.  */
  if (flag_non_call_exceptions
      && stmt_could_throw_p (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  /* Statements that are implicitly live.  Most function calls, asm
     and return statements are required.  Labels and GIMPLE_BIND nodes
     are kept because they are control flow, and we have no way of
     knowing whether they can be removed.  DCE can eliminate all the
     other statements in a block, and CFG can then remove the block
     and labels.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_PREDICT:
    case GIMPLE_LABEL:
      mark_stmt_necessary (stmt, false);
      return;

    case GIMPLE_ASM:
    case GIMPLE_RESX:
    case GIMPLE_RETURN:
    case GIMPLE_CHANGE_DYNAMIC_TYPE:
      mark_stmt_necessary (stmt, true);
      return;

    case GIMPLE_CALL:
      /* Most, but not all function calls are required.  Function calls that
	 produce no result and have no side effects (i.e. const pure
	 functions) are unnecessary.  */
      if (gimple_has_side_effects (stmt))
	{
	  mark_stmt_necessary (stmt, true);
	  return;
	}
      if (!gimple_call_lhs (stmt))
        return;
      lhs = gimple_call_lhs (stmt);
      /* Fall through */

    case GIMPLE_ASSIGN:
      if (!lhs)
        lhs = gimple_assign_lhs (stmt);
      /* These values are mildly magic bits of the EH runtime.  We can't
	 see the entire lifetime of these values until landing pads are
	 generated.  */
      if (TREE_CODE (lhs) == EXC_PTR_EXPR
	  || TREE_CODE (lhs) == FILTER_EXPR)
	{
	  mark_stmt_necessary (stmt, true);
	  return;
	}
      break;

    case GIMPLE_GOTO:
      gcc_assert (!simple_goto_p (stmt));
      mark_stmt_necessary (stmt, true);
      return;

    case GIMPLE_COND:
      gcc_assert (EDGE_COUNT (gimple_bb (stmt)->succs) == 2);
      /* Fall through.  */

    case GIMPLE_SWITCH:
      if (! aggressive)
	mark_stmt_necessary (stmt, true);
      break;

    default:
      break;
    }

  /* If the statement has volatile operands, it needs to be preserved.
     Same for statements that can alter control flow in unpredictable
     ways.  */
  if (gimple_has_volatile_ops (stmt) || is_ctrl_altering_stmt (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  if (is_hidden_global_store (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  return;
}


/* Make corresponding control dependent edges necessary.  We only
   have to do this once for each basic block, so we clear the bitmap
   after we're done.  */
static void
mark_control_dependent_edges_necessary (basic_block bb, struct edge_list *el)
{
  bitmap_iterator bi;
  unsigned edge_number;

  gcc_assert (bb != EXIT_BLOCK_PTR);

  if (bb == ENTRY_BLOCK_PTR)
    return;

  EXECUTE_IF_CONTROL_DEPENDENT (bi, bb->index, edge_number)
    {
      gimple stmt;
      basic_block cd_bb = INDEX_EDGE_PRED_BB (el, edge_number);

      if (TEST_BIT (last_stmt_necessary, cd_bb->index))
	continue;
      SET_BIT (last_stmt_necessary, cd_bb->index);

      stmt = last_stmt (cd_bb);
      if (stmt && is_ctrl_stmt (stmt))
	mark_stmt_necessary (stmt, true);
    }
}


/* Find obviously necessary statements.  These are things like most function
   calls, and stores to file level variables.

   If EL is NULL, control statements are conservatively marked as
   necessary.  Otherwise it contains the list of edges used by control
   dependence analysis.  */

static void
find_obviously_necessary_stmts (struct edge_list *el)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  edge e;
  gimple phi, stmt;

  FOR_EACH_BB (bb)
    {
      /* PHI nodes are never inherently necessary.  */
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  phi = gsi_stmt (gsi);
	  gimple_set_plf (phi, STMT_NECESSARY, false);
	}

      /* Check all statements in the block.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  gimple_set_plf (stmt, STMT_NECESSARY, false);
	  mark_stmt_if_obviously_necessary (stmt, el != NULL);
	}
    }

  if (el)
    {
      /* Prevent the loops from being removed.  We must keep the infinite loops,
	 and we currently do not have a means to recognize the finite ones.  */
      FOR_EACH_BB (bb)
	{
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_DFS_BACK)
	      mark_control_dependent_edges_necessary (e->dest, el);
	}
    }
}


/* Return true if REF is based on an aliased base, otherwise false.  */

static bool
ref_may_be_aliased (tree ref)
{
  while (handled_component_p (ref))
    ref = TREE_OPERAND (ref, 0);
  return !(DECL_P (ref)
	   && !may_be_aliased (ref));
}

struct ref_data {
  tree base;
  HOST_WIDE_INT size;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT max_size;
};

static bitmap visited = NULL;
static unsigned int longest_chain = 0;
static unsigned int total_chain = 0;
static bool chain_ovfl = false;

/* Worker for the walker that marks reaching definitions of REF,
   which is based on a non-aliased decl, necessary.  It returns
   true whenever the defining statement of the current VDEF is
   a kill for REF, as no dominating may-defs are necessary for REF
   anymore.  DATA points to cached get_ref_base_and_extent data for REF.  */

static bool
mark_aliased_reaching_defs_necessary_1 (tree ref, tree vdef, void *data)
{
  gimple def_stmt = SSA_NAME_DEF_STMT (vdef);
  struct ref_data *refd = (struct ref_data *)data;

  /* All stmts we visit are necessary.  */
  mark_operand_necessary (vdef);

  /* If the stmt lhs kills ref, then we can stop walking.  */
  if (gimple_has_lhs (def_stmt)
      && TREE_CODE (gimple_get_lhs (def_stmt)) != SSA_NAME)
    {
      tree base, lhs = gimple_get_lhs (def_stmt);
      HOST_WIDE_INT size, offset, max_size;
      base = get_ref_base_and_extent (lhs, &offset, &size, &max_size);
      /* We can get MEM[symbol: sZ, index: D.8862_1] here,
	 so base == refd->base does not always hold.  */
      if (base == refd->base)
	{
	  /* For a must-alias check we need to be able to constrain
	     the accesses properly.  */
	  if (size != -1 && size == max_size
	      && refd->max_size != -1)
	    {
	      if (offset <= refd->offset
		  && offset + size >= refd->offset + refd->max_size)
		return true;
	    }
	  /* Or they need to be exactly the same.  */
	  else if (operand_equal_p (ref, lhs, 0))
	    return true;
	}
    }

  /* Otherwise keep walking.  */
  return false;
}

static void
mark_aliased_reaching_defs_necessary (gimple stmt, tree ref)
{
  struct ref_data refd;
  unsigned int chain;
  gcc_assert (!chain_ovfl);
  refd.base = get_ref_base_and_extent (ref, &refd.offset, &refd.size,
				       &refd.max_size);
  chain = walk_aliased_vdefs (ref, gimple_vuse (stmt),
			      mark_aliased_reaching_defs_necessary_1,
			      &refd, NULL);
  if (chain > longest_chain)
    longest_chain = chain;
  total_chain += chain;
}

/* Worker for the walker that marks reaching definitions of REF, which
   is not based on a non-aliased decl.  For simplicity we need to end
   up marking all may-defs necessary that are not based on a non-aliased
   decl.  The only job of this walker is to skip may-defs based on
   a non-aliased decl.  */

static bool
mark_all_reaching_defs_necessary_1 (tree ref ATTRIBUTE_UNUSED,
				tree vdef, void *data ATTRIBUTE_UNUSED)
{
  gimple def_stmt = SSA_NAME_DEF_STMT (vdef);

  /* We have to skip already visited (and thus necessary) statements
     to make the chaining work after we dropped back to simple mode.  */
  if (chain_ovfl
      && TEST_BIT (processed, SSA_NAME_VERSION (vdef)))
    {
      gcc_assert (gimple_nop_p (def_stmt)
		  || gimple_plf (def_stmt, STMT_NECESSARY));
      return false;
    }

  /* We want to skip stores to non-aliased variables.  */
  if (!chain_ovfl
      && gimple_assign_single_p (def_stmt))
    {
      tree lhs = gimple_assign_lhs (def_stmt);
      if (!ref_may_be_aliased (lhs))
	return false;
    }

  /* But can stop after the first necessary statement.  */
  mark_operand_necessary (vdef);
  return true;
}

static void
mark_all_reaching_defs_necessary (gimple stmt)
{
  walk_aliased_vdefs (NULL, gimple_vuse (stmt),
		      mark_all_reaching_defs_necessary_1, NULL, &visited);
}

/* Propagate necessity using the operands of necessary statements.
   Process the uses on each statement in the worklist, and add all
   feeding statements which contribute to the calculation of this
   value to the worklist. 

   In conservative mode, EL is NULL.  */

static void
propagate_necessity (struct edge_list *el)
{
  gimple stmt;
  bool aggressive = (el ? true : false); 

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nProcessing worklist:\n");

  while (VEC_length (gimple, worklist) > 0)
    {
      /* Take STMT from worklist.  */
      stmt = VEC_pop (gimple, worklist);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "processing: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      if (aggressive)
	{
	  /* Mark the last statements of the basic blocks that the block
	     containing STMT is control dependent on, but only if we haven't
	     already done so.  */
	  basic_block bb = gimple_bb (stmt);
	  if (bb != ENTRY_BLOCK_PTR
	      && ! TEST_BIT (visited_control_parents, bb->index))
	    {
	      SET_BIT (visited_control_parents, bb->index);
	      mark_control_dependent_edges_necessary (bb, el);
	    }
	}

      if (gimple_code (stmt) == GIMPLE_PHI
	  /* We do not process virtual PHI nodes nor do we track their
	     necessity.  */
	  && is_gimple_reg (gimple_phi_result (stmt)))
	{
	  /* PHI nodes are somewhat special in that each PHI alternative has
	     data and control dependencies.  All the statements feeding the
	     PHI node's arguments are always necessary.  In aggressive mode,
	     we also consider the control dependent edges leading to the
	     predecessor block associated with each PHI alternative as
	     necessary.  */
	  size_t k;

	  for (k = 0; k < gimple_phi_num_args (stmt); k++)
            {
	      tree arg = PHI_ARG_DEF (stmt, k);
	      if (TREE_CODE (arg) == SSA_NAME)
		mark_operand_necessary (arg);
	    }

	  if (aggressive)
	    {
	      for (k = 0; k < gimple_phi_num_args (stmt); k++)
		{
		  basic_block arg_bb = gimple_phi_arg_edge (stmt, k)->src;
		  if (arg_bb != ENTRY_BLOCK_PTR
		      && ! TEST_BIT (visited_control_parents, arg_bb->index))
		    {
		      SET_BIT (visited_control_parents, arg_bb->index);
		      mark_control_dependent_edges_necessary (arg_bb, el);
		    }
		}
	    }
	}
      else
	{
	  /* Propagate through the operands.  Examine all the USE, VUSE and
	     VDEF operands in this statement.  Mark all the statements 
	     which feed this statement's uses as necessary.  */
	  ssa_op_iter iter;
	  tree use;

	  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	    mark_operand_necessary (use);

	  use = gimple_vuse (stmt);
	  if (!use)
	    continue;

	  /* If we dropped to simple mode make all immediately
	     reachable definitions necessary.  */
	  if (chain_ovfl)
	    {
	      mark_all_reaching_defs_necessary (stmt);
	      continue;
	    }

	  /* For statements that may load from memory (have a VUSE) we
	     have to mark all reaching (may-)definitions as necessary.
	     We partition this task into two cases:
	      1) explicit loads based on decls that are not aliased
	      2) implicit loads (like calls) and explicit loads not
	         based on decls that are not aliased (like indirect
		 references or loads from globals)
	     For 1) we mark all reaching may-defs as necessary, stopping
	     at dominating kills.  For 2) we want to mark all dominating
	     references necessary, but non-aliased ones which we handle
	     in 1).  Instead of doing so for each load we rely on the
	     worklist to eventually reach all dominating references and
	     instead just mark the immediately dominating references
	     as necessary (but skipping non-aliased ones).  */

	  if (is_gimple_call (stmt))
	    {
	      unsigned i;

	      /* Calls implicitly load from memory, their arguments
	         in addition may explicitly perform memory loads.
		 This also ensures propagation for case 2 for stores.  */
	      mark_all_reaching_defs_necessary (stmt);
	      for (i = 0; i < gimple_call_num_args (stmt); ++i)
		{
		  tree arg = gimple_call_arg (stmt, i);
		  if (TREE_CODE (arg) == SSA_NAME
		      || is_gimple_min_invariant (arg))
		    continue;
		  if (!ref_may_be_aliased (arg))
		    mark_aliased_reaching_defs_necessary (stmt, arg);
		}
	    }
	  else if (gimple_assign_single_p (stmt))
	    {
	      tree lhs, rhs;
	      bool rhs_aliased = false;
	      /* If this is a load mark things necessary.  */
	      rhs = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (rhs) != SSA_NAME
		  && !is_gimple_min_invariant (rhs))
		{
		  if (!ref_may_be_aliased (rhs))
		    mark_aliased_reaching_defs_necessary (stmt, rhs);
		  else
		    rhs_aliased = true;
		}
	      /* If this is an aliased store, mark things necessary.
		 This is where we make sure to propagate for case 2.  */
	      lhs = gimple_assign_lhs (stmt);
	      if (rhs_aliased
		  || (TREE_CODE (lhs) != SSA_NAME
		      && ref_may_be_aliased (lhs)))
		mark_all_reaching_defs_necessary (stmt);
	    }
	  else if (gimple_code (stmt) == GIMPLE_RETURN)
	    {
	      tree rhs = gimple_return_retval (stmt);
	      /* A return statement may perform a load.  */
	      if (TREE_CODE (rhs) != SSA_NAME
		  && !is_gimple_min_invariant (rhs))
		{
		  if (!ref_may_be_aliased (rhs))
		    mark_aliased_reaching_defs_necessary (stmt, rhs);
		  else
		    mark_all_reaching_defs_necessary (stmt);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_ASM)
	    {
	      unsigned i;
	      mark_all_reaching_defs_necessary (stmt);
	      /* Inputs may perform loads.  */
	      for (i = 0; i < gimple_asm_ninputs (stmt); ++i)
		{
		  tree op = TREE_VALUE (gimple_asm_input_op (stmt, i));
		  if (TREE_CODE (op) != SSA_NAME
		      && !is_gimple_min_invariant (op)
		      && !ref_may_be_aliased (op))
		    mark_aliased_reaching_defs_necessary (stmt, op);
		}
	    }
	  else
	    gcc_unreachable ();

	  /* If we over-used our alias oracle budget drop to simple
	     mode.  The cost metric allows quadratic behavior up to
	     a constant maximal chain and after that falls back to
	     super-linear complexity.  */
	  if (longest_chain > 256
	      && total_chain > 256 * longest_chain)
	    {
	      chain_ovfl = true;
	      if (visited)
		bitmap_clear (visited);
	    }
	}
    }
}


/* Remove dead PHI nodes from block BB.  */

static bool
remove_dead_phis (basic_block bb)
{
  bool something_changed = false;
  gimple_seq phis;
  gimple phi;
  gimple_stmt_iterator gsi;
  phis = phi_nodes (bb);

  for (gsi = gsi_start (phis); !gsi_end_p (gsi);)
    {
      stats.total_phis++;
      phi = gsi_stmt (gsi);

      /* We do not track necessity of virtual PHI nodes.  Instead do
         very simple dead PHI removal here.  */
      if (!is_gimple_reg (gimple_phi_result (phi)))
	{
	  unsigned i;
	  tree vuse;

	  /* Virtual PHI nodes with one or identical arguments
	     can be removed.  */
	  vuse = gimple_phi_arg_def (phi, 0);
	  for (i = 1; i < gimple_phi_num_args (phi); ++i)
	    {
	      if (gimple_phi_arg_def (phi, i) != vuse)
		{
		  vuse = NULL_TREE;
		  break;
		}
	    }
	  if (vuse != NULL_TREE)
	    {
	      tree vdef = gimple_phi_result (phi);
	      use_operand_p use_p;
	      imm_use_iterator iter;
	      gimple use_stmt;
	      FOR_EACH_IMM_USE_STMT (use_stmt, iter, vdef)
		FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		  SET_USE (use_p, vuse);
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vdef))
		SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vuse) = 1;
	    }
	  else
	    gimple_set_plf (phi, STMT_NECESSARY, true);
	}

      if (!gimple_plf (phi, STMT_NECESSARY))
	{
	  something_changed = true;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Deleting : ");
	      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }

	  remove_phi_node (&gsi, true);
	  stats.removed_phis++;
	  continue;
	}

      gsi_next (&gsi);
    }
  return something_changed;
}


/* Remove dead statement pointed to by iterator I.  Receives the basic block BB
   containing I so that we don't have to look it up.  */

static void
remove_dead_stmt (gimple_stmt_iterator *i, basic_block bb)
{
  gimple stmt = gsi_stmt (*i);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Deleting : ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  stats.removed++;

  /* If we have determined that a conditional branch statement contributes
     nothing to the program, then we not only remove it, but we also change
     the flow graph so that the current block will simply fall-thru to its
     immediate post-dominator.  The blocks we are circumventing will be
     removed by cleanup_tree_cfg if this change in the flow graph makes them
     unreachable.  */
  if (is_ctrl_stmt (stmt))
    {
      basic_block post_dom_bb;

      /* The post dominance info has to be up-to-date.  */
      gcc_assert (dom_info_state (CDI_POST_DOMINATORS) == DOM_OK);
      /* Get the immediate post dominator of bb.  */
      post_dom_bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb);

      /* There are three particularly problematical cases.

	 1. Blocks that do not have an immediate post dominator.  This
	    can happen with infinite loops.

	 2. Blocks that are only post dominated by the exit block.  These
	    can also happen for infinite loops as we create fake edges
	    in the dominator tree.

	 3. If the post dominator has PHI nodes we may be able to compute
	    the right PHI args for them.

	 In each of these cases we must remove the control statement
	 as it may reference SSA_NAMEs which are going to be removed and
	 we remove all but one outgoing edge from the block.  */
      if (! post_dom_bb
	  || post_dom_bb == EXIT_BLOCK_PTR
	  || phi_nodes (post_dom_bb))
	;
      else
	{
	  /* Redirect the first edge out of BB to reach POST_DOM_BB.  */
	  redirect_edge_and_branch (EDGE_SUCC (bb, 0), post_dom_bb);
	  PENDING_STMT (EDGE_SUCC (bb, 0)) = NULL;

	  /* It is not sufficient to set cfg_altered below during edge
	     removal, in case BB has two successors and one of them
	     is POST_DOM_BB.  */
	  cfg_altered = true;
	}
      EDGE_SUCC (bb, 0)->probability = REG_BR_PROB_BASE;
      EDGE_SUCC (bb, 0)->count = bb->count;

      /* The edge is no longer associated with a conditional, so it does
	 not have TRUE/FALSE flags.  */
      EDGE_SUCC (bb, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

      /* The lone outgoing edge from BB will be a fallthru edge.  */
      EDGE_SUCC (bb, 0)->flags |= EDGE_FALLTHRU;

      /* Remove the remaining the outgoing edges.  */
      while (!single_succ_p (bb))
	{
	  /* FIXME.  When we remove the edge, we modify the CFG, which
	     in turn modifies the dominator and post-dominator tree.
	     Is it safe to postpone recomputing the dominator and
	     post-dominator tree until the end of this pass given that
	     the post-dominators are used above?  */
	  cfg_altered = true;
          remove_edge (EDGE_SUCC (bb, 1));
	}
    }

  unlink_stmt_vdef (stmt);
  gsi_remove (i, true);  
  release_defs (stmt); 
}


/* Eliminate unnecessary statements. Any instruction not marked as necessary
   contributes nothing to the program, and can be deleted.  */

static bool
eliminate_unnecessary_stmts (void)
{
  bool something_changed = false;
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple stmt;
  tree call;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEliminating unnecessary statements:\n");

  clear_special_calls ();

  FOR_EACH_BB (bb)
    {
      /* Remove dead statements.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  stmt = gsi_stmt (gsi);

	  stats.total++;

	  /* If GSI is not necessary then remove it.  */
	  if (!gimple_plf (stmt, STMT_NECESSARY))
	    {
	      remove_dead_stmt (&gsi, bb);
	      something_changed = true;
	    }
	  else if (is_gimple_call (stmt))
	    {
	      call = gimple_call_fndecl (stmt);
	      if (call)
		{
		  tree name;

		  /* When LHS of var = call (); is dead, simplify it into
		     call (); saving one operand.  */
		  name = gimple_call_lhs (stmt);
		  if (name && TREE_CODE (name) == SSA_NAME
		           && !TEST_BIT (processed, SSA_NAME_VERSION (name)))
		    {
		      something_changed = true;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Deleting LHS of call: ");
			  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
			  fprintf (dump_file, "\n");
			}
		      
		      push_stmt_changes (gsi_stmt_ptr (&gsi));
		      gimple_call_set_lhs (stmt, NULL_TREE);
		      maybe_clean_or_replace_eh_stmt (stmt, stmt);
		      pop_stmt_changes (gsi_stmt_ptr (&gsi));
		      release_ssa_name (name);
		    }
		  notice_special_calls (stmt);
		}
	      gsi_next (&gsi);
	    }
	  else
	    {
	      gsi_next (&gsi);
	    }
	}
    }

  FOR_EACH_BB (bb)
    {
      /* Remove dead PHI nodes.  */
      something_changed |= remove_dead_phis (bb);
    }

  return something_changed;
}


/* Print out removed statement statistics.  */

static void
print_stats (void)
{
  float percg;

  percg = ((float) stats.removed / (float) stats.total) * 100;
  fprintf (dump_file, "Removed %d of %d statements (%d%%)\n",
	   stats.removed, stats.total, (int) percg);

  if (stats.total_phis == 0)
    percg = 0;
  else
    percg = ((float) stats.removed_phis / (float) stats.total_phis) * 100;

  fprintf (dump_file, "Removed %d of %d PHI nodes (%d%%)\n",
	   stats.removed_phis, stats.total_phis, (int) percg);
}

/* Initialization for this pass.  Set up the used data structures.  */

static void
tree_dce_init (bool aggressive)
{
  memset ((void *) &stats, 0, sizeof (stats));

  if (aggressive)
    {
      int i;

      control_dependence_map = XNEWVEC (bitmap, last_basic_block);
      for (i = 0; i < last_basic_block; ++i)
	control_dependence_map[i] = BITMAP_ALLOC (NULL);

      last_stmt_necessary = sbitmap_alloc (last_basic_block);
      sbitmap_zero (last_stmt_necessary);
    }

  processed = sbitmap_alloc (num_ssa_names + 1);
  sbitmap_zero (processed);

  worklist = VEC_alloc (gimple, heap, 64);
  cfg_altered = false;
}

/* Cleanup after this pass.  */

static void
tree_dce_done (bool aggressive)
{
  if (aggressive)
    {
      int i;

      for (i = 0; i < last_basic_block; ++i)
	BITMAP_FREE (control_dependence_map[i]);
      free (control_dependence_map);

      sbitmap_free (visited_control_parents);
      sbitmap_free (last_stmt_necessary);
    }

  sbitmap_free (processed);

  VEC_free (gimple, heap, worklist);
}

/* Main routine to eliminate dead code.

   AGGRESSIVE controls the aggressiveness of the algorithm.
   In conservative mode, we ignore control dependence and simply declare
   all but the most trivially dead branches necessary.  This mode is fast.
   In aggressive mode, control dependences are taken into account, which
   results in more dead code elimination, but at the cost of some time.

   FIXME: Aggressive mode before PRE doesn't work currently because
	  the dominance info is not invalidated after DCE1.  This is
	  not an issue right now because we only run aggressive DCE
	  as the last tree SSA pass, but keep this in mind when you
	  start experimenting with pass ordering.  */

static unsigned int
perform_tree_ssa_dce (bool aggressive)
{
  struct edge_list *el = NULL;
  bool something_changed = 0;

  tree_dce_init (aggressive);

  if (aggressive)
    {
      /* Compute control dependence.  */
      timevar_push (TV_CONTROL_DEPENDENCES);
      calculate_dominance_info (CDI_POST_DOMINATORS);
      el = create_edge_list ();
      find_all_control_dependences (el);
      timevar_pop (TV_CONTROL_DEPENDENCES);

      visited_control_parents = sbitmap_alloc (last_basic_block);
      sbitmap_zero (visited_control_parents);

      mark_dfs_back_edges ();
    }

  find_obviously_necessary_stmts (el);

  longest_chain = 0;
  total_chain = 0;
  chain_ovfl = false;
  propagate_necessity (el);
  BITMAP_FREE (visited);

  something_changed |= eliminate_unnecessary_stmts ();
  something_changed |= cfg_altered;

  /* We do not update postdominators, so free them unconditionally.  */
  free_dominance_info (CDI_POST_DOMINATORS);

  /* If we removed paths in the CFG, then we need to update
     dominators as well.  I haven't investigated the possibility
     of incrementally updating dominators.  */
  if (cfg_altered)
    free_dominance_info (CDI_DOMINATORS);

  statistics_counter_event (cfun, "Statements deleted", stats.removed);
  statistics_counter_event (cfun, "PHI nodes deleted", stats.removed_phis);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & (TDF_STATS|TDF_DETAILS)))
    print_stats ();

  tree_dce_done (aggressive);

  free_edge_list (el);

  if (something_changed)
    return (TODO_update_ssa | TODO_cleanup_cfg | TODO_ggc_collect 
	    | TODO_remove_unused_locals);
  else
    return 0;
}

/* Pass entry points.  */
static unsigned int
tree_ssa_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/false);
}

static unsigned int
tree_ssa_dce_loop (void)
{
  unsigned int todo;
  todo = perform_tree_ssa_dce (/*aggressive=*/false);
  if (todo)
    {
      free_numbers_of_iterations_estimates ();
      scev_reset ();
    }
  return todo;
}

static unsigned int
tree_ssa_cd_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/optimize >= 2);
}

static bool
gate_dce (void)
{
  return flag_tree_dce != 0;
}

struct gimple_opt_pass pass_dce =
{
 {
  GIMPLE_PASS,
  "dce",				/* name */
  gate_dce,				/* gate */
  tree_ssa_dce,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_DCE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_dce_loop =
{
 {
  GIMPLE_PASS,
  "dceloop",				/* name */
  gate_dce,				/* gate */
  tree_ssa_dce_loop,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_DCE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_cd_dce =
{
 {
  GIMPLE_PASS,
  "cddce",				/* name */
  gate_dce,				/* gate */
  tree_ssa_cd_dce,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CD_DCE,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa
  | TODO_verify_flow			/* todo_flags_finish */
 }
};
