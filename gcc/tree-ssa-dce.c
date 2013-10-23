/* Dead code elimination pass for the GNU compiler.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.
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

#include "tree.h"
#include "gimple-pretty-print.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-pass.h"
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

static vec<gimple> worklist;

/* Vector indicating an SSA name has already been processed and marked
   as necessary.  */
static sbitmap processed;

/* Vector indicating that the last statement of a basic block has already
   been marked as necessary.  */
static sbitmap last_stmt_necessary;

/* Vector indicating that BB contains statements that are live.  */
static sbitmap bb_contains_live_stmts;

/* Before we can determine whether a control branch is dead, we need to
   compute which blocks are control dependent on which edges.

   We expect each block to be control dependent on very few edges so we
   use a bitmap for each block recording its edges.  An array holds the
   bitmap.  The Ith bit in the bitmap is set if that block is dependent
   on the Ith edge.  */
static control_dependences *cd;

/* Vector indicating that a basic block has already had all the edges
   processed that it is control dependent on.  */
static sbitmap visited_control_parents;

/* TRUE if this pass alters the CFG (by removing control statements).
   FALSE otherwise.

   If this pass alters the CFG, then it will arrange for the dominators
   to be recomputed.  */
static bool cfg_altered;


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
    worklist.safe_push (stmt);
  if (bb_contains_live_stmts && !is_gimple_debug (stmt))
    bitmap_set_bit (bb_contains_live_stmts, gimple_bb (stmt)->index);
}


/* Mark the statement defining operand OP as necessary.  */

static inline void
mark_operand_necessary (tree op)
{
  gimple stmt;
  int ver;

  gcc_assert (op);

  ver = SSA_NAME_VERSION (op);
  if (bitmap_bit_p (processed, ver))
    {
      stmt = SSA_NAME_DEF_STMT (op);
      gcc_assert (gimple_nop_p (stmt)
		  || gimple_plf (stmt, STMT_NECESSARY));
      return;
    }
  bitmap_set_bit (processed, ver);

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
  if (bb_contains_live_stmts)
    bitmap_set_bit (bb_contains_live_stmts, gimple_bb (stmt)->index);
  worklist.safe_push (stmt);
}


/* Mark STMT as necessary if it obviously is.  Add it to the worklist if
   it can make other statements necessary.

   If AGGRESSIVE is false, control statements are conservatively marked as
   necessary.  */

static void
mark_stmt_if_obviously_necessary (gimple stmt, bool aggressive)
{
  /* With non-call exceptions, we have to assume that all statements could
     throw.  If a statement could throw, it can be deemed necessary.  */
  if (cfun->can_throw_non_call_exceptions
      && !cfun->can_delete_dead_exceptions
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
      mark_stmt_necessary (stmt, true);
      return;

    case GIMPLE_CALL:
      {
	tree callee = gimple_call_fndecl (stmt);
	if (callee != NULL_TREE
	    && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL)
	  switch (DECL_FUNCTION_CODE (callee))
	    {
	    case BUILT_IN_MALLOC:
	    case BUILT_IN_CALLOC:
	    case BUILT_IN_ALLOCA:
	    case BUILT_IN_ALLOCA_WITH_ALIGN:
	      return;

	    default:;
	    }
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
	break;
      }

    case GIMPLE_DEBUG:
      /* Debug temps without a value are not useful.  ??? If we could
	 easily locate the debug temp bind stmt for a use thereof,
	 would could refrain from marking all debug temps here, and
	 mark them only if they're used.  */
      if (!gimple_debug_bind_p (stmt)
	  || gimple_debug_bind_has_value_p (stmt)
	  || TREE_CODE (gimple_debug_bind_get_var (stmt)) != DEBUG_EXPR_DECL)
	mark_stmt_necessary (stmt, false);
      return;

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

    case GIMPLE_ASSIGN:
      if (TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME
	  && TREE_CLOBBER_P (gimple_assign_rhs1 (stmt)))
	return;
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

  if (stmt_may_clobber_global_p (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  return;
}


/* Mark the last statement of BB as necessary.  */

static void
mark_last_stmt_necessary (basic_block bb)
{
  gimple stmt = last_stmt (bb);

  bitmap_set_bit (last_stmt_necessary, bb->index);
  bitmap_set_bit (bb_contains_live_stmts, bb->index);

  /* We actually mark the statement only if it is a control statement.  */
  if (stmt && is_ctrl_stmt (stmt))
    mark_stmt_necessary (stmt, true);
}


/* Mark control dependent edges of BB as necessary.  We have to do this only
   once for each basic block so we set the appropriate bit after we're done.

   When IGNORE_SELF is true, ignore BB in the list of control dependences.  */

static void
mark_control_dependent_edges_necessary (basic_block bb, bool ignore_self)
{
  bitmap_iterator bi;
  unsigned edge_number;
  bool skipped = false;

  gcc_assert (bb != EXIT_BLOCK_PTR);

  if (bb == ENTRY_BLOCK_PTR)
    return;

  EXECUTE_IF_SET_IN_BITMAP (cd->get_edges_dependent_on (bb->index),
			    0, edge_number, bi)
    {
      basic_block cd_bb = cd->get_edge (edge_number)->src;

      if (ignore_self && cd_bb == bb)
	{
	  skipped = true;
	  continue;
	}

      if (!bitmap_bit_p (last_stmt_necessary, cd_bb->index))
	mark_last_stmt_necessary (cd_bb);
    }

  if (!skipped)
    bitmap_set_bit (visited_control_parents, bb->index);
}


/* Find obviously necessary statements.  These are things like most function
   calls, and stores to file level variables.

   If EL is NULL, control statements are conservatively marked as
   necessary.  Otherwise it contains the list of edges used by control
   dependence analysis.  */

static void
find_obviously_necessary_stmts (bool aggressive)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  edge e;
  gimple phi, stmt;
  int flags;

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
	  mark_stmt_if_obviously_necessary (stmt, aggressive);
	}
    }

  /* Pure and const functions are finite and thus have no infinite loops in
     them.  */
  flags = flags_from_decl_or_type (current_function_decl);
  if ((flags & (ECF_CONST|ECF_PURE)) && !(flags & ECF_LOOPING_CONST_OR_PURE))
    return;

  /* Prevent the empty possibly infinite loops from being removed.  */
  if (aggressive)
    {
      loop_iterator li;
      struct loop *loop;
      scev_initialize ();
      if (mark_irreducible_loops ())
	FOR_EACH_BB (bb)
	  {
	    edge_iterator ei;
	    FOR_EACH_EDGE (e, ei, bb->succs)
	      if ((e->flags & EDGE_DFS_BACK)
		  && (e->flags & EDGE_IRREDUCIBLE_LOOP))
		{
	          if (dump_file)
	            fprintf (dump_file, "Marking back edge of irreducible loop %i->%i\n",
		    	     e->src->index, e->dest->index);
		  mark_control_dependent_edges_necessary (e->dest, false);
		}
	  }

      FOR_EACH_LOOP (li, loop, 0)
	if (!finite_loop_p (loop))
	  {
	    if (dump_file)
	      fprintf (dump_file, "can not prove finiteness of loop %i\n", loop->num);
	    mark_control_dependent_edges_necessary (loop->latch, false);
	  }
      scev_finalize ();
    }
}


/* Return true if REF is based on an aliased base, otherwise false.  */

static bool
ref_may_be_aliased (tree ref)
{
  gcc_assert (TREE_CODE (ref) != WITH_SIZE_EXPR);
  while (handled_component_p (ref))
    ref = TREE_OPERAND (ref, 0);
  if (TREE_CODE (ref) == MEM_REF
      && TREE_CODE (TREE_OPERAND (ref, 0)) == ADDR_EXPR)
    ref = TREE_OPERAND (TREE_OPERAND (ref, 0), 0);
  return !(DECL_P (ref)
	   && !may_be_aliased (ref));
}

static bitmap visited = NULL;
static unsigned int longest_chain = 0;
static unsigned int total_chain = 0;
static unsigned int nr_walks = 0;
static bool chain_ovfl = false;

/* Worker for the walker that marks reaching definitions of REF,
   which is based on a non-aliased decl, necessary.  It returns
   true whenever the defining statement of the current VDEF is
   a kill for REF, as no dominating may-defs are necessary for REF
   anymore.  DATA points to the basic-block that contains the
   stmt that refers to REF.  */

static bool
mark_aliased_reaching_defs_necessary_1 (ao_ref *ref, tree vdef, void *data)
{
  gimple def_stmt = SSA_NAME_DEF_STMT (vdef);

  /* All stmts we visit are necessary.  */
  mark_operand_necessary (vdef);

  /* If the stmt lhs kills ref, then we can stop walking.  */
  if (gimple_has_lhs (def_stmt)
      && TREE_CODE (gimple_get_lhs (def_stmt)) != SSA_NAME
      /* The assignment is not necessarily carried out if it can throw
         and we can catch it in the current function where we could inspect
	 the previous value.
         ???  We only need to care about the RHS throwing.  For aggregate
	 assignments or similar calls and non-call exceptions the LHS
	 might throw as well.  */
      && !stmt_can_throw_internal (def_stmt))
    {
      tree base, lhs = gimple_get_lhs (def_stmt);
      HOST_WIDE_INT size, offset, max_size;
      ao_ref_base (ref);
      base = get_ref_base_and_extent (lhs, &offset, &size, &max_size);
      /* We can get MEM[symbol: sZ, index: D.8862_1] here,
	 so base == refd->base does not always hold.  */
      if (base == ref->base)
	{
	  /* For a must-alias check we need to be able to constrain
	     the accesses properly.  */
	  if (size != -1 && size == max_size
	      && ref->max_size != -1)
	    {
	      if (offset <= ref->offset
		  && offset + size >= ref->offset + ref->max_size)
		return true;
	    }
	  /* Or they need to be exactly the same.  */
	  else if (ref->ref
		   /* Make sure there is no induction variable involved
		      in the references (gcc.c-torture/execute/pr42142.c).
		      The simplest way is to check if the kill dominates
		      the use.  */
		   /* But when both are in the same block we cannot
		      easily tell whether we came from a backedge
		      unless we decide to compute stmt UIDs
		      (see PR58246).  */
		   && (basic_block) data != gimple_bb (def_stmt)
		   && dominated_by_p (CDI_DOMINATORS, (basic_block) data,
				      gimple_bb (def_stmt))
		   && operand_equal_p (ref->ref, lhs, 0))
	    return true;
	}
    }

  /* Otherwise keep walking.  */
  return false;
}

static void
mark_aliased_reaching_defs_necessary (gimple stmt, tree ref)
{
  unsigned int chain;
  ao_ref refd;
  gcc_assert (!chain_ovfl);
  ao_ref_init (&refd, ref);
  chain = walk_aliased_vdefs (&refd, gimple_vuse (stmt),
			      mark_aliased_reaching_defs_necessary_1,
			      gimple_bb (stmt), NULL);
  if (chain > longest_chain)
    longest_chain = chain;
  total_chain += chain;
  nr_walks++;
}

/* Worker for the walker that marks reaching definitions of REF, which
   is not based on a non-aliased decl.  For simplicity we need to end
   up marking all may-defs necessary that are not based on a non-aliased
   decl.  The only job of this walker is to skip may-defs based on
   a non-aliased decl.  */

static bool
mark_all_reaching_defs_necessary_1 (ao_ref *ref ATTRIBUTE_UNUSED,
				    tree vdef, void *data ATTRIBUTE_UNUSED)
{
  gimple def_stmt = SSA_NAME_DEF_STMT (vdef);

  /* We have to skip already visited (and thus necessary) statements
     to make the chaining work after we dropped back to simple mode.  */
  if (chain_ovfl
      && bitmap_bit_p (processed, SSA_NAME_VERSION (vdef)))
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

  /* We want to skip statments that do not constitute stores but have
     a virtual definition.  */
  if (is_gimple_call (def_stmt))
    {
      tree callee = gimple_call_fndecl (def_stmt);
      if (callee != NULL_TREE
	  && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_MALLOC:
	  case BUILT_IN_CALLOC:
	  case BUILT_IN_ALLOCA:
	  case BUILT_IN_ALLOCA_WITH_ALIGN:
	  case BUILT_IN_FREE:
	    return false;

	  default:;
	  }
    }

  mark_operand_necessary (vdef);

  return false;
}

static void
mark_all_reaching_defs_necessary (gimple stmt)
{
  walk_aliased_vdefs (NULL, gimple_vuse (stmt),
		      mark_all_reaching_defs_necessary_1, NULL, &visited);
}

/* Return true for PHI nodes with one or identical arguments
   can be removed.  */
static bool
degenerate_phi_p (gimple phi)
{
  unsigned int i;
  tree op = gimple_phi_arg_def (phi, 0);
  for (i = 1; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_def (phi, i) != op)
      return false;
  return true;
}

/* Propagate necessity using the operands of necessary statements.
   Process the uses on each statement in the worklist, and add all
   feeding statements which contribute to the calculation of this
   value to the worklist.

   In conservative mode, EL is NULL.  */

static void
propagate_necessity (bool aggressive)
{
  gimple stmt;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nProcessing worklist:\n");

  while (worklist.length () > 0)
    {
      /* Take STMT from worklist.  */
      stmt = worklist.pop ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "processing: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      if (aggressive)
	{
	  /* Mark the last statement of the basic blocks on which the block
	     containing STMT is control dependent, but only if we haven't
	     already done so.  */
	  basic_block bb = gimple_bb (stmt);
	  if (bb != ENTRY_BLOCK_PTR
	      && !bitmap_bit_p (visited_control_parents, bb->index))
	    mark_control_dependent_edges_necessary (bb, false);
	}

      if (gimple_code (stmt) == GIMPLE_PHI
	  /* We do not process virtual PHI nodes nor do we track their
	     necessity.  */
	  && !virtual_operand_p (gimple_phi_result (stmt)))
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

	  /* For PHI operands it matters from where the control flow arrives
	     to the BB.  Consider the following example:

	     a=exp1;
	     b=exp2;
	     if (test)
		;
	     else
		;
	     c=PHI(a,b)

	     We need to mark control dependence of the empty basic blocks, since they
	     contains computation of PHI operands.

	     Doing so is too restrictive in the case the predecestor block is in
	     the loop. Consider:

	      if (b)
		{
		  int i;
		  for (i = 0; i<1000; ++i)
		    ;
		  j = 0;
		}
	      return j;

	     There is PHI for J in the BB containing return statement.
	     In this case the control dependence of predecestor block (that is
	     within the empty loop) also contains the block determining number
	     of iterations of the block that would prevent removing of empty
	     loop in this case.

	     This scenario can be avoided by splitting critical edges.
	     To save the critical edge splitting pass we identify how the control
	     dependence would look like if the edge was split.

	     Consider the modified CFG created from current CFG by splitting
	     edge B->C.  In the postdominance tree of modified CFG, C' is
	     always child of C.  There are two cases how chlids of C' can look
	     like:

		1) C' is leaf

		   In this case the only basic block C' is control dependent on is B.

		2) C' has single child that is B

		   In this case control dependence of C' is same as control
		   dependence of B in original CFG except for block B itself.
		   (since C' postdominate B in modified CFG)

	     Now how to decide what case happens?  There are two basic options:

		a) C postdominate B.  Then C immediately postdominate B and
		   case 2 happens iff there is no other way from B to C except
		   the edge B->C.

		   There is other way from B to C iff there is succesor of B that
		   is not postdominated by B.  Testing this condition is somewhat
		   expensive, because we need to iterate all succesors of B.
		   We are safe to assume that this does not happen: we will mark B
		   as needed when processing the other path from B to C that is
		   conrol dependent on B and marking control dependencies of B
		   itself is harmless because they will be processed anyway after
		   processing control statement in B.

		b) C does not postdominate B.  Always case 1 happens since there is
		   path from C to exit that does not go through B and thus also C'.  */

	  if (aggressive && !degenerate_phi_p (stmt))
	    {
	      for (k = 0; k < gimple_phi_num_args (stmt); k++)
		{
		  basic_block arg_bb = gimple_phi_arg_edge (stmt, k)->src;

		  if (gimple_bb (stmt)
		      != get_immediate_dominator (CDI_POST_DOMINATORS, arg_bb))
		    {
		      if (!bitmap_bit_p (last_stmt_necessary, arg_bb->index))
			mark_last_stmt_necessary (arg_bb);
		    }
		  else if (arg_bb != ENTRY_BLOCK_PTR
		           && !bitmap_bit_p (visited_control_parents,
					 arg_bb->index))
		    mark_control_dependent_edges_necessary (arg_bb, true);
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

	  /* If this is a call to free which is directly fed by an
	     allocation function do not mark that necessary through
	     processing the argument.  */
	  if (gimple_call_builtin_p (stmt, BUILT_IN_FREE))
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      gimple def_stmt;
	      tree def_callee;
	      /* If the pointer we free is defined by an allocation
		 function do not add the call to the worklist.  */
	      if (TREE_CODE (ptr) == SSA_NAME
		  && is_gimple_call (def_stmt = SSA_NAME_DEF_STMT (ptr))
		  && (def_callee = gimple_call_fndecl (def_stmt))
		  && DECL_BUILT_IN_CLASS (def_callee) == BUILT_IN_NORMAL
		  && (DECL_FUNCTION_CODE (def_callee) == BUILT_IN_MALLOC
		      || DECL_FUNCTION_CODE (def_callee) == BUILT_IN_CALLOC))
		continue;
	    }

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
	     in 1).  By keeping a global visited bitmap for references
	     we walk for 2) we avoid quadratic behavior for those.  */

	  if (is_gimple_call (stmt))
	    {
	      tree callee = gimple_call_fndecl (stmt);
	      unsigned i;

	      /* Calls to functions that are merely acting as barriers
		 or that only store to memory do not make any previous
		 stores necessary.  */
	      if (callee != NULL_TREE
		  && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
		  && (DECL_FUNCTION_CODE (callee) == BUILT_IN_MEMSET
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_MEMSET_CHK
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_MALLOC
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_CALLOC
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_FREE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_VA_END
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_ALLOCA
		      || (DECL_FUNCTION_CODE (callee)
			  == BUILT_IN_ALLOCA_WITH_ALIGN)
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_SAVE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_RESTORE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_ASSUME_ALIGNED))
		continue;

	      /* Calls implicitly load from memory, their arguments
	         in addition may explicitly perform memory loads.  */
	      mark_all_reaching_defs_necessary (stmt);
	      for (i = 0; i < gimple_call_num_args (stmt); ++i)
		{
		  tree arg = gimple_call_arg (stmt, i);
		  if (TREE_CODE (arg) == SSA_NAME
		      || is_gimple_min_invariant (arg))
		    continue;
		  if (TREE_CODE (arg) == WITH_SIZE_EXPR)
		    arg = TREE_OPERAND (arg, 0);
		  if (!ref_may_be_aliased (arg))
		    mark_aliased_reaching_defs_necessary (stmt, arg);
		}
	    }
	  else if (gimple_assign_single_p (stmt))
	    {
	      tree rhs;
	      /* If this is a load mark things necessary.  */
	      rhs = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (rhs) != SSA_NAME
		  && !is_gimple_min_invariant (rhs)
		  && TREE_CODE (rhs) != CONSTRUCTOR)
		{
		  if (!ref_may_be_aliased (rhs))
		    mark_aliased_reaching_defs_necessary (stmt, rhs);
		  else
		    mark_all_reaching_defs_necessary (stmt);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_RETURN)
	    {
	      tree rhs = gimple_return_retval (stmt);
	      /* A return statement may perform a load.  */
	      if (rhs
		  && TREE_CODE (rhs) != SSA_NAME
		  && !is_gimple_min_invariant (rhs)
		  && TREE_CODE (rhs) != CONSTRUCTOR)
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
		      && TREE_CODE (op) != CONSTRUCTOR
		      && !ref_may_be_aliased (op))
		    mark_aliased_reaching_defs_necessary (stmt, op);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_TRANSACTION)
	    {
	      /* The beginning of a transaction is a memory barrier.  */
	      /* ??? If we were really cool, we'd only be a barrier
		 for the memories touched within the transaction.  */
	      mark_all_reaching_defs_necessary (stmt);
	    }
	  else
	    gcc_unreachable ();

	  /* If we over-used our alias oracle budget drop to simple
	     mode.  The cost metric allows quadratic behavior
	     (number of uses times number of may-defs queries) up to
	     a constant maximal number of queries and after that falls back to
	     super-linear complexity.  */
	  if (/* Constant but quadratic for small functions.  */
	      total_chain > 128 * 128
	      /* Linear in the number of may-defs.  */
	      && total_chain > 32 * longest_chain
	      /* Linear in the number of uses.  */
	      && total_chain > nr_walks * 32)
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
  gimple phi;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);)
    {
      stats.total_phis++;
      phi = gsi_stmt (gsi);

      /* We do not track necessity of virtual PHI nodes.  Instead do
         very simple dead PHI removal here.  */
      if (virtual_operand_p (gimple_phi_result (phi)))
	{
	  /* Virtual PHI nodes with one or identical arguments
	     can be removed.  */
	  if (degenerate_phi_p (phi))
	    {
	      tree vdef = gimple_phi_result (phi);
	      tree vuse = gimple_phi_arg_def (phi, 0);

	      use_operand_p use_p;
	      imm_use_iterator iter;
	      gimple use_stmt;
	      FOR_EACH_IMM_USE_STMT (use_stmt, iter, vdef)
		FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		  SET_USE (use_p, vuse);
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (vdef)
	          && TREE_CODE (vuse) == SSA_NAME)
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

/* Forward edge E to respective POST_DOM_BB and update PHIs.  */

static edge
forward_edge_to_pdom (edge e, basic_block post_dom_bb)
{
  gimple_stmt_iterator gsi;
  edge e2 = NULL;
  edge_iterator ei;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Redirecting edge %i->%i to %i\n", e->src->index,
	     e->dest->index, post_dom_bb->index);

  e2 = redirect_edge_and_branch (e, post_dom_bb);
  cfg_altered = true;

  /* If edge was already around, no updating is necessary.  */
  if (e2 != e)
    return e2;

  if (!gimple_seq_empty_p (phi_nodes (post_dom_bb)))
    {
      /* We are sure that for every live PHI we are seeing control dependent BB.
         This means that we can pick any edge to duplicate PHI args from.  */
      FOR_EACH_EDGE (e2, ei, post_dom_bb->preds)
	if (e2 != e)
	  break;
      for (gsi = gsi_start_phis (post_dom_bb); !gsi_end_p (gsi);)
	{
	  gimple phi = gsi_stmt (gsi);
	  tree op;
	  source_location locus;

	  /* PHIs for virtuals have no control dependency relation on them.
	     We are lost here and must force renaming of the symbol.  */
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    {
	      mark_virtual_phi_result_for_renaming (phi);
	      remove_phi_node (&gsi, true);
	      continue;
	    }

	  /* Dead PHI do not imply control dependency.  */
          if (!gimple_plf (phi, STMT_NECESSARY))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  op = gimple_phi_arg_def (phi, e2->dest_idx);
	  locus = gimple_phi_arg_location (phi, e2->dest_idx);
	  add_phi_arg (phi, op, e, locus);
	  /* The resulting PHI if not dead can only be degenerate.  */
	  gcc_assert (degenerate_phi_p (phi));
	  gsi_next (&gsi);
	}
    }
  return e;
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
      edge e, e2;
      edge_iterator ei;

      post_dom_bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb);

      e = find_edge (bb, post_dom_bb);

      /* If edge is already there, try to use it.  This avoids need to update
         PHI nodes.  Also watch for cases where post dominator does not exists
	 or is exit block.  These can happen for infinite loops as we create
	 fake edges in the dominator tree.  */
      if (e)
        ;
      else if (! post_dom_bb || post_dom_bb == EXIT_BLOCK_PTR)
	e = EDGE_SUCC (bb, 0);
      else
        e = forward_edge_to_pdom (EDGE_SUCC (bb, 0), post_dom_bb);
      gcc_assert (e);
      e->probability = REG_BR_PROB_BASE;
      e->count = bb->count;

      /* The edge is no longer associated with a conditional, so it does
	 not have TRUE/FALSE flags.  */
      e->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

      /* The lone outgoing edge from BB will be a fallthru edge.  */
      e->flags |= EDGE_FALLTHRU;

      /* Remove the remaining outgoing edges.  */
      for (ei = ei_start (bb->succs); (e2 = ei_safe_edge (ei)); )
	if (e != e2)
	  {
	    cfg_altered = true;
            remove_edge (e2);
	  }
	else
	  ei_next (&ei);
    }

  /* If this is a store into a variable that is being optimized away,
     add a debug bind stmt if possible.  */
  if (MAY_HAVE_DEBUG_STMTS
      && gimple_assign_single_p (stmt)
      && is_gimple_val (gimple_assign_rhs1 (stmt)))
    {
      tree lhs = gimple_assign_lhs (stmt);
      if ((TREE_CODE (lhs) == VAR_DECL || TREE_CODE (lhs) == PARM_DECL)
	  && !DECL_IGNORED_P (lhs)
	  && is_gimple_reg_type (TREE_TYPE (lhs))
	  && !is_global_var (lhs)
	  && !DECL_HAS_VALUE_EXPR_P (lhs))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  gimple note
	    = gimple_build_debug_bind (lhs, unshare_expr (rhs), stmt);
	  gsi_insert_after (i, note, GSI_SAME_STMT);
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
  gimple_stmt_iterator gsi, psi;
  gimple stmt;
  tree call;
  vec<basic_block> h;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEliminating unnecessary statements:\n");

  clear_special_calls ();

  /* Walking basic blocks and statements in reverse order avoids
     releasing SSA names before any other DEFs that refer to them are
     released.  This helps avoid loss of debug information, as we get
     a chance to propagate all RHSs of removed SSAs into debug uses,
     rather than only the latest ones.  E.g., consider:

     x_3 = y_1 + z_2;
     a_5 = x_3 - b_4;
     # DEBUG a => a_5

     If we were to release x_3 before a_5, when we reached a_5 and
     tried to substitute it into the debug stmt, we'd see x_3 there,
     but x_3's DEF, type, etc would have already been disconnected.
     By going backwards, the debug stmt first changes to:

     # DEBUG a => x_3 - b_4

     and then to:

     # DEBUG a => y_1 + z_2 - b_4

     as desired.  */
  gcc_assert (dom_info_available_p (CDI_DOMINATORS));
  h = get_all_dominated_blocks (CDI_DOMINATORS, single_succ (ENTRY_BLOCK_PTR));

  while (h.length ())
    {
      bb = h.pop ();

      /* Remove dead statements.  */
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi = psi)
	{
	  stmt = gsi_stmt (gsi);

	  psi = gsi;
	  gsi_prev (&psi);

	  stats.total++;

	  /* We can mark a call to free as not necessary if the
	     defining statement of its argument is an allocation
	     function and that is not necessary itself.  */
	  if (gimple_call_builtin_p (stmt, BUILT_IN_FREE))
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      tree callee2;
	      gimple def_stmt;
	      if (TREE_CODE (ptr) != SSA_NAME)
		continue;
	      def_stmt = SSA_NAME_DEF_STMT (ptr);
	      if (!is_gimple_call (def_stmt)
		  || gimple_plf (def_stmt, STMT_NECESSARY))
		continue;
	      callee2 = gimple_call_fndecl (def_stmt);
	      if (callee2 == NULL_TREE
		  || DECL_BUILT_IN_CLASS (callee2) != BUILT_IN_NORMAL
		  || (DECL_FUNCTION_CODE (callee2) != BUILT_IN_MALLOC
		      && DECL_FUNCTION_CODE (callee2) != BUILT_IN_CALLOC))
		continue;
	      gimple_set_plf (stmt, STMT_NECESSARY, false);
	    }

	  /* If GSI is not necessary then remove it.  */
	  if (!gimple_plf (stmt, STMT_NECESSARY))
	    {
	      if (!is_gimple_debug (stmt))
		something_changed = true;
	      remove_dead_stmt (&gsi, bb);
	    }
	  else if (is_gimple_call (stmt))
	    {
	      tree name = gimple_call_lhs (stmt);

	      notice_special_calls (stmt);

	      /* When LHS of var = call (); is dead, simplify it into
		 call (); saving one operand.  */
	      if (name
		  && TREE_CODE (name) == SSA_NAME
		  && !bitmap_bit_p (processed, SSA_NAME_VERSION (name))
		  /* Avoid doing so for allocation calls which we
		     did not mark as necessary, it will confuse the
		     special logic we apply to malloc/free pair removal.  */
		  && (!(call = gimple_call_fndecl (stmt))
		      || DECL_BUILT_IN_CLASS (call) != BUILT_IN_NORMAL
		      || (DECL_FUNCTION_CODE (call) != BUILT_IN_MALLOC
			  && DECL_FUNCTION_CODE (call) != BUILT_IN_CALLOC
			  && DECL_FUNCTION_CODE (call) != BUILT_IN_ALLOCA
			  && (DECL_FUNCTION_CODE (call)
			      != BUILT_IN_ALLOCA_WITH_ALIGN))))
		{
		  something_changed = true;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Deleting LHS of call: ");
		      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
		      fprintf (dump_file, "\n");
		    }

		  gimple_call_set_lhs (stmt, NULL_TREE);
		  maybe_clean_or_replace_eh_stmt (stmt, stmt);
		  update_stmt (stmt);
		  release_ssa_name (name);
		}
	    }
	}
    }

  h.release ();

  /* Since we don't track liveness of virtual PHI nodes, it is possible that we
     rendered some PHI nodes unreachable while they are still in use.
     Mark them for renaming.  */
  if (cfg_altered)
    {
      basic_block prev_bb;

      find_unreachable_blocks ();

      /* Delete all unreachable basic blocks in reverse dominator order.  */
      for (bb = EXIT_BLOCK_PTR->prev_bb; bb != ENTRY_BLOCK_PTR; bb = prev_bb)
	{
	  prev_bb = bb->prev_bb;

	  if (!bitmap_bit_p (bb_contains_live_stmts, bb->index)
	      || !(bb->flags & BB_REACHABLE))
	    {
	      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
		if (virtual_operand_p (gimple_phi_result (gsi_stmt (gsi))))
		  {
		    bool found = false;
		    imm_use_iterator iter;

		    FOR_EACH_IMM_USE_STMT (stmt, iter, gimple_phi_result (gsi_stmt (gsi)))
		      {
			if (!(gimple_bb (stmt)->flags & BB_REACHABLE))
			  continue;
			if (gimple_code (stmt) == GIMPLE_PHI
			    || gimple_plf (stmt, STMT_NECESSARY))
			  {
			    found = true;
			    BREAK_FROM_IMM_USE_STMT (iter);
			  }
		      }
		    if (found)
		      mark_virtual_phi_result_for_renaming (gsi_stmt (gsi));
		  }

	      if (!(bb->flags & BB_REACHABLE))
		{
		  /* Speed up the removal of blocks that don't
		     dominate others.  Walking backwards, this should
		     be the common case.  ??? Do we need to recompute
		     dominators because of cfg_altered?  */
		  if (!MAY_HAVE_DEBUG_STMTS
		      || !first_dom_son (CDI_DOMINATORS, bb))
		    delete_basic_block (bb);
		  else
		    {
		      h = get_all_dominated_blocks (CDI_DOMINATORS, bb);

		      while (h.length ())
			{
			  bb = h.pop ();
			  prev_bb = bb->prev_bb;
			  /* Rearrangements to the CFG may have failed
			     to update the dominators tree, so that
			     formerly-dominated blocks are now
			     otherwise reachable.  */
			  if (!!(bb->flags & BB_REACHABLE))
			    continue;
			  delete_basic_block (bb);
			}

		      h.release ();
		    }
		}
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
      last_stmt_necessary = sbitmap_alloc (last_basic_block);
      bitmap_clear (last_stmt_necessary);
      bb_contains_live_stmts = sbitmap_alloc (last_basic_block);
      bitmap_clear (bb_contains_live_stmts);
    }

  processed = sbitmap_alloc (num_ssa_names + 1);
  bitmap_clear (processed);

  worklist.create (64);
  cfg_altered = false;
}

/* Cleanup after this pass.  */

static void
tree_dce_done (bool aggressive)
{
  if (aggressive)
    {
      delete cd;
      sbitmap_free (visited_control_parents);
      sbitmap_free (last_stmt_necessary);
      sbitmap_free (bb_contains_live_stmts);
      bb_contains_live_stmts = NULL;
    }

  sbitmap_free (processed);

  worklist.release ();
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
  bool something_changed = 0;

  calculate_dominance_info (CDI_DOMINATORS);

  /* Preheaders are needed for SCEV to work.
     Simple lateches and recorded exits improve chances that loop will
     proved to be finite in testcases such as in loop-15.c and loop-24.c  */
  if (aggressive)
    loop_optimizer_init (LOOPS_NORMAL
			 | LOOPS_HAVE_RECORDED_EXITS);

  tree_dce_init (aggressive);

  if (aggressive)
    {
      /* Compute control dependence.  */
      calculate_dominance_info (CDI_POST_DOMINATORS);
      cd = new control_dependences (create_edge_list ());

      visited_control_parents = sbitmap_alloc (last_basic_block);
      bitmap_clear (visited_control_parents);

      mark_dfs_back_edges ();
    }

  find_obviously_necessary_stmts (aggressive);

  if (aggressive)
    loop_optimizer_finalize ();

  longest_chain = 0;
  total_chain = 0;
  nr_walks = 0;
  chain_ovfl = false;
  visited = BITMAP_ALLOC (NULL);
  propagate_necessity (aggressive);
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

  if (something_changed)
    return TODO_update_ssa | TODO_cleanup_cfg;
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

namespace {

const pass_data pass_data_dce =
{
  GIMPLE_PASS, /* type */
  "dce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_DCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_dce : public gimple_opt_pass
{
public:
  pass_dce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dce, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_dce (m_ctxt); }
  bool gate () { return gate_dce (); }
  unsigned int execute () { return tree_ssa_dce (); }

}; // class pass_dce

} // anon namespace

gimple_opt_pass *
make_pass_dce (gcc::context *ctxt)
{
  return new pass_dce (ctxt);
}

namespace {

const pass_data pass_data_dce_loop =
{
  GIMPLE_PASS, /* type */
  "dceloop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_DCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_dce_loop : public gimple_opt_pass
{
public:
  pass_dce_loop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dce_loop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_dce_loop (m_ctxt); }
  bool gate () { return gate_dce (); }
  unsigned int execute () { return tree_ssa_dce_loop (); }

}; // class pass_dce_loop

} // anon namespace

gimple_opt_pass *
make_pass_dce_loop (gcc::context *ctxt)
{
  return new pass_dce_loop (ctxt);
}

namespace {

const pass_data pass_data_cd_dce =
{
  GIMPLE_PASS, /* type */
  "cddce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_CD_DCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_ssa | TODO_verify_flow ), /* todo_flags_finish */
};

class pass_cd_dce : public gimple_opt_pass
{
public:
  pass_cd_dce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cd_dce, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_cd_dce (m_ctxt); }
  bool gate () { return gate_dce (); }
  unsigned int execute () { return tree_ssa_cd_dce (); }

}; // class pass_cd_dce

} // anon namespace

gimple_opt_pass *
make_pass_cd_dce (gcc::context *ctxt)
{
  return new pass_cd_dce (ctxt);
}
