/* Dead code elimination pass for the GNU compiler.
   Copyright (C) 2002-2021 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "calls.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "gimple-fold.h"

static struct stmt_stats
{
  int total;
  int total_phis;
  int removed;
  int removed_phis;
} stats;

#define STMT_NECESSARY GF_PLF_1

static vec<gimple *> worklist;

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

/* When non-NULL holds map from basic block index into the postorder.  */
static int *bb_postorder;


/* True if we should treat any stmt with a vdef as necessary.  */

static inline bool
keep_all_vdefs_p ()
{
  return optimize_debug;
}

/* If STMT is not already marked necessary, mark it, and add it to the
   worklist if ADD_TO_WORKLIST is true.  */

static inline void
mark_stmt_necessary (gimple *stmt, bool add_to_worklist)
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
  if (add_to_worklist && bb_contains_live_stmts && !is_gimple_debug (stmt))
    bitmap_set_bit (bb_contains_live_stmts, gimple_bb (stmt)->index);
}


/* Mark the statement defining operand OP as necessary.  */

static inline void
mark_operand_necessary (tree op)
{
  gimple *stmt;
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
      print_generic_expr (dump_file, op);
      fprintf (dump_file, " stmt ");
      print_gimple_stmt (dump_file, stmt, 0);
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
mark_stmt_if_obviously_necessary (gimple *stmt, bool aggressive)
{
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
	    && fndecl_built_in_p (callee, BUILT_IN_NORMAL))
	  switch (DECL_FUNCTION_CODE (callee))
	    {
	    case BUILT_IN_MALLOC:
	    case BUILT_IN_ALIGNED_ALLOC:
	    case BUILT_IN_CALLOC:
	    CASE_BUILT_IN_ALLOCA:
	    case BUILT_IN_STRDUP:
	    case BUILT_IN_STRNDUP:
	    case BUILT_IN_GOMP_ALLOC:
	      return;

	    default:;
	    }

	if (callee != NULL_TREE
	    && flag_allocation_dce
	    && DECL_IS_REPLACEABLE_OPERATOR_NEW_P (callee))
	  return;

	/* IFN_GOACC_LOOP calls are necessary in that they are used to
	   represent parameter (i.e. step, bound) of a lowered OpenACC
	   partitioned loop.  But this kind of partitioned loop might not
	   survive from aggressive loop removal for it has loop exit and
	   is assumed to be finite.  Therefore, we need to explicitly mark
	   these calls. (An example is libgomp.oacc-c-c++-common/pr84955.c) */
	if (gimple_call_internal_p (stmt, IFN_GOACC_LOOP))
	  {
	    mark_stmt_necessary (stmt, true);
	    return;
	  }
	break;
      }

    case GIMPLE_DEBUG:
      /* Debug temps without a value are not useful.  ??? If we could
	 easily locate the debug temp bind stmt for a use thereof,
	 would could refrain from marking all debug temps here, and
	 mark them only if they're used.  */
      if (gimple_debug_nonbind_marker_p (stmt)
	  || !gimple_debug_bind_p (stmt)
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
      if (gimple_clobber_p (stmt))
	return;
      break;

    default:
      break;
    }

  /* If the statement has volatile operands, it needs to be preserved.
     Same for statements that can alter control flow in unpredictable
     ways.  */
  if (gimple_has_side_effects (stmt) || is_ctrl_altering_stmt (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  /* If a statement could throw, it can be deemed necessary unless we
     are allowed to remove dead EH.  Test this after checking for
     new/delete operators since we always elide their EH.  */
  if (!cfun->can_delete_dead_exceptions
      && stmt_could_throw_p (cfun, stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  if ((gimple_vdef (stmt) && keep_all_vdefs_p ())
      || stmt_may_clobber_global_p (stmt))
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
  gimple *stmt = last_stmt (bb);

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

  gcc_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    return;

  EXECUTE_IF_SET_IN_BITMAP (cd->get_edges_dependent_on (bb->index),
			    0, edge_number, bi)
    {
      basic_block cd_bb = cd->get_edge_src (edge_number);

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
  gimple *phi, *stmt;
  int flags;

  FOR_EACH_BB_FN (bb, cfun)
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
      class loop *loop;
      if (mark_irreducible_loops ())
	FOR_EACH_BB_FN (bb, cfun)
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

      FOR_EACH_LOOP (loop, 0)
	if (!finite_loop_p (loop))
	  {
	    if (dump_file)
	      fprintf (dump_file, "cannot prove finiteness of loop %i\n", loop->num);
	    mark_control_dependent_edges_necessary (loop->latch, false);
	  }
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
  gimple *def_stmt = SSA_NAME_DEF_STMT (vdef);

  /* All stmts we visit are necessary.  */
  if (! gimple_clobber_p (def_stmt))
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
      && !stmt_can_throw_internal (cfun, def_stmt))
    {
      tree base, lhs = gimple_get_lhs (def_stmt);
      poly_int64 size, offset, max_size;
      bool reverse;
      ao_ref_base (ref);
      base
	= get_ref_base_and_extent (lhs, &offset, &size, &max_size, &reverse);
      /* We can get MEM[symbol: sZ, index: D.8862_1] here,
	 so base == refd->base does not always hold.  */
      if (base == ref->base)
	{
	  /* For a must-alias check we need to be able to constrain
	     the accesses properly.  */
	  if (known_eq (size, max_size)
	      && known_subrange_p (ref->offset, ref->max_size, offset, size))
	    return true;
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
mark_aliased_reaching_defs_necessary (gimple *stmt, tree ref)
{
  /* Should have been caught before calling this function.  */
  gcc_checking_assert (!keep_all_vdefs_p ());

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
  gimple *def_stmt = SSA_NAME_DEF_STMT (vdef);

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
  if (gcall *call = dyn_cast <gcall *> (def_stmt))
    {
      tree callee = gimple_call_fndecl (call);
      if (callee != NULL_TREE
	  && fndecl_built_in_p (callee, BUILT_IN_NORMAL))
	switch (DECL_FUNCTION_CODE (callee))
	  {
	  case BUILT_IN_MALLOC:
	  case BUILT_IN_ALIGNED_ALLOC:
	  case BUILT_IN_CALLOC:
	  CASE_BUILT_IN_ALLOCA:
	  case BUILT_IN_FREE:
	  case BUILT_IN_GOMP_ALLOC:
	  case BUILT_IN_GOMP_FREE:
	    return false;

	  default:;
	  }

      if (callee != NULL_TREE
	  && (DECL_IS_REPLACEABLE_OPERATOR_NEW_P (callee)
	      || DECL_IS_OPERATOR_DELETE_P (callee))
	  && gimple_call_from_new_or_delete (call))
	return false;
    }

  if (! gimple_clobber_p (def_stmt))
    mark_operand_necessary (vdef);

  return false;
}

static void
mark_all_reaching_defs_necessary (gimple *stmt)
{
  /* Should have been caught before calling this function.  */
  gcc_checking_assert (!keep_all_vdefs_p ());
  walk_aliased_vdefs (NULL, gimple_vuse (stmt),
		      mark_all_reaching_defs_necessary_1, NULL, &visited);
}

/* Return true for PHI nodes with one or identical arguments
   can be removed.  */
static bool
degenerate_phi_p (gimple *phi)
{
  unsigned int i;
  tree op = gimple_phi_arg_def (phi, 0);
  for (i = 1; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_def (phi, i) != op)
      return false;
  return true;
}

/* Return that NEW_CALL and DELETE_CALL are a valid pair of new
   and delete  operators.  */

static bool
valid_new_delete_pair_p (gimple *new_call, gimple *delete_call)
{
  tree new_asm = DECL_ASSEMBLER_NAME (gimple_call_fndecl (new_call));
  tree delete_asm = DECL_ASSEMBLER_NAME (gimple_call_fndecl (delete_call));
  return valid_new_delete_pair_p (new_asm, delete_asm);
}

/* Propagate necessity using the operands of necessary statements.
   Process the uses on each statement in the worklist, and add all
   feeding statements which contribute to the calculation of this
   value to the worklist.

   In conservative mode, EL is NULL.  */

static void
propagate_necessity (bool aggressive)
{
  gimple *stmt;

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
	  if (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
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
	  gphi *phi = as_a <gphi *> (stmt);
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
		  basic_block arg_bb = gimple_phi_arg_edge (phi, k)->src;

		  if (gimple_bb (stmt)
		      != get_immediate_dominator (CDI_POST_DOMINATORS, arg_bb))
		    {
		      if (!bitmap_bit_p (last_stmt_necessary, arg_bb->index))
			mark_last_stmt_necessary (arg_bb);
		    }
		  else if (arg_bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
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
	  bool is_delete_operator
	    = (is_gimple_call (stmt)
	       && gimple_call_from_new_or_delete (as_a <gcall *> (stmt))
	       && gimple_call_operator_delete_p (as_a <gcall *> (stmt)));
	  if (is_delete_operator
	      || gimple_call_builtin_p (stmt, BUILT_IN_FREE)
	      || gimple_call_builtin_p (stmt, BUILT_IN_GOMP_FREE))
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      gcall *def_stmt;
	      tree def_callee;
	      /* If the pointer we free is defined by an allocation
		 function do not add the call to the worklist.  */
	      if (TREE_CODE (ptr) == SSA_NAME
		  && (def_stmt = dyn_cast <gcall *> (SSA_NAME_DEF_STMT (ptr)))
		  && (def_callee = gimple_call_fndecl (def_stmt))
		  && ((DECL_BUILT_IN_CLASS (def_callee) == BUILT_IN_NORMAL
		       && (DECL_FUNCTION_CODE (def_callee) == BUILT_IN_ALIGNED_ALLOC
			   || DECL_FUNCTION_CODE (def_callee) == BUILT_IN_MALLOC
			   || DECL_FUNCTION_CODE (def_callee) == BUILT_IN_CALLOC
			   || DECL_FUNCTION_CODE (def_callee) == BUILT_IN_GOMP_ALLOC))
		      || (DECL_IS_REPLACEABLE_OPERATOR_NEW_P (def_callee)
			  && gimple_call_from_new_or_delete (def_stmt))))
		{
		  if (is_delete_operator
		      && !valid_new_delete_pair_p (def_stmt, stmt))
		    mark_operand_necessary (gimple_call_arg (stmt, 0));

		  /* Delete operators can have alignment and (or) size
		     as next arguments.  When being a SSA_NAME, they
		     must be marked as necessary.  Similarly GOMP_free.  */
		  if (gimple_call_num_args (stmt) >= 2)
		    for (unsigned i = 1; i < gimple_call_num_args (stmt);
			 i++)
		      {
			tree arg = gimple_call_arg (stmt, i);
			if (TREE_CODE (arg) == SSA_NAME)
			  mark_operand_necessary (arg);
		      }

		  continue;
		}
	    }

	  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	    mark_operand_necessary (use);

	  use = gimple_vuse (stmt);
	  if (!use)
	    continue;

	  /* No need to search for vdefs if we intrinsicly keep them all.  */
	  if (keep_all_vdefs_p ())
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

	  if (gcall *call = dyn_cast <gcall *> (stmt))
	    {
	      tree callee = gimple_call_fndecl (call);
	      unsigned i;

	      /* Calls to functions that are merely acting as barriers
		 or that only store to memory do not make any previous
		 stores necessary.  */
	      if (callee != NULL_TREE
		  && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL
		  && (DECL_FUNCTION_CODE (callee) == BUILT_IN_MEMSET
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_MEMSET_CHK
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_MALLOC
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_ALIGNED_ALLOC
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_CALLOC
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_FREE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_VA_END
		      || ALLOCA_FUNCTION_CODE_P (DECL_FUNCTION_CODE (callee))
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_SAVE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_STACK_RESTORE
		      || DECL_FUNCTION_CODE (callee) == BUILT_IN_ASSUME_ALIGNED))
		continue;

	      if (callee != NULL_TREE
		  && (DECL_IS_REPLACEABLE_OPERATOR_NEW_P (callee)
		      || DECL_IS_OPERATOR_DELETE_P (callee))
		  && gimple_call_from_new_or_delete (call))
		continue;

	      /* Calls implicitly load from memory, their arguments
	         in addition may explicitly perform memory loads.  */
	      mark_all_reaching_defs_necessary (call);
	      for (i = 0; i < gimple_call_num_args (call); ++i)
		{
		  tree arg = gimple_call_arg (call, i);
		  if (TREE_CODE (arg) == SSA_NAME
		      || is_gimple_min_invariant (arg))
		    continue;
		  if (TREE_CODE (arg) == WITH_SIZE_EXPR)
		    arg = TREE_OPERAND (arg, 0);
		  if (!ref_may_be_aliased (arg))
		    mark_aliased_reaching_defs_necessary (call, arg);
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
	  else if (greturn *return_stmt = dyn_cast <greturn *> (stmt))
	    {
	      tree rhs = gimple_return_retval (return_stmt);
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
	  else if (gasm *asm_stmt = dyn_cast <gasm *> (stmt))
	    {
	      unsigned i;
	      mark_all_reaching_defs_necessary (stmt);
	      /* Inputs may perform loads.  */
	      for (i = 0; i < gimple_asm_ninputs (asm_stmt); ++i)
		{
		  tree op = TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
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
  gphi *phi;
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);)
    {
      stats.total_phis++;
      phi = gsi.phi ();

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
	      gimple *use_stmt;
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


/* Remove dead statement pointed to by iterator I.  Receives the basic block BB
   containing I so that we don't have to look it up.  */

static void
remove_dead_stmt (gimple_stmt_iterator *i, basic_block bb,
		  vec<edge> &to_remove_edges)
{
  gimple *stmt = gsi_stmt (*i);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Deleting : ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  stats.removed++;

  /* If we have determined that a conditional branch statement contributes
     nothing to the program, then we not only remove it, but we need to update
     the CFG.  We can chose any of edges out of BB as long as we are sure to not
     close infinite loops.  This is done by always choosing the edge closer to
     exit in inverted_post_order_compute order.  */
  if (is_ctrl_stmt (stmt))
    {
      edge_iterator ei;
      edge e = NULL, e2;

      /* See if there is only one non-abnormal edge.  */
      if (single_succ_p (bb))
        e = single_succ_edge (bb);
      /* Otherwise chose one that is closer to bb with live statement in it.
         To be able to chose one, we compute inverted post order starting from
	 all BBs with live statements.  */
      if (!e)
	{
	  if (!bb_postorder)
	    {
	      auto_vec<int, 20> postorder;
		 inverted_post_order_compute (&postorder,
					      &bb_contains_live_stmts);
	      bb_postorder = XNEWVEC (int, last_basic_block_for_fn (cfun));
	      for (unsigned int i = 0; i < postorder.length (); ++i)
		 bb_postorder[postorder[i]] = i;
	    }
          FOR_EACH_EDGE (e2, ei, bb->succs)
	    if (!e || e2->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
		|| bb_postorder [e->dest->index]
		   < bb_postorder [e2->dest->index])
	      e = e2;
	}
      gcc_assert (e);
      e->probability = profile_probability::always ();

      /* The edge is no longer associated with a conditional, so it does
	 not have TRUE/FALSE flags.
	 We are also safe to drop EH/ABNORMAL flags and turn them into
	 normal control flow, because we know that all the destinations (including
	 those odd edges) are equivalent for program execution.  */
      e->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE | EDGE_EH | EDGE_ABNORMAL);

      /* The lone outgoing edge from BB will be a fallthru edge.  */
      e->flags |= EDGE_FALLTHRU;

      /* Remove the remaining outgoing edges.  */
      FOR_EACH_EDGE (e2, ei, bb->succs)
	if (e != e2)
	  {
	    /* If we made a BB unconditionally exit a loop or removed
	       an entry into an irreducible region, then this transform
	       alters the set of BBs in the loop.  Schedule a fixup.  */
	    if (loop_exit_edge_p (bb->loop_father, e)
		|| (e2->dest->flags & BB_IRREDUCIBLE_LOOP))
	      loops_state_set (LOOPS_NEED_FIXUP);
	    to_remove_edges.safe_push (e2);
	  }
    }

  /* If this is a store into a variable that is being optimized away,
     add a debug bind stmt if possible.  */
  if (MAY_HAVE_DEBUG_BIND_STMTS
      && gimple_assign_single_p (stmt)
      && is_gimple_val (gimple_assign_rhs1 (stmt)))
    {
      tree lhs = gimple_assign_lhs (stmt);
      if ((VAR_P (lhs) || TREE_CODE (lhs) == PARM_DECL)
	  && !DECL_IGNORED_P (lhs)
	  && is_gimple_reg_type (TREE_TYPE (lhs))
	  && !is_global_var (lhs)
	  && !DECL_HAS_VALUE_EXPR_P (lhs))
	{
	  tree rhs = gimple_assign_rhs1 (stmt);
	  gdebug *note
	    = gimple_build_debug_bind (lhs, unshare_expr (rhs), stmt);
	  gsi_insert_after (i, note, GSI_SAME_STMT);
	}
    }

  unlink_stmt_vdef (stmt);
  gsi_remove (i, true);
  release_defs (stmt);
}

/* Helper for maybe_optimize_arith_overflow.  Find in *TP if there are any
   uses of data (SSA_NAME) other than REALPART_EXPR referencing it.  */

static tree
find_non_realpart_uses (tree *tp, int *walk_subtrees, void *data)
{
  if (TYPE_P (*tp) || TREE_CODE (*tp) == REALPART_EXPR)
    *walk_subtrees = 0;
  if (*tp == (tree) data)
    return *tp;
  return NULL_TREE;
}

/* If the IMAGPART_EXPR of the {ADD,SUB,MUL}_OVERFLOW result is never used,
   but REALPART_EXPR is, optimize the {ADD,SUB,MUL}_OVERFLOW internal calls
   into plain unsigned {PLUS,MINUS,MULT}_EXPR, and if needed reset debug
   uses.  */

static void
maybe_optimize_arith_overflow (gimple_stmt_iterator *gsi,
			       enum tree_code subcode)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_call_lhs (stmt);

  if (lhs == NULL || TREE_CODE (lhs) != SSA_NAME)
    return;

  imm_use_iterator imm_iter;
  use_operand_p use_p;
  bool has_debug_uses = false;
  bool has_realpart_uses = false;
  bool has_other_uses = false;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	has_debug_uses = true;
      else if (is_gimple_assign (use_stmt)
	       && gimple_assign_rhs_code (use_stmt) == REALPART_EXPR
	       && TREE_OPERAND (gimple_assign_rhs1 (use_stmt), 0) == lhs)
	has_realpart_uses = true;
      else
	{
	  has_other_uses = true;
	  break;
	}
    }

  if (!has_realpart_uses || has_other_uses)
    return;

  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  location_t loc = gimple_location (stmt);
  tree type = TREE_TYPE (TREE_TYPE (lhs));
  tree utype = type;
  if (!TYPE_UNSIGNED (type))
    utype = build_nonstandard_integer_type (TYPE_PRECISION (type), 1);
  tree result = fold_build2_loc (loc, subcode, utype,
				 fold_convert_loc (loc, utype, arg0),
				 fold_convert_loc (loc, utype, arg1));
  result = fold_convert_loc (loc, type, result);

  if (has_debug_uses)
    {
      gimple *use_stmt;
      FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, lhs)
	{
	  if (!gimple_debug_bind_p (use_stmt))
	    continue;
	  tree v = gimple_debug_bind_get_value (use_stmt);
	  if (walk_tree (&v, find_non_realpart_uses, lhs, NULL))
	    {
	      gimple_debug_bind_reset_value (use_stmt);
	      update_stmt (use_stmt);
	    }
	}
    }

  if (TREE_CODE (result) == INTEGER_CST && TREE_OVERFLOW (result))
    result = drop_tree_overflow (result);
  tree overflow = build_zero_cst (type);
  tree ctype = build_complex_type (type);
  if (TREE_CODE (result) == INTEGER_CST)
    result = build_complex (ctype, result, overflow);
  else
    result = build2_loc (gimple_location (stmt), COMPLEX_EXPR,
			 ctype, result, overflow);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Transforming call: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "because the overflow result is never used into: ");
      print_generic_stmt (dump_file, result, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  gimplify_and_update_call_from_tree (gsi, result);
}

/* Eliminate unnecessary statements. Any instruction not marked as necessary
   contributes nothing to the program, and can be deleted.  */

static bool
eliminate_unnecessary_stmts (void)
{
  bool something_changed = false;
  basic_block bb;
  gimple_stmt_iterator gsi, psi;
  gimple *stmt;
  tree call;
  vec<basic_block> h;
  auto_vec<edge> to_remove_edges;

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
  h = get_all_dominated_blocks (CDI_DOMINATORS,
				single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));

  while (h.length ())
    {
      bb = h.pop ();

      /* Remove dead statements.  */
      auto_bitmap debug_seen;
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi = psi)
	{
	  stmt = gsi_stmt (gsi);

	  psi = gsi;
	  gsi_prev (&psi);

	  stats.total++;

	  /* We can mark a call to free as not necessary if the
	     defining statement of its argument is not necessary
	     (and thus is getting removed).  */
	  if (gimple_plf (stmt, STMT_NECESSARY)
	      && (gimple_call_builtin_p (stmt, BUILT_IN_FREE)
		  || (is_gimple_call (stmt)
		      && gimple_call_from_new_or_delete (as_a <gcall *> (stmt))
		      && gimple_call_operator_delete_p (as_a <gcall *> (stmt)))))
	    {
	      tree ptr = gimple_call_arg (stmt, 0);
	      if (TREE_CODE (ptr) == SSA_NAME)
		{
		  gimple *def_stmt = SSA_NAME_DEF_STMT (ptr);
		  if (!gimple_nop_p (def_stmt)
		      && !gimple_plf (def_stmt, STMT_NECESSARY))
		    gimple_set_plf (stmt, STMT_NECESSARY, false);
		}
	    }

	  /* If GSI is not necessary then remove it.  */
	  if (!gimple_plf (stmt, STMT_NECESSARY))
	    {
	      /* Keep clobbers that we can keep live live.  */
	      if (gimple_clobber_p (stmt))
		{
		  ssa_op_iter iter;
		  use_operand_p use_p;
		  bool dead = false;
		  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
		    {
		      tree name = USE_FROM_PTR (use_p);
		      if (!SSA_NAME_IS_DEFAULT_DEF (name)
			  && !bitmap_bit_p (processed, SSA_NAME_VERSION (name)))
			{
			  dead = true;
			  break;
			}
		    }
		  if (!dead)
		    {
		      bitmap_clear (debug_seen);
		      continue;
		    }
		}
	      if (!is_gimple_debug (stmt))
		something_changed = true;
	      remove_dead_stmt (&gsi, bb, to_remove_edges);
	      continue;
	    }
	  else if (is_gimple_call (stmt))
	    {
	      tree name = gimple_call_lhs (stmt);

	      notice_special_calls (as_a <gcall *> (stmt));

	      /* When LHS of var = call (); is dead, simplify it into
		 call (); saving one operand.  */
	      if (name
		  && TREE_CODE (name) == SSA_NAME
		  && !bitmap_bit_p (processed, SSA_NAME_VERSION (name))
		  /* Avoid doing so for allocation calls which we
		     did not mark as necessary, it will confuse the
		     special logic we apply to malloc/free pair removal.  */
		  && (!(call = gimple_call_fndecl (stmt))
		      || ((DECL_BUILT_IN_CLASS (call) != BUILT_IN_NORMAL
			   || (DECL_FUNCTION_CODE (call) != BUILT_IN_ALIGNED_ALLOC
			       && DECL_FUNCTION_CODE (call) != BUILT_IN_MALLOC
			       && DECL_FUNCTION_CODE (call) != BUILT_IN_CALLOC
			       && !ALLOCA_FUNCTION_CODE_P
			       (DECL_FUNCTION_CODE (call))))
			  && !DECL_IS_REPLACEABLE_OPERATOR_NEW_P (call))))
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

		  /* GOMP_SIMD_LANE (unless three argument) or ASAN_POISON
		     without lhs is not needed.  */
		  if (gimple_call_internal_p (stmt))
		    switch (gimple_call_internal_fn (stmt))
		      {
		      case IFN_GOMP_SIMD_LANE:
			if (gimple_call_num_args (stmt) >= 3
			    && !integer_nonzerop (gimple_call_arg (stmt, 2)))
			  break;
			/* FALLTHRU */
		      case IFN_ASAN_POISON:
			remove_dead_stmt (&gsi, bb, to_remove_edges);
			break;
		      default:
			break;
		      }
		}
	      else if (gimple_call_internal_p (stmt))
		switch (gimple_call_internal_fn (stmt))
		  {
		  case IFN_ADD_OVERFLOW:
		    maybe_optimize_arith_overflow (&gsi, PLUS_EXPR);
		    break;
		  case IFN_SUB_OVERFLOW:
		    maybe_optimize_arith_overflow (&gsi, MINUS_EXPR);
		    break;
		  case IFN_MUL_OVERFLOW:
		    maybe_optimize_arith_overflow (&gsi, MULT_EXPR);
		    break;
		  default:
		    break;
		  }
	    }
	  else if (gimple_debug_bind_p (stmt))
	    {
	      /* We are only keeping the last debug-bind of a
	         non-DEBUG_EXPR_DECL variable in a series of
		 debug-bind stmts.  */
	      tree var = gimple_debug_bind_get_var (stmt);
	      if (TREE_CODE (var) != DEBUG_EXPR_DECL
		  && !bitmap_set_bit (debug_seen, DECL_UID (var)))
		remove_dead_stmt (&gsi, bb, to_remove_edges);
	      continue;
	    }
	  bitmap_clear (debug_seen);
	}

      /* Remove dead PHI nodes.  */
      something_changed |= remove_dead_phis (bb);
    }

  h.release ();

  /* Since we don't track liveness of virtual PHI nodes, it is possible that we
     rendered some PHI nodes unreachable while they are still in use.
     Mark them for renaming.  */
  if (!to_remove_edges.is_empty ())
    {
      basic_block prev_bb;

      /* Remove edges.  We've delayed this to not get bogus debug stmts
         during PHI node removal.  */
      for (unsigned i = 0; i < to_remove_edges.length (); ++i)
	remove_edge (to_remove_edges[i]);
      cfg_altered = true;

      find_unreachable_blocks ();

      /* Delete all unreachable basic blocks in reverse dominator order.  */
      for (bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
	   bb != ENTRY_BLOCK_PTR_FOR_FN (cfun); bb = prev_bb)
	{
	  prev_bb = bb->prev_bb;

	  if (!bitmap_bit_p (bb_contains_live_stmts, bb->index)
	      || !(bb->flags & BB_REACHABLE))
	    {
	      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
		   gsi_next (&gsi))
		if (virtual_operand_p (gimple_phi_result (gsi.phi ())))
		  {
		    bool found = false;
		    imm_use_iterator iter;

		    FOR_EACH_IMM_USE_STMT (stmt, iter,
					   gimple_phi_result (gsi.phi ()))
		      {
			if (!(gimple_bb (stmt)->flags & BB_REACHABLE))
			  continue;
			if (gimple_code (stmt) == GIMPLE_PHI
			    || gimple_plf (stmt, STMT_NECESSARY))
			  {
			    found = true;
			    break;
			  }
		      }
		    if (found)
		      mark_virtual_phi_result_for_renaming (gsi.phi ());
		  }

	      if (!(bb->flags & BB_REACHABLE))
		{
		  /* Speed up the removal of blocks that don't
		     dominate others.  Walking backwards, this should
		     be the common case.  ??? Do we need to recompute
		     dominators because of cfg_altered?  */
		  if (!first_dom_son (CDI_DOMINATORS, bb))
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

  if (bb_postorder)
    free (bb_postorder);
  bb_postorder = NULL;

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
      last_stmt_necessary = sbitmap_alloc (last_basic_block_for_fn (cfun));
      bitmap_clear (last_stmt_necessary);
      bb_contains_live_stmts = sbitmap_alloc (last_basic_block_for_fn (cfun));
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
  bool in_loop_pipeline = scev_initialized_p ();
  if (aggressive && ! in_loop_pipeline)
    {
      scev_initialize ();
      loop_optimizer_init (LOOPS_NORMAL
			   | LOOPS_HAVE_RECORDED_EXITS);
    }

  tree_dce_init (aggressive);

  if (aggressive)
    {
      /* Compute control dependence.  */
      calculate_dominance_info (CDI_POST_DOMINATORS);
      cd = new control_dependences ();

      visited_control_parents =
	sbitmap_alloc (last_basic_block_for_fn (cfun));
      bitmap_clear (visited_control_parents);

      mark_dfs_back_edges ();
    }

  find_obviously_necessary_stmts (aggressive);

  if (aggressive && ! in_loop_pipeline)
    {
      loop_optimizer_finalize ();
      scev_finalize ();
    }

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
    {
      free_numbers_of_iterations_estimates (cfun);
      if (in_loop_pipeline)
	scev_reset ();
      return TODO_update_ssa | TODO_cleanup_cfg;
    }
  return 0;
}

/* Pass entry points.  */
static unsigned int
tree_ssa_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/false);
}

static unsigned int
tree_ssa_cd_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/optimize >= 2);
}

namespace {

const pass_data pass_data_dce =
{
  GIMPLE_PASS, /* type */
  "dce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_DCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_dce : public gimple_opt_pass
{
public:
  pass_dce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dce, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_dce (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_dce != 0; }
  virtual unsigned int execute (function *) { return tree_ssa_dce (); }

}; // class pass_dce

} // anon namespace

gimple_opt_pass *
make_pass_dce (gcc::context *ctxt)
{
  return new pass_dce (ctxt);
}

namespace {

const pass_data pass_data_cd_dce =
{
  GIMPLE_PASS, /* type */
  "cddce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_CD_DCE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_cd_dce : public gimple_opt_pass
{
public:
  pass_cd_dce (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cd_dce, ctxt), update_address_taken_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_cd_dce (m_ctxt); }
  void set_pass_param (unsigned n, bool param)
    {
      gcc_assert (n == 0);
      update_address_taken_p = param;
    }
  virtual bool gate (function *) { return flag_tree_dce != 0; }
  virtual unsigned int execute (function *)
    {
      return (tree_ssa_cd_dce ()
	      | (update_address_taken_p ? TODO_update_address_taken : 0));
    }

private:
  bool update_address_taken_p;
}; // class pass_cd_dce

} // anon namespace

gimple_opt_pass *
make_pass_cd_dce (gcc::context *ctxt)
{
  return new pass_cd_dce (ctxt);
}


/* A cheap DCE interface.  WORKLIST is a list of possibly dead stmts and
   is consumed by this function.  The function has linear complexity in
   the number of dead stmts with a constant factor like the average SSA
   use operands number.  */

void
simple_dce_from_worklist (bitmap worklist)
{
  while (! bitmap_empty_p (worklist))
    {
      /* Pop item.  */
      unsigned i = bitmap_first_set_bit (worklist);
      bitmap_clear_bit (worklist, i);

      tree def = ssa_name (i);
      /* Removed by somebody else or still in use.  */
      if (! def || ! has_zero_uses (def))
	continue;

      gimple *t = SSA_NAME_DEF_STMT (def);
      if (gimple_has_side_effects (t))
	continue;

      /* Add uses to the worklist.  */
      ssa_op_iter iter;
      use_operand_p use_p;
      FOR_EACH_PHI_OR_STMT_USE (use_p, t, iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  if (TREE_CODE (use) == SSA_NAME
	      && ! SSA_NAME_IS_DEFAULT_DEF (use))
	    bitmap_set_bit (worklist, SSA_NAME_VERSION (use));
	}

      /* Remove stmt.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Removing dead stmt:");
	  print_gimple_stmt (dump_file, t, 0);
	}
      gimple_stmt_iterator gsi = gsi_for_stmt (t);
      if (gimple_code (t) == GIMPLE_PHI)
	remove_phi_node (&gsi, true);
      else
	{
	  gsi_remove (&gsi, true);
	  release_defs (t);
	}
    }
}
