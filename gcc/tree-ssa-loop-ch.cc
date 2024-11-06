/* Loop header copying on trees.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-inline.h"
#include "tree-ssa-threadedge.h"
#include "tree-ssa-sccvn.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "value-range.h"
#include "gimple-range.h"
#include "gimple-range-path.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-scalar-evolution.h"

/* Return path query insteance for testing ranges of statements
   in headers of LOOP contained in basic block BB.
   Use RANGER instance.  */

static path_range_query *
get_range_query (class loop *loop,
		 basic_block bb,
		 gimple_ranger &ranger)
{
  auto_vec<basic_block, 8> path;
  for (; bb != loop->header; bb = single_pred_edge (bb)->src)
    path.safe_push (bb);
  path.safe_push (loop->header);
  path.safe_push (loop_preheader_edge (loop)->src);
  return new path_range_query (ranger, path);
}

/* Return edge that is true in the first iteration of the loop
   and NULL otherwise.
   Formulate corrent ranger query to RANGER.  */

static edge
static_loop_exit (class loop *l, basic_block bb, gimple_ranger &ranger,
		  path_range_query *&query)
{
  gcond *last = safe_dyn_cast <gcond *> (*gsi_last_bb (bb));
  edge ret_e;

  if (!last)
    return NULL;

  edge true_e, false_e;
  extract_true_false_edges_from_block (bb, &true_e, &false_e);

  /* If neither edge is the exit edge, this is not a case we'd like to
     special-case.  */
  if (!loop_exit_edge_p (l, true_e) && !loop_exit_edge_p (l, false_e))
    return NULL;

  int_range<1> desired_static_range;
  if (loop_exit_edge_p (l, true_e))
    {
      desired_static_range = range_false ();
      ret_e = true_e;
    }
  else
   {
      desired_static_range = range_true ();
      ret_e = false_e;
   }

  if (!query)
    query = get_range_query (l, gimple_bb (last), ranger);

  int_range<2> r;
  if (!query->range_of_stmt (r, last))
    return NULL;
  return r == desired_static_range ? ret_e : NULL;
}

/* Return true if STMT is static in LOOP.  This means that its value
   is constant in the first iteration.
   Use RANGER and formulate query cached in QUERY.  */

static bool
loop_static_stmt_p (class loop *loop,
		    gimple_ranger &ranger,
		    path_range_query *&query,
		    gimple *stmt)
{
  tree type = gimple_range_type (stmt);
  if (!type || !value_range::supports_type_p (type))
    return false;

  if (!query)
    query = get_range_query (loop, gimple_bb (stmt), ranger);

  value_range r (gimple_range_type (stmt));
  if (!query->range_of_stmt (r, stmt))
    return false;
  return r.singleton_p ();
}

/* Return true if OP is invariant.  */

static bool
loop_invariant_op_p (class loop *loop,
		     tree op)
{
  if (is_gimple_min_invariant (op))
    return true;
  if (SSA_NAME_IS_DEFAULT_DEF (op)
      || !flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (op))))
    return true;
  return gimple_uid (SSA_NAME_DEF_STMT (op)) & 1;
}

/* Return true if OP combines outcome of static and
   loop invariant conditional.  */

static bool
loop_static_op_p (class loop *loop, tree op)
{
  /* Always check for invariant first.  */
  gcc_checking_assert (!is_gimple_min_invariant (op)
		       && !SSA_NAME_IS_DEFAULT_DEF (op)
		       && flow_bb_inside_loop_p (loop,
			       gimple_bb (SSA_NAME_DEF_STMT (op))));
  return gimple_uid (SSA_NAME_DEF_STMT (op)) & 2;
}


/* Return true if OP combines outcome of static and
   loop invariant conditional.  */

static bool
loop_combined_static_and_iv_p (class loop *loop,
			       tree op)
{
  /* Always check for invariant first.  */
  gcc_checking_assert (!is_gimple_min_invariant (op)
		       && !SSA_NAME_IS_DEFAULT_DEF (op)
		       && flow_bb_inside_loop_p (loop,
			       gimple_bb (SSA_NAME_DEF_STMT (op))));
  return gimple_uid (SSA_NAME_DEF_STMT (op)) & 4;
}

/* Decision about posibility of copying a given header.  */

enum ch_decision
{
  /* We do not want to copy this header.  */
  ch_impossible,
  /* We can copy it if it enables wins.  */
  ch_possible,
  /* We can "copy" it if it enables wins and doing
     so will introduce no new code.  */
  ch_possible_zero_cost,
  /* We want to copy.  */
  ch_win,
  /* We want to copy and we will eliminate loop exit.  */
  ch_win_invariant_exit
};

/* Check whether we should duplicate HEADER of LOOP.  At most *LIMIT
   instructions should be duplicated, limit is decreased by the actual
   amount.  */

static ch_decision
should_duplicate_loop_header_p (basic_block header, class loop *loop,
				gimple_ranger *ranger,
				int *limit,
				hash_set <edge> *invariant_exits,
				hash_set <edge> *static_exits)
{
  gimple_stmt_iterator bsi;

  gcc_assert (!header->aux);

  gcc_assert (EDGE_COUNT (header->succs) > 0);
  if (single_succ_p (header))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it is single succ.\n",
		 header->index);
      return ch_impossible;
    }

  if (flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 0)->dest)
      && flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 1)->dest))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: both successors are in loop.\n",
		 loop->num);
      return ch_impossible;
    }

  /* If this is not the original loop header, we want it to have just
     one predecessor in order to match the && pattern.  */
  if (header != loop->header && !single_pred_p (header))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it has mutiple predecestors.\n",
		 header->index);
      return ch_impossible;
    }

  gcond *last = safe_dyn_cast <gcond *> (*gsi_last_bb (header));
  if (!last)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it does not end by conditional.\n",
		 header->index);
      return ch_impossible;
    }

  path_range_query *query = NULL;
  for (gphi_iterator psi = gsi_start_phis (header); !gsi_end_p (psi);
       gsi_next (&psi))
    if (!virtual_operand_p (gimple_phi_result (psi.phi ())))
      {
	bool static_p = loop_static_stmt_p (loop, *ranger,
					    query, psi.phi ());
	gimple_set_uid (psi.phi (), static_p ? 2 : 0);
      }
  bool code_size_cost = false;

  /* Count number of instructions and punt on calls.
     Populate stmts INV flag to later apply heuristics to the
     kind of conditions we want to copy.  */
  for (bsi = gsi_start_bb (header); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *last = gsi_stmt (bsi);

      if (gimple_code (last) == GIMPLE_LABEL)
	continue;

      if (is_gimple_debug (last))
	continue;

      if (gimple_code (last) == GIMPLE_COND)
	break;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Analyzing: ");
	  print_gimple_stmt (dump_file, last, 0, TDF_SLIM);
	}

      if (gimple_code (last) == GIMPLE_CALL
	  && (!gimple_inexpensive_call_p (as_a <gcall *> (last))
	      /* IFN_LOOP_DIST_ALIAS means that inner loop is distributed
		 at current loop's header.  Don't copy in this case.  */
	      || gimple_call_internal_p (last, IFN_LOOP_DIST_ALIAS)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Not duplicating bb %i: it contains call.\n",
		     header->index);
	  if (query)
	    delete query;
	  return ch_impossible;
	}

      /* Classify the stmt is invariant in the loop.  */
      gimple_set_uid (last, 0);
      if (!gimple_vuse (last)
	  && gimple_code (last) != GIMPLE_ASM
	  && (gimple_code (last) != GIMPLE_CALL
	      || gimple_call_flags (last) & ECF_CONST))
	{
	  bool inv = true, static_p = false;
	  ssa_op_iter i;
	  tree op;
	  FOR_EACH_SSA_TREE_OPERAND (op, last, i, SSA_OP_USE)
	    if (!loop_invariant_op_p (loop, op))
	      inv = false;
	  /* Assume that code is reasonably optimized and invariant
	     constants are already identified.  */
	  if (!inv)
	    static_p = loop_static_stmt_p (loop, *ranger, query, last);
	  gimple_set_uid (last, (inv ? 1 : 0) | (static_p ? 2 : 0));
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      if (inv)
		fprintf (dump_file, "    Stmt is loop invariant\n");
	      if (static_p)
		fprintf (dump_file, "    Stmt is static "
			 "(constant in the first iteration)\n");
	    }
	  /* Loop invariants will be optimized out in loop body after
	     duplication; do not account invariant computation in code
	     size costs.

	     Similarly static computations will be optimized out in the
	     duplicatd header.  */
	  if (inv || static_p)
	    continue;

	  /* Match the following:
	     _1 = i_1 < 10   <- static condtion
	     _2 = n != 0     <- loop invariant condition
	     _3 = _1 & _2    <- combined static and iv statement.  */
	  tree_code code;
	  if (gimple_code (last) == GIMPLE_ASSIGN
	      && ((code = gimple_assign_rhs_code (last)) == BIT_AND_EXPR
		  || code == BIT_IOR_EXPR || code == BIT_XOR_EXPR))
	    {
	      tree op1 = gimple_assign_rhs1 (last);
	      tree op2 = gimple_assign_rhs2 (last);

	      if ((loop_invariant_op_p (loop, op1)
		   || loop_combined_static_and_iv_p (loop, op1)
		   || loop_static_op_p (loop, op1))
		  && (loop_invariant_op_p (loop, op2)
		      || loop_combined_static_and_iv_p (loop, op2)
		      || loop_static_op_p (loop, op2)))
		{
		  /* Duplicating loop header with combned conditional will
		     remove this statement in each copy.  But we account for
		     that later when seeing that condition.

		     Note that this may be overly optimistic for bit operations
		     where the static parameter may still result in non-trivial
		     bit operation.  */
		  gimple_set_uid (last, 4);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "    Stmt combines static and invariant op\n");
		  continue;
		}
	    }
	}

      int insns = estimate_num_insns (last, &eni_size_weights);
      *limit -= insns;
      if (insns)
	code_size_cost = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "    Acconting stmt as %i insns\n", insns);
      if (*limit < 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Not duplicating bb %i contains too many insns.\n",
		     header->index);
	  if (query)
	    delete query;
	  return ch_impossible;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Analyzing: ");
      print_gimple_stmt (dump_file, last, 0, TDF_SLIM);
    }

  /* If the condition tests a non-IV loop variant we do not want to rotate
     the loop further.  Unless this is the original loop header.  */
  tree lhs = gimple_cond_lhs (last);
  tree rhs = gimple_cond_rhs (last);
  bool lhs_invariant = loop_invariant_op_p (loop, lhs);
  bool rhs_invariant = loop_invariant_op_p (loop, rhs);

  /* Combined conditional is a result of if combining:

     _1 = i_1 < 10   <- static condtion
     _2 = n != 0     <- loop invariant condition
     _3 = _1 & _2    <- combined static and iv statement
     if (_3 != 0)    <- combined conditional

     Combined conditionals will not be optimized out in either copy.
     However duplicaed header simplifies to:

     if (n < 10)

     while loop body to

     if (i_1 < 10)

     So effectively the resulting code sequence will be of same length as
     the original code.

     Combined conditionals are never static or invariant, so save some work
     below.  */
  if (lhs_invariant != rhs_invariant
      && (lhs_invariant
	  || loop_combined_static_and_iv_p (loop, lhs))
      && (rhs_invariant
	  || loop_combined_static_and_iv_p (loop, rhs)))
   {
     if (query)
       delete query;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Conditional combines static and invariant op.\n");
     return ch_win;
   }

  edge static_exit = static_loop_exit (loop, header, *ranger, query);
  if (query)
    delete query;

  if (static_exit && static_exits)
    {
      static_exits->add (static_exit);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "    Will eliminate peeled conditional in bb %d.\n",
		 static_exit->src->index);
      /* Still look for invariant exits; exit may be both.  */
    }
  if (lhs_invariant && rhs_invariant)
    {
      if (invariant_exits)
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, header->succs)
	    if (loop_exit_edge_p (loop, e))
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file,
			   "    Will elliminate invariant exit %i->%i\n",
			   e->src->index, e->dest->index);
		invariant_exits->add (e);
	      }
	}
      return ch_win_invariant_exit;
    }

  /* If the static exit fully optimize out, it is win to "duplicate"
     it.

     TODO: Even if duplication costs some size we may opt to do so in case
     exit probability is significant enough (do partial peeling).  */
  if (static_exit)
    return !code_size_cost ? ch_possible_zero_cost : ch_possible;

  /* We was not able to prove that conditional will be eliminated.  */
  int insns = estimate_num_insns (last, &eni_size_weights);
  *limit -= insns;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "    Acconting stmt as %i insns\n", insns);
  if (*limit < 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i contains too many insns.\n",
		 header->index);
      return ch_impossible;
    }

  return ch_possible;
}

/* Checks whether LOOP is a do-while style loop.  */

static bool
do_while_loop_p (class loop *loop)
{
  gimple *stmt = last_nondebug_stmt (loop->latch);

  /* If the latch of the loop is not empty, it is not a do-while loop.  */
  if (stmt
      && gimple_code (stmt) != GIMPLE_LABEL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: latch is not empty.\n",
		 loop->num);
      return false;
    }

  /* If the latch does not have a single predecessor, it is not a
     do-while loop.  */
  if (!single_pred_p (loop->latch))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: latch has multiple "
		 "predecessors.\n", loop->num);
      return false;
    }
  basic_block pred = single_pred (loop->latch);

  /* If the latch predecessor doesn't exit the loop, it is not a
     do-while loop.  */
  if (!loop_exits_from_bb_p (loop, pred))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: latch predecessor "
		 "does not exit loop.\n", loop->num);
      return false;
    }
  gcond *last = safe_dyn_cast <gcond *> (*gsi_last_bb (pred));
  if (last
      && (gimple_cond_lhs (last) == boolean_false_node
	  || gimple_cond_lhs (last) == boolean_true_node)
      && gimple_cond_rhs (last) == boolean_false_node)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: latch predecessor "
		 "contains exit we optimized out.\n", loop->num);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Loop %i is do-while loop\n", loop->num);

  return true;
}

/* Update profile after header copying of LOOP.
   REGION is the original (in loop) sequence, REGION_COPY is the
   duplicated header (now outside of loop). N_REGION is number of
   bbs duplicated.
   ELIMINATED_EDGE is edge to be removed from duplicated sequence.
   INVARIANT_EXITS are edges in the loop body to be elimianted
   since they are loop invariants

   So We expect the following:

      // region_copy_start entry will be scaled to entry_count
	 if (cond1)         <- this condition will become false
			       and we update probabilities
	   goto loop_exit;
	 if (cond2)         <- this condition is loop invariant
	   goto loop_exit;
	 goto loop_header   <- this will be redirected to loop.
       // region_copy_end
     loop:
	       <body>
       // region start
     loop_header:
	       if (cond1)   <- we need to update probability here
		 goto loop_exit;
	       if (cond2)   <- and determine scaling factor here.
			       moreover cond2 is now always true
		 goto loop_exit;
	       else
		 goto loop;
       // region end

     Adding support for more exits can be done similarly,
     but only consumer so far is tree-ssa-loop-ch and it uses only this
     to handle the common case of peeling headers which have
     conditionals known to be always true upon entry.  */

static void
update_profile_after_ch (class loop *loop,
			 basic_block *region, basic_block *region_copy,
			 unsigned n_region,
			 hash_set <edge> *invariant_exits,
			 hash_set <edge> *static_exits,
			 profile_count entry_count)
{
  for (unsigned int i = 0; i < n_region; i++)
    {
      edge exit_e, exit_e_copy, e, e_copy;
      if (EDGE_COUNT (region[i]->succs) == 1)
	{
	  region_copy[i]->count = entry_count;
	  region[i]->count -= entry_count;
	  continue;
	}

      gcc_checking_assert (EDGE_COUNT (region[i]->succs) == 2);
      if (loop_exit_edge_p (loop,
			    EDGE_SUCC (region[i], 0)))
	{
	  exit_e = EDGE_SUCC (region[i], 0);
	  exit_e_copy = EDGE_SUCC (region_copy[i], 0);
	  e = EDGE_SUCC (region[i], 1);
	  e_copy = EDGE_SUCC (region_copy[i], 1);
	}
      else
	{
	  exit_e = EDGE_SUCC (region[i], 1);
	  exit_e_copy = EDGE_SUCC (region_copy[i], 1);
	  e = EDGE_SUCC (region[i], 0);
	  e_copy = EDGE_SUCC (region_copy[i], 0);
	}
      gcc_assert (i == n_region - 1
		  || (e->dest == region[i + 1]
		      && e_copy->dest == region_copy[i + 1]));
      region_copy[i]->count = entry_count;
      profile_count exit_e_count = exit_e->count ();
      bool was_static = false;
      if (static_exits->contains (exit_e))
	{
	  /* Update profile and the conditional.
	     CFG update is done by caller.  */
	  static_exits->remove (exit_e);
	  was_static = true;
	  e_copy->probability = profile_probability::always ();
	  exit_e_copy->probability = profile_probability::never ();
	  gcond *cond_stmt
		  = as_a <gcond *>(*gsi_last_bb (region_copy[i]));
	  if (e_copy->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_true (cond_stmt);
	  else
	    gimple_cond_make_false (cond_stmt);
	  update_stmt (cond_stmt);
	  /* Header copying is a special case of jump threading, so use
	     common code to update loop body exit condition.  */
	  update_bb_profile_for_threading (region[i], entry_count, e);
	}
      else
	region[i]->count -= region_copy[i]->count;
      if (invariant_exits->contains (exit_e))
	{
	  invariant_exits->remove (exit_e);
	  /* All exits will happen in exit_e_copy which is out of the
	     loop, so increase probability accordingly.
	     If the edge is eliminated_edge we already corrected
	     profile above.  */
	  if (entry_count.nonzero_p () && !was_static)
	    set_edge_probability_and_rescale_others
		    (exit_e_copy, exit_e_count.probability_in
							(entry_count));
	  /* Eliminate in-loop conditional.  */
	  e->probability = profile_probability::always ();
	  exit_e->probability = profile_probability::never ();
	  gcond *cond_stmt = as_a <gcond *>(*gsi_last_bb (region[i]));
	  if (e->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_true (cond_stmt);
	  else
	    gimple_cond_make_false (cond_stmt);
	  update_stmt (cond_stmt);
	}
      entry_count = e_copy->count ();
    }
  /* Be sure that we seen all invariant exit edges we are supposed to update.
     We may have recorded some static exists we decided to not duplicate.  */
  gcc_checking_assert (invariant_exits->is_empty ());
}

namespace {

/* Common superclass for both header-copying phases.  */
class ch_base : public gimple_opt_pass
{
  protected:
    ch_base (pass_data data, gcc::context *ctxt)
      : gimple_opt_pass (data, ctxt)
    {}

  /* Copies headers of all loops in FUN for which process_loop_p is true.  */
  unsigned int copy_headers (function *fun);

  /* Return true to copy headers of LOOP or false to skip.  */
  virtual bool process_loop_p (class loop *loop) = 0;
};

const pass_data pass_data_ch =
{
  GIMPLE_PASS, /* type */
  "ch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_CH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ch : public ch_base
{
public:
  pass_ch (gcc::context *ctxt)
    : ch_base (pass_data_ch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_tree_ch != 0; }

  /* Initialize and finalize loop structures, copying headers inbetween.  */
  unsigned int execute (function *) final override;

  opt_pass * clone () final override { return new pass_ch (m_ctxt); }

protected:
  /* ch_base method: */
  bool process_loop_p (class loop *loop) final override;
}; // class pass_ch

const pass_data pass_data_ch_vect =
{
  GIMPLE_PASS, /* type */
  "ch_vect", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_CH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

/* This is a more aggressive version of the same pass, designed to run just
   before if-conversion and vectorization, to put more loops into the form
   required for those phases.  */
class pass_ch_vect : public ch_base
{
public:
  pass_ch_vect (gcc::context *ctxt)
    : ch_base (pass_data_ch_vect, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
  {
    return flag_tree_ch != 0
	   && (flag_tree_loop_vectorize != 0 || fun->has_force_vectorize_loops);
  }

  /* Just copy headers, no initialization/finalization of loop structures.  */
  unsigned int execute (function *) final override;

protected:
  /* ch_base method: */
  bool process_loop_p (class loop *loop) final override;
}; // class pass_ch_vect

/* Sort comparator to order loops after the specified order.  */

static int
ch_order_loops (const void *a_, const void *b_, void *order_)
{
  int *order = (int *)order_;
  const class loop *a = *(const class loop * const *)a_;
  const class loop *b = *(const class loop * const *)b_;
  if (a->num == b->num)
    return 0;
  if (order[a->num] < order[b->num])
    return -1;
  return 1;
}

/* For all loops, copy the condition at the end of the loop body in front
   of the loop.  This is beneficial since it increases efficiency of
   code motion optimizations.  It also saves one jump on entry to the loop.  */

unsigned int
ch_base::copy_headers (function *fun)
{
  basic_block *bbs, *copied_bbs;
  unsigned bbs_size;
  bool changed = false;

  if (number_of_loops (fun) <= 1)
    return 0;

  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (fun));
  copied_bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (fun));
  bbs_size = n_basic_blocks_for_fn (fun);

  struct candidate
    {
      class loop *loop;
      unsigned int nheaders;
      hash_set <edge> *invariant_exits, *static_exits;
    };
  auto_vec<struct candidate> candidates;
  auto_vec<std::pair<edge, loop_p> > copied;
  auto_vec<class loop *> loops_to_unloop;
  auto_vec<int> loops_to_unloop_nunroll;

  mark_dfs_back_edges ();
  gimple_ranger *ranger = new gimple_ranger;
  for (auto loop : loops_list (cfun, 0))
    {
      int initial_limit = optimize_loop_for_speed_p (loop)
			  ? param_max_loop_header_insns : 0;
      int remaining_limit = initial_limit;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Analyzing loop %i\n", loop->num);

      /* If the loop is already a do-while style one (either because it was
	 written as such, or because jump threading transformed it into one),
	 we might be in fact peeling the first iteration of the loop.  This
	 in general is not a good idea.  Also avoid touching infinite loops.  */
      if (!loop_has_exit_edges (loop)
	  || !process_loop_p (loop))
	continue;

      basic_block header = loop->header;
      estimate_numbers_of_iterations (loop);
      if (!get_max_loop_iterations_int (loop))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Loop %d never loops.\n", loop->num);
	  scale_loop_profile (loop, profile_probability::always (), 0);
	  loops_to_unloop.safe_push (loop);
	  loops_to_unloop_nunroll.safe_push (0);
	  continue;
	}

      /* Iterate the header copying up to limit; this takes care of the cases
	 like while (a && b) {...}, where we want to have both of the conditions
	 copied.  TODO -- handle while (a || b) - like cases, by not requiring
	 the header to have just a single successor and copying up to
	 postdominator.  */
      unsigned int nheaders = 0;
      unsigned int last_win_nheaders = 0;
      bool last_win_invariant_exit = false;
      ch_decision ret;
      auto_vec <ch_decision, 32> decision;
      hash_set <edge> *invariant_exits = new hash_set <edge>;
      hash_set <edge> *static_exits = new hash_set <edge>;
      while ((ret = should_duplicate_loop_header_p (header, loop, ranger,
						    &remaining_limit,
						    invariant_exits,
						    static_exits))
	     != ch_impossible)
	{
	  nheaders++;
	  decision.safe_push (ret);
	  if (ret >= ch_win)
	    {
	      last_win_nheaders = nheaders;
	      last_win_invariant_exit = (ret == ch_win_invariant_exit);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "    Duplicating bb %i is a win\n",
			 header->index);
	    }
	  else
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "    May duplicate bb %i\n", header->index);

	  /* Find a successor of header that is inside a loop; i.e. the new
	     header after the condition is copied.  */
	  if (flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 0)->dest))
	    header = EDGE_SUCC (header, 0)->dest;
	  else
	    header = EDGE_SUCC (header, 1)->dest;
	}

      /* Try to turn loop into do-while.  This means ensuring that
	 last duplicated header will not have loop invariant exit.  */
      if (last_win_nheaders && last_win_invariant_exit
	  && nheaders > last_win_nheaders)
	{
	  last_win_nheaders++;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "    Duplicating additional BB to obtain do-while loop\n");
	}
      else if (!last_win_nheaders && nheaders && !do_while_loop_p (loop))
	{
	  last_win_nheaders = 1;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "    Duplicating header BB to obtain do-while loop\n");
	}
      /* "Duplicate" all BBs with zero cost following last basic blocks we
	 decided to copy.  */
      while (last_win_nheaders < decision.length ()
	     && decision[last_win_nheaders] == ch_possible_zero_cost)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "    Duplicating extra bb is a win; it has zero cost\n");
	  last_win_nheaders++;
	}

      if (last_win_nheaders)
	candidates.safe_push ({loop, last_win_nheaders,
			      invariant_exits, static_exits});
      else
	{
	  delete invariant_exits;
	  delete static_exits;
	}
    }
  /* Do not use ranger after we change the IL and not have updated SSA.  */
  delete ranger;

  for (auto candidate : candidates)
    {
      class loop *loop = candidate.loop;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Copying headers of loop %i\n", loop->num);

      basic_block header = loop->header;
      edge nonexit = NULL;
      edge exit = NULL;
      unsigned int n_bbs = 0;
      int nexits = 0;
      profile_count exit_count = profile_count::zero ();
      profile_count entry_count = profile_count::zero ();
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, loop->header->preds)
	if (e->src != loop->latch)
	  entry_count += e->count ();
      while (n_bbs < candidate.nheaders)
	{
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "    Will duplicate bb %i\n", header->index);
	  /* Find a successor of header that is inside a loop; i.e. the new
	     header after the condition is copied.  */
	  if (flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 0)->dest))
	    {
	      nonexit = EDGE_SUCC (header, 0);
	      exit = EDGE_SUCC (header, 1);
	    }
	  else
	    {
	      nonexit = EDGE_SUCC (header, 1);
	      exit = EDGE_SUCC (header, 0);
	    }
	  exit_count += exit->count ();
	  nexits++;
	  bbs[n_bbs++] = header;
	  gcc_assert (bbs_size > n_bbs);
	  header = nonexit->dest;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Duplicating header of the loop %d up to edge %d->%d\n",
		 loop->num, exit->src->index, exit->dest->index);

      /* Ensure that the header will have just the latch as a predecessor
	 inside the loop.  */
      if (!single_pred_p (nonexit->dest))
	{
	  header = split_edge (nonexit);
	  exit = single_pred_edge (header);
	}

      edge entry = loop_preheader_edge (loop);

      propagate_threaded_block_debug_into (nonexit->dest, entry->dest);
      if (!gimple_duplicate_seme_region (entry, exit, bbs, n_bbs, copied_bbs,
					 true))
	{
	  delete candidate.static_exits;
	  delete candidate.invariant_exits;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Duplication failed.\n");
	  continue;
	}
      if (loop->header->count.initialized_p ())
	update_profile_after_ch (loop, bbs, copied_bbs, n_bbs,
				 candidate.invariant_exits,
				 candidate.static_exits,
				 entry_count);
      delete candidate.static_exits;
      delete candidate.invariant_exits;
      copied.safe_push (std::make_pair (entry, loop));

      /* If the loop has the form "for (i = j; i < j + 10; i++)" then
	 this copying can introduce a case where we rely on undefined
	 signed overflow to eliminate the preheader condition, because
	 we assume that "j < j + 10" is true.  We don't want to warn
	 about that case for -Wstrict-overflow, because in general we
	 don't warn about overflow involving loops.  Prevent the
	 warning by setting the no_warning flag in the condition.  */
      if (warn_strict_overflow > 0)
	{
	  unsigned int i;

	  for (i = 0; i < n_bbs; ++i)
	    {
	      gimple_stmt_iterator bsi;

	      for (bsi = gsi_start_bb (copied_bbs[i]);
		   !gsi_end_p (bsi);
		   gsi_next (&bsi))
		{
		  gimple *stmt = gsi_stmt (bsi);
		  if (gimple_code (stmt) == GIMPLE_COND)
		    {
		      tree lhs = gimple_cond_lhs (stmt);
		      if (gimple_cond_code (stmt) != EQ_EXPR
			  && gimple_cond_code (stmt) != NE_EXPR
			  && INTEGRAL_TYPE_P (TREE_TYPE (lhs))
			  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (lhs)))
			suppress_warning (stmt, OPT_Wstrict_overflow_);
		    }
		  else if (is_gimple_assign (stmt))
		    {
		      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
		      tree rhs1 = gimple_assign_rhs1 (stmt);
		      if (TREE_CODE_CLASS (rhs_code) == tcc_comparison
			  && rhs_code != EQ_EXPR
			  && rhs_code != NE_EXPR
			  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
			  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (rhs1)))
			suppress_warning (stmt, OPT_Wstrict_overflow_);
		    }
		}
	    }
	}

      /* Update header of the loop.  */
      loop->header = header;
      /* Find correct latch.  We only duplicate chain of conditionals so
	 there should be precisely two edges to the new header.  One entry
	 edge and one to latch.  */
      FOR_EACH_EDGE (e, ei, loop->header->preds)
	if (header != e->src)
	  {
	    loop->latch = e->src;
	    break;
	  }
      /* Ensure that the latch is simple.  */
      if (!single_succ_p (loop_latch_edge (loop)->src))
	split_edge (loop_latch_edge (loop));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (do_while_loop_p (loop))
	    fprintf (dump_file, "Loop %d is now do-while loop.\n", loop->num);
	  else
	    fprintf (dump_file, "Loop %d is still not do-while loop.\n",
		     loop->num);
	  fprintf (dump_file, "Exit count: ");
	  exit_count.dump (dump_file);
	  fprintf (dump_file, "\nEntry count: ");
	  entry_count.dump (dump_file);
	  fprintf (dump_file, "\n");
	}

      /* We possibly decreased number of iterations by 1.  */
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      bool precise = (nexits == (int) exits.length ());
      /* Check that loop may not terminate in other way than via
	 basic blocks we duplicated.  */
      if (precise)
	{
	  basic_block *bbs = get_loop_body (loop);
	  for (unsigned i = 0; i < loop->num_nodes && precise; ++i)
	   {
	     basic_block bb = bbs[i];
	     bool found_exit = false;
	     FOR_EACH_EDGE (e, ei, bb->succs)
	      if (!flow_bb_inside_loop_p (loop, e->dest))
		{
		  found_exit = true;
		  break;
		}
	     /* If BB has exit, it was duplicated.  */
	     if (found_exit)
	       continue;
	     /* Give up on irreducible loops.  */
	     if (bb->flags & BB_IRREDUCIBLE_LOOP)
	       {
		 precise = false;
		 break;
	       }
	     /* Check that inner loops are finite.  */
	     for (class loop *l = bb->loop_father; l != loop && precise;
		  l = loop_outer (l))
	       if (!l->finite_p)
		 {
		   precise = false;
		   break;
		 }
	     /* Verify that there is no statement that may be terminate
		execution in a way not visible to CFG.  */
	     for (gimple_stmt_iterator bsi = gsi_start_bb (bb);
		  !gsi_end_p (bsi); gsi_next (&bsi))
	       if (stmt_can_terminate_bb_p (gsi_stmt (bsi)))
		 precise = false;
	   }
	  free (bbs);
	}
      if (precise
	  && get_max_loop_iterations_int (loop) == 1)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Loop %d no longer loops.\n", loop->num);
	  scale_loop_profile (loop, profile_probability::always (), 0);
	  loops_to_unloop.safe_push (loop);
	  loops_to_unloop_nunroll.safe_push (0);
	}
      else if (precise)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Peeled all exits:"
		     " decreased number of iterations of loop %d by 1.\n",
		     loop->num);
	  adjust_loop_info_after_peeling (loop, 1, true);
	}
      else if (exit_count >= entry_count.apply_scale (9, 10))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Peeled likely exits: likely decreased number "
		     "of iterations of loop %d by 1.\n", loop->num);
	  adjust_loop_info_after_peeling (loop, 1, false);
	}
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Not decreased number"
		 " of iterations of loop %d; likely exits remains.\n",
		 loop->num);

      changed = true;
    }

  if (changed)
    {
      update_ssa (TODO_update_ssa);
      /* After updating SSA form perform CSE on the loop header
	 copies.  This is esp. required for the pass before
	 vectorization since nothing cleans up copied exit tests
	 that can now be simplified.  CSE from the entry of the
	 region we copied till all loop exit blocks but not
	 entering the loop itself.  */
      for (unsigned i = 0; i < copied.length (); ++i)
	{
	  edge entry = copied[i].first;
	  loop_p loop = copied[i].second;
	  auto_vec<edge> exit_edges = get_loop_exit_edges (loop);
	  bitmap exit_bbs = BITMAP_ALLOC (NULL);
	  for (unsigned j = 0; j < exit_edges.length (); ++j)
	    bitmap_set_bit (exit_bbs, exit_edges[j]->dest->index);
	  bitmap_set_bit (exit_bbs, loop->header->index);
	  do_rpo_vn (cfun, entry, exit_bbs);
	  BITMAP_FREE (exit_bbs);
	}
    }
  if (!loops_to_unloop.is_empty ())
    {
      /* Make sure loops are ordered inner to outer for unlooping.  */
      if (loops_to_unloop.length () != 1)
	{
	  auto_vec<int, 8> order;
	  order.safe_grow (number_of_loops (cfun), true);
	  int i = 0;
	  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
	    order[loop->num] = i++;
	  loops_to_unloop.sort (ch_order_loops, order.address ());
	}
      bool irred_invalidated;
      auto_bitmap lc_invalidated;
      auto_vec<edge> edges_to_remove;
      unloop_loops (loops_to_unloop, loops_to_unloop_nunroll, edges_to_remove,
		    lc_invalidated, &irred_invalidated);
      if (loops_state_satisfies_p (fun, LOOP_CLOSED_SSA)
	  && !bitmap_empty_p (lc_invalidated))
	rewrite_into_loop_closed_ssa (NULL, 0);
      changed = true;
    }
  free (bbs);
  free (copied_bbs);

  return changed ? TODO_cleanup_cfg : 0;
}

/* Initialize the loop structures we need, and finalize after.  */

unsigned int
pass_ch::execute (function *fun)
{
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  scev_initialize ();

  unsigned int res = copy_headers (fun);

  scev_finalize ();
  loop_optimizer_finalize ();
  return res;
}

/* Assume an earlier phase has already initialized all the loop structures that
   we need here (and perhaps others too), and that these will be finalized by
   a later phase.  */

unsigned int
pass_ch_vect::execute (function *fun)
{
  return copy_headers (fun);
}

/* Apply header copying according to a very simple test of do-while shape.  */

bool
pass_ch::process_loop_p (class loop *)
{
  return true;
}

/* Apply header-copying to loops where we might enable vectorization.  */

bool
pass_ch_vect::process_loop_p (class loop *loop)
{
  if (!flag_tree_loop_vectorize && !loop->force_vectorize)
    return false;

  if (loop->dont_vectorize)
    return false;

  /* The vectorizer won't handle anything with multiple exits, so skip.  */
  edge exit = single_exit (loop);
  if (!exit)
    return false;

  if (!do_while_loop_p (loop))
    return true;

  return false;
}

} // anon namespace

gimple_opt_pass *
make_pass_ch_vect (gcc::context *ctxt)
{
  return new pass_ch_vect (ctxt);
}

gimple_opt_pass *
make_pass_ch (gcc::context *ctxt)
{
  return new pass_ch (ctxt);
}
