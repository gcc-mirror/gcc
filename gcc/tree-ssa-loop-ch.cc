/* Loop header copying on trees.
   Copyright (C) 2004-2023 Free Software Foundation, Inc.

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
#include "cfganal.h"

/* Duplicates headers of loops if they are small enough, so that the statements
   in the loop body are always executed when the loop is entered.  This
   increases effectiveness of code motion optimizations, and reduces the need
   for loop preconditioning.  */

/* Given a path through edge E, whose last statement is COND, return
   the range of the solved conditional in R.  */

static void
edge_range_query (irange &r, edge e, gcond *cond, gimple_ranger &ranger)
{
  auto_vec<basic_block> path (2);
  path.safe_push (e->dest);
  path.safe_push (e->src);
  path_range_query query (ranger, path);
  if (!query.range_of_stmt (r, cond))
    r.set_varying (boolean_type_node);
}

/* Return true if the condition on the first iteration of the loop can
   be statically determined.  */

static bool
entry_loop_condition_is_static (class loop *l, gimple_ranger *ranger)
{
  edge e = loop_preheader_edge (l);
  gcond *last = safe_dyn_cast <gcond *> (last_stmt (e->dest));

  if (!last)
    return false;

  edge true_e, false_e;
  extract_true_false_edges_from_block (e->dest, &true_e, &false_e);

  /* If neither edge is the exit edge, this is not a case we'd like to
     special-case.  */
  if (!loop_exit_edge_p (l, true_e) && !loop_exit_edge_p (l, false_e))
    return false;

  tree desired_static_value;
  if (loop_exit_edge_p (l, true_e))
    desired_static_value = boolean_false_node;
  else
    desired_static_value = boolean_true_node;

  int_range<2> r;
  edge_range_query (r, e, last, *ranger);
  return r == int_range<2> (desired_static_value, desired_static_value);
}

/* Check whether we should duplicate HEADER of LOOP.  At most *LIMIT
   instructions should be duplicated, limit is decreased by the actual
   amount.  */

static bool
should_duplicate_loop_header_p (basic_block header, class loop *loop,
				int *limit)
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
      return false;
    }

  if (flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 0)->dest)
      && flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 1)->dest))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: both successors are in loop.\n",
		 loop->num);
      return false;
    }

  /* If this is not the original loop header, we want it to have just
     one predecessor in order to match the && pattern.  */
  if (header != loop->header && !single_pred_p (header))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it has mutiple predecestors.\n",
		 header->index);
      return false;
    }

  gcond *last = safe_dyn_cast <gcond *> (last_stmt (header));
  if (!last)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it does not end by conditional.\n",
		 header->index);
      return false;
    }

  for (gphi_iterator psi = gsi_start_phis (header); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (INTEGRAL_TYPE_P (TREE_TYPE (res))
	  || POINTER_TYPE_P (TREE_TYPE (res)))
	gimple_set_uid (phi, 1 /* IV */);
      else
	gimple_set_uid (phi, 0);
    }

  /* Count number of instructions and punt on calls.
     Populate stmts INV/IV flag to later apply heuristics to the
     kind of conditions we want to copy.  */
  for (bsi = gsi_start_bb (header); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple *last = gsi_stmt (bsi);

      if (gimple_code (last) == GIMPLE_LABEL)
	continue;

      if (is_gimple_debug (last))
	continue;

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
	  return false;
	}

      *limit -= estimate_num_insns (last, &eni_size_weights);
      if (*limit < 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Not duplicating bb %i contains too many insns.\n",
		     header->index);
	  return false;
	}

      /* Classify the stmt based on whether its computation is based
         on a IV or whether it is invariant in the loop.  */
      gimple_set_uid (last, 0);
      if (!gimple_vuse (last))
	{
	  bool inv = true;
	  bool iv = false;
	  ssa_op_iter i;
	  tree op;
	  FOR_EACH_SSA_TREE_OPERAND (op, last, i, SSA_OP_USE)
	    if (!SSA_NAME_IS_DEFAULT_DEF (op)
		&& flow_bb_inside_loop_p (loop,
					  gimple_bb (SSA_NAME_DEF_STMT (op))))
	      {
		if (!(gimple_uid (SSA_NAME_DEF_STMT (op)) & 2 /* INV */))
		  inv = false;
		if (gimple_uid (SSA_NAME_DEF_STMT (op)) & 1 /* IV */)
		  iv = true;
	      }
	  gimple_set_uid (last, (iv ? 1 : 0) | (inv ? 2 : 0));
	}
    }

  /* If the condition tests a non-IV loop variant we do not want to rotate
     the loop further.  Unless this is the original loop header.  */
  tree lhs = gimple_cond_lhs (last);
  tree rhs = gimple_cond_rhs (last);
  if (header != loop->header
      && ((TREE_CODE (lhs) == SSA_NAME
	   && !SSA_NAME_IS_DEFAULT_DEF (lhs)
	   && flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (lhs)))
	   && gimple_uid (SSA_NAME_DEF_STMT (lhs)) == 0)
	  || (TREE_CODE (rhs) == SSA_NAME
	      && !SSA_NAME_IS_DEFAULT_DEF (rhs)
	      && flow_bb_inside_loop_p (loop,
					gimple_bb (SSA_NAME_DEF_STMT (rhs)))
	      && gimple_uid (SSA_NAME_DEF_STMT (rhs)) == 0)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: condition based on non-IV loop"
		 " variant.\n", header->index);
      return false;
    }

  return true;
}

/* Checks whether LOOP is a do-while style loop.  */

static bool
do_while_loop_p (class loop *loop)
{
  gimple *stmt = last_stmt (loop->latch);

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

  /* If the latch predecessor doesn't exit the loop, it is not a
     do-while loop.  */
  if (!loop_exits_from_bb_p (loop, single_pred (loop->latch)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: latch predecessor "
		 "does not exit loop.\n", loop->num);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Loop %i is do-while loop\n", loop->num);

  return true;
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

/* For all loops, copy the condition at the end of the loop body in front
   of the loop.  This is beneficial since it increases efficiency of
   code motion optimizations.  It also saves one jump on entry to the loop.  */

unsigned int
ch_base::copy_headers (function *fun)
{
  basic_block header;
  edge exit, entry;
  basic_block *bbs, *copied_bbs;
  unsigned n_bbs;
  unsigned bbs_size;
  bool changed = false;

  if (number_of_loops (fun) <= 1)
    return 0;

  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (fun));
  copied_bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (fun));
  bbs_size = n_basic_blocks_for_fn (fun);

  auto_vec<loop_p> candidates;
  auto_vec<std::pair<edge, loop_p> > copied;

  mark_dfs_back_edges ();
  gimple_ranger *ranger = new gimple_ranger;
  for (auto loop : loops_list (cfun, 0))
    {
      int initial_limit = param_max_loop_header_insns;
      int remaining_limit = initial_limit;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Analyzing loop %i\n", loop->num);

      header = loop->header;

      /* If the loop is already a do-while style one (either because it was
	 written as such, or because jump threading transformed it into one),
	 we might be in fact peeling the first iteration of the loop.  This
	 in general is not a good idea.  Also avoid touching infinite loops.  */
      if (!loop_has_exit_edges (loop)
	  || !process_loop_p (loop))
	continue;

      /* Avoid loop header copying when optimizing for size unless we can
	 determine that the loop condition is static in the first
	 iteration.  */
      if (optimize_loop_for_size_p (loop)
	  && !loop->force_vectorize
	  && !entry_loop_condition_is_static (loop, ranger))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Not duplicating bb %i: optimizing for size.\n",
		     header->index);
	  continue;
	}

      if (should_duplicate_loop_header_p (header, loop, &remaining_limit))
	candidates.safe_push (loop);
    }
  /* Do not use ranger after we change the IL and not have updated SSA.  */
  delete ranger;

  for (auto loop : candidates)
    {
      int initial_limit = param_max_loop_header_insns;
      int remaining_limit = initial_limit;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Copying headers of loop %i\n", loop->num);

      header = loop->header;

      /* Iterate the header copying up to limit; this takes care of the cases
	 like while (a && b) {...}, where we want to have both of the conditions
	 copied.  TODO -- handle while (a || b) - like cases, by not requiring
	 the header to have just a single successor and copying up to
	 postdominator.  */

      exit = NULL;
      n_bbs = 0;
      while (should_duplicate_loop_header_p (header, loop, &remaining_limit))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "    Will duplicate bb %i\n", header->index);

	  /* Find a successor of header that is inside a loop; i.e. the new
	     header after the condition is copied.  */
	  if (flow_bb_inside_loop_p (loop, EDGE_SUCC (header, 0)->dest))
	    exit = EDGE_SUCC (header, 0);
	  else
	    exit = EDGE_SUCC (header, 1);
	  bbs[n_bbs++] = header;
	  gcc_assert (bbs_size > n_bbs);
	  header = exit->dest;
	}

      if (!exit)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Duplicating header of the loop %d up to edge %d->%d,"
		 " %i insns.\n",
		 loop->num, exit->src->index, exit->dest->index,
		 initial_limit - remaining_limit);

      /* Ensure that the header will have just the latch as a predecessor
	 inside the loop.  */
      if (!single_pred_p (exit->dest))
	exit = single_pred_edge (split_edge (exit));

      entry = loop_preheader_edge (loop);

      propagate_threaded_block_debug_into (exit->dest, entry->dest);
      if (!gimple_duplicate_sese_region (entry, exit, bbs, n_bbs, copied_bbs,
					 true))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Duplication failed.\n");
	  continue;
	}
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

      /* Ensure that the latch and the preheader is simple (we know that they
	 are not now, since there was the loop exit condition.  */
      split_edge (loop_preheader_edge (loop));
      split_edge (loop_latch_edge (loop));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (do_while_loop_p (loop))
	    fprintf (dump_file, "Loop %d is now do-while loop.\n", loop->num);
	  else
	    fprintf (dump_file, "Loop %d is still not do-while loop.\n",
		     loop->num);
	}

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
  free (bbs);
  free (copied_bbs);

  return changed ? TODO_cleanup_cfg : 0;
}

/* Initialize the loop structures we need, and finalize after.  */

unsigned int
pass_ch::execute (function *fun)
{
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS
		       | LOOPS_HAVE_SIMPLE_LATCHES
		       | LOOPS_HAVE_RECORDED_EXITS);

  unsigned int res = copy_headers (fun);

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
pass_ch::process_loop_p (class loop *loop)
{
  return !do_while_loop_p (loop);
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
