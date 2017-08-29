/* Loop header copying on trees.
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
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-inline.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"
#include "params.h"

/* Duplicates headers of loops if they are small enough, so that the statements
   in the loop body are always executed when the loop is entered.  This
   increases effectiveness of code motion optimizations, and reduces the need
   for loop preconditioning.  */

/* Check whether we should duplicate HEADER of LOOP.  At most *LIMIT
   instructions should be duplicated, limit is decreased by the actual
   amount.  */

static bool
should_duplicate_loop_header_p (basic_block header, struct loop *loop,
				int *limit)
{
  gimple_stmt_iterator bsi;
  gimple *last;

  gcc_assert (!header->aux);

  /* Loop header copying usually increases size of the code.  This used not to
     be true, since quite often it is possible to verify that the condition is
     satisfied in the first iteration and therefore to eliminate it.  Jump
     threading handles these cases now.  */
  if (optimize_loop_for_size_p (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: optimizing for size.\n",
		 header->index);
      return false;
    }

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
		 "  Not duplicating bb %i: both sucessors are in loop.\n",
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

  last = last_stmt (header);
  if (gimple_code (last) != GIMPLE_COND)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Not duplicating bb %i: it does not end by conditional.\n",
		 header->index);
      return false;
    }

  /* Count number of instructions and punt on calls.  */
  for (bsi = gsi_start_bb (header); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      last = gsi_stmt (bsi);

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
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "    Will duplicate bb %i\n", header->index); 
  return true;
}

/* Checks whether LOOP is a do-while style loop.  */

static bool
do_while_loop_p (struct loop *loop)
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

  /* If the header contains just a condition, it is not a do-while loop.  */
  stmt = last_and_only_stmt (loop->header);
  if (stmt
      && gimple_code (stmt) == GIMPLE_COND)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %i is not do-while loop: "
		 "header contains just condition.\n", loop->num);
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
  virtual bool process_loop_p (struct loop *loop) = 0;
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
  virtual bool gate (function *) { return flag_tree_ch != 0; }
  
  /* Initialize and finalize loop structures, copying headers inbetween.  */
  virtual unsigned int execute (function *);

  opt_pass * clone () { return new pass_ch (m_ctxt); }

protected:
  /* ch_base method: */
  virtual bool process_loop_p (struct loop *loop);
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
  virtual bool gate (function *fun)
  {
    return flag_tree_ch != 0
	   && (flag_tree_loop_vectorize != 0 || fun->has_force_vectorize_loops);
  }
  
  /* Just copy headers, no initialization/finalization of loop structures.  */
  virtual unsigned int execute (function *);

protected:
  /* ch_base method: */
  virtual bool process_loop_p (struct loop *loop);
}; // class pass_ch_vect

/* For all loops, copy the condition at the end of the loop body in front
   of the loop.  This is beneficial since it increases efficiency of
   code motion optimizations.  It also saves one jump on entry to the loop.  */

unsigned int
ch_base::copy_headers (function *fun)
{
  struct loop *loop;
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

  FOR_EACH_LOOP (loop, 0)
    {
      int initial_limit = PARAM_VALUE (PARAM_MAX_LOOP_HEADER_INSNS);
      int remaining_limit = initial_limit;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Analyzing loop %i\n", loop->num);

      header = loop->header;

      /* If the loop is already a do-while style one (either because it was
	 written as such, or because jump threading transformed it into one),
	 we might be in fact peeling the first iteration of the loop.  This
	 in general is not a good idea.  */
      if (!process_loop_p (loop))
	continue;

      /* Iterate the header copying up to limit; this takes care of the cases
	 like while (a && b) {...}, where we want to have both of the conditions
	 copied.  TODO -- handle while (a || b) - like cases, by not requiring
	 the header to have just a single successor and copying up to
	 postdominator.  */

      exit = NULL;
      n_bbs = 0;
      while (should_duplicate_loop_header_p (header, loop, &remaining_limit))
	{
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
	  fprintf (dump_file, "Duplication failed.\n");
	  continue;
	}

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
		    gimple_set_no_warning (stmt, true);
		  else if (is_gimple_assign (stmt))
		    {
		      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
		      if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
			gimple_set_no_warning (stmt, true);
		    }
		}
	    }
	}

      /* Ensure that the latch and the preheader is simple (we know that they
	 are not now, since there was the loop exit condition.  */
      split_edge (loop_preheader_edge (loop));
      split_edge (loop_latch_edge (loop));

      changed = true;
    }

  if (changed)
    update_ssa (TODO_update_ssa);
  free (bbs);
  free (copied_bbs);

  return changed ? TODO_cleanup_cfg : 0;
}

/* Initialize the loop structures we need, and finalize after.  */

unsigned int
pass_ch::execute (function *fun)
{
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS
		       | LOOPS_HAVE_SIMPLE_LATCHES);

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
pass_ch::process_loop_p (struct loop *loop)
{
  return !do_while_loop_p (loop);
}

/* Apply header-copying to loops where we might enable vectorization.  */

bool
pass_ch_vect::process_loop_p (struct loop *loop)
{
  if (!flag_tree_loop_vectorize && !loop->force_vectorize)
    return false;

  if (loop->dont_vectorize)
    return false;

  if (!do_while_loop_p (loop))
    return true;

 /* The vectorizer won't handle anything with multiple exits, so skip.  */
  edge exit = single_exit (loop);
  if (!exit)
    return false;

  /* Copy headers iff there looks to be code in the loop after the exit block,
     i.e. the exit block has an edge to another block (besides the latch,
     which should be empty).  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, exit->src->succs)
    if (!loop_exit_edge_p (loop, e)
	&& e->dest != loop->header
	&& e->dest != loop->latch)
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
