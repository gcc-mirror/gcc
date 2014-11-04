/* Optimize and expand sanitizer functions.
   Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "hash-table.h"
#include "predict.h"
#include "vec.h"
#include "hashtab.h"
#include "hash-set.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "hash-map.h"
#include "plugin-api.h"
#include "tree-pass.h"
#include "asan.h"
#include "gimple-pretty-print.h"
#include "tm_p.h"
#include "langhooks.h"
#include "ubsan.h"
#include "params.h"


/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

struct sanopt_info
{
  /* True if this BB has been visited.  */
  bool visited_p;
};

/* This is used to carry various hash maps and variables used
   in sanopt_optimize_walker.  */

struct sanopt_ctx
{
  /* This map maps a pointer (the first argument of UBSAN_NULL) to
     a vector of UBSAN_NULL call statements that check this pointer.  */
  hash_map<tree, auto_vec<gimple> > null_check_map;

  /* Number of IFN_ASAN_CHECK statements.  */
  int asan_num_accesses;
};


/* Try to optimize away redundant UBSAN_NULL checks.
   
   We walk blocks in the CFG via a depth first search of the dominator
   tree; we push unique UBSAN_NULL statements into a vector in the
   NULL_CHECK_MAP as we enter the blocks.  When leaving a block, we
   mark the block as visited; then when checking the statements in the
   vector, we ignore statements that are coming from already visited
   blocks, because these cannot dominate anything anymore.
   CTX is a sanopt context.  */

static void
sanopt_optimize_walker (basic_block bb, struct sanopt_ctx *ctx)
{
  basic_block son;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
    {
      gimple stmt = gsi_stmt (gsi);
      bool remove = false;

      if (is_gimple_call (stmt)
	  && gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_UBSAN_NULL:
	    {
	      gcc_assert (gimple_call_num_args (stmt) == 3);
	      tree ptr = gimple_call_arg (stmt, 0);
	      tree cur_align = gimple_call_arg (stmt, 2);
	      gcc_assert (TREE_CODE (cur_align) == INTEGER_CST);

	      auto_vec<gimple> &v = ctx->null_check_map.get_or_insert (ptr);
	      if (v.is_empty ())
		/* For this PTR we don't have any UBSAN_NULL stmts
		   recorded, so there's nothing to optimize yet.  */
		v.safe_push (stmt);
	      else
		{
		  /* We already have recorded a UBSAN_NULL check
		     for this pointer.  Perhaps we can drop this one.
		     But only if this check doesn't specify stricter
		     alignment.  */
		  int i;
		  gimple g;

		  while (!v.is_empty ())
		    {
		      gimple g = v.last ();
		      /* Remove statements for BBs that have been
			 already processed.  */
		      sanopt_info *si = (sanopt_info *) gimple_bb (g)->aux;
		      if (si->visited_p)
			v.pop ();
		      else
			{
			  /* At this point we shouldn't have any statements
			     that aren't dominating the current BB.  */
			  tree align = gimple_call_arg (g, 2);
			  remove = tree_int_cst_le (cur_align, align);
			  break;
			}
		    }

		  if (remove)
		    {
		      /* Drop this check.  */
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Optimizing out\n  ");
			  print_gimple_stmt (dump_file, stmt, 0,
					     dump_flags);
			  fprintf (dump_file, "\n");
			}
		      gsi_remove (&gsi, true);
		    }
		  else if (v.length () < 30)
		    v.safe_push (stmt);
		  }
	    }
	  case IFN_ASAN_CHECK:
	    ctx->asan_num_accesses++;
	    break;
	  default:
	    break;
	  }

      /* If we were able to remove the current statement, gsi_remove
	 already pointed us to the next statement.  */
      if (!remove)
	gsi_next (&gsi);
    }

  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    sanopt_optimize_walker (son, ctx);

  /* We're leaving this BB, so mark it to that effect.  */
  sanopt_info *info = (sanopt_info *) bb->aux;
  info->visited_p = true;
}

/* Try to remove redundant sanitizer checks in function FUN.  */

static int
sanopt_optimize (function *fun)
{
  struct sanopt_ctx ctx;
  ctx.asan_num_accesses = 0;

  /* Set up block info for each basic block.  */
  alloc_aux_for_blocks (sizeof (sanopt_info));

  /* We're going to do a dominator walk, so ensure that we have
     dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* Recursively walk the dominator tree optimizing away
     redundant checks.  */
  sanopt_optimize_walker (ENTRY_BLOCK_PTR_FOR_FN (fun), &ctx);

  free_aux_for_blocks ();

  return ctx.asan_num_accesses;
}

/* Perform optimization of sanitize functions.  */

namespace {

const pass_data pass_data_sanopt =
{
  GIMPLE_PASS, /* type */
  "sanopt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg | PROP_gimple_leh ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_sanopt : public gimple_opt_pass
{
public:
  pass_sanopt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sanopt, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_sanitize; }
  virtual unsigned int execute (function *);

}; // class pass_sanopt

unsigned int
pass_sanopt::execute (function *fun)
{
  basic_block bb;
  int asan_num_accesses = 0;

  /* Try to remove redundant checks.  */
  if (optimize
      && (flag_sanitize & (SANITIZE_NULL | SANITIZE_ALIGNMENT)))
    asan_num_accesses = sanopt_optimize (fun);
  else if (flag_sanitize & SANITIZE_ADDRESS)
    {
      gimple_stmt_iterator gsi;
      FOR_EACH_BB_FN (bb, fun)
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  {
 	    gimple stmt = gsi_stmt (gsi);
	    if (is_gimple_call (stmt) && gimple_call_internal_p (stmt)
		&& gimple_call_internal_fn (stmt) == IFN_ASAN_CHECK)
	      ++asan_num_accesses;
	  }
    }

  bool use_calls = ASAN_INSTRUMENTATION_WITH_CALL_THRESHOLD < INT_MAX
    && asan_num_accesses >= ASAN_INSTRUMENTATION_WITH_CALL_THRESHOLD;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	{
	  gimple stmt = gsi_stmt (gsi);
	  bool no_next = false;

	  if (!is_gimple_call (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if (gimple_call_internal_p (stmt))
	    {
	      enum internal_fn ifn = gimple_call_internal_fn (stmt);
	      switch (ifn)
		{
		case IFN_UBSAN_NULL:
		  no_next = ubsan_expand_null_ifn (&gsi);
		  break;
		case IFN_UBSAN_BOUNDS:
		  no_next = ubsan_expand_bounds_ifn (&gsi);
		  break;
		case IFN_UBSAN_OBJECT_SIZE:
		  no_next = ubsan_expand_objsize_ifn (&gsi);
		  break;
		case IFN_ASAN_CHECK:
		  no_next = asan_expand_check_ifn (&gsi, use_calls);
		  break;
		default:
		  break;
		}
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Expanded\n  ");
	      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
	      fprintf (dump_file, "\n");
	    }

	  if (!no_next)
	    gsi_next (&gsi);
	}
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_sanopt (gcc::context *ctxt)
{
  return new pass_sanopt (ctxt);
}
