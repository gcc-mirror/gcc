/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004  Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Converted to use trees by Dale Johannesen, Apple Computer.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Generate basic block profile instrumentation and auxiliary files.
   Profile generation is optimized, so that not all arcs in the basic
   block graph need instrumenting. First, the BB graph is closed with
   one entry (function start), and one exit (function exit).  Any
   ABNORMAL_EDGE cannot be instrumented (because there is no control
   path to place the code). We close the graph by inserting fake
   EDGE_FAKE edges to the EXIT_BLOCK, from the sources of abnormal
   edges that do not go to the exit_block. We ignore such abnormal
   edges.  Naturally these fake edges are never directly traversed,
   and so *cannot* be directly instrumented.  Some other graph
   massaging is done. To optimize the instrumentation we generate the
   BB minimal span tree, only edges that are not on the span tree
   (plus the entry point) need instrumenting. From that information
   all other edge counts can be deduced.  By construction all fake
   edges must be on the spanning tree. We also attempt to place
   EDGE_CRITICAL edges on the spanning tree.

   The auxiliary file generated is <dumpbase>.bbg. The format is
   described in full in gcov-io.h.  */

/* ??? Register allocation should use basic block execution counts to
   give preference to the most commonly executed blocks.  */

/* ??? Should calculate branch probabilities before instrumenting code, since
   then we can use arc counts to help decide which arcs to instrument.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "flags.h"
#include "output.h"
#include "regs.h"
#include "expr.h"
#include "function.h"
#include "toplev.h"
#include "coverage.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "value-prof.h"



/* Do initialization work for the edge profiler.  */

static void
tree_init_edge_profiler (void)
{
}

/* Output instructions as GIMPLE trees to increment the edge 
   execution count, and insert them on E.  We rely on 
   bsi_insert_on_edge to preserve the order.  */

static void
tree_gen_edge_profiler (int edgeno, edge e)
{
  tree tmp1 = create_tmp_var (GCOV_TYPE_NODE, "PROF");
  tree tmp2 = create_tmp_var (GCOV_TYPE_NODE, "PROF");
  tree ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  tree stmt1 = build (MODIFY_EXPR, GCOV_TYPE_NODE, tmp1, ref);
  tree stmt2 = build (MODIFY_EXPR, GCOV_TYPE_NODE, tmp2,
		      build (PLUS_EXPR, GCOV_TYPE_NODE, 
			     tmp1, integer_one_node));
  tree stmt3 = build (MODIFY_EXPR, GCOV_TYPE_NODE, ref, tmp2);
  bsi_insert_on_edge (e, stmt1);
  bsi_insert_on_edge (e, stmt2);
  bsi_insert_on_edge (e, stmt3);
}

/* Output instructions as GIMPLE trees to increment the interval histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the 
   tag of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_interval_profiler (histogram_value value ATTRIBUTE_UNUSED, 
			    unsigned tag ATTRIBUTE_UNUSED, 
			    unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Output instructions as GIMPLE trees to increment the power of two histogram 
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag 
   of the section for counters, BASE is offset of the counter position.  */

static void
tree_gen_pow2_profiler (histogram_value value ATTRIBUTE_UNUSED, 
			unsigned tag ATTRIBUTE_UNUSED,
			unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Output instructions as GIMPLE trees for code to find the most common value.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_one_value_profiler (histogram_value value ATTRIBUTE_UNUSED, 
			    unsigned tag ATTRIBUTE_UNUSED,
			    unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Output instructions as GIMPLE trees for code to find the most common value 
   of a difference between two evaluations of an expression.
   VALUE is the expression whose value is profiled.  TAG is the tag of the
   section for counters, BASE is offset of the counter position.  */

static void
tree_gen_const_delta_profiler (histogram_value value ATTRIBUTE_UNUSED, 
				unsigned tag ATTRIBUTE_UNUSED,
				unsigned base ATTRIBUTE_UNUSED)
{
  /* FIXME implement this.  */
#ifdef ENABLE_CHECKING
  internal_error ("unimplemented functionality");
#endif
  gcc_unreachable ();
}

/* Return 1 if tree-based profiling is in effect, else 0.
   If it is, set up hooks for tree-based profiling.
   Gate for pass_tree_profile.  */

static bool do_tree_profiling (void)
{
  if (flag_tree_based_profiling
      && (profile_arc_flag || flag_test_coverage || flag_branch_probabilities))
    {
      tree_register_profile_hooks ();
      tree_register_value_prof_hooks ();
      return true;
    }
  return false;
}

/* Return the file on which profile dump output goes, if any.  */

static FILE *tree_profile_dump_file (void) {
  return dump_file;
}

struct tree_opt_pass pass_tree_profile = 
{
  "tree_profile",			/* name */
  do_tree_profiling,			/* gate */
  branch_prob,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_BRANCH_PROB,			/* tv_id */
  PROP_gimple_leh | PROP_cfg,		/* properties_required */
  PROP_gimple_leh | PROP_cfg,		/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_stmts,			/* todo_flags_finish */
  0					/* letter */
};

struct profile_hooks tree_profile_hooks =
{
  tree_init_edge_profiler,      /* init_edge_profiler */
  tree_gen_edge_profiler,	/* gen_edge_profiler */
  tree_gen_interval_profiler,   /* gen_interval_profiler */
  tree_gen_pow2_profiler,       /* gen_pow2_profiler */
  tree_gen_one_value_profiler,  /* gen_one_value_profiler */
  tree_gen_const_delta_profiler,/* gen_const_delta_profiler */
  tree_profile_dump_file	/* profile_dump_file */
};
