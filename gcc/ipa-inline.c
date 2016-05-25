/* Inlining decision heuristics.
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

/*  Inlining decision heuristics

    The implementation of inliner is organized as follows:

    inlining heuristics limits

      can_inline_edge_p allow to check that particular inlining is allowed
      by the limits specified by user (allowed function growth, growth and so
      on).

      Functions are inlined when it is obvious the result is profitable (such
      as functions called once or when inlining reduce code size).
      In addition to that we perform inlining of small functions and recursive
      inlining.

    inlining heuristics

       The inliner itself is split into two passes:

       pass_early_inlining

	 Simple local inlining pass inlining callees into current function.
	 This pass makes no use of whole unit analysis and thus it can do only
	 very simple decisions based on local properties.

	 The strength of the pass is that it is run in topological order
	 (reverse postorder) on the callgraph. Functions are converted into SSA
	 form just before this pass and optimized subsequently. As a result, the
	 callees of the function seen by the early inliner was already optimized
	 and results of early inlining adds a lot of optimization opportunities
	 for the local optimization.

	 The pass handle the obvious inlining decisions within the compilation
	 unit - inlining auto inline functions, inlining for size and
	 flattening.

	 main strength of the pass is the ability to eliminate abstraction
	 penalty in C++ code (via combination of inlining and early
	 optimization) and thus improve quality of analysis done by real IPA
	 optimizers.

	 Because of lack of whole unit knowledge, the pass can not really make
	 good code size/performance tradeoffs.  It however does very simple
	 speculative inlining allowing code size to grow by
	 EARLY_INLINING_INSNS when callee is leaf function.  In this case the
	 optimizations performed later are very likely to eliminate the cost.

       pass_ipa_inline

	 This is the real inliner able to handle inlining with whole program
	 knowledge. It performs following steps:

	 1) inlining of small functions.  This is implemented by greedy
	 algorithm ordering all inlinable cgraph edges by their badness and
	 inlining them in this order as long as inline limits allows doing so.

	 This heuristics is not very good on inlining recursive calls. Recursive
	 calls can be inlined with results similar to loop unrolling. To do so,
	 special purpose recursive inliner is executed on function when
	 recursive edge is met as viable candidate.

	 2) Unreachable functions are removed from callgraph.  Inlining leads
	 to devirtualization and other modification of callgraph so functions
	 may become unreachable during the process. Also functions declared as
	 extern inline or virtual functions are removed, since after inlining
	 we no longer need the offline bodies.

	 3) Functions called once and not exported from the unit are inlined.
	 This should almost always lead to reduction of code size by eliminating
	 the need for offline copy of the function.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "trans-mem.h"
#include "calls.h"
#include "tree-inline.h"
#include "params.h"
#include "profile.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "sreal.h"
#include "auto-profile.h"
#include "builtins.h"
#include "fibonacci_heap.h"

typedef fibonacci_heap <sreal, cgraph_edge> edge_heap_t;
typedef fibonacci_node <sreal, cgraph_edge> edge_heap_node_t;

/* Statistics we collect about inlining algorithm.  */
static int overall_size;
static gcov_type max_count;
static gcov_type spec_rem;

/* Pre-computed constants 1/CGRAPH_FREQ_BASE and 1/100. */
static sreal cgraph_freq_base_rec, percent_rec;

/* Return false when inlining edge E would lead to violating
   limits on function unit growth or stack usage growth.  

   The relative function body growth limit is present generally
   to avoid problems with non-linear behavior of the compiler.
   To allow inlining huge functions into tiny wrapper, the limit
   is always based on the bigger of the two functions considered.

   For stack growth limits we always base the growth in stack usage
   of the callers.  We want to prevent applications from segfaulting
   on stack overflow when functions with huge stack frames gets
   inlined. */

static bool
caller_growth_limits (struct cgraph_edge *e)
{
  struct cgraph_node *to = e->caller;
  struct cgraph_node *what = e->callee->ultimate_alias_target ();
  int newsize;
  int limit = 0;
  HOST_WIDE_INT stack_size_limit = 0, inlined_stack;
  inline_summary *info, *what_info, *outer_info = inline_summaries->get (to);

  /* Look for function e->caller is inlined to.  While doing
     so work out the largest function body on the way.  As
     described above, we want to base our function growth
     limits based on that.  Not on the self size of the
     outer function, not on the self size of inline code
     we immediately inline to.  This is the most relaxed
     interpretation of the rule "do not grow large functions
     too much in order to prevent compiler from exploding".  */
  while (true)
    {
      info = inline_summaries->get (to);
      if (limit < info->self_size)
	limit = info->self_size;
      if (stack_size_limit < info->estimated_self_stack_size)
	stack_size_limit = info->estimated_self_stack_size;
      if (to->global.inlined_to)
        to = to->callers->caller;
      else
	break;
    }

  what_info = inline_summaries->get (what);

  if (limit < what_info->self_size)
    limit = what_info->self_size;

  limit += limit * PARAM_VALUE (PARAM_LARGE_FUNCTION_GROWTH) / 100;

  /* Check the size after inlining against the function limits.  But allow
     the function to shrink if it went over the limits by forced inlining.  */
  newsize = estimate_size_after_inlining (to, e);
  if (newsize >= info->size
      && newsize > PARAM_VALUE (PARAM_LARGE_FUNCTION_INSNS)
      && newsize > limit)
    {
      e->inline_failed = CIF_LARGE_FUNCTION_GROWTH_LIMIT;
      return false;
    }

  if (!what_info->estimated_stack_size)
    return true;

  /* FIXME: Stack size limit often prevents inlining in Fortran programs
     due to large i/o datastructures used by the Fortran front-end.
     We ought to ignore this limit when we know that the edge is executed
     on every invocation of the caller (i.e. its call statement dominates
     exit block).  We do not track this information, yet.  */
  stack_size_limit += ((gcov_type)stack_size_limit
		       * PARAM_VALUE (PARAM_STACK_FRAME_GROWTH) / 100);

  inlined_stack = (outer_info->stack_frame_offset
		   + outer_info->estimated_self_stack_size
		   + what_info->estimated_stack_size);
  /* Check new stack consumption with stack consumption at the place
     stack is used.  */
  if (inlined_stack > stack_size_limit
      /* If function already has large stack usage from sibling
	 inline call, we can inline, too.
	 This bit overoptimistically assume that we are good at stack
	 packing.  */
      && inlined_stack > info->estimated_stack_size
      && inlined_stack > PARAM_VALUE (PARAM_LARGE_STACK_FRAME))
    {
      e->inline_failed = CIF_LARGE_STACK_FRAME_GROWTH_LIMIT;
      return false;
    }
  return true;
}

/* Dump info about why inlining has failed.  */

static void
report_inline_failed_reason (struct cgraph_edge *e)
{
  if (dump_file)
    {
      fprintf (dump_file, "  not inlinable: %s/%i -> %s/%i, %s\n",
	       xstrdup_for_dump (e->caller->name ()), e->caller->order,
	       xstrdup_for_dump (e->callee->name ()), e->callee->order,
	       cgraph_inline_failed_string (e->inline_failed));
      if ((e->inline_failed == CIF_TARGET_OPTION_MISMATCH
	   || e->inline_failed == CIF_OPTIMIZATION_MISMATCH)
	  && e->caller->lto_file_data
	  && e->callee->ultimate_alias_target ()->lto_file_data)
	{
	  fprintf (dump_file, "  LTO objects: %s, %s\n",
		   e->caller->lto_file_data->file_name,
		   e->callee->ultimate_alias_target ()->lto_file_data->file_name);
	}
      if (e->inline_failed == CIF_TARGET_OPTION_MISMATCH)
	cl_target_option_print_diff
	 (dump_file, 2, target_opts_for_fn (e->caller->decl),
          target_opts_for_fn (e->callee->ultimate_alias_target ()->decl));
      if (e->inline_failed == CIF_OPTIMIZATION_MISMATCH)
	cl_optimization_print_diff
	  (dump_file, 2, opts_for_fn (e->caller->decl),
	   opts_for_fn (e->callee->ultimate_alias_target ()->decl));
    }
}

 /* Decide whether sanitizer-related attributes allow inlining. */

static bool
sanitize_attrs_match_for_inline_p (const_tree caller, const_tree callee)
{
  /* Don't care if sanitizer is disabled */
  if (!(flag_sanitize & SANITIZE_ADDRESS))
    return true;

  if (!caller || !callee)
    return true;

  return !!lookup_attribute ("no_sanitize_address",
      DECL_ATTRIBUTES (caller)) == 
      !!lookup_attribute ("no_sanitize_address",
      DECL_ATTRIBUTES (callee));
}

/* Used for flags where it is safe to inline when caller's value is
   grater than callee's.  */
#define check_maybe_up(flag) \
      (opts_for_fn (caller->decl)->x_##flag		\
       != opts_for_fn (callee->decl)->x_##flag		\
       && (!always_inline 				\
	   || opts_for_fn (caller->decl)->x_##flag	\
	      < opts_for_fn (callee->decl)->x_##flag))
/* Used for flags where it is safe to inline when caller's value is
   smaller than callee's.  */
#define check_maybe_down(flag) \
      (opts_for_fn (caller->decl)->x_##flag		\
       != opts_for_fn (callee->decl)->x_##flag		\
       && (!always_inline 				\
	   || opts_for_fn (caller->decl)->x_##flag	\
	      > opts_for_fn (callee->decl)->x_##flag))
/* Used for flags where exact match is needed for correctness.  */
#define check_match(flag) \
      (opts_for_fn (caller->decl)->x_##flag		\
       != opts_for_fn (callee->decl)->x_##flag)

 /* Decide if we can inline the edge and possibly update
   inline_failed reason.  
   We check whether inlining is possible at all and whether
   caller growth limits allow doing so.  

   if REPORT is true, output reason to the dump file.  

   if DISREGARD_LIMITS is true, ignore size limits.*/

static bool
can_inline_edge_p (struct cgraph_edge *e, bool report,
		   bool disregard_limits = false, bool early = false)
{
  gcc_checking_assert (e->inline_failed);

  if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
    {
      if (report)
        report_inline_failed_reason (e);
      return false;
    }

  bool inlinable = true;
  enum availability avail;
  cgraph_node *caller = e->caller->global.inlined_to
		        ? e->caller->global.inlined_to : e->caller;
  cgraph_node *callee = e->callee->ultimate_alias_target (&avail, caller);
  tree caller_tree = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (caller->decl);
  tree callee_tree
    = callee ? DECL_FUNCTION_SPECIFIC_OPTIMIZATION (callee->decl) : NULL;

  if (!callee->definition)
    {
      e->inline_failed = CIF_BODY_NOT_AVAILABLE;
      inlinable = false;
    }
  else if (callee->calls_comdat_local)
    {
      e->inline_failed = CIF_USES_COMDAT_LOCAL;
      inlinable = false;
    }
  else if (avail <= AVAIL_INTERPOSABLE)
    {
      e->inline_failed = CIF_OVERWRITABLE;
      inlinable = false;
    }
  /* All edges with call_stmt_cannot_inline_p should have inline_failed
     initialized to one of FINAL_ERROR reasons.  */
  else if (e->call_stmt_cannot_inline_p)
    gcc_unreachable ();
  /* Don't inline if the functions have different EH personalities.  */
  else if (DECL_FUNCTION_PERSONALITY (caller->decl)
	   && DECL_FUNCTION_PERSONALITY (callee->decl)
	   && (DECL_FUNCTION_PERSONALITY (caller->decl)
	       != DECL_FUNCTION_PERSONALITY (callee->decl)))
    {
      e->inline_failed = CIF_EH_PERSONALITY;
      inlinable = false;
    }
  /* TM pure functions should not be inlined into non-TM_pure
     functions.  */
  else if (is_tm_pure (callee->decl) && !is_tm_pure (caller->decl))
    {
      e->inline_failed = CIF_UNSPECIFIED;
      inlinable = false;
    }
  /* Check compatibility of target optimization options.  */
  else if (!targetm.target_option.can_inline_p (caller->decl,
						callee->decl))
    {
      e->inline_failed = CIF_TARGET_OPTION_MISMATCH;
      inlinable = false;
    }
  else if (!inline_summaries->get (callee)->inlinable)
    {
      e->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
      inlinable = false;
    }
  else if (inline_summaries->get (caller)->contains_cilk_spawn)
    {
      e->inline_failed = CIF_CILK_SPAWN;
      inlinable = false;
    }
  /* Don't inline a function with mismatched sanitization attributes. */
  else if (!sanitize_attrs_match_for_inline_p (caller->decl, callee->decl))
    {
      e->inline_failed = CIF_ATTRIBUTE_MISMATCH;
      inlinable = false;
    }
  /* Check if caller growth allows the inlining.  */
  else if (!DECL_DISREGARD_INLINE_LIMITS (callee->decl)
	   && !disregard_limits
	   && !lookup_attribute ("flatten",
				 DECL_ATTRIBUTES (caller->decl))
           && !caller_growth_limits (e))
    inlinable = false;
  /* Don't inline a function with a higher optimization level than the
     caller.  FIXME: this is really just tip of iceberg of handling
     optimization attribute.  */
  else if (caller_tree != callee_tree)
    {
      bool always_inline =
	     (DECL_DISREGARD_INLINE_LIMITS (callee->decl)
	      && lookup_attribute ("always_inline",
				   DECL_ATTRIBUTES (callee->decl)));
      inline_summary *caller_info = inline_summaries->get (caller);
      inline_summary *callee_info = inline_summaries->get (callee);

     /* Until GCC 4.9 we did not check the semantics alterning flags
	bellow and inline across optimization boundry.
	Enabling checks bellow breaks several packages by refusing
	to inline library always_inline functions. See PR65873.
	Disable the check for early inlining for now until better solution
	is found.  */
     if (always_inline && early)
	;
      /* There are some options that change IL semantics which means
         we cannot inline in these cases for correctness reason.
	 Not even for always_inline declared functions.  */
      /* Strictly speaking only when the callee contains signed integer
         math where overflow is undefined.  */
     else if ((check_maybe_up (flag_strict_overflow)
	       /* this flag is set by optimize.  Allow inlining across
		  optimize boundary.  */
	       && (!opt_for_fn (caller->decl, optimize)
		   == !opt_for_fn (callee->decl, optimize) || !always_inline))
	      || check_match (flag_wrapv)
	      || check_match (flag_trapv)
	      /* When caller or callee does FP math, be sure FP codegen flags
		 compatible.  */
	      || ((caller_info->fp_expressions && callee_info->fp_expressions)
		  && (check_maybe_up (flag_rounding_math)
		      || check_maybe_up (flag_trapping_math)
		      || check_maybe_down (flag_unsafe_math_optimizations)
		      || check_maybe_down (flag_finite_math_only)
		      || check_maybe_up (flag_signaling_nans)
		      || check_maybe_down (flag_cx_limited_range)
		      || check_maybe_up (flag_signed_zeros)
		      || check_maybe_down (flag_associative_math)
		      || check_maybe_down (flag_reciprocal_math)
		      /* Strictly speaking only when the callee contains function
			 calls that may end up setting errno.  */
		      || check_maybe_up (flag_errno_math)))
	      /* We do not want to make code compiled with exceptions to be
		 brought into a non-EH function unless we know that the callee
		 does not throw.
		 This is tracked by DECL_FUNCTION_PERSONALITY.  */
	      || (check_maybe_up (flag_non_call_exceptions)
		  && DECL_FUNCTION_PERSONALITY (callee->decl))
	      || (check_maybe_up (flag_exceptions)
		  && DECL_FUNCTION_PERSONALITY (callee->decl))
	      /* When devirtualization is diabled for callee, it is not safe
		 to inline it as we possibly mangled the type info.
		 Allow early inlining of always inlines.  */
	      || (!early && check_maybe_down (flag_devirtualize)))
	{
	  e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	  inlinable = false;
	}
      /* gcc.dg/pr43564.c.  Apply user-forced inline even at -O0.  */
      else if (always_inline)
	;
      /* When user added an attribute to the callee honor it.  */
      else if (lookup_attribute ("optimize", DECL_ATTRIBUTES (callee->decl))
	       && opts_for_fn (caller->decl) != opts_for_fn (callee->decl))
	{
	  e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	  inlinable = false;
	}
      /* If explicit optimize attribute are not used, the mismatch is caused
	 by different command line options used to build different units.
	 Do not care about COMDAT functions - those are intended to be
         optimized with the optimization flags of module they are used in.
	 Also do not care about mixing up size/speed optimization when
	 DECL_DISREGARD_INLINE_LIMITS is set.  */
      else if ((callee->merged_comdat
	        && !lookup_attribute ("optimize",
				      DECL_ATTRIBUTES (caller->decl)))
	       || DECL_DISREGARD_INLINE_LIMITS (callee->decl))
	;
      /* If mismatch is caused by merging two LTO units with different
	 optimizationflags we want to be bit nicer.  However never inline
	 if one of functions is not optimized at all.  */
      else if (!opt_for_fn (callee->decl, optimize)
      	       || !opt_for_fn (caller->decl, optimize))
	{
	  e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	  inlinable = false;
	}
      /* If callee is optimized for size and caller is not, allow inlining if
	 code shrinks or we are in MAX_INLINE_INSNS_SINGLE limit and callee
	 is inline (and thus likely an unified comdat).  This will allow caller
	 to run faster.  */
      else if (opt_for_fn (callee->decl, optimize_size)
	       > opt_for_fn (caller->decl, optimize_size))
	{
	  int growth = estimate_edge_growth (e);
	  if (growth > 0
	      && (!DECL_DECLARED_INLINE_P (callee->decl)
		  && growth >= MAX (MAX_INLINE_INSNS_SINGLE,
				    MAX_INLINE_INSNS_AUTO)))
	    {
	      e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	      inlinable = false;
	    }
	}
      /* If callee is more aggressively optimized for performance than caller,
	 we generally want to inline only cheap (runtime wise) functions.  */
      else if (opt_for_fn (callee->decl, optimize_size)
	       < opt_for_fn (caller->decl, optimize_size)
	       || (opt_for_fn (callee->decl, optimize)
		   > opt_for_fn (caller->decl, optimize)))
	{
	  if (estimate_edge_time (e)
	      >= 20 + inline_edge_summary (e)->call_stmt_time)
	    {
	      e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	      inlinable = false;
	    }
	}

    }

  if (!inlinable && report)
    report_inline_failed_reason (e);
  return inlinable;
}


/* Return true if the edge E is inlinable during early inlining.  */

static bool
can_early_inline_edge_p (struct cgraph_edge *e)
{
  struct cgraph_node *callee = e->callee->ultimate_alias_target ();
  /* Early inliner might get called at WPA stage when IPA pass adds new
     function.  In this case we can not really do any of early inlining
     because function bodies are missing.  */
  if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
    return false;
  if (!gimple_has_body_p (callee->decl))
    {
      e->inline_failed = CIF_BODY_NOT_AVAILABLE;
      return false;
    }
  /* In early inliner some of callees may not be in SSA form yet
     (i.e. the callgraph is cyclic and we did not process
     the callee by early inliner, yet).  We don't have CIF code for this
     case; later we will re-do the decision in the real inliner.  */
  if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (e->caller->decl))
      || !gimple_in_ssa_p (DECL_STRUCT_FUNCTION (callee->decl)))
    {
      if (dump_file)
	fprintf (dump_file, "  edge not inlinable: not in SSA form\n");
      return false;
    }
  if (!can_inline_edge_p (e, true, false, true))
    return false;
  return true;
}


/* Return number of calls in N.  Ignore cheap builtins.  */

static int
num_calls (struct cgraph_node *n)
{
  struct cgraph_edge *e;
  int num = 0;

  for (e = n->callees; e; e = e->next_callee)
    if (!is_inexpensive_builtin (e->callee->decl))
      num++;
  return num;
}


/* Return true if we are interested in inlining small function.  */

static bool
want_early_inline_function_p (struct cgraph_edge *e)
{
  bool want_inline = true;
  struct cgraph_node *callee = e->callee->ultimate_alias_target ();

  if (DECL_DISREGARD_INLINE_LIMITS (callee->decl))
    ;
  /* For AutoFDO, we need to make sure that before profile summary, all
     hot paths' IR look exactly the same as profiled binary. As a result,
     in einliner, we will disregard size limit and inline those callsites
     that are:
       * inlined in the profiled binary, and
       * the cloned callee has enough samples to be considered "hot".  */
  else if (flag_auto_profile && afdo_callsite_hot_enough_for_early_inline (e))
    ;
  else if (!DECL_DECLARED_INLINE_P (callee->decl)
	   && !opt_for_fn (e->caller->decl, flag_inline_small_functions))
    {
      e->inline_failed = CIF_FUNCTION_NOT_INLINE_CANDIDATE;
      report_inline_failed_reason (e);
      want_inline = false;
    }
  else
    {
      int growth = estimate_edge_growth (e);
      int n;

      if (growth <= 0)
	;
      else if (!e->maybe_hot_p ()
	       && growth > 0)
	{
	  if (dump_file)
	    fprintf (dump_file, "  will not early inline: %s/%i->%s/%i, "
		     "call is cold and code would grow by %i\n",
		     xstrdup_for_dump (e->caller->name ()),
		     e->caller->order,
		     xstrdup_for_dump (callee->name ()), callee->order,
		     growth);
	  want_inline = false;
	}
      else if (growth > PARAM_VALUE (PARAM_EARLY_INLINING_INSNS))
	{
	  if (dump_file)
	    fprintf (dump_file, "  will not early inline: %s/%i->%s/%i, "
		     "growth %i exceeds --param early-inlining-insns\n",
		     xstrdup_for_dump (e->caller->name ()),
		     e->caller->order,
		     xstrdup_for_dump (callee->name ()), callee->order,
		     growth);
	  want_inline = false;
	}
      else if ((n = num_calls (callee)) != 0
	       && growth * (n + 1) > PARAM_VALUE (PARAM_EARLY_INLINING_INSNS))
	{
	  if (dump_file)
	    fprintf (dump_file, "  will not early inline: %s/%i->%s/%i, "
		     "growth %i exceeds --param early-inlining-insns "
		     "divided by number of calls\n",
		     xstrdup_for_dump (e->caller->name ()),
		     e->caller->order,
		     xstrdup_for_dump (callee->name ()), callee->order,
		     growth);
	  want_inline = false;
	}
    }
  return want_inline;
}

/* Compute time of the edge->caller + edge->callee execution when inlining
   does not happen.  */

inline sreal
compute_uninlined_call_time (struct inline_summary *callee_info,
			     struct cgraph_edge *edge)
{
  sreal uninlined_call_time = (sreal)callee_info->time;
  cgraph_node *caller = (edge->caller->global.inlined_to 
			 ? edge->caller->global.inlined_to
			 : edge->caller);

  if (edge->count && caller->count)
    uninlined_call_time *= (sreal)edge->count / caller->count;
  if (edge->frequency)
    uninlined_call_time *= cgraph_freq_base_rec * edge->frequency;
  else
    uninlined_call_time = uninlined_call_time >> 11;

  int caller_time = inline_summaries->get (caller)->time;
  return uninlined_call_time + caller_time;
}

/* Same as compute_uinlined_call_time but compute time when inlining
   does happen.  */

inline sreal
compute_inlined_call_time (struct cgraph_edge *edge,
			   int edge_time)
{
  cgraph_node *caller = (edge->caller->global.inlined_to 
			 ? edge->caller->global.inlined_to
			 : edge->caller);
  int caller_time = inline_summaries->get (caller)->time;
  sreal time = edge_time;

  if (edge->count && caller->count)
    time *= (sreal)edge->count / caller->count;
  if (edge->frequency)
    time *= cgraph_freq_base_rec * edge->frequency;
  else
    time = time >> 11;

  /* This calculation should match one in ipa-inline-analysis.
     FIXME: Once ipa-inline-analysis is converted to sreal this can be
     simplified.  */
  time -= (sreal) ((gcov_type) edge->frequency
		   * inline_edge_summary (edge)->call_stmt_time
	           * (INLINE_TIME_SCALE / CGRAPH_FREQ_BASE)) / INLINE_TIME_SCALE;
  time += caller_time;
  if (time <= 0)
    time = ((sreal) 1) >> 8;
  gcc_checking_assert (time >= 0);
  return time;
}

/* Return true if the speedup for inlining E is bigger than
   PARAM_MAX_INLINE_MIN_SPEEDUP.  */

static bool
big_speedup_p (struct cgraph_edge *e)
{
  sreal time = compute_uninlined_call_time (inline_summaries->get (e->callee),
					    e);
  sreal inlined_time = compute_inlined_call_time (e, estimate_edge_time (e));

  if (time - inlined_time
      > (sreal) time * PARAM_VALUE (PARAM_INLINE_MIN_SPEEDUP)
	 * percent_rec)
    return true;
  return false;
}

/* Return true if we are interested in inlining small function.
   When REPORT is true, report reason to dump file.  */

static bool
want_inline_small_function_p (struct cgraph_edge *e, bool report)
{
  bool want_inline = true;
  struct cgraph_node *callee = e->callee->ultimate_alias_target ();

  if (DECL_DISREGARD_INLINE_LIMITS (callee->decl))
    ;
  else if (!DECL_DECLARED_INLINE_P (callee->decl)
	   && !opt_for_fn (e->caller->decl, flag_inline_small_functions))
    {
      e->inline_failed = CIF_FUNCTION_NOT_INLINE_CANDIDATE;
      want_inline = false;
    }
  /* Do fast and conservative check if the function can be good
     inline candidate.  At the moment we allow inline hints to
     promote non-inline functions to inline and we increase
     MAX_INLINE_INSNS_SINGLE 16-fold for inline functions.  */
  else if ((!DECL_DECLARED_INLINE_P (callee->decl)
	   && (!e->count || !e->maybe_hot_p ()))
	   && inline_summaries->get (callee)->min_size
		- inline_edge_summary (e)->call_stmt_size
	      > MAX (MAX_INLINE_INSNS_SINGLE, MAX_INLINE_INSNS_AUTO))
    {
      e->inline_failed = CIF_MAX_INLINE_INSNS_AUTO_LIMIT;
      want_inline = false;
    }
  else if ((DECL_DECLARED_INLINE_P (callee->decl) || e->count)
	   && inline_summaries->get (callee)->min_size
		- inline_edge_summary (e)->call_stmt_size
	      > 16 * MAX_INLINE_INSNS_SINGLE)
    {
      e->inline_failed = (DECL_DECLARED_INLINE_P (callee->decl)
			  ? CIF_MAX_INLINE_INSNS_SINGLE_LIMIT
			  : CIF_MAX_INLINE_INSNS_AUTO_LIMIT);
      want_inline = false;
    }
  else
    {
      int growth = estimate_edge_growth (e);
      inline_hints hints = estimate_edge_hints (e);
      bool big_speedup = big_speedup_p (e);

      if (growth <= 0)
	;
      /* Apply MAX_INLINE_INSNS_SINGLE limit.  Do not do so when
	 hints suggests that inlining given function is very profitable.  */
      else if (DECL_DECLARED_INLINE_P (callee->decl)
	       && growth >= MAX_INLINE_INSNS_SINGLE
	       && ((!big_speedup
		    && !(hints & (INLINE_HINT_indirect_call
				  | INLINE_HINT_known_hot
				  | INLINE_HINT_loop_iterations
				  | INLINE_HINT_array_index
				  | INLINE_HINT_loop_stride)))
		   || growth >= MAX_INLINE_INSNS_SINGLE * 16))
	{
          e->inline_failed = CIF_MAX_INLINE_INSNS_SINGLE_LIMIT;
	  want_inline = false;
	}
      else if (!DECL_DECLARED_INLINE_P (callee->decl)
	       && !opt_for_fn (e->caller->decl, flag_inline_functions))
	{
	  /* growth_likely_positive is expensive, always test it last.  */
          if (growth >= MAX_INLINE_INSNS_SINGLE
	      || growth_likely_positive (callee, growth))
	    {
              e->inline_failed = CIF_NOT_DECLARED_INLINED;
	      want_inline = false;
 	    }
	}
      /* Apply MAX_INLINE_INSNS_AUTO limit for functions not declared inline
	 Upgrade it to MAX_INLINE_INSNS_SINGLE when hints suggests that
	 inlining given function is very profitable.  */
      else if (!DECL_DECLARED_INLINE_P (callee->decl)
	       && !big_speedup
	       && !(hints & INLINE_HINT_known_hot)
	       && growth >= ((hints & (INLINE_HINT_indirect_call
				       | INLINE_HINT_loop_iterations
			               | INLINE_HINT_array_index
				       | INLINE_HINT_loop_stride))
			     ? MAX (MAX_INLINE_INSNS_AUTO,
				    MAX_INLINE_INSNS_SINGLE)
			     : MAX_INLINE_INSNS_AUTO))
	{
	  /* growth_likely_positive is expensive, always test it last.  */
          if (growth >= MAX_INLINE_INSNS_SINGLE
	      || growth_likely_positive (callee, growth))
	    {
	      e->inline_failed = CIF_MAX_INLINE_INSNS_AUTO_LIMIT;
	      want_inline = false;
 	    }
	}
      /* If call is cold, do not inline when function body would grow. */
      else if (!e->maybe_hot_p ()
	       && (growth >= MAX_INLINE_INSNS_SINGLE
		   || growth_likely_positive (callee, growth)))
	{
          e->inline_failed = CIF_UNLIKELY_CALL;
	  want_inline = false;
	}
    }
  if (!want_inline && report)
    report_inline_failed_reason (e);
  return want_inline;
}

/* EDGE is self recursive edge.
   We hand two cases - when function A is inlining into itself
   or when function A is being inlined into another inliner copy of function
   A within function B.  

   In first case OUTER_NODE points to the toplevel copy of A, while
   in the second case OUTER_NODE points to the outermost copy of A in B.

   In both cases we want to be extra selective since
   inlining the call will just introduce new recursive calls to appear.  */

static bool
want_inline_self_recursive_call_p (struct cgraph_edge *edge,
				   struct cgraph_node *outer_node,
				   bool peeling,
				   int depth)
{
  char const *reason = NULL;
  bool want_inline = true;
  int caller_freq = CGRAPH_FREQ_BASE;
  int max_depth = PARAM_VALUE (PARAM_MAX_INLINE_RECURSIVE_DEPTH_AUTO);

  if (DECL_DECLARED_INLINE_P (edge->caller->decl))
    max_depth = PARAM_VALUE (PARAM_MAX_INLINE_RECURSIVE_DEPTH);

  if (!edge->maybe_hot_p ())
    {
      reason = "recursive call is cold";
      want_inline = false;
    }
  else if (max_count && !outer_node->count)
    {
      reason = "not executed in profile";
      want_inline = false;
    }
  else if (depth > max_depth)
    {
      reason = "--param max-inline-recursive-depth exceeded.";
      want_inline = false;
    }

  if (outer_node->global.inlined_to)
    caller_freq = outer_node->callers->frequency;

  if (!caller_freq)
    {
      reason = "function is inlined and unlikely";
      want_inline = false;
    }

  if (!want_inline)
    ;
  /* Inlining of self recursive function into copy of itself within other function
     is transformation similar to loop peeling.

     Peeling is profitable if we can inline enough copies to make probability
     of actual call to the self recursive function very small.  Be sure that
     the probability of recursion is small.

     We ensure that the frequency of recursing is at most 1 - (1/max_depth).
     This way the expected number of recision is at most max_depth.  */
  else if (peeling)
    {
      int max_prob = CGRAPH_FREQ_BASE - ((CGRAPH_FREQ_BASE + max_depth - 1)
					 / max_depth);
      int i;
      for (i = 1; i < depth; i++)
	max_prob = max_prob * max_prob / CGRAPH_FREQ_BASE;
      if (max_count
	  && (edge->count * CGRAPH_FREQ_BASE / outer_node->count
	      >= max_prob))
	{
	  reason = "profile of recursive call is too large";
	  want_inline = false;
	}
      if (!max_count
	  && (edge->frequency * CGRAPH_FREQ_BASE / caller_freq
	      >= max_prob))
	{
	  reason = "frequency of recursive call is too large";
	  want_inline = false;
	}
    }
  /* Recursive inlining, i.e. equivalent of unrolling, is profitable if recursion
     depth is large.  We reduce function call overhead and increase chances that
     things fit in hardware return predictor.

     Recursive inlining might however increase cost of stack frame setup
     actually slowing down functions whose recursion tree is wide rather than
     deep.

     Deciding reliably on when to do recursive inlining without profile feedback
     is tricky.  For now we disable recursive inlining when probability of self
     recursion is low. 

     Recursive inlining of self recursive call within loop also results in large loop
     depths that generally optimize badly.  We may want to throttle down inlining
     in those cases.  In particular this seems to happen in one of libstdc++ rb tree
     methods.  */
  else
    {
      if (max_count
	  && (edge->count * 100 / outer_node->count
	      <= PARAM_VALUE (PARAM_MIN_INLINE_RECURSIVE_PROBABILITY)))
	{
	  reason = "profile of recursive call is too small";
	  want_inline = false;
	}
      else if (!max_count
	       && (edge->frequency * 100 / caller_freq
	           <= PARAM_VALUE (PARAM_MIN_INLINE_RECURSIVE_PROBABILITY)))
	{
	  reason = "frequency of recursive call is too small";
	  want_inline = false;
	}
    }
  if (!want_inline && dump_file)
    fprintf (dump_file, "   not inlining recursively: %s\n", reason);
  return want_inline;
}

/* Return true when NODE has uninlinable caller;
   set HAS_HOT_CALL if it has hot call. 
   Worker for cgraph_for_node_and_aliases.  */

static bool
check_callers (struct cgraph_node *node, void *has_hot_call)
{
  struct cgraph_edge *e;
   for (e = node->callers; e; e = e->next_caller)
     {
       if (!opt_for_fn (e->caller->decl, flag_inline_functions_called_once))
	 return true;
       if (!can_inline_edge_p (e, true))
         return true;
       if (e->recursive_p ())
	 return true;
       if (!(*(bool *)has_hot_call) && e->maybe_hot_p ())
	 *(bool *)has_hot_call = true;
     }
  return false;
}

/* If NODE has a caller, return true.  */

static bool
has_caller_p (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (node->callers)
    return true;
  return false;
}

/* Decide if inlining NODE would reduce unit size by eliminating
   the offline copy of function.  
   When COLD is true the cold calls are considered, too.  */

static bool
want_inline_function_to_all_callers_p (struct cgraph_node *node, bool cold)
{
  bool has_hot_call = false;

  /* Aliases gets inlined along with the function they alias.  */
  if (node->alias)
    return false;
  /* Already inlined?  */
  if (node->global.inlined_to)
    return false;
  /* Does it have callers?  */
  if (!node->call_for_symbol_and_aliases (has_caller_p, NULL, true))
    return false;
  /* Inlining into all callers would increase size?  */
  if (estimate_growth (node) > 0)
    return false;
  /* All inlines must be possible.  */
  if (node->call_for_symbol_and_aliases (check_callers, &has_hot_call,
					 true))
    return false;
  if (!cold && !has_hot_call)
    return false;
  return true;
}

/* A cost model driving the inlining heuristics in a way so the edges with
   smallest badness are inlined first.  After each inlining is performed
   the costs of all caller edges of nodes affected are recomputed so the
   metrics may accurately depend on values such as number of inlinable callers
   of the function or function body size.  */

static sreal
edge_badness (struct cgraph_edge *edge, bool dump)
{
  sreal badness;
  int growth, edge_time;
  struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
  struct inline_summary *callee_info = inline_summaries->get (callee);
  inline_hints hints;
  cgraph_node *caller = (edge->caller->global.inlined_to 
			 ? edge->caller->global.inlined_to
			 : edge->caller);

  growth = estimate_edge_growth (edge);
  edge_time = estimate_edge_time (edge);
  hints = estimate_edge_hints (edge);
  gcc_checking_assert (edge_time >= 0);
  gcc_checking_assert (edge_time <= callee_info->time);
  gcc_checking_assert (growth <= callee_info->size);

  if (dump)
    {
      fprintf (dump_file, "    Badness calculation for %s/%i -> %s/%i\n",
	       xstrdup_for_dump (edge->caller->name ()),
	       edge->caller->order,
	       xstrdup_for_dump (callee->name ()),
	       edge->callee->order);
      fprintf (dump_file, "      size growth %i, time %i ",
	       growth,
	       edge_time);
      dump_inline_hints (dump_file, hints);
      if (big_speedup_p (edge))
	fprintf (dump_file, " big_speedup");
      fprintf (dump_file, "\n");
    }

  /* Always prefer inlining saving code size.  */
  if (growth <= 0)
    {
      badness = (sreal) (-SREAL_MIN_SIG + growth) << (SREAL_MAX_EXP / 256);
      if (dump)
	fprintf (dump_file, "      %f: Growth %d <= 0\n", badness.to_double (),
		 growth);
    }
   /* Inlining into EXTERNAL functions is not going to change anything unless
      they are themselves inlined.  */
   else if (DECL_EXTERNAL (caller->decl))
    {
      if (dump)
	fprintf (dump_file, "      max: function is external\n");
      return sreal::max ();
    }
  /* When profile is available. Compute badness as:
     
                 time_saved * caller_count
     goodness =  -------------------------------------------------
	         growth_of_caller * overall_growth * combined_size

     badness = - goodness

     Again use negative value to make calls with profile appear hotter
     then calls without.
  */
  else if (opt_for_fn (caller->decl, flag_guess_branch_prob) || caller->count)
    {
      sreal numerator, denominator;
      int overall_growth;

      numerator = (compute_uninlined_call_time (callee_info, edge)
		   - compute_inlined_call_time (edge, edge_time));
      if (numerator == 0)
	numerator = ((sreal) 1 >> 8);
      if (caller->count)
	numerator *= caller->count;
      else if (opt_for_fn (caller->decl, flag_branch_probabilities))
	numerator = numerator >> 11;
      denominator = growth;

      overall_growth = callee_info->growth;

      /* Look for inliner wrappers of the form:

	 inline_caller ()
	   {
	     do_fast_job...
	     if (need_more_work)
	       noninline_callee ();
	   }
	 Withhout panilizing this case, we usually inline noninline_callee
	 into the inline_caller because overall_growth is small preventing
	 further inlining of inline_caller.

	 Penalize only callgraph edges to functions with small overall
	 growth ...
	*/
      if (growth > overall_growth
	  /* ... and having only one caller which is not inlined ... */
	  && callee_info->single_caller
	  && !edge->caller->global.inlined_to
	  /* ... and edges executed only conditionally ... */
	  && edge->frequency < CGRAPH_FREQ_BASE
	  /* ... consider case where callee is not inline but caller is ... */
	  && ((!DECL_DECLARED_INLINE_P (edge->callee->decl)
	       && DECL_DECLARED_INLINE_P (caller->decl))
	      /* ... or when early optimizers decided to split and edge
		 frequency still indicates splitting is a win ... */
	      || (callee->split_part && !caller->split_part
		  && edge->frequency
		     < CGRAPH_FREQ_BASE
		       * PARAM_VALUE
			  (PARAM_PARTIAL_INLINING_ENTRY_PROBABILITY) / 100
		  /* ... and do not overwrite user specified hints.   */
		  && (!DECL_DECLARED_INLINE_P (edge->callee->decl)
		      || DECL_DECLARED_INLINE_P (caller->decl)))))
	{
	  struct inline_summary *caller_info = inline_summaries->get (caller);
	  int caller_growth = caller_info->growth;

	  /* Only apply the penalty when caller looks like inline candidate,
	     and it is not called once and.  */
	  if (!caller_info->single_caller && overall_growth < caller_growth
	      && caller_info->inlinable
	      && caller_info->size
		 < (DECL_DECLARED_INLINE_P (caller->decl)
		    ? MAX_INLINE_INSNS_SINGLE : MAX_INLINE_INSNS_AUTO))
	    {
	      if (dump)
		fprintf (dump_file,
			 "     Wrapper penalty. Increasing growth %i to %i\n",
			 overall_growth, caller_growth);
	      overall_growth = caller_growth;
	    }
	}
      if (overall_growth > 0)
        {
	  /* Strongly preffer functions with few callers that can be inlined
	     fully.  The square root here leads to smaller binaries at average.
	     Watch however for extreme cases and return to linear function
	     when growth is large.  */
	  if (overall_growth < 256)
	    overall_growth *= overall_growth;
	  else
	    overall_growth += 256 * 256 - 256;
	  denominator *= overall_growth;
        }
      denominator *= inline_summaries->get (caller)->self_size + growth;

      badness = - numerator / denominator;

      if (dump)
	{
	  fprintf (dump_file,
		   "      %f: guessed profile. frequency %f, count %" PRId64
		   " caller count %" PRId64
		   " time w/o inlining %f, time w/ inlining %f"
		   " overall growth %i (current) %i (original)"
		   " %i (compensated)\n",
		   badness.to_double (),
		  (double)edge->frequency / CGRAPH_FREQ_BASE,
		   edge->count, caller->count,
		   compute_uninlined_call_time (callee_info, edge).to_double (),
		   compute_inlined_call_time (edge, edge_time).to_double (),
		   estimate_growth (callee),
		   callee_info->growth, overall_growth);
	}
    }
  /* When function local profile is not available or it does not give
     useful information (ie frequency is zero), base the cost on
     loop nest and overall size growth, so we optimize for overall number
     of functions fully inlined in program.  */
  else
    {
      int nest = MIN (inline_edge_summary (edge)->loop_depth, 8);
      badness = growth;

      /* Decrease badness if call is nested.  */
      if (badness > 0)
	badness = badness >> nest;
      else
	badness = badness << nest;
      if (dump)
	fprintf (dump_file, "      %f: no profile. nest %i\n",
		 badness.to_double (), nest);
    }
  gcc_checking_assert (badness != 0);

  if (edge->recursive_p ())
    badness = badness.shift (badness > 0 ? 4 : -4);
  if ((hints & (INLINE_HINT_indirect_call
		| INLINE_HINT_loop_iterations
		| INLINE_HINT_array_index
		| INLINE_HINT_loop_stride))
      || callee_info->growth <= 0)
    badness = badness.shift (badness > 0 ? -2 : 2);
  if (hints & (INLINE_HINT_same_scc))
    badness = badness.shift (badness > 0 ? 3 : -3);
  else if (hints & (INLINE_HINT_in_scc))
    badness = badness.shift (badness > 0 ? 2 : -2);
  else if (hints & (INLINE_HINT_cross_module))
    badness = badness.shift (badness > 0 ? 1 : -1);
  if (DECL_DISREGARD_INLINE_LIMITS (callee->decl))
    badness = badness.shift (badness > 0 ? -4 : 4);
  else if ((hints & INLINE_HINT_declared_inline))
    badness = badness.shift (badness > 0 ? -3 : 3);
  if (dump)
    fprintf (dump_file, "      Adjusted by hints %f\n", badness.to_double ());
  return badness;
}

/* Recompute badness of EDGE and update its key in HEAP if needed.  */
static inline void
update_edge_key (edge_heap_t *heap, struct cgraph_edge *edge)
{
  sreal badness = edge_badness (edge, false);
  if (edge->aux)
    {
      edge_heap_node_t *n = (edge_heap_node_t *) edge->aux;
      gcc_checking_assert (n->get_data () == edge);

      /* fibonacci_heap::replace_key does busy updating of the
	 heap that is unnecesarily expensive.
	 We do lazy increases: after extracting minimum if the key
	 turns out to be out of date, it is re-inserted into heap
	 with correct value.  */
      if (badness < n->get_key ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "  decreasing badness %s/%i -> %s/%i, %f"
		       " to %f\n",
		       xstrdup_for_dump (edge->caller->name ()),
		       edge->caller->order,
		       xstrdup_for_dump (edge->callee->name ()),
		       edge->callee->order,
		       n->get_key ().to_double (),
		       badness.to_double ());
	    }
	  heap->decrease_key (n, badness);
	}
    }
  else
    {
       if (dump_file && (dump_flags & TDF_DETAILS))
	 {
	   fprintf (dump_file,
		    "  enqueuing call %s/%i -> %s/%i, badness %f\n",
		    xstrdup_for_dump (edge->caller->name ()),
		    edge->caller->order,
		    xstrdup_for_dump (edge->callee->name ()),
		    edge->callee->order,
		    badness.to_double ());
	 }
      edge->aux = heap->insert (badness, edge);
    }
}


/* NODE was inlined.
   All caller edges needs to be resetted because
   size estimates change. Similarly callees needs reset
   because better context may be known.  */

static void
reset_edge_caches (struct cgraph_node *node)
{
  struct cgraph_edge *edge;
  struct cgraph_edge *e = node->callees;
  struct cgraph_node *where = node;
  struct ipa_ref *ref;

  if (where->global.inlined_to)
    where = where->global.inlined_to;

  for (edge = where->callers; edge; edge = edge->next_caller)
    if (edge->inline_failed)
      reset_edge_growth_cache (edge);

  FOR_EACH_ALIAS (where, ref)
    reset_edge_caches (dyn_cast <cgraph_node *> (ref->referring));

  if (!e)
    return;

  while (true)
    if (!e->inline_failed && e->callee->callees)
      e = e->callee->callees;
    else
      {
	if (e->inline_failed)
	  reset_edge_growth_cache (e);
	if (e->next_callee)
	  e = e->next_callee;
	else
	  {
	    do
	      {
		if (e->caller == node)
		  return;
		e = e->caller->callers;
	      }
	    while (!e->next_callee);
	    e = e->next_callee;
	  }
      }
}

/* Recompute HEAP nodes for each of caller of NODE.
   UPDATED_NODES track nodes we already visited, to avoid redundant work.
   When CHECK_INLINABLITY_FOR is set, re-check for specified edge that
   it is inlinable. Otherwise check all edges.  */

static void
update_caller_keys (edge_heap_t *heap, struct cgraph_node *node,
		    bitmap updated_nodes,
		    struct cgraph_edge *check_inlinablity_for)
{
  struct cgraph_edge *edge;
  struct ipa_ref *ref;

  if ((!node->alias && !inline_summaries->get (node)->inlinable)
      || node->global.inlined_to)
    return;
  if (!bitmap_set_bit (updated_nodes, node->uid))
    return;

  FOR_EACH_ALIAS (node, ref)
    {
      struct cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      update_caller_keys (heap, alias, updated_nodes, check_inlinablity_for);
    }

  for (edge = node->callers; edge; edge = edge->next_caller)
    if (edge->inline_failed)
      {
        if (!check_inlinablity_for
	    || check_inlinablity_for == edge)
	  {
	    if (can_inline_edge_p (edge, false)
		&& want_inline_small_function_p (edge, false))
	      update_edge_key (heap, edge);
	    else if (edge->aux)
	      {
		report_inline_failed_reason (edge);
		heap->delete_node ((edge_heap_node_t *) edge->aux);
		edge->aux = NULL;
	      }
	  }
	else if (edge->aux)
	  update_edge_key (heap, edge);
      }
}

/* Recompute HEAP nodes for each uninlined call in NODE.
   This is used when we know that edge badnesses are going only to increase
   (we introduced new call site) and thus all we need is to insert newly
   created edges into heap.  */

static void
update_callee_keys (edge_heap_t *heap, struct cgraph_node *node,
		    bitmap updated_nodes)
{
  struct cgraph_edge *e = node->callees;

  if (!e)
    return;
  while (true)
    if (!e->inline_failed && e->callee->callees)
      e = e->callee->callees;
    else
      {
	enum availability avail;
	struct cgraph_node *callee;
	/* We do not reset callee growth cache here.  Since we added a new call,
	   growth chould have just increased and consequentely badness metric
           don't need updating.  */
	if (e->inline_failed
	    && (callee = e->callee->ultimate_alias_target (&avail, e->caller))
	    && inline_summaries->get (callee)->inlinable
	    && avail >= AVAIL_AVAILABLE
	    && !bitmap_bit_p (updated_nodes, callee->uid))
	  {
	    if (can_inline_edge_p (e, false)
		&& want_inline_small_function_p (e, false))
	      update_edge_key (heap, e);
	    else if (e->aux)
	      {
		report_inline_failed_reason (e);
		heap->delete_node ((edge_heap_node_t *) e->aux);
		e->aux = NULL;
	      }
	  }
	if (e->next_callee)
	  e = e->next_callee;
	else
	  {
	    do
	      {
		if (e->caller == node)
		  return;
		e = e->caller->callers;
	      }
	    while (!e->next_callee);
	    e = e->next_callee;
	  }
      }
}

/* Enqueue all recursive calls from NODE into priority queue depending on
   how likely we want to recursively inline the call.  */

static void
lookup_recursive_calls (struct cgraph_node *node, struct cgraph_node *where,
			edge_heap_t *heap)
{
  struct cgraph_edge *e;
  enum availability avail;

  for (e = where->callees; e; e = e->next_callee)
    if (e->callee == node
	|| (e->callee->ultimate_alias_target (&avail, e->caller) == node
	    && avail > AVAIL_INTERPOSABLE))
      {
	/* When profile feedback is available, prioritize by expected number
	   of calls.  */
        heap->insert (!max_count ? -e->frequency
		      : -(e->count / ((max_count + (1<<24) - 1) / (1<<24))),
		      e);
      }
  for (e = where->callees; e; e = e->next_callee)
    if (!e->inline_failed)
      lookup_recursive_calls (node, e->callee, heap);
}

/* Decide on recursive inlining: in the case function has recursive calls,
   inline until body size reaches given argument.  If any new indirect edges
   are discovered in the process, add them to *NEW_EDGES, unless NEW_EDGES
   is NULL.  */

static bool
recursive_inlining (struct cgraph_edge *edge,
		    vec<cgraph_edge *> *new_edges)
{
  int limit = PARAM_VALUE (PARAM_MAX_INLINE_INSNS_RECURSIVE_AUTO);
  edge_heap_t heap (sreal::min ());
  struct cgraph_node *node;
  struct cgraph_edge *e;
  struct cgraph_node *master_clone = NULL, *next;
  int depth = 0;
  int n = 0;

  node = edge->caller;
  if (node->global.inlined_to)
    node = node->global.inlined_to;

  if (DECL_DECLARED_INLINE_P (node->decl))
    limit = PARAM_VALUE (PARAM_MAX_INLINE_INSNS_RECURSIVE);

  /* Make sure that function is small enough to be considered for inlining.  */
  if (estimate_size_after_inlining (node, edge)  >= limit)
    return false;
  lookup_recursive_calls (node, node, &heap);
  if (heap.empty ())
    return false;

  if (dump_file)
    fprintf (dump_file,
	     "  Performing recursive inlining on %s\n",
	     node->name ());

  /* Do the inlining and update list of recursive call during process.  */
  while (!heap.empty ())
    {
      struct cgraph_edge *curr = heap.extract_min ();
      struct cgraph_node *cnode, *dest = curr->callee;

      if (!can_inline_edge_p (curr, true))
	continue;

      /* MASTER_CLONE is produced in the case we already started modified
	 the function. Be sure to redirect edge to the original body before
	 estimating growths otherwise we will be seeing growths after inlining
	 the already modified body.  */
      if (master_clone)
	{
	  curr->redirect_callee (master_clone);
	  reset_edge_growth_cache (curr);
	}

      if (estimate_size_after_inlining (node, curr) > limit)
	{
	  curr->redirect_callee (dest);
	  reset_edge_growth_cache (curr);
	  break;
	}

      depth = 1;
      for (cnode = curr->caller;
	   cnode->global.inlined_to; cnode = cnode->callers->caller)
	if (node->decl
	    == curr->callee->ultimate_alias_target ()->decl)
          depth++;

      if (!want_inline_self_recursive_call_p (curr, node, false, depth))
	{
	  curr->redirect_callee (dest);
	  reset_edge_growth_cache (curr);
	  continue;
	}

      if (dump_file)
	{
	  fprintf (dump_file,
		   "   Inlining call of depth %i", depth);
	  if (node->count)
	    {
	      fprintf (dump_file, " called approx. %.2f times per call",
		       (double)curr->count / node->count);
	    }
	  fprintf (dump_file, "\n");
	}
      if (!master_clone)
	{
	  /* We need original clone to copy around.  */
	  master_clone = node->create_clone (node->decl, node->count,
	    CGRAPH_FREQ_BASE, false, vNULL,
	    true, NULL, NULL);
	  for (e = master_clone->callees; e; e = e->next_callee)
	    if (!e->inline_failed)
	      clone_inlined_nodes (e, true, false, NULL, CGRAPH_FREQ_BASE);
	  curr->redirect_callee (master_clone);
          reset_edge_growth_cache (curr);
	}

      inline_call (curr, false, new_edges, &overall_size, true);
      lookup_recursive_calls (node, curr->callee, &heap);
      n++;
    }

  if (!heap.empty () && dump_file)
    fprintf (dump_file, "    Recursive inlining growth limit met.\n");

  if (!master_clone)
    return false;

  if (dump_file)
    fprintf (dump_file,
	     "\n   Inlined %i times, "
	     "body grown from size %i to %i, time %i to %i\n", n,
	     inline_summaries->get (master_clone)->size, inline_summaries->get (node)->size,
	     inline_summaries->get (master_clone)->time, inline_summaries->get (node)->time);

  /* Remove master clone we used for inlining.  We rely that clones inlined
     into master clone gets queued just before master clone so we don't
     need recursion.  */
  for (node = symtab->first_function (); node != master_clone;
       node = next)
    {
      next = symtab->next_function (node);
      if (node->global.inlined_to == master_clone)
	node->remove ();
    }
  master_clone->remove ();
  return true;
}


/* Given whole compilation unit estimate of INSNS, compute how large we can
   allow the unit to grow.  */

static int
compute_max_insns (int insns)
{
  int max_insns = insns;
  if (max_insns < PARAM_VALUE (PARAM_LARGE_UNIT_INSNS))
    max_insns = PARAM_VALUE (PARAM_LARGE_UNIT_INSNS);

  return ((int64_t) max_insns
	  * (100 + PARAM_VALUE (PARAM_INLINE_UNIT_GROWTH)) / 100);
}


/* Compute badness of all edges in NEW_EDGES and add them to the HEAP.  */

static void
add_new_edges_to_heap (edge_heap_t *heap, vec<cgraph_edge *> new_edges)
{
  while (new_edges.length () > 0)
    {
      struct cgraph_edge *edge = new_edges.pop ();

      gcc_assert (!edge->aux);
      if (edge->inline_failed
	  && can_inline_edge_p (edge, true)
	  && want_inline_small_function_p (edge, true))
        edge->aux = heap->insert (edge_badness (edge, false), edge);
    }
}

/* Remove EDGE from the fibheap.  */

static void
heap_edge_removal_hook (struct cgraph_edge *e, void *data)
{
  if (e->aux)
    {
      ((edge_heap_t *)data)->delete_node ((edge_heap_node_t *)e->aux);
      e->aux = NULL;
    }
}

/* Return true if speculation of edge E seems useful.
   If ANTICIPATE_INLINING is true, be conservative and hope that E
   may get inlined.  */

bool
speculation_useful_p (struct cgraph_edge *e, bool anticipate_inlining)
{
  enum availability avail;
  struct cgraph_node *target = e->callee->ultimate_alias_target (&avail,
								 e->caller);
  struct cgraph_edge *direct, *indirect;
  struct ipa_ref *ref;

  gcc_assert (e->speculative && !e->indirect_unknown_callee);

  if (!e->maybe_hot_p ())
    return false;

  /* See if IP optimizations found something potentially useful about the
     function.  For now we look only for CONST/PURE flags.  Almost everything
     else we propagate is useless.  */
  if (avail >= AVAIL_AVAILABLE)
    {
      int ecf_flags = flags_from_decl_or_type (target->decl);
      if (ecf_flags & ECF_CONST)
        {
	  e->speculative_call_info (direct, indirect, ref);
	  if (!(indirect->indirect_info->ecf_flags & ECF_CONST))
	    return true;
        }
      else if (ecf_flags & ECF_PURE)
        {
	  e->speculative_call_info (direct, indirect, ref);
	  if (!(indirect->indirect_info->ecf_flags & ECF_PURE))
	    return true;
        }
    }
  /* If we did not managed to inline the function nor redirect
     to an ipa-cp clone (that are seen by having local flag set),
     it is probably pointless to inline it unless hardware is missing
     indirect call predictor.  */
  if (!anticipate_inlining && e->inline_failed && !target->local.local)
    return false;
  /* For overwritable targets there is not much to do.  */
  if (e->inline_failed && !can_inline_edge_p (e, false, true))
    return false;
  /* OK, speculation seems interesting.  */
  return true;
}

/* We know that EDGE is not going to be inlined.
   See if we can remove speculation.  */

static void
resolve_noninline_speculation (edge_heap_t *edge_heap, struct cgraph_edge *edge)
{
  if (edge->speculative && !speculation_useful_p (edge, false))
    {
      struct cgraph_node *node = edge->caller;
      struct cgraph_node *where = node->global.inlined_to
				  ? node->global.inlined_to : node;
      bitmap updated_nodes = BITMAP_ALLOC (NULL);

      spec_rem += edge->count;
      edge->resolve_speculation ();
      reset_edge_caches (where);
      inline_update_overall_summary (where);
      update_caller_keys (edge_heap, where,
			  updated_nodes, NULL);
      update_callee_keys (edge_heap, where,
			  updated_nodes);
      BITMAP_FREE (updated_nodes);
    }
}

/* Return true if NODE should be accounted for overall size estimate.
   Skip all nodes optimized for size so we can measure the growth of hot
   part of program no matter of the padding.  */

bool
inline_account_function_p (struct cgraph_node *node)
{
   return (!DECL_EXTERNAL (node->decl)
	   && !opt_for_fn (node->decl, optimize_size)
	   && node->frequency != NODE_FREQUENCY_UNLIKELY_EXECUTED);
}

/* Count number of callers of NODE and store it into DATA (that
   points to int.  Worker for cgraph_for_node_and_aliases.  */

static bool
sum_callers (struct cgraph_node *node, void *data)
{
  struct cgraph_edge *e;
  int *num_calls = (int *)data;

  for (e = node->callers; e; e = e->next_caller)
    (*num_calls)++;
  return false;
}

/* We use greedy algorithm for inlining of small functions:
   All inline candidates are put into prioritized heap ordered in
   increasing badness.

   The inlining of small functions is bounded by unit growth parameters.  */

static void
inline_small_functions (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;
  edge_heap_t edge_heap (sreal::min ());
  bitmap updated_nodes = BITMAP_ALLOC (NULL);
  int min_size, max_size;
  auto_vec<cgraph_edge *> new_indirect_edges;
  int initial_size = 0;
  struct cgraph_node **order = XCNEWVEC (cgraph_node *, symtab->cgraph_count);
  struct cgraph_edge_hook_list *edge_removal_hook_holder;
  new_indirect_edges.create (8);

  edge_removal_hook_holder
    = symtab->add_edge_removal_hook (&heap_edge_removal_hook, &edge_heap);

  /* Compute overall unit size and other global parameters used by badness
     metrics.  */

  max_count = 0;
  ipa_reduced_postorder (order, true, true, NULL);
  free (order);

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->global.inlined_to)
      {
	if (!node->alias && node->analyzed
	    && (node->has_gimple_body_p () || node->thunk.thunk_p))
	  {
	    struct inline_summary *info = inline_summaries->get (node);
	    struct ipa_dfs_info *dfs = (struct ipa_dfs_info *) node->aux;

	    /* Do not account external functions, they will be optimized out
	       if not inlined.  Also only count the non-cold portion of program.  */
	    if (inline_account_function_p (node))
	      initial_size += info->size;
	    info->growth = estimate_growth (node);

	    int num_calls = 0;
	    node->call_for_symbol_and_aliases (sum_callers, &num_calls,
					       true);
	    if (num_calls == 1)
	      info->single_caller = true;
	    if (dfs && dfs->next_cycle)
	      {
		struct cgraph_node *n2;
		int id = dfs->scc_no + 1;
		for (n2 = node; n2;
		     n2 = ((struct ipa_dfs_info *) node->aux)->next_cycle)
		  {
		    struct inline_summary *info2 = inline_summaries->get (n2);
		    if (info2->scc_no)
		      break;
		    info2->scc_no = id;
		  }
	      }
	  }

	for (edge = node->callers; edge; edge = edge->next_caller)
	  if (max_count < edge->count)
	    max_count = edge->count;
      }
  ipa_free_postorder_info ();
  initialize_growth_caches ();

  if (dump_file)
    fprintf (dump_file,
	     "\nDeciding on inlining of small functions.  Starting with size %i.\n",
	     initial_size);

  overall_size = initial_size;
  max_size = compute_max_insns (overall_size);
  min_size = overall_size;

  /* Populate the heap with all edges we might inline.  */

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      bool update = false;
      struct cgraph_edge *next = NULL;
      bool has_speculative = false;

      if (dump_file)
	fprintf (dump_file, "Enqueueing calls in %s/%i.\n",
		 node->name (), node->order);

      for (edge = node->callees; edge; edge = next)
	{
	  next = edge->next_callee;
	  if (edge->inline_failed
	      && !edge->aux
	      && can_inline_edge_p (edge, true)
	      && want_inline_small_function_p (edge, true)
	      && edge->inline_failed)
	    {
	      gcc_assert (!edge->aux);
	      update_edge_key (&edge_heap, edge);
	    }
	  if (edge->speculative)
	    has_speculative = true;
	}
      if (has_speculative)
	for (edge = node->callees; edge; edge = next)
	  if (edge->speculative && !speculation_useful_p (edge,
							  edge->aux != NULL))
	    {
	      edge->resolve_speculation ();
	      update = true;
	    }
      if (update)
	{
	  struct cgraph_node *where = node->global.inlined_to
				      ? node->global.inlined_to : node;
	  inline_update_overall_summary (where);
	  reset_edge_caches (where);
          update_caller_keys (&edge_heap, where,
			      updated_nodes, NULL);
          update_callee_keys (&edge_heap, where,
			      updated_nodes);
          bitmap_clear (updated_nodes);
	}
    }

  gcc_assert (in_lto_p
	      || !max_count
	      || (profile_info && flag_branch_probabilities));

  while (!edge_heap.empty ())
    {
      int old_size = overall_size;
      struct cgraph_node *where, *callee;
      sreal badness = edge_heap.min_key ();
      sreal current_badness;
      int growth;

      edge = edge_heap.extract_min ();
      gcc_assert (edge->aux);
      edge->aux = NULL;
      if (!edge->inline_failed || !edge->callee->analyzed)
	continue;

#if CHECKING_P
      /* Be sure that caches are maintained consistent.  */
      sreal cached_badness = edge_badness (edge, false);
 
      int old_size_est = estimate_edge_size (edge);
      int old_time_est = estimate_edge_time (edge);
      int old_hints_est = estimate_edge_hints (edge);

      reset_edge_growth_cache (edge);
      gcc_assert (old_size_est == estimate_edge_size (edge));
      gcc_assert (old_time_est == estimate_edge_time (edge));
      /* FIXME:

         gcc_assert (old_hints_est == estimate_edge_hints (edge));

	 fails with profile feedback because some hints depends on
	 maybe_hot_edge_p predicate and because callee gets inlined to other
	 calls, the edge may become cold.
	 This ought to be fixed by computing relative probabilities
	 for given invocation but that will be better done once whole
	 code is converted to sreals.  Disable for now and revert to "wrong"
	 value so enable/disable checking paths agree.  */
      edge_growth_cache[edge->uid].hints = old_hints_est + 1;

      /* When updating the edge costs, we only decrease badness in the keys.
	 Increases of badness are handled lazilly; when we see key with out
	 of date value on it, we re-insert it now.  */
      current_badness = edge_badness (edge, false);
      /* Disable checking for profile because roundoff errors may cause slight
         deviations in the order.  */
      gcc_assert (max_count || cached_badness == current_badness);
      gcc_assert (current_badness >= badness);
#else
      current_badness = edge_badness (edge, false);
#endif
      if (current_badness != badness)
	{
	  if (edge_heap.min () && current_badness > edge_heap.min_key ())
	    {
	      edge->aux = edge_heap.insert (current_badness, edge);
	      continue;
	    }
	  else
	    badness = current_badness;
	}

      if (!can_inline_edge_p (edge, true))
	{
	  resolve_noninline_speculation (&edge_heap, edge);
	  continue;
	}
      
      callee = edge->callee->ultimate_alias_target ();
      growth = estimate_edge_growth (edge);
      if (dump_file)
	{
	  fprintf (dump_file,
		   "\nConsidering %s/%i with %i size\n",
		   callee->name (), callee->order,
		   inline_summaries->get (callee)->size);
	  fprintf (dump_file,
		   " to be inlined into %s/%i in %s:%i\n"
		   " Estimated badness is %f, frequency %.2f.\n",
		   edge->caller->name (), edge->caller->order,
		   edge->call_stmt
		   && (LOCATION_LOCUS (gimple_location ((const gimple *)
							edge->call_stmt))
		       > BUILTINS_LOCATION)
		   ? gimple_filename ((const gimple *) edge->call_stmt)
		   : "unknown",
		   edge->call_stmt
		   ? gimple_lineno ((const gimple *) edge->call_stmt)
		   : -1,
		   badness.to_double (),
		   edge->frequency / (double)CGRAPH_FREQ_BASE);
	  if (edge->count)
	    fprintf (dump_file," Called %" PRId64"x\n",
		     edge->count);
	  if (dump_flags & TDF_DETAILS)
	    edge_badness (edge, true);
	}

      if (overall_size + growth > max_size
	  && !DECL_DISREGARD_INLINE_LIMITS (callee->decl))
	{
	  edge->inline_failed = CIF_INLINE_UNIT_GROWTH_LIMIT;
	  report_inline_failed_reason (edge);
	  resolve_noninline_speculation (&edge_heap, edge);
	  continue;
	}

      if (!want_inline_small_function_p (edge, true))
	{
	  resolve_noninline_speculation (&edge_heap, edge);
	  continue;
	}

      /* Heuristics for inlining small functions work poorly for
	 recursive calls where we do effects similar to loop unrolling.
	 When inlining such edge seems profitable, leave decision on
	 specific inliner.  */
      if (edge->recursive_p ())
	{
	  where = edge->caller;
	  if (where->global.inlined_to)
	    where = where->global.inlined_to;
	  if (!recursive_inlining (edge,
				   opt_for_fn (edge->caller->decl,
					       flag_indirect_inlining)
				   ? &new_indirect_edges : NULL))
	    {
	      edge->inline_failed = CIF_RECURSIVE_INLINING;
	      resolve_noninline_speculation (&edge_heap, edge);
	      continue;
	    }
	  reset_edge_caches (where);
	  /* Recursive inliner inlines all recursive calls of the function
	     at once. Consequently we need to update all callee keys.  */
	  if (opt_for_fn (edge->caller->decl, flag_indirect_inlining))
	    add_new_edges_to_heap (&edge_heap, new_indirect_edges);
          update_callee_keys (&edge_heap, where, updated_nodes);
	  bitmap_clear (updated_nodes);
	}
      else
	{
	  struct cgraph_node *outer_node = NULL;
	  int depth = 0;

	  /* Consider the case where self recursive function A is inlined
	     into B.  This is desired optimization in some cases, since it
	     leads to effect similar of loop peeling and we might completely
	     optimize out the recursive call.  However we must be extra
	     selective.  */

	  where = edge->caller;
	  while (where->global.inlined_to)
	    {
	      if (where->decl == callee->decl)
		outer_node = where, depth++;
	      where = where->callers->caller;
	    }
	  if (outer_node
	      && !want_inline_self_recursive_call_p (edge, outer_node,
						     true, depth))
	    {
	      edge->inline_failed
		= (DECL_DISREGARD_INLINE_LIMITS (edge->callee->decl)
		   ? CIF_RECURSIVE_INLINING : CIF_UNSPECIFIED);
	      resolve_noninline_speculation (&edge_heap, edge);
	      continue;
	    }
	  else if (depth && dump_file)
	    fprintf (dump_file, " Peeling recursion with depth %i\n", depth);

	  gcc_checking_assert (!callee->global.inlined_to);
	  inline_call (edge, true, &new_indirect_edges, &overall_size, true);
	  add_new_edges_to_heap (&edge_heap, new_indirect_edges);

	  reset_edge_caches (edge->callee);

	  update_callee_keys (&edge_heap, where, updated_nodes);
	}
      where = edge->caller;
      if (where->global.inlined_to)
	where = where->global.inlined_to;

      /* Our profitability metric can depend on local properties
	 such as number of inlinable calls and size of the function body.
	 After inlining these properties might change for the function we
	 inlined into (since it's body size changed) and for the functions
	 called by function we inlined (since number of it inlinable callers
	 might change).  */
      update_caller_keys (&edge_heap, where, updated_nodes, NULL);
      /* Offline copy count has possibly changed, recompute if profile is
	 available.  */
      if (max_count)
        {
	  struct cgraph_node *n = cgraph_node::get (edge->callee->decl);
	  if (n != edge->callee && n->analyzed)
	    update_callee_keys (&edge_heap, n, updated_nodes);
        }
      bitmap_clear (updated_nodes);

      if (dump_file)
	{
	  fprintf (dump_file,
		   " Inlined into %s which now has time %i and size %i,"
		   "net change of %+i.\n",
		   edge->caller->name (),
		   inline_summaries->get (edge->caller)->time,
		   inline_summaries->get (edge->caller)->size,
		   overall_size - old_size);
	}
      if (min_size > overall_size)
	{
	  min_size = overall_size;
	  max_size = compute_max_insns (min_size);

	  if (dump_file)
	    fprintf (dump_file, "New minimal size reached: %i\n", min_size);
	}
    }

  free_growth_caches ();
  if (dump_file)
    fprintf (dump_file,
	     "Unit growth for small function inlining: %i->%i (%i%%)\n",
	     initial_size, overall_size,
	     initial_size ? overall_size * 100 / (initial_size) - 100: 0);
  BITMAP_FREE (updated_nodes);
  symtab->remove_edge_removal_hook (edge_removal_hook_holder);
}

/* Flatten NODE.  Performed both during early inlining and
   at IPA inlining time.  */

static void
flatten_function (struct cgraph_node *node, bool early)
{
  struct cgraph_edge *e;

  /* We shouldn't be called recursively when we are being processed.  */
  gcc_assert (node->aux == NULL);

  node->aux = (void *) node;

  for (e = node->callees; e; e = e->next_callee)
    {
      struct cgraph_node *orig_callee;
      struct cgraph_node *callee = e->callee->ultimate_alias_target ();

      /* We've hit cycle?  It is time to give up.  */
      if (callee->aux)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Not inlining %s into %s to avoid cycle.\n",
		     xstrdup_for_dump (callee->name ()),
		     xstrdup_for_dump (e->caller->name ()));
	  e->inline_failed = CIF_RECURSIVE_INLINING;
	  continue;
	}

      /* When the edge is already inlined, we just need to recurse into
	 it in order to fully flatten the leaves.  */
      if (!e->inline_failed)
	{
	  flatten_function (callee, early);
	  continue;
	}

      /* Flatten attribute needs to be processed during late inlining. For
	 extra code quality we however do flattening during early optimization,
	 too.  */
      if (!early
	  ? !can_inline_edge_p (e, true)
	  : !can_early_inline_edge_p (e))
	continue;

      if (e->recursive_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "Not inlining: recursive call.\n");
	  continue;
	}

      if (gimple_in_ssa_p (DECL_STRUCT_FUNCTION (node->decl))
	  != gimple_in_ssa_p (DECL_STRUCT_FUNCTION (callee->decl)))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not inlining: SSA form does not match.\n");
	  continue;
	}

      /* Inline the edge and flatten the inline clone.  Avoid
         recursing through the original node if the node was cloned.  */
      if (dump_file)
	fprintf (dump_file, " Inlining %s into %s.\n",
		 xstrdup_for_dump (callee->name ()),
		 xstrdup_for_dump (e->caller->name ()));
      orig_callee = callee;
      inline_call (e, true, NULL, NULL, false);
      if (e->callee != orig_callee)
	orig_callee->aux = (void *) node;
      flatten_function (e->callee, early);
      if (e->callee != orig_callee)
	orig_callee->aux = NULL;
    }

  node->aux = NULL;
  if (!node->global.inlined_to)
    inline_update_overall_summary (node);
}

/* Inline NODE to all callers.  Worker for cgraph_for_node_and_aliases.
   DATA points to number of calls originally found so we avoid infinite
   recursion.  */

static bool
inline_to_all_callers_1 (struct cgraph_node *node, void *data,
			 hash_set<cgraph_node *> *callers)
{
  int *num_calls = (int *)data;
  bool callee_removed = false;

  while (node->callers && !node->global.inlined_to)
    {
      struct cgraph_node *caller = node->callers->caller;

      if (!can_inline_edge_p (node->callers, true)
	  || node->callers->recursive_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "Uninlinable call found; giving up.\n");
	  *num_calls = 0;
	  return false;
	}

      if (dump_file)
	{
	  fprintf (dump_file,
		   "\nInlining %s size %i.\n",
		   node->name (),
		   inline_summaries->get (node)->size);
	  fprintf (dump_file,
		   " Called once from %s %i insns.\n",
		   node->callers->caller->name (),
		   inline_summaries->get (node->callers->caller)->size);
	}

      /* Remember which callers we inlined to, delaying updating the
	 overall summary.  */
      callers->add (node->callers->caller);
      inline_call (node->callers, true, NULL, NULL, false, &callee_removed);
      if (dump_file)
	fprintf (dump_file,
		 " Inlined into %s which now has %i size\n",
		 caller->name (),
		 inline_summaries->get (caller)->size);
      if (!(*num_calls)--)
	{
	  if (dump_file)
	    fprintf (dump_file, "New calls found; giving up.\n");
	  return callee_removed;
	}
      if (callee_removed)
	return true;
    }
  return false;
}

/* Wrapper around inline_to_all_callers_1 doing delayed overall summary
   update.  */

static bool
inline_to_all_callers (struct cgraph_node *node, void *data)
{
  hash_set<cgraph_node *> callers;
  bool res = inline_to_all_callers_1 (node, data, &callers);
  /* Perform the delayed update of the overall summary of all callers
     processed.  This avoids quadratic behavior in the cases where
     we have a lot of calls to the same function.  */
  for (hash_set<cgraph_node *>::iterator i = callers.begin ();
       i != callers.end (); ++i)
    inline_update_overall_summary (*i);
  return res;
}

/* Output overall time estimate.  */
static void
dump_overall_stats (void)
{
  int64_t sum_weighted = 0, sum = 0;
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->global.inlined_to
	&& !node->alias)
      {
	int time = inline_summaries->get (node)->time;
	sum += time;
	sum_weighted += time * node->count;
      }
  fprintf (dump_file, "Overall time estimate: "
	   "%" PRId64" weighted by profile: "
	   "%" PRId64"\n", sum, sum_weighted);
}

/* Output some useful stats about inlining.  */

static void
dump_inline_stats (void)
{
  int64_t inlined_cnt = 0, inlined_indir_cnt = 0;
  int64_t inlined_virt_cnt = 0, inlined_virt_indir_cnt = 0;
  int64_t noninlined_cnt = 0, noninlined_indir_cnt = 0;
  int64_t noninlined_virt_cnt = 0, noninlined_virt_indir_cnt = 0;
  int64_t  inlined_speculative = 0, inlined_speculative_ply = 0;
  int64_t indirect_poly_cnt = 0, indirect_cnt = 0;
  int64_t reason[CIF_N_REASONS][3];
  int i;
  struct cgraph_node *node;

  memset (reason, 0, sizeof (reason));
  FOR_EACH_DEFINED_FUNCTION (node)
  {
    struct cgraph_edge *e;
    for (e = node->callees; e; e = e->next_callee)
      {
	if (e->inline_failed)
	  {
	    reason[(int) e->inline_failed][0] += e->count;
	    reason[(int) e->inline_failed][1] += e->frequency;
	    reason[(int) e->inline_failed][2] ++;
	    if (DECL_VIRTUAL_P (e->callee->decl))
	      {
		if (e->indirect_inlining_edge)
		  noninlined_virt_indir_cnt += e->count;
		else
		  noninlined_virt_cnt += e->count;
	      }
	    else
	      {
		if (e->indirect_inlining_edge)
		  noninlined_indir_cnt += e->count;
		else
		  noninlined_cnt += e->count;
	      }
	  }
	else
	  {
	    if (e->speculative)
	      {
		if (DECL_VIRTUAL_P (e->callee->decl))
		  inlined_speculative_ply += e->count;
		else
		  inlined_speculative += e->count;
	      }
	    else if (DECL_VIRTUAL_P (e->callee->decl))
	      {
		if (e->indirect_inlining_edge)
		  inlined_virt_indir_cnt += e->count;
		else
		  inlined_virt_cnt += e->count;
	      }
	    else
	      {
		if (e->indirect_inlining_edge)
		  inlined_indir_cnt += e->count;
		else
		  inlined_cnt += e->count;
	      }
	  }
      }
    for (e = node->indirect_calls; e; e = e->next_callee)
      if (e->indirect_info->polymorphic)
	indirect_poly_cnt += e->count;
      else
	indirect_cnt += e->count;
  }
  if (max_count)
    {
      fprintf (dump_file,
	       "Inlined %" PRId64 " + speculative "
	       "%" PRId64 " + speculative polymorphic "
	       "%" PRId64 " + previously indirect "
	       "%" PRId64 " + virtual "
	       "%" PRId64 " + virtual and previously indirect "
	       "%" PRId64 "\n" "Not inlined "
	       "%" PRId64 " + previously indirect "
	       "%" PRId64 " + virtual "
	       "%" PRId64 " + virtual and previously indirect "
	       "%" PRId64 " + stil indirect "
	       "%" PRId64 " + still indirect polymorphic "
	       "%" PRId64 "\n", inlined_cnt,
	       inlined_speculative, inlined_speculative_ply,
	       inlined_indir_cnt, inlined_virt_cnt, inlined_virt_indir_cnt,
	       noninlined_cnt, noninlined_indir_cnt, noninlined_virt_cnt,
	       noninlined_virt_indir_cnt, indirect_cnt, indirect_poly_cnt);
      fprintf (dump_file,
	       "Removed speculations %" PRId64 "\n",
	       spec_rem);
    }
  dump_overall_stats ();
  fprintf (dump_file, "\nWhy inlining failed?\n");
  for (i = 0; i < CIF_N_REASONS; i++)
    if (reason[i][2])
      fprintf (dump_file, "%-50s: %8i calls, %8i freq, %" PRId64" count\n",
	       cgraph_inline_failed_string ((cgraph_inline_failed_t) i),
	       (int) reason[i][2], (int) reason[i][1], reason[i][0]);
}

/* Decide on the inlining.  We do so in the topological order to avoid
   expenses on updating data structures.  */

static unsigned int
ipa_inline (void)
{
  struct cgraph_node *node;
  int nnodes;
  struct cgraph_node **order;
  int i;
  int cold;
  bool remove_functions = false;

  if (!optimize)
    return 0;

  cgraph_freq_base_rec = (sreal) 1 / (sreal) CGRAPH_FREQ_BASE;
  percent_rec = (sreal) 1 / (sreal) 100;

  order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);

  if (in_lto_p && optimize)
    ipa_update_after_lto_read ();

  if (dump_file)
    dump_inline_summaries (dump_file);

  nnodes = ipa_reverse_postorder (order);

  FOR_EACH_FUNCTION (node)
    {
      node->aux = 0;

      /* Recompute the default reasons for inlining because they may have
	 changed during merging.  */
      if (in_lto_p)
	{
	  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
	    {
	      gcc_assert (e->inline_failed);
	      initialize_inline_failed (e);
	    }
	  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
	    initialize_inline_failed (e);
	}
    }

  if (dump_file)
    fprintf (dump_file, "\nFlattening functions:\n");

  /* In the first pass handle functions to be flattened.  Do this with
     a priority so none of our later choices will make this impossible.  */
  for (i = nnodes - 1; i >= 0; i--)
    {
      node = order[i];

      /* Handle nodes to be flattened.
	 Ideally when processing callees we stop inlining at the
	 entry of cycles, possibly cloning that entry point and
	 try to flatten itself turning it into a self-recursive
	 function.  */
      if (lookup_attribute ("flatten",
			    DECL_ATTRIBUTES (node->decl)) != NULL)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Flattening %s\n", node->name ());
	  flatten_function (node, false);
	}
    }
  if (dump_file)
    dump_overall_stats ();

  inline_small_functions ();

  gcc_assert (symtab->state == IPA_SSA);
  symtab->state = IPA_SSA_AFTER_INLINING;
  /* Do first after-inlining removal.  We want to remove all "stale" extern
     inline functions and virtual functions so we really know what is called
     once.  */
  symtab->remove_unreachable_nodes (dump_file);
  free (order);

  /* Inline functions with a property that after inlining into all callers the
     code size will shrink because the out-of-line copy is eliminated. 
     We do this regardless on the callee size as long as function growth limits
     are met.  */
  if (dump_file)
    fprintf (dump_file,
	     "\nDeciding on functions to be inlined into all callers and "
	     "removing useless speculations:\n");

  /* Inlining one function called once has good chance of preventing
     inlining other function into the same callee.  Ideally we should
     work in priority order, but probably inlining hot functions first
     is good cut without the extra pain of maintaining the queue.

     ??? this is not really fitting the bill perfectly: inlining function
     into callee often leads to better optimization of callee due to
     increased context for optimization.
     For example if main() function calls a function that outputs help
     and then function that does the main optmization, we should inline
     the second with priority even if both calls are cold by themselves.

     We probably want to implement new predicate replacing our use of
     maybe_hot_edge interpreted as maybe_hot_edge || callee is known
     to be hot.  */
  for (cold = 0; cold <= 1; cold ++)
    {
      FOR_EACH_DEFINED_FUNCTION (node)
	{
	  struct cgraph_edge *edge, *next;
	  bool update=false;

	  for (edge = node->callees; edge; edge = next)
	    {
	      next = edge->next_callee;
	      if (edge->speculative && !speculation_useful_p (edge, false))
		{
		  edge->resolve_speculation ();
		  spec_rem += edge->count;
		  update = true;
		  remove_functions = true;
		}
	    }
	  if (update)
	    {
	      struct cgraph_node *where = node->global.inlined_to
					  ? node->global.inlined_to : node;
	      reset_edge_caches (where);
	      inline_update_overall_summary (where);
	    }
	  if (want_inline_function_to_all_callers_p (node, cold))
	    {
	      int num_calls = 0;
	      node->call_for_symbol_and_aliases (sum_callers, &num_calls,
						 true);
	      while (node->call_for_symbol_and_aliases
		       (inline_to_all_callers, &num_calls, true))
		;
	      remove_functions = true;
	    }
	}
    }

  /* Free ipa-prop structures if they are no longer needed.  */
  if (optimize)
    ipa_free_all_structures_after_iinln ();

  if (dump_file)
    {
      fprintf (dump_file,
	       "\nInlined %i calls, eliminated %i functions\n\n",
	       ncalls_inlined, nfunctions_inlined);
      dump_inline_stats ();
    }

  if (dump_file)
    dump_inline_summaries (dump_file);
  /* In WPA we use inline summaries for partitioning process.  */
  if (!flag_wpa)
    inline_free_summary ();
  return remove_functions ? TODO_remove_functions : 0;
}

/* Inline always-inline function calls in NODE.  */

static bool
inline_always_inline_functions (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  bool inlined = false;

  for (e = node->callees; e; e = e->next_callee)
    {
      struct cgraph_node *callee = e->callee->ultimate_alias_target ();
      if (!DECL_DISREGARD_INLINE_LIMITS (callee->decl))
	continue;

      if (e->recursive_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "  Not inlining recursive call to %s.\n",
		     e->callee->name ());
	  e->inline_failed = CIF_RECURSIVE_INLINING;
	  continue;
	}

      if (!can_early_inline_edge_p (e))
	{
	  /* Set inlined to true if the callee is marked "always_inline" but
	     is not inlinable.  This will allow flagging an error later in
	     expand_call_inline in tree-inline.c.  */
	  if (lookup_attribute ("always_inline",
				 DECL_ATTRIBUTES (callee->decl)) != NULL)
	    inlined = true;
	  continue;
	}

      if (dump_file)
	fprintf (dump_file, "  Inlining %s into %s (always_inline).\n",
		 xstrdup_for_dump (e->callee->name ()),
		 xstrdup_for_dump (e->caller->name ()));
      inline_call (e, true, NULL, NULL, false);
      inlined = true;
    }
  if (inlined)
    inline_update_overall_summary (node);

  return inlined;
}

/* Decide on the inlining.  We do so in the topological order to avoid
   expenses on updating data structures.  */

static bool
early_inline_small_functions (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  bool inlined = false;

  for (e = node->callees; e; e = e->next_callee)
    {
      struct cgraph_node *callee = e->callee->ultimate_alias_target ();
      if (!inline_summaries->get (callee)->inlinable
	  || !e->inline_failed)
	continue;

      /* Do not consider functions not declared inline.  */
      if (!DECL_DECLARED_INLINE_P (callee->decl)
	  && !opt_for_fn (node->decl, flag_inline_small_functions)
	  && !opt_for_fn (node->decl, flag_inline_functions))
	continue;

      if (dump_file)
	fprintf (dump_file, "Considering inline candidate %s.\n",
		 callee->name ());

      if (!can_early_inline_edge_p (e))
	continue;

      if (e->recursive_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "  Not inlining: recursive call.\n");
	  continue;
	}

      if (!want_early_inline_function_p (e))
	continue;

      if (dump_file)
	fprintf (dump_file, " Inlining %s into %s.\n",
		 xstrdup_for_dump (callee->name ()),
		 xstrdup_for_dump (e->caller->name ()));
      inline_call (e, true, NULL, NULL, false);
      inlined = true;
    }

  if (inlined)
    inline_update_overall_summary (node);

  return inlined;
}

unsigned int
early_inliner (function *fun)
{
  struct cgraph_node *node = cgraph_node::get (current_function_decl);
  struct cgraph_edge *edge;
  unsigned int todo = 0;
  int iterations = 0;
  bool inlined = false;

  if (seen_error ())
    return 0;

  /* Do nothing if datastructures for ipa-inliner are already computed.  This
     happens when some pass decides to construct new function and
     cgraph_add_new_function calls lowering passes and early optimization on
     it.  This may confuse ourself when early inliner decide to inline call to
     function clone, because function clones don't have parameter list in
     ipa-prop matching their signature.  */
  if (ipa_node_params_sum)
    return 0;

  if (flag_checking)
    node->verify ();
  node->remove_all_references ();

  /* Rebuild this reference because it dosn't depend on
     function's body and it's required to pass cgraph_node
     verification.  */
  if (node->instrumented_version
      && !node->instrumentation_clone)
    node->create_reference (node->instrumented_version, IPA_REF_CHKP, NULL);

  /* Even when not optimizing or not inlining inline always-inline
     functions.  */
  inlined = inline_always_inline_functions (node);

  if (!optimize
      || flag_no_inline
      || !flag_early_inlining
      /* Never inline regular functions into always-inline functions
	 during incremental inlining.  This sucks as functions calling
	 always inline functions will get less optimized, but at the
	 same time inlining of functions calling always inline
	 function into an always inline function might introduce
	 cycles of edges to be always inlined in the callgraph.

	 We might want to be smarter and just avoid this type of inlining.  */
      || (DECL_DISREGARD_INLINE_LIMITS (node->decl)
	  && lookup_attribute ("always_inline",
			       DECL_ATTRIBUTES (node->decl))))
    ;
  else if (lookup_attribute ("flatten",
			     DECL_ATTRIBUTES (node->decl)) != NULL)
    {
      /* When the function is marked to be flattened, recursively inline
	 all calls in it.  */
      if (dump_file)
	fprintf (dump_file,
		 "Flattening %s\n", node->name ());
      flatten_function (node, true);
      inlined = true;
    }
  else
    {
      /* If some always_inline functions was inlined, apply the changes.
	 This way we will not account always inline into growth limits and
	 moreover we will inline calls from always inlines that we skipped
	 previously because of conditional above.  */
      if (inlined)
	{
	  timevar_push (TV_INTEGRATION);
	  todo |= optimize_inline_calls (current_function_decl);
	  /* optimize_inline_calls call above might have introduced new
	     statements that don't have inline parameters computed.  */
	  for (edge = node->callees; edge; edge = edge->next_callee)
	    {
	      if (inline_edge_summary_vec.length () > (unsigned) edge->uid)
		{
		  struct inline_edge_summary *es = inline_edge_summary (edge);
		  es->call_stmt_size
		    = estimate_num_insns (edge->call_stmt, &eni_size_weights);
		  es->call_stmt_time
		    = estimate_num_insns (edge->call_stmt, &eni_time_weights);
		}
	    }
	  inline_update_overall_summary (node);
	  inlined = false;
	  timevar_pop (TV_INTEGRATION);
	}
      /* We iterate incremental inlining to get trivial cases of indirect
	 inlining.  */
      while (iterations < PARAM_VALUE (PARAM_EARLY_INLINER_MAX_ITERATIONS)
	     && early_inline_small_functions (node))
	{
	  timevar_push (TV_INTEGRATION);
	  todo |= optimize_inline_calls (current_function_decl);

	  /* Technically we ought to recompute inline parameters so the new
 	     iteration of early inliner works as expected.  We however have
	     values approximately right and thus we only need to update edge
	     info that might be cleared out for newly discovered edges.  */
	  for (edge = node->callees; edge; edge = edge->next_callee)
	    {
	      /* We have no summary for new bound store calls yet.  */
	      if (inline_edge_summary_vec.length () > (unsigned)edge->uid)
		{
		  struct inline_edge_summary *es = inline_edge_summary (edge);
		  es->call_stmt_size
		    = estimate_num_insns (edge->call_stmt, &eni_size_weights);
		  es->call_stmt_time
		    = estimate_num_insns (edge->call_stmt, &eni_time_weights);
		}
	      if (edge->callee->decl
		  && !gimple_check_call_matching_types (
		      edge->call_stmt, edge->callee->decl, false))
		{
 		  edge->inline_failed = CIF_MISMATCHED_ARGUMENTS;
		  edge->call_stmt_cannot_inline_p = true;
		}
	    }
	  if (iterations < PARAM_VALUE (PARAM_EARLY_INLINER_MAX_ITERATIONS) - 1)
	    inline_update_overall_summary (node);
	  timevar_pop (TV_INTEGRATION);
	  iterations++;
	  inlined = false;
	}
      if (dump_file)
	fprintf (dump_file, "Iterations: %i\n", iterations);
    }

  if (inlined)
    {
      timevar_push (TV_INTEGRATION);
      todo |= optimize_inline_calls (current_function_decl);
      timevar_pop (TV_INTEGRATION);
    }

  fun->always_inline_functions_inlined = true;

  return todo;
}

/* Do inlining of small functions.  Doing so early helps profiling and other
   passes to be somewhat more effective and avoids some code duplication in
   later real inlining pass for testcases with very many function calls.  */

namespace {

const pass_data pass_data_early_inline =
{
  GIMPLE_PASS, /* type */
  "einline", /* name */
  OPTGROUP_INLINE, /* optinfo_flags */
  TV_EARLY_INLINING, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_early_inline : public gimple_opt_pass
{
public:
  pass_early_inline (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_inline, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_early_inline

unsigned int
pass_early_inline::execute (function *fun)
{
  return early_inliner (fun);
}

} // anon namespace

gimple_opt_pass *
make_pass_early_inline (gcc::context *ctxt)
{
  return new pass_early_inline (ctxt);
}

namespace {

const pass_data pass_data_ipa_inline =
{
  IPA_PASS, /* type */
  "inline", /* name */
  OPTGROUP_INLINE, /* optinfo_flags */
  TV_IPA_INLINING, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_inline : public ipa_opt_pass_d
{
public:
  pass_ipa_inline (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_inline, ctxt,
		      inline_generate_summary, /* generate_summary */
		      inline_write_summary, /* write_summary */
		      inline_read_summary, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      inline_transform, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return ipa_inline (); }

}; // class pass_ipa_inline

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_inline (gcc::context *ctxt)
{
  return new pass_ipa_inline (ctxt);
}
