/* Inlining decision heuristics.
   Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

	 Because of lack of whole unit knowledge, the pass cannot really make
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
#include "profile.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "sreal.h"
#include "auto-profile.h"
#include "builtins.h"
#include "fibonacci_heap.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"

typedef fibonacci_heap <sreal, cgraph_edge> edge_heap_t;
typedef fibonacci_node <sreal, cgraph_edge> edge_heap_node_t;

/* Statistics we collect about inlining algorithm.  */
static int overall_size;
static profile_count max_count;
static profile_count spec_rem;

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
  ipa_size_summary *outer_info = ipa_size_summaries->get (to);

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
      ipa_size_summary *size_info = ipa_size_summaries->get (to);
      if (limit < size_info->self_size)
	limit = size_info->self_size;
      if (stack_size_limit < size_info->estimated_self_stack_size)
	stack_size_limit = size_info->estimated_self_stack_size;
      if (to->inlined_to)
        to = to->callers->caller;
      else
	break;
    }

  ipa_fn_summary *what_info = ipa_fn_summaries->get (what);
  ipa_size_summary *what_size_info = ipa_size_summaries->get (what);

  if (limit < what_size_info->self_size)
    limit = what_size_info->self_size;

  limit += limit * opt_for_fn (to->decl, param_large_function_growth) / 100;

  /* Check the size after inlining against the function limits.  But allow
     the function to shrink if it went over the limits by forced inlining.  */
  newsize = estimate_size_after_inlining (to, e);
  if (newsize >= ipa_size_summaries->get (what)->size
      && newsize > opt_for_fn (to->decl, param_large_function_insns)
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
		       * opt_for_fn (to->decl, param_stack_frame_growth)
		       / 100);

  inlined_stack = (ipa_get_stack_frame_offset (to)
		   + outer_info->estimated_self_stack_size
		   + what_info->estimated_stack_size);
  /* Check new stack consumption with stack consumption at the place
     stack is used.  */
  if (inlined_stack > stack_size_limit
      /* If function already has large stack usage from sibling
	 inline call, we can inline, too.
	 This bit overoptimistically assume that we are good at stack
	 packing.  */
      && inlined_stack > ipa_fn_summaries->get (to)->estimated_stack_size
      && inlined_stack > opt_for_fn (to->decl, param_large_stack_frame))
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
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
		       "  not inlinable: %C -> %C, %s\n",
		       e->caller, e->callee,
		       cgraph_inline_failed_string (e->inline_failed));
      if ((e->inline_failed == CIF_TARGET_OPTION_MISMATCH
	   || e->inline_failed == CIF_OPTIMIZATION_MISMATCH)
	  && e->caller->lto_file_data
	  && e->callee->ultimate_alias_target ()->lto_file_data)
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			   "  LTO objects: %s, %s\n",
			   e->caller->lto_file_data->file_name,
			   e->callee->ultimate_alias_target ()->lto_file_data->file_name);
	}
      if (e->inline_failed == CIF_TARGET_OPTION_MISMATCH)
	if (dump_file)
	  cl_target_option_print_diff
	    (dump_file, 2, target_opts_for_fn (e->caller->decl),
	     target_opts_for_fn (e->callee->ultimate_alias_target ()->decl));
      if (e->inline_failed == CIF_OPTIMIZATION_MISMATCH)
	if (dump_file)
	  cl_optimization_print_diff
	    (dump_file, 2, opts_for_fn (e->caller->decl),
	     opts_for_fn (e->callee->ultimate_alias_target ()->decl));
    }
}

 /* Decide whether sanitizer-related attributes allow inlining. */

static bool
sanitize_attrs_match_for_inline_p (const_tree caller, const_tree callee)
{
  if (!caller || !callee)
    return true;

  /* Allow inlining always_inline functions into no_sanitize_address
     functions.  */
  if (!sanitize_flags_p (SANITIZE_ADDRESS, caller)
      && lookup_attribute ("always_inline", DECL_ATTRIBUTES (callee)))
    return true;

  return ((sanitize_flags_p (SANITIZE_ADDRESS, caller)
	   == sanitize_flags_p (SANITIZE_ADDRESS, callee))
	  && (sanitize_flags_p (SANITIZE_POINTER_COMPARE, caller)
	      == sanitize_flags_p (SANITIZE_POINTER_COMPARE, callee))
	  && (sanitize_flags_p (SANITIZE_POINTER_SUBTRACT, caller)
	      == sanitize_flags_p (SANITIZE_POINTER_SUBTRACT, callee)));
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

   if REPORT is true, output reason to the dump file. */

static bool
can_inline_edge_p (struct cgraph_edge *e, bool report,
		   bool early = false)
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
  cgraph_node *caller = (e->caller->inlined_to
			 ? e->caller->inlined_to : e->caller);
  cgraph_node *callee = e->callee->ultimate_alias_target (&avail, caller);

  if (!callee->definition)
    {
      e->inline_failed = CIF_BODY_NOT_AVAILABLE;
      inlinable = false;
    }
  if (!early && (!opt_for_fn (callee->decl, optimize)
		 || !opt_for_fn (caller->decl, optimize)))
    {
      e->inline_failed = CIF_FUNCTION_NOT_OPTIMIZED;
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
  else if (ipa_fn_summaries->get (callee) == NULL
	   || !ipa_fn_summaries->get (callee)->inlinable)
    {
      e->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
      inlinable = false;
    }
  /* Don't inline a function with mismatched sanitization attributes. */
  else if (!sanitize_attrs_match_for_inline_p (caller->decl, callee->decl))
    {
      e->inline_failed = CIF_ATTRIBUTE_MISMATCH;
      inlinable = false;
    }
  if (!inlinable && report)
    report_inline_failed_reason (e);
  return inlinable;
}

/* Return inlining_insns_single limit for function N. If HINT is true
   scale up the bound.  */

static int
inline_insns_single (cgraph_node *n, bool hint)
{
  if (hint)
    return opt_for_fn (n->decl, param_max_inline_insns_single)
	   * opt_for_fn (n->decl, param_inline_heuristics_hint_percent) / 100;
  return opt_for_fn (n->decl, param_max_inline_insns_single);
}

/* Return inlining_insns_auto limit for function N. If HINT is true
   scale up the bound.   */

static int
inline_insns_auto (cgraph_node *n, bool hint)
{
  int max_inline_insns_auto = opt_for_fn (n->decl, param_max_inline_insns_auto);
  if (hint)
    return max_inline_insns_auto
	   * opt_for_fn (n->decl, param_inline_heuristics_hint_percent) / 100;
  return max_inline_insns_auto;
}

/* Decide if we can inline the edge and possibly update
   inline_failed reason.  
   We check whether inlining is possible at all and whether
   caller growth limits allow doing so.  

   if REPORT is true, output reason to the dump file.

   if DISREGARD_LIMITS is true, ignore size limits.  */

static bool
can_inline_edge_by_limits_p (struct cgraph_edge *e, bool report,
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
  cgraph_node *caller = (e->caller->inlined_to
			 ? e->caller->inlined_to : e->caller);
  cgraph_node *callee = e->callee->ultimate_alias_target (&avail, caller);
  tree caller_tree = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (caller->decl);
  tree callee_tree
    = callee ? DECL_FUNCTION_SPECIFIC_OPTIMIZATION (callee->decl) : NULL;
  /* Check if caller growth allows the inlining.  */
  if (!DECL_DISREGARD_INLINE_LIMITS (callee->decl)
      && !disregard_limits
      && !lookup_attribute ("flatten",
     		 DECL_ATTRIBUTES (caller->decl))
      && !caller_growth_limits (e))
    inlinable = false;
  else if (callee->externally_visible
	   && !DECL_DISREGARD_INLINE_LIMITS (callee->decl)
	   && flag_live_patching == LIVE_PATCHING_INLINE_ONLY_STATIC)
    {
      e->inline_failed = CIF_EXTERN_LIVE_ONLY_STATIC;
      inlinable = false;
    }
  /* Don't inline a function with a higher optimization level than the
     caller.  FIXME: this is really just tip of iceberg of handling
     optimization attribute.  */
  else if (caller_tree != callee_tree)
    {
      bool always_inline =
	     (DECL_DISREGARD_INLINE_LIMITS (callee->decl)
	      && lookup_attribute ("always_inline",
				   DECL_ATTRIBUTES (callee->decl)));
      ipa_fn_summary *caller_info = ipa_fn_summaries->get (caller);
      ipa_fn_summary *callee_info = ipa_fn_summaries->get (callee);

     /* Until GCC 4.9 we did not check the semantics-altering flags
	below and inlined across optimization boundaries.
	Enabling checks below breaks several packages by refusing
	to inline library always_inline functions. See PR65873.
	Disable the check for early inlining for now until better solution
	is found.  */
     if (always_inline && early)
	;
      /* There are some options that change IL semantics which means
         we cannot inline in these cases for correctness reason.
	 Not even for always_inline declared functions.  */
     else if (check_match (flag_wrapv)
	      || check_match (flag_trapv)
	      || check_match (flag_pcc_struct_return)
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
		      || check_maybe_down (flag_fp_int_builtin_inexact)
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
	      /* When devirtualization is disabled for callee, it is not safe
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
	 optimization flags we want to be bit nicer.  However never inline
	 if one of functions is not optimized at all.  */
      else if (!opt_for_fn (callee->decl, optimize)
      	       || !opt_for_fn (caller->decl, optimize))
	{
	  e->inline_failed = CIF_OPTIMIZATION_MISMATCH;
	  inlinable = false;
	}
      /* If callee is optimized for size and caller is not, allow inlining if
	 code shrinks or we are in param_max_inline_insns_single limit and
	 callee is inline (and thus likely an unified comdat).
	 This will allow caller to run faster.  */
      else if (opt_for_fn (callee->decl, optimize_size)
	       > opt_for_fn (caller->decl, optimize_size))
	{
	  int growth = estimate_edge_growth (e);
	  if (growth > opt_for_fn (caller->decl, param_max_inline_insns_size)
	      && (!DECL_DECLARED_INLINE_P (callee->decl)
		  && growth >= MAX (inline_insns_single (caller, false),
				    inline_insns_auto (caller, false))))
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
	      >= 20 + ipa_call_summaries->get (e)->call_stmt_time)
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
     function.  In this case we cannot really do any of early inlining
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
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			 "  edge not inlinable: not in SSA form\n");
      return false;
    }
  if (!can_inline_edge_p (e, true, true)
      || !can_inline_edge_by_limits_p (e, true, false, true))
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
      /* First take care of very large functions.  */
      int min_growth = estimate_min_edge_growth (e), growth = 0;
      int n;
      int early_inlining_insns = param_early_inlining_insns;

      if (min_growth > early_inlining_insns)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  will not early inline: %C->%C, "
			     "call is cold and code would grow "
			     "at least by %i\n",
			     e->caller, callee,
			     min_growth);
	  want_inline = false;
	}
      else
        growth = estimate_edge_growth (e);


      if (!want_inline || growth <= param_max_inline_insns_size)
	;
      else if (!e->maybe_hot_p ())
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  will not early inline: %C->%C, "
			     "call is cold and code would grow by %i\n",
			     e->caller, callee,
			     growth);
	  want_inline = false;
	}
      else if (growth > early_inlining_insns)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  will not early inline: %C->%C, "
			     "growth %i exceeds --param early-inlining-insns\n",
			     e->caller, callee, growth);
	  want_inline = false;
	}
      else if ((n = num_calls (callee)) != 0
	       && growth * (n + 1) > early_inlining_insns)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  will not early inline: %C->%C, "
			     "growth %i exceeds --param early-inlining-insns "
			     "divided by number of calls\n",
			     e->caller, callee, growth);
	  want_inline = false;
	}
    }
  return want_inline;
}

/* Compute time of the edge->caller + edge->callee execution when inlining
   does not happen.  */

inline sreal
compute_uninlined_call_time (struct cgraph_edge *edge,
			     sreal uninlined_call_time,
			     sreal freq)
{
  cgraph_node *caller = (edge->caller->inlined_to
			 ? edge->caller->inlined_to
			 : edge->caller);

  if (freq > 0)
    uninlined_call_time *= freq;
  else
    uninlined_call_time = uninlined_call_time >> 11;

  sreal caller_time = ipa_fn_summaries->get (caller)->time;
  return uninlined_call_time + caller_time;
}

/* Same as compute_uinlined_call_time but compute time when inlining
   does happen.  */

inline sreal
compute_inlined_call_time (struct cgraph_edge *edge,
			   sreal time,
			   sreal freq)
{
  cgraph_node *caller = (edge->caller->inlined_to
			 ? edge->caller->inlined_to
			 : edge->caller);
  sreal caller_time = ipa_fn_summaries->get (caller)->time;

  if (freq > 0)
    time *= freq;
  else
    time = time >> 11;

  /* This calculation should match one in ipa-inline-analysis.c
     (estimate_edge_size_and_time).  */
  time -= (sreal)ipa_call_summaries->get (edge)->call_stmt_time * freq;
  time += caller_time;
  if (time <= 0)
    time = ((sreal) 1) >> 8;
  gcc_checking_assert (time >= 0);
  return time;
}

/* Determine time saved by inlining EDGE of frequency FREQ
   where callee's runtime w/o inlining is UNINLINED_TYPE
   and with inlined is INLINED_TYPE.  */

inline sreal
inlining_speedup (struct cgraph_edge *edge,
    		  sreal freq,
		  sreal uninlined_time,
		  sreal inlined_time)
{
  sreal speedup = uninlined_time - inlined_time;
  /* Handling of call_time should match one in ipa-inline-fnsummary.c
     (estimate_edge_size_and_time).  */
  sreal call_time = ipa_call_summaries->get (edge)->call_stmt_time;

  if (freq > 0)
    {
      speedup = (speedup + call_time);
      if (freq != 1)
       speedup = speedup * freq;
    }
  else if (freq == 0)
    speedup = speedup >> 11;
  gcc_checking_assert (speedup >= 0);
  return speedup;
}

/* Return true if the speedup for inlining E is bigger than
   PARAM_MAX_INLINE_MIN_SPEEDUP.  */

static bool
big_speedup_p (struct cgraph_edge *e)
{
  sreal unspec_time;
  sreal spec_time = estimate_edge_time (e, &unspec_time);
  sreal freq = e->sreal_frequency ();
  sreal time = compute_uninlined_call_time (e, unspec_time, freq);
  sreal inlined_time = compute_inlined_call_time (e, spec_time, freq);
  cgraph_node *caller = (e->caller->inlined_to
			 ? e->caller->inlined_to
			 : e->caller);
  int limit = opt_for_fn (caller->decl, param_inline_min_speedup);

  if ((time - inlined_time) * 100 > time * limit)
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
  cgraph_node *to  = (e->caller->inlined_to
		      ? e->caller->inlined_to : e->caller);

  /* Allow this function to be called before can_inline_edge_p,
     since it's usually cheaper.  */
  if (cgraph_inline_failed_type (e->inline_failed) == CIF_FINAL_ERROR)
    want_inline = false;
  else if (DECL_DISREGARD_INLINE_LIMITS (callee->decl))
    ;
  else if (!DECL_DECLARED_INLINE_P (callee->decl)
	   && !opt_for_fn (e->caller->decl, flag_inline_small_functions))
    {
      e->inline_failed = CIF_FUNCTION_NOT_INLINE_CANDIDATE;
      want_inline = false;
    }
  /* Do fast and conservative check if the function can be good
     inline candidate.  */
  else if ((!DECL_DECLARED_INLINE_P (callee->decl)
	   && (!e->count.ipa ().initialized_p () || !e->maybe_hot_p ()))
	   && ipa_fn_summaries->get (callee)->min_size
		- ipa_call_summaries->get (e)->call_stmt_size
	      > inline_insns_auto (e->caller, true))
    {
      e->inline_failed = CIF_MAX_INLINE_INSNS_AUTO_LIMIT;
      want_inline = false;
    }
  else if ((DECL_DECLARED_INLINE_P (callee->decl)
	    || e->count.ipa ().nonzero_p ())
	   && ipa_fn_summaries->get (callee)->min_size
		- ipa_call_summaries->get (e)->call_stmt_size
	      > inline_insns_single (e->caller, true))
    {
      e->inline_failed = (DECL_DECLARED_INLINE_P (callee->decl)
			  ? CIF_MAX_INLINE_INSNS_SINGLE_LIMIT
			  : CIF_MAX_INLINE_INSNS_AUTO_LIMIT);
      want_inline = false;
    }
  else
    {
      int growth = estimate_edge_growth (e);
      ipa_hints hints = estimate_edge_hints (e);
      bool apply_hints = (hints & (INLINE_HINT_indirect_call
				   | INLINE_HINT_known_hot
				   | INLINE_HINT_loop_iterations
				   | INLINE_HINT_loop_stride));

      if (growth <= opt_for_fn (to->decl,
				param_max_inline_insns_size))
	;
      /* Apply param_max_inline_insns_single limit.  Do not do so when
	 hints suggests that inlining given function is very profitable.
	 Avoid computation of big_speedup_p when not necessary to change
	 outcome of decision.  */
      else if (DECL_DECLARED_INLINE_P (callee->decl)
	       && growth >= inline_insns_single (e->caller, apply_hints)
	       && (apply_hints
		   || growth >= inline_insns_single (e->caller, true)
		   || !big_speedup_p (e)))
	{
          e->inline_failed = CIF_MAX_INLINE_INSNS_SINGLE_LIMIT;
	  want_inline = false;
	}
      else if (!DECL_DECLARED_INLINE_P (callee->decl)
	       && !opt_for_fn (e->caller->decl, flag_inline_functions)
	       && growth >= opt_for_fn (to->decl,
					param_max_inline_insns_small))
	{
	  /* growth_positive_p is expensive, always test it last.  */
          if (growth >= inline_insns_single (e->caller, false)
	      || growth_positive_p (callee, e, growth))
	    {
              e->inline_failed = CIF_NOT_DECLARED_INLINED;
	      want_inline = false;
 	    }
	}
      /* Apply param_max_inline_insns_auto limit for functions not declared
	 inline.  Bypass the limit when speedup seems big.  */
      else if (!DECL_DECLARED_INLINE_P (callee->decl)
	       && growth >= inline_insns_auto (e->caller, apply_hints)
	       && (apply_hints
		   || growth >= inline_insns_auto (e->caller, true)
		   || !big_speedup_p (e)))
	{
	  /* growth_positive_p is expensive, always test it last.  */
          if (growth >= inline_insns_single (e->caller, false)
	      || growth_positive_p (callee, e, growth))
	    {
	      e->inline_failed = CIF_MAX_INLINE_INSNS_AUTO_LIMIT;
	      want_inline = false;
 	    }
	}
      /* If call is cold, do not inline when function body would grow. */
      else if (!e->maybe_hot_p ()
	       && (growth >= inline_insns_single (e->caller, false)
		   || growth_positive_p (callee, e, growth)))
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
   We handle two cases - when function A is inlining into itself
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
  sreal caller_freq = 1;
  int max_depth = opt_for_fn (outer_node->decl,
			      param_max_inline_recursive_depth_auto);

  if (DECL_DECLARED_INLINE_P (edge->caller->decl))
    max_depth = opt_for_fn (outer_node->decl,
			    param_max_inline_recursive_depth);

  if (!edge->maybe_hot_p ())
    {
      reason = "recursive call is cold";
      want_inline = false;
    }
  else if (depth > max_depth)
    {
      reason = "--param max-inline-recursive-depth exceeded.";
      want_inline = false;
    }
  else if (outer_node->inlined_to
	   && (caller_freq = outer_node->callers->sreal_frequency ()) == 0)
    {
      reason = "caller frequency is 0";
      want_inline = false;
    }

  if (!want_inline)
    ;
  /* Inlining of self recursive function into copy of itself within other
     function is transformation similar to loop peeling.

     Peeling is profitable if we can inline enough copies to make probability
     of actual call to the self recursive function very small.  Be sure that
     the probability of recursion is small.

     We ensure that the frequency of recursing is at most 1 - (1/max_depth).
     This way the expected number of recursion is at most max_depth.  */
  else if (peeling)
    {
      sreal max_prob = (sreal)1 - ((sreal)1 / (sreal)max_depth);
      int i;
      for (i = 1; i < depth; i++)
	max_prob = max_prob * max_prob;
      if (edge->sreal_frequency () >= max_prob * caller_freq)
	{
	  reason = "frequency of recursive call is too large";
	  want_inline = false;
	}
    }
  /* Recursive inlining, i.e. equivalent of unrolling, is profitable if
     recursion depth is large.  We reduce function call overhead and increase
     chances that things fit in hardware return predictor.

     Recursive inlining might however increase cost of stack frame setup
     actually slowing down functions whose recursion tree is wide rather than
     deep.

     Deciding reliably on when to do recursive inlining without profile feedback
     is tricky.  For now we disable recursive inlining when probability of self
     recursion is low. 

     Recursive inlining of self recursive call within loop also results in
     large loop depths that generally optimize badly.  We may want to throttle
     down inlining in those cases.  In particular this seems to happen in one
     of libstdc++ rb tree methods.  */
  else
    {
      if (edge->sreal_frequency () * 100
          <= caller_freq
	     * opt_for_fn (outer_node->decl,
			   param_min_inline_recursive_probability))
	{
	  reason = "frequency of recursive call is too small";
	  want_inline = false;
	}
    }
  if (!want_inline && dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, edge->call_stmt,
		     "   not inlining recursively: %s\n", reason);
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
       if (!opt_for_fn (e->caller->decl, flag_inline_functions_called_once)
	   || !opt_for_fn (e->caller->decl, optimize))
	 return true;
       if (!can_inline_edge_p (e, true))
         return true;
       if (e->recursive_p ())
	 return true;
       if (!can_inline_edge_by_limits_p (e, true))
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
  if (node->inlined_to)
    return false;
  /* Does it have callers?  */
  if (!node->call_for_symbol_and_aliases (has_caller_p, NULL, true))
    return false;
  /* Inlining into all callers would increase size?  */
  if (growth_positive_p (node, NULL, INT_MIN) > 0)
    return false;
  /* All inlines must be possible.  */
  if (node->call_for_symbol_and_aliases (check_callers, &has_hot_call,
					 true))
    return false;
  if (!cold && !has_hot_call)
    return false;
  return true;
}

/* Return true if WHERE of SIZE is a possible candidate for wrapper heuristics
   in estimate_edge_badness.  */

static bool
wrapper_heuristics_may_apply (struct cgraph_node *where, int size)
{
  return size < (DECL_DECLARED_INLINE_P (where->decl)
		 ? inline_insns_single (where, false)
		 : inline_insns_auto (where, false));
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
  int growth;
  sreal edge_time, unspec_edge_time;
  struct cgraph_node *callee = edge->callee->ultimate_alias_target ();
  class ipa_fn_summary *callee_info = ipa_fn_summaries->get (callee);
  ipa_hints hints;
  cgraph_node *caller = (edge->caller->inlined_to
			 ? edge->caller->inlined_to
			 : edge->caller);

  growth = estimate_edge_growth (edge);
  edge_time = estimate_edge_time (edge, &unspec_edge_time);
  hints = estimate_edge_hints (edge);
  gcc_checking_assert (edge_time >= 0);
  /* Check that inlined time is better, but tolerate some roundoff issues.
     FIXME: When callee profile drops to 0 we account calls more.  This
     should be fixed by never doing that.  */
  gcc_checking_assert ((edge_time * 100
			- callee_info->time * 101).to_int () <= 0
			|| callee->count.ipa ().initialized_p ());
  gcc_checking_assert (growth <= ipa_size_summaries->get (callee)->size);

  if (dump)
    {
      fprintf (dump_file, "    Badness calculation for %s -> %s\n",
	       edge->caller->dump_name (),
	       edge->callee->dump_name ());
      fprintf (dump_file, "      size growth %i, time %f unspec %f ",
	       growth,
	       edge_time.to_double (),
	       unspec_edge_time.to_double ());
      ipa_dump_hints (dump_file, hints);
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
  else if (opt_for_fn (caller->decl, flag_guess_branch_prob)
	   || caller->count.ipa ().nonzero_p ())
    {
      sreal numerator, denominator;
      int overall_growth;
      sreal freq = edge->sreal_frequency ();

      numerator = inlining_speedup (edge, freq, unspec_edge_time, edge_time);
      if (numerator <= 0)
	numerator = ((sreal) 1 >> 8);
      if (caller->count.ipa ().nonzero_p ())
	numerator *= caller->count.ipa ().to_gcov_type ();
      else if (caller->count.ipa ().initialized_p ())
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
	 Without penalizing this case, we usually inline noninline_callee
	 into the inline_caller because overall_growth is small preventing
	 further inlining of inline_caller.

	 Penalize only callgraph edges to functions with small overall
	 growth ...
	*/
      if (growth > overall_growth
	  /* ... and having only one caller which is not inlined ... */
	  && callee_info->single_caller
	  && !edge->caller->inlined_to
	  /* ... and edges executed only conditionally ... */
	  && freq < 1
	  /* ... consider case where callee is not inline but caller is ... */
	  && ((!DECL_DECLARED_INLINE_P (edge->callee->decl)
	       && DECL_DECLARED_INLINE_P (caller->decl))
	      /* ... or when early optimizers decided to split and edge
		 frequency still indicates splitting is a win ... */
	      || (callee->split_part && !caller->split_part
		  && freq * 100
			 < opt_for_fn (caller->decl,
				       param_partial_inlining_entry_probability)
		  /* ... and do not overwrite user specified hints.   */
		  && (!DECL_DECLARED_INLINE_P (edge->callee->decl)
		      || DECL_DECLARED_INLINE_P (caller->decl)))))
	{
	  ipa_fn_summary *caller_info = ipa_fn_summaries->get (caller);
	  int caller_growth = caller_info->growth;

	  /* Only apply the penalty when caller looks like inline candidate,
	     and it is not called once.  */
	  if (!caller_info->single_caller && overall_growth < caller_growth
	      && caller_info->inlinable
	      && wrapper_heuristics_may_apply
	     	 (caller, ipa_size_summaries->get (caller)->size))
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
	  /* Strongly prefer functions with few callers that can be inlined
	     fully.  The square root here leads to smaller binaries at average.
	     Watch however for extreme cases and return to linear function
	     when growth is large.  */
	  if (overall_growth < 256)
	    overall_growth *= overall_growth;
	  else
	    overall_growth += 256 * 256 - 256;
	  denominator *= overall_growth;
        }
      denominator *= ipa_size_summaries->get (caller)->size + growth;

      badness = - numerator / denominator;

      if (dump)
	{
	  fprintf (dump_file,
		   "      %f: guessed profile. frequency %f, count %" PRId64
		   " caller count %" PRId64
		   " time saved %f"
		   " overall growth %i (current) %i (original)"
		   " %i (compensated)\n",
		   badness.to_double (),
		   freq.to_double (),
		   edge->count.ipa ().initialized_p () ? edge->count.ipa ().to_gcov_type () : -1,
		   caller->count.ipa ().initialized_p () ? caller->count.ipa ().to_gcov_type () : -1,
		   inlining_speedup (edge, freq, unspec_edge_time, edge_time).to_double (),
		   estimate_growth (callee),
		   callee_info->growth, overall_growth);
	}
    }
  /* When function local profile is not available or it does not give
     useful information (i.e. frequency is zero), base the cost on
     loop nest and overall size growth, so we optimize for overall number
     of functions fully inlined in program.  */
  else
    {
      int nest = MIN (ipa_call_summaries->get (edge)->loop_depth, 8);
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
	 heap that is unnecessarily expensive.
	 We do lazy increases: after extracting minimum if the key
	 turns out to be out of date, it is re-inserted into heap
	 with correct value.  */
      if (badness < n->get_key ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "  decreasing badness %s -> %s, %f to %f\n",
		       edge->caller->dump_name (),
		       edge->callee->dump_name (),
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
		    "  enqueuing call %s -> %s, badness %f\n",
		    edge->caller->dump_name (),
		    edge->callee->dump_name (),
		    badness.to_double ());
	 }
      edge->aux = heap->insert (badness, edge);
    }
}


/* NODE was inlined.
   All caller edges needs to be reset because
   size estimates change. Similarly callees needs reset
   because better context may be known.  */

static void
reset_edge_caches (struct cgraph_node *node)
{
  struct cgraph_edge *edge;
  struct cgraph_edge *e = node->callees;
  struct cgraph_node *where = node;
  struct ipa_ref *ref;

  if (where->inlined_to)
    where = where->inlined_to;

  reset_node_cache (where);

  if (edge_growth_cache != NULL)
    for (edge = where->callers; edge; edge = edge->next_caller)
      if (edge->inline_failed)
	edge_growth_cache->remove (edge);

  FOR_EACH_ALIAS (where, ref)
    reset_edge_caches (dyn_cast <cgraph_node *> (ref->referring));

  if (!e)
    return;

  while (true)
    if (!e->inline_failed && e->callee->callees)
      e = e->callee->callees;
    else
      {
	if (edge_growth_cache != NULL && e->inline_failed)
	  edge_growth_cache->remove (e);
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

  if ((!node->alias && !ipa_fn_summaries->get (node)->inlinable)
      || node->inlined_to)
    return;
  if (!bitmap_set_bit (updated_nodes, node->get_uid ()))
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
		&& want_inline_small_function_p (edge, false)
		&& can_inline_edge_by_limits_p (edge, false))
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

/* Recompute HEAP nodes for each uninlined call in NODE
   If UPDATE_SINCE is non-NULL check if edges called within that function
   are inlinable (typically UPDATE_SINCE is the inline clone we introduced
   where all edges have new context).
  
   This is used when we know that edge badnesses are going only to increase
   (we introduced new call site) and thus all we need is to insert newly
   created edges into heap.  */

static void
update_callee_keys (edge_heap_t *heap, struct cgraph_node *node,
		    struct cgraph_node *update_since,
		    bitmap updated_nodes)
{
  struct cgraph_edge *e = node->callees;
  bool check_inlinability = update_since == node;

  if (!e)
    return;
  while (true)
    if (!e->inline_failed && e->callee->callees)
      {
	if (e->callee == update_since)
	  check_inlinability = true;
        e = e->callee->callees;
      }
    else
      {
	enum availability avail;
	struct cgraph_node *callee;
	if (!check_inlinability)
	  {
	    if (e->aux
		&& !bitmap_bit_p (updated_nodes,
		 		  e->callee->ultimate_alias_target
				    (&avail, e->caller)->get_uid ()))
	      update_edge_key (heap, e);
	  }
	/* We do not reset callee growth cache here.  Since we added a new call,
	   growth should have just increased and consequently badness metric
           don't need updating.  */
	else if (e->inline_failed
		 && (callee = e->callee->ultimate_alias_target (&avail,
		  						e->caller))
		 && avail >= AVAIL_AVAILABLE
		 && ipa_fn_summaries->get (callee) != NULL
		 && ipa_fn_summaries->get (callee)->inlinable
		 && !bitmap_bit_p (updated_nodes, callee->get_uid ()))
	  {
	    if (can_inline_edge_p (e, false)
		&& want_inline_small_function_p (e, false)
		&& can_inline_edge_by_limits_p (e, false))
	      {
		gcc_checking_assert (check_inlinability || can_inline_edge_p (e, false));
		gcc_checking_assert (check_inlinability || e->aux);
	        update_edge_key (heap, e);
	      }
	    else if (e->aux)
	      {
		report_inline_failed_reason (e);
		heap->delete_node ((edge_heap_node_t *) e->aux);
		e->aux = NULL;
	      }
	  }
	/* In case we redirected to unreachable node we only need to remove the
	   fibheap entry.  */
	else if (e->aux)
	  {
	    heap->delete_node ((edge_heap_node_t *) e->aux);
	    e->aux = NULL;
	  }
	if (e->next_callee)
	  e = e->next_callee;
	else
	  {
	    do
	      {
		if (e->caller == node)
		  return;
		if (e->caller == update_since)
		  check_inlinability = false;
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
      heap->insert (-e->sreal_frequency (), e);
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
  cgraph_node *to  = (edge->caller->inlined_to
		      ? edge->caller->inlined_to : edge->caller);
  int limit = opt_for_fn (to->decl,
			  param_max_inline_insns_recursive_auto);
  edge_heap_t heap (sreal::min ());
  struct cgraph_node *node;
  struct cgraph_edge *e;
  struct cgraph_node *master_clone = NULL, *next;
  int depth = 0;
  int n = 0;

  node = edge->caller;
  if (node->inlined_to)
    node = node->inlined_to;

  if (DECL_DECLARED_INLINE_P (node->decl))
    limit = opt_for_fn (to->decl, param_max_inline_insns_recursive);

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

      if (!can_inline_edge_p (curr, true)
	  || !can_inline_edge_by_limits_p (curr, true))
	continue;

      /* MASTER_CLONE is produced in the case we already started modified
	 the function. Be sure to redirect edge to the original body before
	 estimating growths otherwise we will be seeing growths after inlining
	 the already modified body.  */
      if (master_clone)
	{
	  curr->redirect_callee (master_clone);
	  if (edge_growth_cache != NULL)
	    edge_growth_cache->remove (curr);
	}

      if (estimate_size_after_inlining (node, curr) > limit)
	{
	  curr->redirect_callee (dest);
	  if (edge_growth_cache != NULL)
	    edge_growth_cache->remove (curr);
	  break;
	}

      depth = 1;
      for (cnode = curr->caller;
	   cnode->inlined_to; cnode = cnode->callers->caller)
	if (node->decl
	    == curr->callee->ultimate_alias_target ()->decl)
          depth++;

      if (!want_inline_self_recursive_call_p (curr, node, false, depth))
	{
	  curr->redirect_callee (dest);
	  if (edge_growth_cache != NULL)
	    edge_growth_cache->remove (curr);
	  continue;
	}

      if (dump_file)
	{
	  fprintf (dump_file,
		   "   Inlining call of depth %i", depth);
	  if (node->count.nonzero_p () && curr->count.initialized_p ())
	    {
	      fprintf (dump_file, " called approx. %.2f times per call",
		       (double)curr->count.to_gcov_type ()
		       / node->count.to_gcov_type ());
	    }
	  fprintf (dump_file, "\n");
	}
      if (!master_clone)
	{
	  /* We need original clone to copy around.  */
	  master_clone = node->create_clone (node->decl, node->count,
	    false, vNULL, true, NULL, NULL);
	  for (e = master_clone->callees; e; e = e->next_callee)
	    if (!e->inline_failed)
	      clone_inlined_nodes (e, true, false, NULL);
	  curr->redirect_callee (master_clone);
	  if (edge_growth_cache != NULL)
	    edge_growth_cache->remove (curr);
	}

      inline_call (curr, false, new_edges, &overall_size, true);
      reset_node_cache (node);
      lookup_recursive_calls (node, curr->callee, &heap);
      n++;
    }

  if (!heap.empty () && dump_file)
    fprintf (dump_file, "    Recursive inlining growth limit met.\n");

  if (!master_clone)
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, edge->call_stmt,
		     "\n   Inlined %i times, "
		     "body grown from size %i to %i, time %f to %f\n", n,
		     ipa_size_summaries->get (master_clone)->size,
		     ipa_size_summaries->get (node)->size,
		     ipa_fn_summaries->get (master_clone)->time.to_double (),
		     ipa_fn_summaries->get (node)->time.to_double ());

  /* Remove master clone we used for inlining.  We rely that clones inlined
     into master clone gets queued just before master clone so we don't
     need recursion.  */
  for (node = symtab->first_function (); node != master_clone;
       node = next)
    {
      next = symtab->next_function (node);
      if (node->inlined_to == master_clone)
	node->remove ();
    }
  master_clone->remove ();
  return true;
}


/* Given whole compilation unit estimate of INSNS, compute how large we can
   allow the unit to grow.  */

static int64_t
compute_max_insns (cgraph_node *node, int insns)
{
  int max_insns = insns;
  if (max_insns < opt_for_fn (node->decl, param_large_unit_insns))
    max_insns = opt_for_fn (node->decl, param_large_unit_insns);

  return ((int64_t) max_insns
	  * (100 + opt_for_fn (node->decl, param_inline_unit_growth)) / 100);
}


/* Compute badness of all edges in NEW_EDGES and add them to the HEAP.  */

static void
add_new_edges_to_heap (edge_heap_t *heap, vec<cgraph_edge *> new_edges)
{
  while (new_edges.length () > 0)
    {
      struct cgraph_edge *edge = new_edges.pop ();

      gcc_assert (!edge->aux);
      gcc_assert (edge->callee);
      if (edge->inline_failed
	  && can_inline_edge_p (edge, true)
	  && want_inline_small_function_p (edge, true)
	  && can_inline_edge_by_limits_p (edge, true))
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
  /* If we have already decided to inline the edge, it seems useful.  */
  if (!e->inline_failed)
    return true;

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
  if (!anticipate_inlining && !target->local)
    return false;
  /* For overwritable targets there is not much to do.  */
  if (!can_inline_edge_p (e, false)
      || !can_inline_edge_by_limits_p (e, false, true))
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
      struct cgraph_node *where = node->inlined_to
				  ? node->inlined_to : node;
      auto_bitmap updated_nodes;

      if (edge->count.ipa ().initialized_p ())
        spec_rem += edge->count.ipa ();
      edge->resolve_speculation ();
      reset_edge_caches (where);
      ipa_update_overall_fn_summary (where);
      update_caller_keys (edge_heap, where,
			  updated_nodes, NULL);
      update_callee_keys (edge_heap, where, NULL,
			  updated_nodes);
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

/* We only propagate across edges with non-interposable callee.  */

inline bool
ignore_edge_p (struct cgraph_edge *e)
{
  enum availability avail;
  e->callee->function_or_virtual_thunk_symbol (&avail, e->caller);
  return (avail <= AVAIL_INTERPOSABLE);
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
  auto_bitmap updated_nodes;
  int min_size;
  auto_vec<cgraph_edge *> new_indirect_edges;
  int initial_size = 0;
  struct cgraph_node **order = XCNEWVEC (cgraph_node *, symtab->cgraph_count);
  struct cgraph_edge_hook_list *edge_removal_hook_holder;
  new_indirect_edges.create (8);

  edge_removal_hook_holder
    = symtab->add_edge_removal_hook (&heap_edge_removal_hook, &edge_heap);

  /* Compute overall unit size and other global parameters used by badness
     metrics.  */

  max_count = profile_count::uninitialized ();
  ipa_reduced_postorder (order, true, ignore_edge_p);
  free (order);

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->inlined_to)
      {
	if (!node->alias && node->analyzed
	    && (node->has_gimple_body_p () || node->thunk.thunk_p)
	    && opt_for_fn (node->decl, optimize))
	  {
	    class ipa_fn_summary *info = ipa_fn_summaries->get (node);
	    struct ipa_dfs_info *dfs = (struct ipa_dfs_info *) node->aux;

	    /* Do not account external functions, they will be optimized out
	       if not inlined.  Also only count the non-cold portion of program.  */
	    if (inline_account_function_p (node))
	      initial_size += ipa_size_summaries->get (node)->size;
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
		     n2 = ((struct ipa_dfs_info *) n2->aux)->next_cycle)
		  if (opt_for_fn (n2->decl, optimize))
		    {
		      ipa_fn_summary *info2 = ipa_fn_summaries->get
			 (n2->inlined_to ? n2->inlined_to : n2);
		      if (info2->scc_no)
			break;
		      info2->scc_no = id;
		    }
	      }
	  }

	for (edge = node->callers; edge; edge = edge->next_caller)
	  max_count = max_count.max (edge->count.ipa ());
      }
  ipa_free_postorder_info ();
  initialize_growth_caches ();

  if (dump_file)
    fprintf (dump_file,
	     "\nDeciding on inlining of small functions.  Starting with size %i.\n",
	     initial_size);

  overall_size = initial_size;
  min_size = overall_size;

  /* Populate the heap with all edges we might inline.  */

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      bool update = false;
      struct cgraph_edge *next = NULL;
      bool has_speculative = false;

      if (!opt_for_fn (node->decl, optimize))
	continue;

      if (dump_file)
	fprintf (dump_file, "Enqueueing calls in %s.\n", node->dump_name ());

      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  if (edge->inline_failed
	      && !edge->aux
	      && can_inline_edge_p (edge, true)
	      && want_inline_small_function_p (edge, true)
	      && can_inline_edge_by_limits_p (edge, true)
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
	  {
	    next = edge->next_callee;
	    if (edge->speculative
		&& !speculation_useful_p (edge, edge->aux != NULL))
	      {
		edge->resolve_speculation ();
		update = true;
	      }
	  }
      if (update)
	{
	  struct cgraph_node *where = node->inlined_to
				      ? node->inlined_to : node;
	  ipa_update_overall_fn_summary (where);
	  reset_edge_caches (where);
          update_caller_keys (&edge_heap, where,
			      updated_nodes, NULL);
          update_callee_keys (&edge_heap, where, NULL,
			      updated_nodes);
          bitmap_clear (updated_nodes);
	}
    }

  gcc_assert (in_lto_p
	      || !(max_count > 0)
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

      /* Be sure that caches are maintained consistent.
	 This check is affected by scaling roundoff errors when compiling for
	 IPA this we skip it in that case.  */
      if (flag_checking && !edge->callee->count.ipa_p ()
	  && (!max_count.initialized_p () || !max_count.nonzero_p ()))
	{
	  sreal cached_badness = edge_badness (edge, false);
     
	  int old_size_est = estimate_edge_size (edge);
	  sreal old_time_est = estimate_edge_time (edge);
	  int old_hints_est = estimate_edge_hints (edge);

	  if (edge_growth_cache != NULL)
	    edge_growth_cache->remove (edge);
	  reset_node_cache (edge->caller->inlined_to
			    ? edge->caller->inlined_to
			    : edge->caller);
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
	  edge_growth_cache->get (edge)->hints = old_hints_est + 1;

	  /* When updating the edge costs, we only decrease badness in the keys.
	     Increases of badness are handled lazily; when we see key with out
	     of date value on it, we re-insert it now.  */
	  current_badness = edge_badness (edge, false);
	  gcc_assert (cached_badness == current_badness);
	  gcc_assert (current_badness >= badness);
	}
      else
        current_badness = edge_badness (edge, false);
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

      if (!can_inline_edge_p (edge, true)
	  || !can_inline_edge_by_limits_p (edge, true))
	{
	  resolve_noninline_speculation (&edge_heap, edge);
	  continue;
	}
      
      callee = edge->callee->ultimate_alias_target ();
      growth = estimate_edge_growth (edge);
      if (dump_file)
	{
	  fprintf (dump_file,
		   "\nConsidering %s with %i size\n",
		   callee->dump_name (),
		   ipa_size_summaries->get (callee)->size);
	  fprintf (dump_file,
		   " to be inlined into %s in %s:%i\n"
		   " Estimated badness is %f, frequency %.2f.\n",
		   edge->caller->dump_name (),
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
		   edge->sreal_frequency ().to_double ());
	  if (edge->count.ipa ().initialized_p ())
	    {
	      fprintf (dump_file, " Called ");
	      edge->count.ipa ().dump (dump_file);
	      fprintf (dump_file, " times\n");
            }
	  if (dump_flags & TDF_DETAILS)
	    edge_badness (edge, true);
	}

      where = edge->caller;

      if (overall_size + growth > compute_max_insns (where, min_size)
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

      profile_count old_count = callee->count;

      /* Heuristics for inlining small functions work poorly for
	 recursive calls where we do effects similar to loop unrolling.
	 When inlining such edge seems profitable, leave decision on
	 specific inliner.  */
      if (edge->recursive_p ())
	{
	  if (where->inlined_to)
	    where = where->inlined_to;
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
          update_callee_keys (&edge_heap, where, where, updated_nodes);
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
	  while (where->inlined_to)
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

	  gcc_checking_assert (!callee->inlined_to);

	  int old_size = ipa_size_summaries->get (where)->size;
	  sreal old_time = ipa_fn_summaries->get (where)->time;

	  inline_call (edge, true, &new_indirect_edges, &overall_size, true);
	  reset_edge_caches (edge->callee);
	  add_new_edges_to_heap (&edge_heap, new_indirect_edges);

	  /* If caller's size and time increased we do not need to update
	     all edges because badness is not going to decrease.  */
	  if (old_size <= ipa_size_summaries->get (where)->size
	      && old_time <= ipa_fn_summaries->get (where)->time
	      /* Wrapper penalty may be non-monotonous in this respect.
	         Fortunately it only affects small functions.  */
	      && !wrapper_heuristics_may_apply (where, old_size))
	    update_callee_keys (&edge_heap, edge->callee, edge->callee,
			   	updated_nodes);
	  else
	    update_callee_keys (&edge_heap, where,
				edge->callee,
			   	updated_nodes);
	}
      where = edge->caller;
      if (where->inlined_to)
	where = where->inlined_to;

      /* Our profitability metric can depend on local properties
	 such as number of inlinable calls and size of the function body.
	 After inlining these properties might change for the function we
	 inlined into (since it's body size changed) and for the functions
	 called by function we inlined (since number of it inlinable callers
	 might change).  */
      update_caller_keys (&edge_heap, where, updated_nodes, NULL);
      /* Offline copy count has possibly changed, recompute if profile is
	 available.  */
      struct cgraph_node *n
	      = cgraph_node::get (edge->callee->decl)->ultimate_alias_target ();
      if (n != edge->callee && n->analyzed && !(n->count == old_count)
	  && n->count.ipa_p ())
	update_callee_keys (&edge_heap, n, NULL, updated_nodes);
      bitmap_clear (updated_nodes);

      if (dump_enabled_p ())
	{
	  ipa_fn_summary *s = ipa_fn_summaries->get (where);

	  /* dump_printf can't handle %+i.  */
	  char buf_net_change[100];
	  snprintf (buf_net_change, sizeof buf_net_change, "%+i",
		    overall_size - old_size);

	  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, edge->call_stmt,
			   " Inlined %C into %C which now has time %f and "
			   "size %i, net change of %s%s.\n",
			   edge->callee, edge->caller,
			   s->time.to_double (),
			   ipa_size_summaries->get (edge->caller)->size,
			   buf_net_change,
			   cross_module_call_p (edge) ? " (cross module)":"");
	}
      if (min_size > overall_size)
	{
	  min_size = overall_size;

	  if (dump_file)
	    fprintf (dump_file, "New minimal size reached: %i\n", min_size);
	}
    }

  free_growth_caches ();
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE,
		 "Unit growth for small function inlining: %i->%i (%i%%)\n",
		 initial_size, overall_size,
		 initial_size ? overall_size * 100 / (initial_size) - 100: 0);
  symtab->remove_edge_removal_hook (edge_removal_hook_holder);
}

/* Flatten NODE.  Performed both during early inlining and
   at IPA inlining time.  */

static void
flatten_function (struct cgraph_node *node, bool early, bool update)
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
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "Not inlining %C into %C to avoid cycle.\n",
			     callee, e->caller);
	  if (cgraph_inline_failed_type (e->inline_failed) != CIF_FINAL_ERROR)
	    e->inline_failed = CIF_RECURSIVE_INLINING;
	  continue;
	}

      /* When the edge is already inlined, we just need to recurse into
	 it in order to fully flatten the leaves.  */
      if (!e->inline_failed)
	{
	  flatten_function (callee, early, false);
	  continue;
	}

      /* Flatten attribute needs to be processed during late inlining. For
	 extra code quality we however do flattening during early optimization,
	 too.  */
      if (!early
	  ? !can_inline_edge_p (e, true)
	    && !can_inline_edge_by_limits_p (e, true)
	  : !can_early_inline_edge_p (e))
	continue;

      if (e->recursive_p ())
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "Not inlining: recursive call.\n");
	  continue;
	}

      if (gimple_in_ssa_p (DECL_STRUCT_FUNCTION (node->decl))
	  != gimple_in_ssa_p (DECL_STRUCT_FUNCTION (callee->decl)))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "Not inlining: SSA form does not match.\n");
	  continue;
	}

      /* Inline the edge and flatten the inline clone.  Avoid
         recursing through the original node if the node was cloned.  */
      if (dump_enabled_p ())
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, e->call_stmt,
			 " Inlining %C into %C.\n",
			 callee, e->caller);
      orig_callee = callee;
      inline_call (e, true, NULL, NULL, false);
      if (e->callee != orig_callee)
	orig_callee->aux = (void *) node;
      flatten_function (e->callee, early, false);
      if (e->callee != orig_callee)
	orig_callee->aux = NULL;
    }

  node->aux = NULL;
  cgraph_node *where = node->inlined_to ? node->inlined_to : node;
  if (update && opt_for_fn (where->decl, optimize))
    ipa_update_overall_fn_summary (where);
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

  while (node->callers && !node->inlined_to)
    {
      struct cgraph_node *caller = node->callers->caller;

      if (!can_inline_edge_p (node->callers, true)
	  || !can_inline_edge_by_limits_p (node->callers, true)
	  || node->callers->recursive_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "Uninlinable call found; giving up.\n");
	  *num_calls = 0;
	  return false;
	}

      if (dump_file)
	{
	  cgraph_node *ultimate = node->ultimate_alias_target ();
	  fprintf (dump_file,
		   "\nInlining %s size %i.\n",
		   ultimate->name (),
		   ipa_size_summaries->get (ultimate)->size);
	  fprintf (dump_file,
		   " Called once from %s %i insns.\n",
		   node->callers->caller->name (),
		   ipa_size_summaries->get (node->callers->caller)->size);
	}

      /* Remember which callers we inlined to, delaying updating the
	 overall summary.  */
      callers->add (node->callers->caller);
      inline_call (node->callers, true, NULL, NULL, false, &callee_removed);
      if (dump_file)
	fprintf (dump_file,
		 " Inlined into %s which now has %i size\n",
		 caller->name (),
		 ipa_size_summaries->get (caller)->size);
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
    ipa_update_overall_fn_summary ((*i)->inlined_to ? (*i)->inlined_to : *i);
  return res;
}

/* Output overall time estimate.  */
static void
dump_overall_stats (void)
{
  sreal sum_weighted = 0, sum = 0;
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->inlined_to
	&& !node->alias)
      {
	ipa_fn_summary *s = ipa_fn_summaries->get (node);
	if (s != NULL)
	  {
	  sum += s->time;
	  if (node->count.ipa ().initialized_p ())
	    sum_weighted += s->time * node->count.ipa ().to_gcov_type ();
	  }
      }
  fprintf (dump_file, "Overall time estimate: "
	   "%f weighted by profile: "
	   "%f\n", sum.to_double (), sum_weighted.to_double ());
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
  int64_t reason[CIF_N_REASONS][2];
  sreal reason_freq[CIF_N_REASONS];
  int i;
  struct cgraph_node *node;

  memset (reason, 0, sizeof (reason));
  for (i=0; i < CIF_N_REASONS; i++)
    reason_freq[i] = 0;
  FOR_EACH_DEFINED_FUNCTION (node)
  {
    struct cgraph_edge *e;
    for (e = node->callees; e; e = e->next_callee)
      {
	if (e->inline_failed)
	  {
	    if (e->count.ipa ().initialized_p ())
	      reason[(int) e->inline_failed][0] += e->count.ipa ().to_gcov_type ();
	    reason_freq[(int) e->inline_failed] += e->sreal_frequency ();
	    reason[(int) e->inline_failed][1] ++;
	    if (DECL_VIRTUAL_P (e->callee->decl)
		&& e->count.ipa ().initialized_p ())
	      {
		if (e->indirect_inlining_edge)
		  noninlined_virt_indir_cnt += e->count.ipa ().to_gcov_type ();
		else
		  noninlined_virt_cnt += e->count.ipa ().to_gcov_type ();
	      }
	    else if (e->count.ipa ().initialized_p ())
	      {
		if (e->indirect_inlining_edge)
		  noninlined_indir_cnt += e->count.ipa ().to_gcov_type ();
		else
		  noninlined_cnt += e->count.ipa ().to_gcov_type ();
	      }
	  }
	else if (e->count.ipa ().initialized_p ())
	  {
	    if (e->speculative)
	      {
		if (DECL_VIRTUAL_P (e->callee->decl))
		  inlined_speculative_ply += e->count.ipa ().to_gcov_type ();
		else
		  inlined_speculative += e->count.ipa ().to_gcov_type ();
	      }
	    else if (DECL_VIRTUAL_P (e->callee->decl))
	      {
		if (e->indirect_inlining_edge)
		  inlined_virt_indir_cnt += e->count.ipa ().to_gcov_type ();
		else
		  inlined_virt_cnt += e->count.ipa ().to_gcov_type ();
	      }
	    else
	      {
		if (e->indirect_inlining_edge)
		  inlined_indir_cnt += e->count.ipa ().to_gcov_type ();
		else
		  inlined_cnt += e->count.ipa ().to_gcov_type ();
	      }
	  }
      }
    for (e = node->indirect_calls; e; e = e->next_callee)
      if (e->indirect_info->polymorphic
	  & e->count.ipa ().initialized_p ())
	indirect_poly_cnt += e->count.ipa ().to_gcov_type ();
      else if (e->count.ipa ().initialized_p ())
	indirect_cnt += e->count.ipa ().to_gcov_type ();
  }
  if (max_count.initialized_p ())
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
	       "%" PRId64 " + still indirect "
	       "%" PRId64 " + still indirect polymorphic "
	       "%" PRId64 "\n", inlined_cnt,
	       inlined_speculative, inlined_speculative_ply,
	       inlined_indir_cnt, inlined_virt_cnt, inlined_virt_indir_cnt,
	       noninlined_cnt, noninlined_indir_cnt, noninlined_virt_cnt,
	       noninlined_virt_indir_cnt, indirect_cnt, indirect_poly_cnt);
      fprintf (dump_file, "Removed speculations ");
      spec_rem.dump (dump_file);
      fprintf (dump_file, "\n");
    }
  dump_overall_stats ();
  fprintf (dump_file, "\nWhy inlining failed?\n");
  for (i = 0; i < CIF_N_REASONS; i++)
    if (reason[i][1])
      fprintf (dump_file, "%-50s: %8i calls, %8f freq, %" PRId64" count\n",
	       cgraph_inline_failed_string ((cgraph_inline_failed_t) i),
	       (int) reason[i][1], reason_freq[i].to_double (), reason[i][0]);
}

/* Called when node is removed.  */

static void
flatten_remove_node_hook (struct cgraph_node *node, void *data)
{
  if (lookup_attribute ("flatten", DECL_ATTRIBUTES (node->decl)) == NULL)
    return;

  hash_set<struct cgraph_node *> *removed
    = (hash_set<struct cgraph_node *> *) data;
  removed->add (node);
}

/* Decide on the inlining.  We do so in the topological order to avoid
   expenses on updating data structures.  */

static unsigned int
ipa_inline (void)
{
  struct cgraph_node *node;
  int nnodes;
  struct cgraph_node **order;
  int i, j;
  int cold;
  bool remove_functions = false;

  order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);

  if (dump_file)
    ipa_dump_fn_summaries (dump_file);

  nnodes = ipa_reverse_postorder (order);
  spec_rem = profile_count::zero ();

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

  /* First shrink order array, so that it only contains nodes with
     flatten attribute.  */
  for (i = nnodes - 1, j = i; i >= 0; i--)
    {
      node = order[i];
      if (node->definition
	  && lookup_attribute ("flatten",
			       DECL_ATTRIBUTES (node->decl)) != NULL)
	order[j--] = order[i];
    }

  /* After the above loop, order[j + 1] ... order[nnodes - 1] contain
     nodes with flatten attribute.  If there is more than one such
     node, we need to register a node removal hook, as flatten_function
     could remove other nodes with flatten attribute.  See PR82801.  */
  struct cgraph_node_hook_list *node_removal_hook_holder = NULL;
  hash_set<struct cgraph_node *> *flatten_removed_nodes = NULL;
  if (j < nnodes - 2)
    {
      flatten_removed_nodes = new hash_set<struct cgraph_node *>;
      node_removal_hook_holder
	= symtab->add_cgraph_removal_hook (&flatten_remove_node_hook,
					   flatten_removed_nodes);
    }

  /* In the first pass handle functions to be flattened.  Do this with
     a priority so none of our later choices will make this impossible.  */
  for (i = nnodes - 1; i > j; i--)
    {
      node = order[i];
      if (flatten_removed_nodes
	  && flatten_removed_nodes->contains (node))
	continue;

      /* Handle nodes to be flattened.
	 Ideally when processing callees we stop inlining at the
	 entry of cycles, possibly cloning that entry point and
	 try to flatten itself turning it into a self-recursive
	 function.  */
      if (dump_file)
	fprintf (dump_file, "Flattening %s\n", node->name ());
      flatten_function (node, false, true);
    }

  if (j < nnodes - 2)
    {
      symtab->remove_cgraph_removal_hook (node_removal_hook_holder);
      delete flatten_removed_nodes;
    }
  free (order);

  if (dump_file)
    dump_overall_stats ();

  inline_small_functions ();

  gcc_assert (symtab->state == IPA_SSA);
  symtab->state = IPA_SSA_AFTER_INLINING;
  /* Do first after-inlining removal.  We want to remove all "stale" extern
     inline functions and virtual functions so we really know what is called
     once.  */
  symtab->remove_unreachable_nodes (dump_file);

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
     and then function that does the main optimization, we should inline
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

	  if (!opt_for_fn (node->decl, optimize)
	      || !opt_for_fn (node->decl, flag_inline_functions_called_once))
	    continue;

	  for (edge = node->callees; edge; edge = next)
	    {
	      next = edge->next_callee;
	      if (edge->speculative && !speculation_useful_p (edge, false))
		{
		  if (edge->count.ipa ().initialized_p ())
		    spec_rem += edge->count.ipa ();
		  edge->resolve_speculation ();
		  update = true;
		  remove_functions = true;
		}
	    }
	  if (update)
	    {
	      struct cgraph_node *where = node->inlined_to
					  ? node->inlined_to : node;
	      reset_edge_caches (where);
	      ipa_update_overall_fn_summary (where);
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
  ipa_free_all_structures_after_iinln ();

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE,
		 "\nInlined %i calls, eliminated %i functions\n\n",
		 ncalls_inlined, nfunctions_inlined);
  if (dump_file)
    dump_inline_stats ();

  if (dump_file)
    ipa_dump_fn_summaries (dump_file);
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
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  Not inlining recursive call to %C.\n",
			     e->callee);
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

      if (dump_enabled_p ())
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, e->call_stmt,
			 "  Inlining %C into %C (always_inline).\n",
			 e->callee, e->caller);
      inline_call (e, true, NULL, NULL, false);
      inlined = true;
    }
  if (inlined)
    ipa_update_overall_fn_summary (node);

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

      /* We can encounter not-yet-analyzed function during
	 early inlining on callgraphs with strongly
	 connected components.  */
      ipa_fn_summary *s = ipa_fn_summaries->get (callee);
      if (s == NULL || !s->inlinable || !e->inline_failed)
	continue;

      /* Do not consider functions not declared inline.  */
      if (!DECL_DECLARED_INLINE_P (callee->decl)
	  && !opt_for_fn (node->decl, flag_inline_small_functions)
	  && !opt_for_fn (node->decl, flag_inline_functions))
	continue;

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, e->call_stmt,
			 "Considering inline candidate %C.\n",
			 callee);

      if (!can_early_inline_edge_p (e))
	continue;

      if (e->recursive_p ())
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, e->call_stmt,
			     "  Not inlining: recursive call.\n");
	  continue;
	}

      if (!want_early_inline_function_p (e))
	continue;

      if (dump_enabled_p ())
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, e->call_stmt,
			 " Inlining %C into %C.\n",
			 callee, e->caller);
      inline_call (e, true, NULL, NULL, false);
      inlined = true;
    }

  if (inlined)
    ipa_update_overall_fn_summary (node);

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
      if (dump_enabled_p ())
	dump_printf (MSG_OPTIMIZED_LOCATIONS,
		     "Flattening %C\n", node);
      flatten_function (node, true, true);
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
	      /* We can enounter not-yet-analyzed function during
		 early inlining on callgraphs with strongly
		 connected components.  */
	      ipa_call_summary *es = ipa_call_summaries->get_create (edge);
	      es->call_stmt_size
		= estimate_num_insns (edge->call_stmt, &eni_size_weights);
	      es->call_stmt_time
		= estimate_num_insns (edge->call_stmt, &eni_time_weights);
	    }
	  ipa_update_overall_fn_summary (node);
	  inlined = false;
	  timevar_pop (TV_INTEGRATION);
	}
      /* We iterate incremental inlining to get trivial cases of indirect
	 inlining.  */
      while (iterations < param_early_inliner_max_iterations
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
	      ipa_call_summary *es = ipa_call_summaries->get_create (edge);
	      es->call_stmt_size
		= estimate_num_insns (edge->call_stmt, &eni_size_weights);
	      es->call_stmt_time
		= estimate_num_insns (edge->call_stmt, &eni_time_weights);
	    }
	  if (iterations < param_early_inliner_max_iterations - 1)
	    ipa_update_overall_fn_summary (node);
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
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
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
