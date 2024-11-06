/* Interprocedural constant propagation
   Copyright (C) 2005-2024 Free Software Foundation, Inc.

   Contributed by Razya Ladelsky <RAZYA@il.ibm.com> and Martin Jambor
   <mjambor@suse.cz>

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

/* Interprocedural constant propagation (IPA-CP).

   The goal of this transformation is to

   1) discover functions which are always invoked with some arguments with the
      same known constant values and modify the functions so that the
      subsequent optimizations can take advantage of the knowledge, and

   2) partial specialization - create specialized versions of functions
      transformed in this way if some parameters are known constants only in
      certain contexts but the estimated tradeoff between speedup and cost size
      is deemed good.

   The algorithm also propagates types and attempts to perform type based
   devirtualization.  Types are propagated much like constants.

   The algorithm basically consists of three stages.  In the first, functions
   are analyzed one at a time and jump functions are constructed for all known
   call-sites.  In the second phase, the pass propagates information from the
   jump functions across the call to reveal what values are available at what
   call sites, performs estimations of effects of known values on functions and
   their callees, and finally decides what specialized extra versions should be
   created.  In the third, the special versions materialize and appropriate
   calls are redirected.

   The algorithm used is to a certain extent based on "Interprocedural Constant
   Propagation", by David Callahan, Keith D Cooper, Ken Kennedy, Linda Torczon,
   Comp86, pg 152-161 and "A Methodology for Procedure Cloning" by Keith D
   Cooper, Mary W. Hall, and Ken Kennedy.


   First stage - intraprocedural analysis
   =======================================

   This phase computes jump_function and modification flags.

   A jump function for a call-site represents the values passed as an actual
   arguments of a given call-site. In principle, there are three types of
   values:

   Pass through - the caller's formal parameter is passed as an actual
		  argument, plus an operation on it can be performed.
   Constant - a constant is passed as an actual argument.
   Unknown - neither of the above.

   All jump function types are described in detail in ipa-prop.h, together with
   the data structures that represent them and methods of accessing them.

   ipcp_generate_summary() is the main function of the first stage.

   Second stage - interprocedural analysis
   ========================================

   This stage is itself divided into two phases.  In the first, we propagate
   known values over the call graph, in the second, we make cloning decisions.
   It uses a different algorithm than the original Callahan's paper.

   First, we traverse the functions topologically from callers to callees and,
   for each strongly connected component (SCC), we propagate constants
   according to previously computed jump functions.  We also record what known
   values depend on other known values and estimate local effects.  Finally, we
   propagate cumulative information about these effects from dependent values
   to those on which they depend.

   Second, we again traverse the call graph in the same topological order and
   make clones for functions which we know are called with the same values in
   all contexts and decide about extra specialized clones of functions just for
   some contexts - these decisions are based on both local estimates and
   cumulative estimates propagated from callees.

   ipcp_propagate_stage() and ipcp_decision_stage() together constitute the
   third stage.

   Third phase - materialization of clones, call statement updates.
   ============================================

   This stage is currently performed by call graph code (mainly in cgraphunit.cc
   and tree-inline.cc) according to instructions inserted to the call graph by
   the second stage.  */

#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "predict.h"
#include "sreal.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "tree-ssa-ccp.h"
#include "stringpool.h"
#include "attribs.h"
#include "dbgcnt.h"
#include "symtab-clones.h"
#include "gimple-range.h"


/* Allocation pools for values and their sources in ipa-cp.  */

object_allocator<ipcp_value<tree> > ipcp_cst_values_pool
  ("IPA-CP constant values");

object_allocator<ipcp_value<ipa_polymorphic_call_context> >
  ipcp_poly_ctx_values_pool ("IPA-CP polymorphic contexts");

object_allocator<ipcp_value_source<tree> > ipcp_sources_pool
  ("IPA-CP value sources");

object_allocator<ipcp_agg_lattice> ipcp_agg_lattice_pool
  ("IPA_CP aggregate lattices");

/* Base count to use in heuristics when using profile feedback.  */

static profile_count base_count;

/* Original overall size of the program.  */

static long overall_size, orig_overall_size;

/* Node name to unique clone suffix number map.  */
static hash_map<const char *, unsigned> *clone_num_suffixes;

/* Return the param lattices structure corresponding to the Ith formal
   parameter of the function described by INFO.  */
static inline class ipcp_param_lattices *
ipa_get_parm_lattices (class ipa_node_params *info, int i)
{
  gcc_assert (i >= 0 && i < ipa_get_param_count (info));
  gcc_checking_assert (!info->ipcp_orig_node);
  return &(info->lattices[i]);
}

/* Return the lattice corresponding to the scalar value of the Ith formal
   parameter of the function described by INFO.  */
static inline ipcp_lattice<tree> *
ipa_get_scalar_lat (class ipa_node_params *info, int i)
{
  class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
  return &plats->itself;
}

/* Return the lattice corresponding to the scalar value of the Ith formal
   parameter of the function described by INFO.  */
static inline ipcp_lattice<ipa_polymorphic_call_context> *
ipa_get_poly_ctx_lat (class ipa_node_params *info, int i)
{
  class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
  return &plats->ctxlat;
}

/* Return whether LAT is a lattice with a single constant and without an
   undefined value.  */

template <typename valtype>
inline bool
ipcp_lattice<valtype>::is_single_const ()
{
  if (bottom || contains_variable || values_count != 1)
    return false;
  else
    return true;
}

/* Return true iff X and Y should be considered equal values by IPA-CP.  */

bool
values_equal_for_ipcp_p (tree x, tree y)
{
  gcc_checking_assert (x != NULL_TREE && y != NULL_TREE);

  if (x == y)
    return true;

  if (TREE_CODE (x) == ADDR_EXPR
      && TREE_CODE (y) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (x, 0)) == CONST_DECL
	  || (TREE_CODE (TREE_OPERAND (x, 0)) == VAR_DECL
	      && DECL_IN_CONSTANT_POOL (TREE_OPERAND (x, 0))))
      && (TREE_CODE (TREE_OPERAND (y, 0)) == CONST_DECL
	  || (TREE_CODE (TREE_OPERAND (y, 0)) == VAR_DECL
	      && DECL_IN_CONSTANT_POOL (TREE_OPERAND (y, 0)))))
    return TREE_OPERAND (x, 0) == TREE_OPERAND (y, 0)
	   || operand_equal_p (DECL_INITIAL (TREE_OPERAND (x, 0)),
			       DECL_INITIAL (TREE_OPERAND (y, 0)), 0);
  else
    return operand_equal_p (x, y, 0);
}

/* Print V which is extracted from a value in a lattice to F.  */

static void
print_ipcp_constant_value (FILE * f, ipa_polymorphic_call_context v)
{
  v.dump(f, false);
}

/* Print a lattice LAT to F.  */

template <typename valtype>
void
ipcp_lattice<valtype>::print (FILE * f, bool dump_sources, bool dump_benefits)
{
  ipcp_value<valtype> *val;
  bool prev = false;

  if (bottom)
    {
      fprintf (f, "BOTTOM\n");
      return;
    }

  if (!values_count && !contains_variable)
    {
      fprintf (f, "TOP\n");
      return;
    }

  if (contains_variable)
    {
      fprintf (f, "VARIABLE");
      prev = true;
      if (dump_benefits)
	fprintf (f, "\n");
    }

  for (val = values; val; val = val->next)
    {
      if (dump_benefits && prev)
	fprintf (f, "               ");
      else if (!dump_benefits && prev)
	fprintf (f, ", ");
      else
	prev = true;

      print_ipcp_constant_value (f, val->value);

      if (dump_sources)
	{
	  ipcp_value_source<valtype> *s;

	  if (val->self_recursion_generated_p ())
	    fprintf (f, " [self_gen(%i), from:",
		     val->self_recursion_generated_level);
	  else
	    fprintf (f, " [scc: %i, from:", val->scc_no);
	  for (s = val->sources; s; s = s->next)
	    fprintf (f, " %i(%f)", s->cs->caller->order,
		     s->cs->sreal_frequency ().to_double ());
	  fprintf (f, "]");
	}

      if (dump_benefits)
	fprintf (f, " [loc_time: %g, loc_size: %i, "
		 "prop_time: %g, prop_size: %i]\n",
		 val->local_time_benefit.to_double (), val->local_size_cost,
		 val->prop_time_benefit.to_double (), val->prop_size_cost);
    }
  if (!dump_benefits)
    fprintf (f, "\n");
}

void
ipcp_bits_lattice::print (FILE *f)
{
  if (top_p ())
    fprintf (f, "         Bits unknown (TOP)\n");
  else if (bottom_p ())
    fprintf (f, "         Bits unusable (BOTTOM)\n");
  else
    {
      fprintf (f, "         Bits: value = "); print_hex (get_value (), f);
      fprintf (f, ", mask = "); print_hex (get_mask (), f);
      fprintf (f, "\n");
    }
}

/* Print value range lattice to F.  */

void
ipcp_vr_lattice::print (FILE * f)
{
  m_vr.dump (f);
}

/* Print all ipcp_lattices of all functions to F.  */

static void
print_all_lattices (FILE * f, bool dump_sources, bool dump_benefits)
{
  struct cgraph_node *node;
  int i, count;

  fprintf (f, "\nLattices:\n");
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      class ipa_node_params *info;

      info = ipa_node_params_sum->get (node);
      /* Skip unoptimized functions and constprop clones since we don't make
	 lattices for them.  */
      if (!info || info->ipcp_orig_node)
	continue;
      fprintf (f, "  Node: %s:\n", node->dump_name ());
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_agg_lattice *aglat;
	  class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  fprintf (f, "    param [%d]: ", i);
	  plats->itself.print (f, dump_sources, dump_benefits);
	  fprintf (f, "         ctxs: ");
	  plats->ctxlat.print (f, dump_sources, dump_benefits);
	  plats->bits_lattice.print (f);
	  fprintf (f, "         ");
	  plats->m_value_range.print (f);
	  fprintf (f, "\n");
	  if (plats->virt_call)
	    fprintf (f, "        virt_call flag set\n");

	  if (plats->aggs_bottom)
	    {
	      fprintf (f, "        AGGS BOTTOM\n");
	      continue;
	    }
	  if (plats->aggs_contain_variable)
	    fprintf (f, "        AGGS VARIABLE\n");
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    {
	      fprintf (f, "        %soffset " HOST_WIDE_INT_PRINT_DEC ": ",
		       plats->aggs_by_ref ? "ref " : "", aglat->offset);
	      aglat->print (f, dump_sources, dump_benefits);
	    }
	}
    }
}

/* Determine whether it is at all technically possible to create clones of NODE
   and store this information in the ipa_node_params structure associated
   with NODE.  */

static void
determine_versionability (struct cgraph_node *node,
			  class ipa_node_params *info)
{
  const char *reason = NULL;

  /* There are a number of generic reasons functions cannot be versioned.  We
     also cannot remove parameters if there are type attributes such as fnspec
     present.  */
  if (node->alias || node->thunk)
    reason = "alias or thunk";
  else if (!node->versionable)
    reason = "not a tree_versionable_function";
  else if (node->get_availability () <= AVAIL_INTERPOSABLE)
    reason = "insufficient body availability";
  else if (!opt_for_fn (node->decl, optimize)
	   || !opt_for_fn (node->decl, flag_ipa_cp))
    reason = "non-optimized function";
  else if (lookup_attribute ("omp declare simd", DECL_ATTRIBUTES (node->decl)))
    {
      /* Ideally we should clone the SIMD clones themselves and create
	 vector copies of them, so IPA-cp and SIMD clones can happily
	 coexist, but that may not be worth the effort.  */
      reason = "function has SIMD clones";
    }
  else if (lookup_attribute ("target_clones", DECL_ATTRIBUTES (node->decl)))
    {
      /* Ideally we should clone the target clones themselves and create
	 copies of them, so IPA-cp and target clones can happily
	 coexist, but that may not be worth the effort.  */
      reason = "function target_clones attribute";
    }
  /* Don't clone decls local to a comdat group; it breaks and for C++
     decloned constructors, inlining is always better anyway.  */
  else if (node->comdat_local_p ())
    reason = "comdat-local function";
  else if (node->calls_comdat_local)
    {
      /* TODO: call is versionable if we make sure that all
	 callers are inside of a comdat group.  */
      reason = "calls comdat-local function";
    }

  /* Functions calling BUILT_IN_VA_ARG_PACK and BUILT_IN_VA_ARG_PACK_LEN
     work only when inlined.  Cloning them may still lead to better code
     because ipa-cp will not give up on cloning further.  If the function is
     external this however leads to wrong code because we may end up producing
     offline copy of the function.  */
  if (DECL_EXTERNAL (node->decl))
    for (cgraph_edge *edge = node->callees; !reason && edge;
	 edge = edge->next_callee)
      if (fndecl_built_in_p (edge->callee->decl, BUILT_IN_NORMAL))
        {
	  if (DECL_FUNCTION_CODE (edge->callee->decl) == BUILT_IN_VA_ARG_PACK)
	    reason = "external function which calls va_arg_pack";
	  if (DECL_FUNCTION_CODE (edge->callee->decl)
	      == BUILT_IN_VA_ARG_PACK_LEN)
	    reason = "external function which calls va_arg_pack_len";
        }

  if (reason && dump_file && !node->alias && !node->thunk)
    fprintf (dump_file, "Function %s is not versionable, reason: %s.\n",
	     node->dump_name (), reason);

  info->versionable = (reason == NULL);
}

/* Return true if it is at all technically possible to create clones of a
   NODE.  */

static bool
ipcp_versionable_function_p (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  return info && info->versionable;
}

/* Structure holding accumulated information about callers of a node.  */

struct caller_statistics
{
  /* If requested (see below), self-recursive call counts are summed into this
     field.  */
  profile_count rec_count_sum;
  /* The sum of all ipa counts of all the other (non-recursive) calls.  */
  profile_count count_sum;
  /* Sum of all frequencies for all calls.  */
  sreal freq_sum;
  /* Number of calls and hot calls respectively.  */
  int n_calls, n_hot_calls;
  /* If itself is set up, also count the number of non-self-recursive
     calls.  */
  int n_nonrec_calls;
  /* If non-NULL, this is the node itself and calls from it should have their
     counts included in rec_count_sum and not count_sum.  */
  cgraph_node *itself;
};

/* Initialize fields of STAT to zeroes and optionally set it up so that edges
   from IGNORED_CALLER are not counted.  */

static inline void
init_caller_stats (caller_statistics *stats, cgraph_node *itself = NULL)
{
  stats->rec_count_sum = profile_count::zero ();
  stats->count_sum = profile_count::zero ();
  stats->n_calls = 0;
  stats->n_hot_calls = 0;
  stats->n_nonrec_calls = 0;
  stats->freq_sum = 0;
  stats->itself = itself;
}

/* Worker callback of cgraph_for_node_and_aliases accumulating statistics of
   non-thunk incoming edges to NODE.  */

static bool
gather_caller_stats (struct cgraph_node *node, void *data)
{
  struct caller_statistics *stats = (struct caller_statistics *) data;
  struct cgraph_edge *cs;

  for (cs = node->callers; cs; cs = cs->next_caller)
    if (!cs->caller->thunk)
      {
	ipa_node_params *info = ipa_node_params_sum->get (cs->caller);
	if (info && info->node_dead)
	  continue;

	if (cs->count.ipa ().initialized_p ())
	  {
	    if (stats->itself && stats->itself == cs->caller)
	      stats->rec_count_sum += cs->count.ipa ();
	    else
	      stats->count_sum += cs->count.ipa ();
	  }
	stats->freq_sum += cs->sreal_frequency ();
	stats->n_calls++;
	if (stats->itself && stats->itself != cs->caller)
	  stats->n_nonrec_calls++;

	if (cs->maybe_hot_p ())
	  stats->n_hot_calls ++;
      }
  return false;

}

/* Return true if this NODE is viable candidate for cloning.  */

static bool
ipcp_cloning_candidate_p (struct cgraph_node *node)
{
  struct caller_statistics stats;

  gcc_checking_assert (node->has_gimple_body_p ());

  if (!opt_for_fn (node->decl, flag_ipa_cp_clone))
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; "
		 "-fipa-cp-clone disabled.\n",
		 node->dump_name ());
      return false;
    }

  if (node->optimize_for_size_p ())
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; "
		 "optimizing it for size.\n",
		 node->dump_name ());
      return false;
    }

  init_caller_stats (&stats);
  node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats, false);

  if (ipa_size_summaries->get (node)->self_size < stats.n_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Considering %s for cloning; code might shrink.\n",
		 node->dump_name ());
      return true;
    }

  /* When profile is available and function is hot, propagate into it even if
     calls seems cold; constant propagation can improve function's speed
     significantly.  */
  if (stats.count_sum > profile_count::zero ()
      && node->count.ipa ().initialized_p ())
    {
      if (stats.count_sum > node->count.ipa ().apply_scale (90, 100))
	{
	  if (dump_file)
	    fprintf (dump_file, "Considering %s for cloning; "
		     "usually called directly.\n",
		     node->dump_name ());
	  return true;
	}
    }
  if (!stats.n_hot_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; no hot calls.\n",
		 node->dump_name ());
      return false;
    }
  if (dump_file)
    fprintf (dump_file, "Considering %s for cloning.\n",
	     node->dump_name ());
  return true;
}

template <typename valtype>
class value_topo_info
{
public:
  /* Head of the linked list of topologically sorted values. */
  ipcp_value<valtype> *values_topo;
  /* Stack for creating SCCs, represented by a linked list too.  */
  ipcp_value<valtype> *stack;
  /* Counter driving the algorithm in add_val_to_toposort.  */
  int dfs_counter;

  value_topo_info () : values_topo (NULL), stack (NULL), dfs_counter (0)
  {}
  void add_val (ipcp_value<valtype> *cur_val);
  void propagate_effects ();
};

/* Arrays representing a topological ordering of call graph nodes and a stack
   of nodes used during constant propagation and also data required to perform
   topological sort of values and propagation of benefits in the determined
   order.  */

class ipa_topo_info
{
public:
  /* Array with obtained topological order of cgraph nodes.  */
  struct cgraph_node **order;
  /* Stack of cgraph nodes used during propagation within SCC until all values
     in the SCC stabilize.  */
  struct cgraph_node **stack;
  int nnodes, stack_top;

  value_topo_info<tree> constants;
  value_topo_info<ipa_polymorphic_call_context> contexts;

  ipa_topo_info () : order(NULL), stack(NULL), nnodes(0), stack_top(0),
    constants ()
  {}
};

/* Skip edges from and to nodes without ipa_cp enabled.
   Ignore not available symbols.  */

static bool
ignore_edge_p (cgraph_edge *e)
{
  enum availability avail;
  cgraph_node *ultimate_target
    = e->callee->function_or_virtual_thunk_symbol (&avail, e->caller);

  return (avail <= AVAIL_INTERPOSABLE
	  || !opt_for_fn (ultimate_target->decl, optimize)
	  || !opt_for_fn (ultimate_target->decl, flag_ipa_cp));
}

/* Allocate the arrays in TOPO and topologically sort the nodes into order.  */

static void
build_toporder_info (class ipa_topo_info *topo)
{
  topo->order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  topo->stack = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);

  gcc_checking_assert (topo->stack_top == 0);
  topo->nnodes = ipa_reduced_postorder (topo->order, true,
					ignore_edge_p);
}

/* Free information about strongly connected components and the arrays in
   TOPO.  */

static void
free_toporder_info (class ipa_topo_info *topo)
{
  ipa_free_postorder_info ();
  free (topo->order);
  free (topo->stack);
}

/* Add NODE to the stack in TOPO, unless it is already there.  */

static inline void
push_node_to_stack (class ipa_topo_info *topo, struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  if (info->node_enqueued)
    return;
  info->node_enqueued = 1;
  topo->stack[topo->stack_top++] = node;
}

/* Pop a node from the stack in TOPO and return it or return NULL if the stack
   is empty.  */

static struct cgraph_node *
pop_node_from_stack (class ipa_topo_info *topo)
{
  if (topo->stack_top)
    {
      struct cgraph_node *node;
      topo->stack_top--;
      node = topo->stack[topo->stack_top];
      ipa_node_params_sum->get (node)->node_enqueued = 0;
      return node;
    }
  else
    return NULL;
}

/* Set lattice LAT to bottom and return true if it previously was not set as
   such.  */

template <typename valtype>
inline bool
ipcp_lattice<valtype>::set_to_bottom ()
{
  bool ret = !bottom;
  bottom = true;
  return ret;
}

/* Mark lattice as containing an unknown value and return true if it previously
   was not marked as such.  */

template <typename valtype>
inline bool
ipcp_lattice<valtype>::set_contains_variable ()
{
  bool ret = !contains_variable;
  contains_variable = true;
  return ret;
}

/* Set all aggregate lattices in PLATS to bottom and return true if they were
   not previously set as such.  */

static inline bool
set_agg_lats_to_bottom (class ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_bottom;
  plats->aggs_bottom = true;
  return ret;
}

/* Mark all aggregate lattices in PLATS as containing an unknown value and
   return true if they were not previously marked as such.  */

static inline bool
set_agg_lats_contain_variable (class ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_contain_variable;
  plats->aggs_contain_variable = true;
  return ret;
}

bool
ipcp_vr_lattice::meet_with (const ipcp_vr_lattice &other)
{
  return meet_with_1 (other.m_vr);
}

/* Meet the current value of the lattice with the range described by
   P_VR.  */

bool
ipcp_vr_lattice::meet_with (const vrange &p_vr)
{
  return meet_with_1 (p_vr);
}

/* Meet the current value of the lattice with the range described by
   OTHER_VR.  Return TRUE if anything changed.  */

bool
ipcp_vr_lattice::meet_with_1 (const vrange &other_vr)
{
  if (bottom_p ())
    return false;

  if (other_vr.varying_p ())
    return set_to_bottom ();

  bool res;
  if (flag_checking)
    {
      value_range save (m_vr);
      res = m_vr.union_ (other_vr);
      gcc_assert (res == (m_vr != save));
    }
  else
    res = m_vr.union_ (other_vr);
  return res;
}

/* Return true if value range information in the lattice is yet unknown.  */

bool
ipcp_vr_lattice::top_p () const
{
  return m_vr.undefined_p ();
}

/* Return true if value range information in the lattice is known to be
   unusable.  */

bool
ipcp_vr_lattice::bottom_p () const
{
  return m_vr.varying_p ();
}

/* Set value range information in the lattice to bottom.  Return true if it
   previously was in a different state.  */

bool
ipcp_vr_lattice::set_to_bottom ()
{
  if (m_vr.varying_p ())
    return false;

  /* Setting an unsupported type here forces the temporary to default
     to unsupported_range, which can handle VARYING/DEFINED ranges,
     but nothing else (union, intersect, etc).  This allows us to set
     bottoms on any ranges, and is safe as all users of the lattice
     check for bottom first.  */
  m_vr.set_type (void_type_node);
  m_vr.set_varying (void_type_node);

  return true;
}

/* Set lattice value to bottom, if it already isn't the case.  */

bool
ipcp_bits_lattice::set_to_bottom ()
{
  if (bottom_p ())
    return false;
  m_lattice_val = IPA_BITS_VARYING;
  m_value = 0;
  m_mask = -1;
  return true;
}

/* Set to constant if it isn't already. Only meant to be called
   when switching state from TOP.  */

bool
ipcp_bits_lattice::set_to_constant (widest_int value, widest_int mask)
{
  gcc_assert (top_p ());
  m_lattice_val = IPA_BITS_CONSTANT;
  m_value = wi::bit_and (wi::bit_not (mask), value);
  m_mask = mask;
  return true;
}

/* Return true if any of the known bits are non-zero.  */

bool
ipcp_bits_lattice::known_nonzero_p () const
{
  if (!constant_p ())
    return false;
  return wi::ne_p (wi::bit_and (wi::bit_not (m_mask), m_value), 0);
}

/* Convert operand to value, mask form.  */

void
ipcp_bits_lattice::get_value_and_mask (tree operand, widest_int *valuep, widest_int *maskp)
{
  wide_int get_nonzero_bits (const_tree);

  if (TREE_CODE (operand) == INTEGER_CST)
    {
      *valuep = wi::to_widest (operand);
      *maskp = 0;
    }
  else
    {
      *valuep = 0;
      *maskp = -1;
    }
}

/* Meet operation, similar to ccp_lattice_meet, we xor values
   if this->value, value have different values at same bit positions, we want
   to drop that bit to varying. Return true if mask is changed.
   This function assumes that the lattice value is in CONSTANT state.  If
   DROP_ALL_ONES, mask out any known bits with value one afterwards.  */

bool
ipcp_bits_lattice::meet_with_1 (widest_int value, widest_int mask,
				unsigned precision, bool drop_all_ones)
{
  gcc_assert (constant_p ());

  widest_int old_mask = m_mask;
  m_mask = (m_mask | mask) | (m_value ^ value);
  if (drop_all_ones)
    m_mask |= m_value;
  m_value &= ~m_mask;

  if (wi::sext (m_mask, precision) == -1)
    return set_to_bottom ();

  return m_mask != old_mask;
}

/* Meet the bits lattice with operand
   described by <value, mask, sgn, precision.  */

bool
ipcp_bits_lattice::meet_with (widest_int value, widest_int mask,
			      unsigned precision)
{
  if (bottom_p ())
    return false;

  if (top_p ())
    {
      if (wi::sext (mask, precision) == -1)
	return set_to_bottom ();
      return set_to_constant (value, mask);
    }

  return meet_with_1 (value, mask, precision, false);
}

/* Meet bits lattice with the result of bit_value_binop (other, operand)
   if code is binary operation or bit_value_unop (other) if code is unary op.
   In the case when code is nop_expr, no adjustment is required.  If
   DROP_ALL_ONES, mask out any known bits with value one afterwards.  */

bool
ipcp_bits_lattice::meet_with (ipcp_bits_lattice& other, unsigned precision,
			      signop sgn, enum tree_code code, tree operand,
			      bool drop_all_ones)
{
  if (other.bottom_p ())
    return set_to_bottom ();

  if (bottom_p () || other.top_p ())
    return false;

  widest_int adjusted_value, adjusted_mask;

  if (TREE_CODE_CLASS (code) == tcc_binary)
    {
      tree type = TREE_TYPE (operand);
      widest_int o_value, o_mask;
      get_value_and_mask (operand, &o_value, &o_mask);

      bit_value_binop (code, sgn, precision, &adjusted_value, &adjusted_mask,
		       sgn, precision, other.get_value (), other.get_mask (),
		       TYPE_SIGN (type), TYPE_PRECISION (type), o_value, o_mask);

      if (wi::sext (adjusted_mask, precision) == -1)
	return set_to_bottom ();
    }

  else if (TREE_CODE_CLASS (code) == tcc_unary)
    {
      bit_value_unop (code, sgn, precision, &adjusted_value,
		      &adjusted_mask, sgn, precision, other.get_value (),
		      other.get_mask ());

      if (wi::sext (adjusted_mask, precision) == -1)
	return set_to_bottom ();
    }

  else
    return set_to_bottom ();

  if (top_p ())
    {
      if (drop_all_ones)
	{
	  adjusted_mask |= adjusted_value;
	  adjusted_value &= ~adjusted_mask;
	}
      if (wi::sext (adjusted_mask, precision) == -1)
	return set_to_bottom ();
      return set_to_constant (adjusted_value, adjusted_mask);
    }
  else
    return meet_with_1 (adjusted_value, adjusted_mask, precision,
			drop_all_ones);
}

/* Dump the contents of the list to FILE.  */

void
ipa_argagg_value_list::dump (FILE *f)
{
  bool comma = false;
  for (const ipa_argagg_value &av : m_elts)
    {
      fprintf (f, "%s %i[%u]=", comma ? "," : "",
	       av.index, av.unit_offset);
      print_generic_expr (f, av.value);
      if (av.by_ref)
	fprintf (f, "(by_ref)");
      if (av.killed)
	fprintf (f, "(killed)");
      comma = true;
    }
  fprintf (f, "\n");
}

/* Dump the contents of the list to stderr.  */

void
ipa_argagg_value_list::debug ()
{
  dump (stderr);
}

/* Return the item describing a constant stored for INDEX at UNIT_OFFSET or
   NULL if there is no such constant.  */

const ipa_argagg_value *
ipa_argagg_value_list::get_elt (int index, unsigned unit_offset) const
{
  ipa_argagg_value key;
  key.index = index;
  key.unit_offset = unit_offset;
  const ipa_argagg_value *res
    = std::lower_bound (m_elts.begin (), m_elts.end (), key,
			[] (const ipa_argagg_value &elt,
			    const ipa_argagg_value &val)
			{
			  if (elt.index < val.index)
			    return true;
			  if (elt.index > val.index)
			    return false;
			  if (elt.unit_offset < val.unit_offset)
			    return true;
			  return false;
			});

  if (res == m_elts.end ()
      || res->index != index
      || res->unit_offset != unit_offset)
    res = nullptr;

  /* TODO: perhaps remove the check (that the underlying array is indeed
     sorted) if it turns out it can be too slow? */
  if (!flag_checking)
    return res;

  const ipa_argagg_value *slow_res = NULL;
  int prev_index = -1;
  unsigned prev_unit_offset = 0;
  for (const ipa_argagg_value &av : m_elts)
    {
      gcc_assert (prev_index < 0
		  || prev_index < av.index
		  || prev_unit_offset < av.unit_offset);
      prev_index = av.index;
      prev_unit_offset = av.unit_offset;
      if (av.index == index
	  && av.unit_offset == unit_offset)
	slow_res = &av;
    }
  gcc_assert (res == slow_res);

  return res;
}

/* Return the first item describing a constant stored for parameter with INDEX,
   regardless of offset or reference, or NULL if there is no such constant.  */

const ipa_argagg_value *
ipa_argagg_value_list::get_elt_for_index (int index) const
{
  const ipa_argagg_value *res
    = std::lower_bound (m_elts.begin (), m_elts.end (), index,
			[] (const ipa_argagg_value &elt, unsigned idx)
			{
			  return elt.index < idx;
			});
  if (res == m_elts.end ()
      || res->index != index)
    res = nullptr;
  return res;
}

/* Return the aggregate constant stored for INDEX at UNIT_OFFSET, not
   performing any check of whether value is passed by reference, or NULL_TREE
   if there is no such constant.  */

tree
ipa_argagg_value_list::get_value (int index, unsigned unit_offset) const
{
  const ipa_argagg_value *av = get_elt (index, unit_offset);
  return av ? av->value : NULL_TREE;
}

/* Return the aggregate constant stored for INDEX at UNIT_OFFSET, if it is
   passed by reference or not according to BY_REF, or NULL_TREE if there is
   no such constant.  */

tree
ipa_argagg_value_list::get_value (int index, unsigned unit_offset,
				    bool by_ref) const
{
  const ipa_argagg_value *av = get_elt (index, unit_offset);
  if (av && av->by_ref == by_ref)
    return av->value;
  return NULL_TREE;
}

/* Return true if all elements present in OTHER are also present in this
   list.  */

bool
ipa_argagg_value_list::superset_of_p (const ipa_argagg_value_list &other) const
{
  unsigned j = 0;
  for (unsigned i = 0; i < other.m_elts.size (); i++)
    {
      unsigned other_index = other.m_elts[i].index;
      unsigned other_offset = other.m_elts[i].unit_offset;

      while (j < m_elts.size ()
	     && (m_elts[j].index < other_index
		 || (m_elts[j].index == other_index
		     && m_elts[j].unit_offset < other_offset)))
       j++;

      if (j >= m_elts.size ()
	  || m_elts[j].index != other_index
	  || m_elts[j].unit_offset != other_offset
	  || m_elts[j].by_ref != other.m_elts[i].by_ref
	  || !m_elts[j].value
	  || !values_equal_for_ipcp_p (m_elts[j].value, other.m_elts[i].value))
	return false;
    }
  return true;
}

/* Push all items in this list that describe parameter SRC_INDEX into RES as
   ones describing DST_INDEX while subtracting UNIT_DELTA from their unit
   offsets but skip those which would end up with a negative offset.  */

void
ipa_argagg_value_list::push_adjusted_values (unsigned src_index,
					     unsigned dest_index,
					     unsigned unit_delta,
					     vec<ipa_argagg_value> *res) const
{
  const ipa_argagg_value *av = get_elt_for_index (src_index);
  if (!av)
    return;
  unsigned prev_unit_offset = 0;
  bool first = true;
  for (; av < m_elts.end (); ++av)
    {
      if (av->index > src_index)
	return;
      if (av->index == src_index
	  && (av->unit_offset >= unit_delta)
	  && av->value)
	{
	  ipa_argagg_value new_av;
	  gcc_checking_assert (av->value);
	  new_av.value = av->value;
	  new_av.unit_offset = av->unit_offset - unit_delta;
	  new_av.index = dest_index;
	  new_av.by_ref = av->by_ref;
	  gcc_assert (!av->killed);
	  new_av.killed = false;

	  /* Quick check that the offsets we push are indeed increasing.  */
	  gcc_assert (first
		      || new_av.unit_offset > prev_unit_offset);
	  prev_unit_offset = new_av.unit_offset;
	  first = false;

	  res->safe_push (new_av);
	}
    }
}

/* Push to RES information about single lattices describing aggregate values in
   PLATS as those describing parameter DEST_INDEX and the original offset minus
   UNIT_DELTA.  Return true if any item has been pushed to RES.  */

static bool
push_agg_values_from_plats (ipcp_param_lattices *plats, int dest_index,
			    unsigned unit_delta,
			    vec<ipa_argagg_value> *res)
{
  if (plats->aggs_contain_variable)
    return false;

  bool pushed_sth = false;
  bool first = true;
  unsigned prev_unit_offset = 0;
  for (struct ipcp_agg_lattice *aglat = plats->aggs; aglat; aglat = aglat->next)
    if (aglat->is_single_const ()
	&& (aglat->offset / BITS_PER_UNIT - unit_delta) >= 0)
      {
	ipa_argagg_value iav;
	iav.value = aglat->values->value;
	iav.unit_offset = aglat->offset / BITS_PER_UNIT - unit_delta;
	iav.index = dest_index;
	iav.by_ref = plats->aggs_by_ref;
	iav.killed = false;

	gcc_assert (first
		    || iav.unit_offset > prev_unit_offset);
	prev_unit_offset = iav.unit_offset;
	first = false;

	pushed_sth = true;
	res->safe_push (iav);
      }
  return pushed_sth;
}

/* Turn all values in LIST that are not present in OTHER into NULL_TREEs.
   Return the number of remaining valid entries.  */

static unsigned
intersect_argaggs_with (vec<ipa_argagg_value> &elts,
			const vec<ipa_argagg_value> &other)
{
  unsigned valid_entries = 0;
  unsigned j = 0;
  for (unsigned i = 0; i < elts.length (); i++)
    {
      if (!elts[i].value)
	continue;

      unsigned this_index = elts[i].index;
      unsigned this_offset = elts[i].unit_offset;

      while (j < other.length ()
	     && (other[j].index < this_index
		 || (other[j].index == this_index
		     && other[j].unit_offset < this_offset)))
	j++;

      if (j >= other.length ())
	{
	  elts[i].value = NULL_TREE;
	  continue;
	}

      if (other[j].index == this_index
	  && other[j].unit_offset == this_offset
	  && other[j].by_ref == elts[i].by_ref
	  && other[j].value
	  && values_equal_for_ipcp_p (other[j].value, elts[i].value))
	valid_entries++;
      else
	elts[i].value = NULL_TREE;
    }
  return valid_entries;
}

/* Mark bot aggregate and scalar lattices as containing an unknown variable,
   return true is any of them has not been marked as such so far.  */

static inline bool
set_all_contains_variable (class ipcp_param_lattices *plats)
{
  bool ret;
  ret = plats->itself.set_contains_variable ();
  ret |= plats->ctxlat.set_contains_variable ();
  ret |= set_agg_lats_contain_variable (plats);
  ret |= plats->bits_lattice.set_to_bottom ();
  ret |= plats->m_value_range.set_to_bottom ();
  return ret;
}

/* Worker of call_for_symbol_thunks_and_aliases, increment the integer DATA
   points to by the number of callers to NODE.  */

static bool
count_callers (cgraph_node *node, void *data)
{
  int *caller_count = (int *) data;

  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    /* Local thunks can be handled transparently, but if the thunk cannot
       be optimized out, count it as a real use.  */
    if (!cs->caller->thunk || !cs->caller->local)
      ++*caller_count;
  return false;
}

/* Worker of call_for_symbol_thunks_and_aliases, it is supposed to be called on
   the one caller of some other node.  Set the caller's corresponding flag.  */

static bool
set_single_call_flag (cgraph_node *node, void *)
{
  cgraph_edge *cs = node->callers;
  /* Local thunks can be handled transparently, skip them.  */
  while (cs && cs->caller->thunk && cs->caller->local)
    cs = cs->next_caller;
  if (cs)
    if (ipa_node_params* info = ipa_node_params_sum->get (cs->caller))
      {
	info->node_calling_single_call = true;
	return true;
      }
  return false;
}

/* Initialize ipcp_lattices.  */

static void
initialize_node_lattices (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  struct cgraph_edge *ie;
  bool disable = false, variable = false;
  int i;

  gcc_checking_assert (node->has_gimple_body_p ());

  if (!ipa_get_param_count (info))
    disable = true;
  else if (node->local)
    {
      int caller_count = 0;
      node->call_for_symbol_thunks_and_aliases (count_callers, &caller_count,
						true);
      gcc_checking_assert (caller_count > 0);
      if (caller_count == 1)
	node->call_for_symbol_thunks_and_aliases (set_single_call_flag,
						  NULL, true);
    }
  else
    {
      /* When cloning is allowed, we can assume that externally visible
	 functions are not called.  We will compensate this by cloning
	 later.  */
      if (ipcp_versionable_function_p (node)
	  && ipcp_cloning_candidate_p (node))
	variable = true;
      else
	disable = true;
    }

  if (dump_file && (dump_flags & TDF_DETAILS)
      && !node->alias && !node->thunk)
    {
      fprintf (dump_file, "Initializing lattices of %s\n",
	       node->dump_name ());
      if (disable || variable)
	fprintf (dump_file, "  Marking all lattices as %s\n",
		 disable ? "BOTTOM" : "VARIABLE");
    }

  auto_vec<bool, 16> surviving_params;
  bool pre_modified = false;

  clone_info *cinfo = clone_info::get (node);

  if (!disable && cinfo && cinfo->param_adjustments)
    {
      /* At the moment all IPA optimizations should use the number of
	 parameters of the prevailing decl as the m_always_copy_start.
	 Handling any other value would complicate the code below, so for the
	 time bing let's only assert it is so.  */
      gcc_assert ((cinfo->param_adjustments->m_always_copy_start
		   == ipa_get_param_count (info))
		  || cinfo->param_adjustments->m_always_copy_start < 0);

      pre_modified = true;
      cinfo->param_adjustments->get_surviving_params (&surviving_params);

      if (dump_file && (dump_flags & TDF_DETAILS)
	  && !node->alias && !node->thunk)
	{
	  bool first = true;
	  for (int j = 0; j < ipa_get_param_count (info); j++)
	    {
	      if (j < (int) surviving_params.length ()
		  && surviving_params[j])
		continue;
	      if (first)
		{
		  fprintf (dump_file,
			   "  The following parameters are dead on arrival:");
		  first = false;
		}
	      fprintf (dump_file, " %u", j);
	    }
	  if (!first)
	      fprintf (dump_file, "\n");
	}
    }

  for (i = 0; i < ipa_get_param_count (info); i++)
    {
      ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      tree type = ipa_get_type (info, i);
      if (disable
	  || !ipa_get_type (info, i)
	  || (pre_modified && (surviving_params.length () <= (unsigned) i
			       || !surviving_params[i])))
	{
	  plats->itself.set_to_bottom ();
	  plats->ctxlat.set_to_bottom ();
	  set_agg_lats_to_bottom (plats);
	  plats->bits_lattice.set_to_bottom ();
	  plats->m_value_range.init (type);
	  plats->m_value_range.set_to_bottom ();
	}
      else
	{
	  plats->m_value_range.init (type);
	  if (variable)
	    set_all_contains_variable (plats);
	}
    }

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    if (ie->indirect_info->polymorphic
	&& ie->indirect_info->param_index >= 0)
      {
	gcc_checking_assert (ie->indirect_info->param_index >= 0);
	ipa_get_parm_lattices (info,
			       ie->indirect_info->param_index)->virt_call = 1;
      }
}

/* Return true if VALUE can be safely IPA-CP propagated to a parameter of type
   PARAM_TYPE.  */

static bool
ipacp_value_safe_for_type (tree param_type, tree value)
{
  tree val_type = TREE_TYPE (value);
  if (param_type == val_type
      || useless_type_conversion_p (param_type, val_type)
      || fold_convertible_p (param_type, value))
    return true;
  else
    return false;
}

/* Return the result of a (possibly arithmetic) operation on the constant
   value INPUT.  OPERAND is 2nd operand for binary operation.  RES_TYPE is
   the type of the parameter to which the result is passed.  Return
   NULL_TREE if that cannot be determined or be considered an
   interprocedural invariant.  */

static tree
ipa_get_jf_arith_result (enum tree_code opcode, tree input, tree operand,
			 tree res_type)
{
  tree res;

  if (opcode == NOP_EXPR)
    return input;
  if (!is_gimple_ip_invariant (input))
    return NULL_TREE;

  if (opcode == ASSERT_EXPR)
    {
      if (values_equal_for_ipcp_p (input, operand))
	return input;
      else
	return NULL_TREE;
    }

  if (!res_type)
    {
      if (TREE_CODE_CLASS (opcode) == tcc_comparison)
	res_type = boolean_type_node;
      else if (expr_type_first_operand_type_p (opcode))
	res_type = TREE_TYPE (input);
      else
	return NULL_TREE;
    }

  if (TREE_CODE_CLASS (opcode) == tcc_unary)
    res = fold_unary (opcode, res_type, input);
  else
    res = fold_binary (opcode, res_type, input, operand);

  if (res && !is_gimple_ip_invariant (res))
    return NULL_TREE;

  return res;
}

/* Return the result of a (possibly arithmetic) pass through jump function
   JFUNC on the constant value INPUT.  RES_TYPE is the type of the parameter
   to which the result is passed.  Return NULL_TREE if that cannot be
   determined or be considered an interprocedural invariant.  */

static tree
ipa_get_jf_pass_through_result (struct ipa_jump_func *jfunc, tree input,
				tree res_type)
{
  return ipa_get_jf_arith_result (ipa_get_jf_pass_through_operation (jfunc),
				  input,
				  ipa_get_jf_pass_through_operand (jfunc),
				  res_type);
}

/* Return the result of an ancestor jump function JFUNC on the constant value
   INPUT.  Return NULL_TREE if that cannot be determined.  */

static tree
ipa_get_jf_ancestor_result (struct ipa_jump_func *jfunc, tree input)
{
  gcc_checking_assert (TREE_CODE (input) != TREE_BINFO);
  if (TREE_CODE (input) == ADDR_EXPR)
    {
      gcc_checking_assert (is_gimple_ip_invariant_address (input));
      poly_int64 off = ipa_get_jf_ancestor_offset (jfunc);
      if (known_eq (off, 0))
	return input;
      poly_int64 byte_offset = exact_div (off, BITS_PER_UNIT);
      return build1 (ADDR_EXPR, TREE_TYPE (input),
		     fold_build2 (MEM_REF, TREE_TYPE (TREE_TYPE (input)), input,
				  build_int_cst (ptr_type_node, byte_offset)));
    }
  else if (ipa_get_jf_ancestor_keep_null (jfunc)
	   && zerop (input))
    return input;
  else
    return NULL_TREE;
}

/* Determine whether JFUNC evaluates to a single known constant value and if
   so, return it.  Otherwise return NULL.  INFO describes the caller node or
   the one it is inlined to, so that pass-through jump functions can be
   evaluated.  PARM_TYPE is the type of the parameter to which the result is
   passed.  */

tree
ipa_value_from_jfunc (class ipa_node_params *info, struct ipa_jump_func *jfunc,
		      tree parm_type)
{
  if (jfunc->type == IPA_JF_CONST)
    return ipa_get_jf_constant (jfunc);
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      tree input;
      int idx;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	idx = ipa_get_jf_pass_through_formal_id (jfunc);
      else
	idx = ipa_get_jf_ancestor_formal_id (jfunc);

      if (info->ipcp_orig_node)
	input = info->known_csts[idx];
      else
	{
	  ipcp_lattice<tree> *lat;

	  if (info->lattices.is_empty ()
	      || idx >= ipa_get_param_count (info))
	    return NULL_TREE;
	  lat = ipa_get_scalar_lat (info, idx);
	  if (!lat->is_single_const ())
	    return NULL_TREE;
	  input = lat->values->value;
	}

      if (!input)
	return NULL_TREE;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	return ipa_get_jf_pass_through_result (jfunc, input, parm_type);
      else
	return ipa_get_jf_ancestor_result (jfunc, input);
    }
  else
    return NULL_TREE;
}

/* Determine whether JFUNC evaluates to single known polymorphic context, given
   that INFO describes the caller node or the one it is inlined to, CS is the
   call graph edge corresponding to JFUNC and CSIDX index of the described
   parameter.  */

ipa_polymorphic_call_context
ipa_context_from_jfunc (ipa_node_params *info, cgraph_edge *cs, int csidx,
			ipa_jump_func *jfunc)
{
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  ipa_polymorphic_call_context ctx;
  ipa_polymorphic_call_context *edge_ctx
    = cs ? ipa_get_ith_polymorhic_call_context (args, csidx) : NULL;

  if (edge_ctx && !edge_ctx->useless_p ())
    ctx = *edge_ctx;

  if (jfunc->type == IPA_JF_PASS_THROUGH
      || jfunc->type == IPA_JF_ANCESTOR)
    {
      ipa_polymorphic_call_context srcctx;
      int srcidx;
      bool type_preserved = true;
      if (jfunc->type == IPA_JF_PASS_THROUGH)
	{
	  if (ipa_get_jf_pass_through_operation (jfunc) != NOP_EXPR)
	    return ctx;
	  type_preserved = ipa_get_jf_pass_through_type_preserved (jfunc);
	  srcidx = ipa_get_jf_pass_through_formal_id (jfunc);
	}
      else
	{
	  type_preserved = ipa_get_jf_ancestor_type_preserved (jfunc);
	  srcidx = ipa_get_jf_ancestor_formal_id (jfunc);
	}
      if (info->ipcp_orig_node)
	{
	  if (info->known_contexts.exists ())
	    srcctx = info->known_contexts[srcidx];
	}
      else
	{
	  if (info->lattices.is_empty ()
	      || srcidx >= ipa_get_param_count (info))
	    return ctx;
	  ipcp_lattice<ipa_polymorphic_call_context> *lat;
	  lat = ipa_get_poly_ctx_lat (info, srcidx);
	  if (!lat->is_single_const ())
	    return ctx;
	  srcctx = lat->values->value;
	}
      if (srcctx.useless_p ())
	return ctx;
      if (jfunc->type == IPA_JF_ANCESTOR)
	srcctx.offset_by (ipa_get_jf_ancestor_offset (jfunc));
      if (!type_preserved)
	srcctx.possible_dynamic_type_change (cs->in_polymorphic_cdtor);
      srcctx.combine_with (ctx);
      return srcctx;
    }

  return ctx;
}

/* Emulate effects of unary OPERATION and/or conversion from SRC_TYPE to
   DST_TYPE on value range in SRC_VR and store it to DST_VR.  Return true if
   the result is a range that is not VARYING nor UNDEFINED.  */

static bool
ipa_vr_operation_and_type_effects (vrange &dst_vr,
				   const vrange &src_vr,
				   enum tree_code operation,
				   tree dst_type, tree src_type)
{
  if (!ipa_vr_supported_type_p (dst_type)
      || !ipa_vr_supported_type_p (src_type))
    return false;

  range_op_handler handler (operation);
  if (!handler)
    return false;

  value_range varying (dst_type);
  varying.set_varying (dst_type);

  return (handler.operand_check_p (dst_type, src_type, dst_type)
	  && handler.fold_range (dst_vr, dst_type, src_vr, varying)
	  && !dst_vr.varying_p ()
	  && !dst_vr.undefined_p ());
}

/* Same as above, but the SRC_VR argument is an IPA_VR which must
   first be extracted onto a vrange.  */

static bool
ipa_vr_operation_and_type_effects (vrange &dst_vr,
				   const ipa_vr &src_vr,
				   enum tree_code operation,
				   tree dst_type, tree src_type)
{
  value_range tmp;
  src_vr.get_vrange (tmp);
  return ipa_vr_operation_and_type_effects (dst_vr, tmp, operation,
					    dst_type, src_type);
}

/* Determine range of JFUNC given that INFO describes the caller node or
   the one it is inlined to, CS is the call graph edge corresponding to JFUNC
   and PARM_TYPE of the parameter.  */

void
ipa_value_range_from_jfunc (vrange &vr,
			    ipa_node_params *info, cgraph_edge *cs,
			    ipa_jump_func *jfunc, tree parm_type)
{
  vr.set_undefined ();

  if (jfunc->m_vr)
    ipa_vr_operation_and_type_effects (vr,
				       *jfunc->m_vr,
				       NOP_EXPR, parm_type,
				       jfunc->m_vr->type ());
  if (vr.singleton_p ())
    return;
  if (jfunc->type == IPA_JF_PASS_THROUGH)
    {
      int idx;
      ipcp_transformation *sum
	= ipcp_get_transformation_summary (cs->caller->inlined_to
					   ? cs->caller->inlined_to
					   : cs->caller);
      if (!sum || !sum->m_vr)
	return;

      idx = ipa_get_jf_pass_through_formal_id (jfunc);

      if (!(*sum->m_vr)[idx].known_p ())
	return;
      tree vr_type = ipa_get_type (info, idx);
      value_range srcvr;
      (*sum->m_vr)[idx].get_vrange (srcvr);

      enum tree_code operation = ipa_get_jf_pass_through_operation (jfunc);

      if (TREE_CODE_CLASS (operation) == tcc_unary)
	{
	  value_range res (parm_type);

	  if (ipa_vr_operation_and_type_effects (res,
						 srcvr,
						 operation, parm_type,
						 vr_type))
	    vr.intersect (res);
	}
      else
	{
	  value_range op_res (vr_type);
	  value_range res (vr_type);
	  tree op = ipa_get_jf_pass_through_operand (jfunc);
	  value_range op_vr (TREE_TYPE (op));
	  range_op_handler handler (operation);

	  ipa_range_set_and_normalize (op_vr, op);

	  if (!handler
	      || !op_res.supports_type_p (vr_type)
	      /* Sometimes we try to fold comparison operators using a
		 pointer type to hold the result instead of a boolean
		 type.  Avoid trapping in the sanity check in
		 fold_range until this is fixed.  */
	      || srcvr.undefined_p ()
	      || op_vr.undefined_p ()
	      || !handler.operand_check_p (vr_type, srcvr.type (), op_vr.type ())
	      || !handler.fold_range (op_res, vr_type, srcvr, op_vr))
	    op_res.set_varying (vr_type);

	  if (ipa_vr_operation_and_type_effects (res,
						 op_res,
						 NOP_EXPR, parm_type,
						 vr_type))
	    vr.intersect (res);
	}
    }
}

/* Determine whether ITEM, jump function for an aggregate part, evaluates to a
   single known constant value and if so, return it.  Otherwise return NULL.
   NODE and INFO describes the caller node or the one it is inlined to, and
   its related info.  */

tree
ipa_agg_value_from_jfunc (ipa_node_params *info, cgraph_node *node,
			  const ipa_agg_jf_item *item)
{
  tree value = NULL_TREE;
  int src_idx;

  if (item->offset < 0
      || item->jftype == IPA_JF_UNKNOWN
      || item->offset >= (HOST_WIDE_INT) UINT_MAX * BITS_PER_UNIT)
    return NULL_TREE;

  if (item->jftype == IPA_JF_CONST)
    return item->value.constant;

  gcc_checking_assert (item->jftype == IPA_JF_PASS_THROUGH
		       || item->jftype == IPA_JF_LOAD_AGG);

  src_idx = item->value.pass_through.formal_id;

  if (info->ipcp_orig_node)
    {
      if (item->jftype == IPA_JF_PASS_THROUGH)
	value = info->known_csts[src_idx];
      else if (ipcp_transformation *ts = ipcp_get_transformation_summary (node))
	{
	  ipa_argagg_value_list avl (ts);
	  value = avl.get_value (src_idx,
				 item->value.load_agg.offset / BITS_PER_UNIT,
				 item->value.load_agg.by_ref);
	}
    }
  else if (!info->lattices.is_empty ())
    {
      class ipcp_param_lattices *src_plats
	= ipa_get_parm_lattices (info, src_idx);

      if (item->jftype == IPA_JF_PASS_THROUGH)
	{
	  struct ipcp_lattice<tree> *lat = &src_plats->itself;

	  if (!lat->is_single_const ())
	    return NULL_TREE;

	  value = lat->values->value;
	}
      else if (src_plats->aggs
	       && !src_plats->aggs_bottom
	       && !src_plats->aggs_contain_variable
	       && src_plats->aggs_by_ref == item->value.load_agg.by_ref)
	{
	  struct ipcp_agg_lattice *aglat;

	  for (aglat = src_plats->aggs; aglat; aglat = aglat->next)
	    {
	      if (aglat->offset > item->value.load_agg.offset)
		break;

	      if (aglat->offset == item->value.load_agg.offset)
		{
		  if (aglat->is_single_const ())
		    value = aglat->values->value;
		  break;
		}
	    }
	}
    }

  if (!value)
    return NULL_TREE;

  if (item->jftype == IPA_JF_LOAD_AGG)
    {
      tree load_type = item->value.load_agg.type;
      tree value_type = TREE_TYPE (value);

      /* Ensure value type is compatible with load type.  */
      if (!useless_type_conversion_p (load_type, value_type))
	return NULL_TREE;
    }

  return ipa_get_jf_arith_result (item->value.pass_through.operation,
				  value,
				  item->value.pass_through.operand,
				  item->type);
}

/* Process all items in AGG_JFUNC relative to caller (or the node the original
  caller is inlined to) NODE which described by INFO and push the results to
  RES as describing values passed in parameter DST_INDEX.  */

void
ipa_push_agg_values_from_jfunc (ipa_node_params *info, cgraph_node *node,
				ipa_agg_jump_function *agg_jfunc,
				unsigned dst_index,
				vec<ipa_argagg_value> *res)
{
  unsigned prev_unit_offset = 0;
  bool first = true;

  for (const ipa_agg_jf_item &item : agg_jfunc->items)
    {
      tree value = ipa_agg_value_from_jfunc (info, node, &item);
      if (!value)
	continue;

      ipa_argagg_value iav;
      iav.value = value;
      iav.unit_offset = item.offset / BITS_PER_UNIT;
      iav.index = dst_index;
      iav.by_ref = agg_jfunc->by_ref;
      iav.killed = 0;

      gcc_assert (first
		  || iav.unit_offset > prev_unit_offset);
      prev_unit_offset = iav.unit_offset;
      first = false;

      res->safe_push (iav);
    }
}

/* If checking is enabled, verify that no lattice is in the TOP state, i.e. not
   bottom, not containing a variable component and without any known value at
   the same time.  */

DEBUG_FUNCTION void
ipcp_verify_propagated_values (void)
{
  struct cgraph_node *node;

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      ipa_node_params *info = ipa_node_params_sum->get (node);
      if (!opt_for_fn (node->decl, flag_ipa_cp)
	  || !opt_for_fn (node->decl, optimize))
	continue;
      int i, count = ipa_get_param_count (info);

      for (i = 0; i < count; i++)
	{
	  ipcp_lattice<tree> *lat = ipa_get_scalar_lat (info, i);

	  if (!lat->bottom
	      && !lat->contains_variable
	      && lat->values_count == 0)
	    {
	      if (dump_file)
		{
		  symtab->dump (dump_file);
		  fprintf (dump_file, "\nIPA lattices after constant "
			   "propagation, before gcc_unreachable:\n");
		  print_all_lattices (dump_file, true, false);
		}

	      gcc_unreachable ();
	    }
	}
    }
}

/* Return true iff X and Y should be considered equal contexts by IPA-CP.  */

static bool
values_equal_for_ipcp_p (ipa_polymorphic_call_context x,
			 ipa_polymorphic_call_context y)
{
  return x.equal_to (y);
}


/* Add a new value source to the value represented by THIS, marking that a
   value comes from edge CS and (if the underlying jump function is a
   pass-through or an ancestor one) from a caller value SRC_VAL of a caller
   parameter described by SRC_INDEX.  OFFSET is negative if the source was the
   scalar value of the parameter itself or the offset within an aggregate.  */

template <typename valtype>
void
ipcp_value<valtype>::add_source (cgraph_edge *cs, ipcp_value *src_val,
				 int src_idx, HOST_WIDE_INT offset)
{
  ipcp_value_source<valtype> *src;

  src = new (ipcp_sources_pool.allocate ()) ipcp_value_source<valtype>;
  src->offset = offset;
  src->cs = cs;
  src->val = src_val;
  src->index = src_idx;

  src->next = sources;
  sources = src;
}

/* Allocate a new ipcp_value holding a tree constant, initialize its value to
   SOURCE and clear all other fields.  */

static ipcp_value<tree> *
allocate_and_init_ipcp_value (tree cst, unsigned same_lat_gen_level)
{
  ipcp_value<tree> *val;

  val = new (ipcp_cst_values_pool.allocate ()) ipcp_value<tree>();
  val->value = cst;
  val->self_recursion_generated_level = same_lat_gen_level;
  return val;
}

/* Allocate a new ipcp_value holding a polymorphic context, initialize its
   value to SOURCE and clear all other fields.  */

static ipcp_value<ipa_polymorphic_call_context> *
allocate_and_init_ipcp_value (ipa_polymorphic_call_context ctx,
			      unsigned same_lat_gen_level)
{
  ipcp_value<ipa_polymorphic_call_context> *val;

  val = new (ipcp_poly_ctx_values_pool.allocate ())
    ipcp_value<ipa_polymorphic_call_context>();
  val->value = ctx;
  val->self_recursion_generated_level = same_lat_gen_level;
  return val;
}

/* Try to add NEWVAL to LAT, potentially creating a new ipcp_value for it.  CS,
   SRC_VAL SRC_INDEX and OFFSET are meant for add_source and have the same
   meaning.  OFFSET -1 means the source is scalar and not a part of an
   aggregate.  If non-NULL, VAL_P records address of existing or newly added
   ipcp_value.

   If the value is generated for a self-recursive call as a result of an
   arithmetic pass-through jump-function acting on a value in the same lattice,
   SAME_LAT_GEN_LEVEL must be the length of such chain, otherwise it must be
   zero.  If it is non-zero, PARAM_IPA_CP_VALUE_LIST_SIZE limit is ignored.  */

template <typename valtype>
bool
ipcp_lattice<valtype>::add_value (valtype newval, cgraph_edge *cs,
				  ipcp_value<valtype> *src_val,
				  int src_idx, HOST_WIDE_INT offset,
				  ipcp_value<valtype> **val_p,
				  unsigned same_lat_gen_level)
{
  ipcp_value<valtype> *val, *last_val = NULL;

  if (val_p)
    *val_p = NULL;

  if (bottom)
    return false;

  for (val = values; val; last_val = val, val = val->next)
    if (values_equal_for_ipcp_p (val->value, newval))
      {
	if (val_p)
	  *val_p = val;

	if (val->self_recursion_generated_level < same_lat_gen_level)
	  val->self_recursion_generated_level = same_lat_gen_level;

	if (ipa_edge_within_scc (cs))
	  {
	    ipcp_value_source<valtype> *s;
	    for (s = val->sources; s; s = s->next)
	      if (s->cs == cs && s->val == src_val)
		break;
	    if (s)
	      return false;
	  }

	val->add_source (cs, src_val, src_idx, offset);
	return false;
      }

  if (!same_lat_gen_level && values_count >= opt_for_fn (cs->callee->decl,
						param_ipa_cp_value_list_size))
    {
      /* We can only free sources, not the values themselves, because sources
	 of other values in this SCC might point to them.   */
      for (val = values; val; val = val->next)
	{
	  while (val->sources)
	    {
	      ipcp_value_source<valtype> *src = val->sources;
	      val->sources = src->next;
	      ipcp_sources_pool.remove ((ipcp_value_source<tree>*)src);
	    }
	}
      values = NULL;
      return set_to_bottom ();
    }

  values_count++;
  val = allocate_and_init_ipcp_value (newval, same_lat_gen_level);
  val->add_source (cs, src_val, src_idx, offset);
  val->next = NULL;

  /* Add the new value to end of value list, which can reduce iterations
     of propagation stage for recursive function.  */
  if (last_val)
    last_val->next = val;
  else
    values = val;

  if (val_p)
    *val_p = val;

  return true;
}

/* A helper function that returns result of operation specified by OPCODE on
   the value of SRC_VAL.  If non-NULL, OPND1_TYPE is expected type for the
   value of SRC_VAL.  If the operation is binary, OPND2 is a constant value
   acting as its second operand.  If non-NULL, RES_TYPE is expected type of
   the result.  */

static tree
get_val_across_arith_op (enum tree_code opcode,
			 tree opnd1_type,
			 tree opnd2,
			 ipcp_value<tree> *src_val,
			 tree res_type)
{
  tree opnd1 = src_val->value;

  /* Skip source values that is incompatible with specified type.  */
  if (opnd1_type
      && !useless_type_conversion_p (opnd1_type, TREE_TYPE (opnd1)))
    return NULL_TREE;

  return ipa_get_jf_arith_result (opcode, opnd1, opnd2, res_type);
}

/* Propagate values through an arithmetic transformation described by a jump
   function associated with edge CS, taking values from SRC_LAT and putting
   them into DEST_LAT.  OPND1_TYPE is expected type for the values in SRC_LAT.
   OPND2 is a constant value if transformation is a binary operation.
   SRC_OFFSET specifies offset in an aggregate if SRC_LAT describes lattice of
   a part of the aggregate.  SRC_IDX is the index of the source parameter.
   RES_TYPE is the value type of result being propagated into.  Return true if
   DEST_LAT changed.  */

static bool
propagate_vals_across_arith_jfunc (cgraph_edge *cs,
				   enum tree_code opcode,
				   tree opnd1_type,
				   tree opnd2,
				   ipcp_lattice<tree> *src_lat,
				   ipcp_lattice<tree> *dest_lat,
				   HOST_WIDE_INT src_offset,
				   int src_idx,
				   tree res_type)
{
  ipcp_value<tree> *src_val;
  bool ret = false;

  /* Due to circular dependencies, propagating within an SCC through arithmetic
     transformation would create infinite number of values.  But for
     self-feeding recursive function, we could allow propagation in a limited
     count, and this can enable a simple kind of recursive function versioning.
     For other scenario, we would just make lattices bottom.  */
  if (opcode != NOP_EXPR && ipa_edge_within_scc (cs))
    {
      int i;

      int max_recursive_depth = opt_for_fn(cs->caller->decl,
					   param_ipa_cp_max_recursive_depth);
      if (src_lat != dest_lat || max_recursive_depth < 1)
	return dest_lat->set_contains_variable ();

      /* No benefit if recursive execution is in low probability.  */
      if (cs->sreal_frequency () * 100
	  <= ((sreal) 1) * opt_for_fn (cs->caller->decl,
				       param_ipa_cp_min_recursive_probability))
	return dest_lat->set_contains_variable ();

      auto_vec<ipcp_value<tree> *, 8> val_seeds;

      for (src_val = src_lat->values; src_val; src_val = src_val->next)
	{
	  /* Now we do not use self-recursively generated value as propagation
	     source, this is absolutely conservative, but could avoid explosion
	     of lattice's value space, especially when one recursive function
	     calls another recursive.  */
	  if (src_val->self_recursion_generated_p ())
	    {
	      ipcp_value_source<tree> *s;

	      /* If the lattice has already been propagated for the call site,
		 no need to do that again.  */
	      for (s = src_val->sources; s; s = s->next)
		if (s->cs == cs)
		  return dest_lat->set_contains_variable ();
	    }
	  else
	    val_seeds.safe_push (src_val);
	}

      gcc_assert ((int) val_seeds.length () <= param_ipa_cp_value_list_size);

      /* Recursively generate lattice values with a limited count.  */
      FOR_EACH_VEC_ELT (val_seeds, i, src_val)
	{
	  for (int j = 1; j < max_recursive_depth; j++)
	    {
	      tree cstval = get_val_across_arith_op (opcode, opnd1_type, opnd2,
						     src_val, res_type);
	      if (!cstval
		  || !ipacp_value_safe_for_type (res_type, cstval))
		break;

	      ret |= dest_lat->add_value (cstval, cs, src_val, src_idx,
					  src_offset, &src_val, j);
	      gcc_checking_assert (src_val);
	    }
	}
      ret |= dest_lat->set_contains_variable ();
    }
  else
    for (src_val = src_lat->values; src_val; src_val = src_val->next)
      {
	/* Now we do not use self-recursively generated value as propagation
	   source, otherwise it is easy to make value space of normal lattice
	   overflow.  */
	if (src_val->self_recursion_generated_p ())
	  {
	    ret |= dest_lat->set_contains_variable ();
	    continue;
	  }

	tree cstval = get_val_across_arith_op (opcode, opnd1_type, opnd2,
					       src_val, res_type);
	if (cstval
	    && ipacp_value_safe_for_type (res_type, cstval))
	  ret |= dest_lat->add_value (cstval, cs, src_val, src_idx,
				      src_offset);
	else
	  ret |= dest_lat->set_contains_variable ();
      }

  return ret;
}

/* Propagate values through a pass-through jump function JFUNC associated with
   edge CS, taking values from SRC_LAT and putting them into DEST_LAT.  SRC_IDX
   is the index of the source parameter.  PARM_TYPE is the type of the
   parameter to which the result is passed.  */

static bool
propagate_vals_across_pass_through (cgraph_edge *cs, ipa_jump_func *jfunc,
				    ipcp_lattice<tree> *src_lat,
				    ipcp_lattice<tree> *dest_lat, int src_idx,
				    tree parm_type)
{
  return propagate_vals_across_arith_jfunc (cs,
				ipa_get_jf_pass_through_operation (jfunc),
				NULL_TREE,
				ipa_get_jf_pass_through_operand (jfunc),
				src_lat, dest_lat, -1, src_idx, parm_type);
}

/* Propagate values through an ancestor jump function JFUNC associated with
   edge CS, taking values from SRC_LAT and putting them into DEST_LAT.  SRC_IDX
   is the index of the source parameter.  */

static bool
propagate_vals_across_ancestor (struct cgraph_edge *cs,
				struct ipa_jump_func *jfunc,
				ipcp_lattice<tree> *src_lat,
				ipcp_lattice<tree> *dest_lat, int src_idx,
				tree param_type)
{
  ipcp_value<tree> *src_val;
  bool ret = false;

  if (ipa_edge_within_scc (cs))
    return dest_lat->set_contains_variable ();

  for (src_val = src_lat->values; src_val; src_val = src_val->next)
    {
      tree t = ipa_get_jf_ancestor_result (jfunc, src_val->value);

      if (t && ipacp_value_safe_for_type (param_type, t))
	ret |= dest_lat->add_value (t, cs, src_val, src_idx);
      else
	ret |= dest_lat->set_contains_variable ();
    }

  return ret;
}

/* Propagate scalar values across jump function JFUNC that is associated with
   edge CS and put the values into DEST_LAT.  PARM_TYPE is the type of the
   parameter to which the result is passed.  */

static bool
propagate_scalar_across_jump_function (struct cgraph_edge *cs,
				       struct ipa_jump_func *jfunc,
				       ipcp_lattice<tree> *dest_lat,
				       tree param_type)
{
  if (dest_lat->bottom)
    return false;

  if (jfunc->type == IPA_JF_CONST)
    {
      tree val = ipa_get_jf_constant (jfunc);
      if (ipacp_value_safe_for_type (param_type, val))
	return dest_lat->add_value (val, cs, NULL, 0);
      else
	return dest_lat->set_contains_variable ();
    }
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      ipcp_lattice<tree> *src_lat;
      int src_idx;
      bool ret;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      else
	src_idx = ipa_get_jf_ancestor_formal_id (jfunc);

      src_lat = ipa_get_scalar_lat (caller_info, src_idx);
      if (src_lat->bottom)
	return dest_lat->set_contains_variable ();

      /* If we would need to clone the caller and cannot, do not propagate.  */
      if (!ipcp_versionable_function_p (cs->caller)
	  && (src_lat->contains_variable
	      || (src_lat->values_count > 1)))
	return dest_lat->set_contains_variable ();

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	ret = propagate_vals_across_pass_through (cs, jfunc, src_lat,
						  dest_lat, src_idx,
						  param_type);
      else
	ret = propagate_vals_across_ancestor (cs, jfunc, src_lat, dest_lat,
					      src_idx, param_type);

      if (src_lat->contains_variable)
	ret |= dest_lat->set_contains_variable ();

      return ret;
    }

  /* TODO: We currently do not handle member method pointers in IPA-CP (we only
     use it for indirect inlining), we should propagate them too.  */
  return dest_lat->set_contains_variable ();
}

/* Propagate scalar values across jump function JFUNC that is associated with
   edge CS and describes argument IDX and put the values into DEST_LAT.  */

static bool
propagate_context_across_jump_function (cgraph_edge *cs,
			  ipa_jump_func *jfunc, int idx,
			  ipcp_lattice<ipa_polymorphic_call_context> *dest_lat)
{
  if (dest_lat->bottom)
    return false;
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  bool ret = false;
  bool added_sth = false;
  bool type_preserved = true;

  ipa_polymorphic_call_context edge_ctx, *edge_ctx_ptr
    = ipa_get_ith_polymorhic_call_context (args, idx);

  if (edge_ctx_ptr)
    edge_ctx = *edge_ctx_ptr;

  if (jfunc->type == IPA_JF_PASS_THROUGH
      || jfunc->type == IPA_JF_ANCESTOR)
    {
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      int src_idx;
      ipcp_lattice<ipa_polymorphic_call_context> *src_lat;

      /* TODO: Once we figure out how to propagate speculations, it will
	 probably be a good idea to switch to speculation if type_preserved is
	 not set instead of punting.  */
      if (jfunc->type == IPA_JF_PASS_THROUGH)
	{
	  if (ipa_get_jf_pass_through_operation (jfunc) != NOP_EXPR)
	    goto prop_fail;
	  type_preserved = ipa_get_jf_pass_through_type_preserved (jfunc);
	  src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
	}
      else
	{
	  type_preserved = ipa_get_jf_ancestor_type_preserved (jfunc);
	  src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
	}

      src_lat = ipa_get_poly_ctx_lat (caller_info, src_idx);
      /* If we would need to clone the caller and cannot, do not propagate.  */
      if (!ipcp_versionable_function_p (cs->caller)
	  && (src_lat->contains_variable
	      || (src_lat->values_count > 1)))
	goto prop_fail;

      ipcp_value<ipa_polymorphic_call_context> *src_val;
      for (src_val = src_lat->values; src_val; src_val = src_val->next)
	{
	  ipa_polymorphic_call_context cur = src_val->value;

	  if (!type_preserved)
	    cur.possible_dynamic_type_change (cs->in_polymorphic_cdtor);
	  if (jfunc->type == IPA_JF_ANCESTOR)
	    cur.offset_by (ipa_get_jf_ancestor_offset (jfunc));
	  /* TODO: In cases we know how the context is going to be used,
	     we can improve the result by passing proper OTR_TYPE.  */
	  cur.combine_with (edge_ctx);
	  if (!cur.useless_p ())
	    {
	      if (src_lat->contains_variable
		  && !edge_ctx.equal_to (cur))
		ret |= dest_lat->set_contains_variable ();
	      ret |= dest_lat->add_value (cur, cs, src_val, src_idx);
	      added_sth = true;
	    }
	}
    }

 prop_fail:
  if (!added_sth)
    {
      if (!edge_ctx.useless_p ())
	ret |= dest_lat->add_value (edge_ctx, cs);
      else
	ret |= dest_lat->set_contains_variable ();
    }

  return ret;
}

/* Propagate bits across jfunc that is associated with
   edge cs and update dest_lattice accordingly.  */

bool
propagate_bits_across_jump_function (cgraph_edge *cs, int idx,
				     ipa_jump_func *jfunc,
				     ipcp_bits_lattice *dest_lattice)
{
  if (dest_lattice->bottom_p ())
    return false;

  enum availability availability;
  cgraph_node *callee = cs->callee->function_symbol (&availability);
  ipa_node_params *callee_info = ipa_node_params_sum->get (callee);
  tree parm_type = ipa_get_type (callee_info, idx);

  /* For K&R C programs, ipa_get_type() could return NULL_TREE.  Avoid the
     transform for these cases.  Similarly, we can have bad type mismatches
     with LTO, avoid doing anything with those too.  */
  if (!parm_type
      || (!INTEGRAL_TYPE_P (parm_type) && !POINTER_TYPE_P (parm_type)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Setting dest_lattice to bottom, because type of "
		 "param %i of %s is NULL or unsuitable for bits propagation\n",
		 idx, cs->callee->dump_name ());

      return dest_lattice->set_to_bottom ();
    }

  unsigned precision = TYPE_PRECISION (parm_type);
  signop sgn = TYPE_SIGN (parm_type);

  if (jfunc->type == IPA_JF_PASS_THROUGH
      || jfunc->type == IPA_JF_ANCESTOR)
    {
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      tree operand = NULL_TREE;
      enum tree_code code;
      unsigned src_idx;
      bool keep_null = false;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	{
	  code = ipa_get_jf_pass_through_operation (jfunc);
	  src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
	  if (code != NOP_EXPR)
	    operand = ipa_get_jf_pass_through_operand (jfunc);
	}
      else
	{
	  code = POINTER_PLUS_EXPR;
	  src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
	  unsigned HOST_WIDE_INT offset
	    = ipa_get_jf_ancestor_offset (jfunc) / BITS_PER_UNIT;
	  keep_null = (ipa_get_jf_ancestor_keep_null (jfunc) || !offset);
	  operand = build_int_cstu (size_type_node, offset);
	}

      class ipcp_param_lattices *src_lats
	= ipa_get_parm_lattices (caller_info, src_idx);

      /* Try to propagate bits if src_lattice is bottom, but jfunc is known.
	 for eg consider:
	 int f(int x)
	 {
	   g (x & 0xff);
	 }
	 Assume lattice for x is bottom, however we can still propagate
	 result of x & 0xff == 0xff, which gets computed during ccp1 pass
	 and we store it in jump function during analysis stage.  */

      if (!src_lats->bits_lattice.bottom_p ())
	{
	  bool drop_all_ones
	    = keep_null && !src_lats->bits_lattice.known_nonzero_p ();

	  return dest_lattice->meet_with (src_lats->bits_lattice, precision,
					  sgn, code, operand, drop_all_ones);
	}
    }

  value_range vr (parm_type);
  if (jfunc->m_vr)
    {
      jfunc->m_vr->get_vrange (vr);
      if (!vr.undefined_p () && !vr.varying_p ())
	{
	  irange_bitmask bm = vr.get_bitmask ();
	  widest_int mask
	    = widest_int::from (bm.mask (), TYPE_SIGN (parm_type));
	  widest_int value
	    = widest_int::from (bm.value (), TYPE_SIGN (parm_type));
	  return dest_lattice->meet_with (value, mask, precision);
	}
    }
  return dest_lattice->set_to_bottom ();
}

/* Propagate value range across jump function JFUNC that is associated with
   edge CS with param of callee of PARAM_TYPE and update DEST_PLATS
   accordingly.  */

static bool
propagate_vr_across_jump_function (cgraph_edge *cs, ipa_jump_func *jfunc,
				   class ipcp_param_lattices *dest_plats,
				   tree param_type)
{
  ipcp_vr_lattice *dest_lat = &dest_plats->m_value_range;

  if (dest_lat->bottom_p ())
    return false;

  if (!param_type
      || !ipa_vr_supported_type_p (param_type))
    return dest_lat->set_to_bottom ();

  if (jfunc->type == IPA_JF_PASS_THROUGH)
    {
      enum tree_code operation = ipa_get_jf_pass_through_operation (jfunc);
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      int src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      class ipcp_param_lattices *src_lats
	= ipa_get_parm_lattices (caller_info, src_idx);
      tree operand_type = ipa_get_type (caller_info, src_idx);

      if (src_lats->m_value_range.bottom_p ())
	return dest_lat->set_to_bottom ();

      value_range vr (param_type);
      if (TREE_CODE_CLASS (operation) == tcc_unary)
	ipa_vr_operation_and_type_effects (vr,
					   src_lats->m_value_range.m_vr,
					   operation, param_type,
					   operand_type);
      /* A crude way to prevent unbounded number of value range updates
	 in SCC components.  We should allow limited number of updates within
	 SCC, too.  */
      else if (!ipa_edge_within_scc (cs))
	{
	  tree op = ipa_get_jf_pass_through_operand (jfunc);
	  value_range op_vr (TREE_TYPE (op));
	  value_range op_res (param_type);
	  range_op_handler handler (operation);

	  ipa_range_set_and_normalize (op_vr, op);

	  if (!handler
	      || !ipa_vr_supported_type_p (operand_type)
	      /* Sometimes we try to fold comparison operators using a
		 pointer type to hold the result instead of a boolean
		 type.  Avoid trapping in the sanity check in
		 fold_range until this is fixed.  */
	      || src_lats->m_value_range.m_vr.undefined_p ()
	      || op_vr.undefined_p ()
	      || !handler.operand_check_p (operand_type,
					   src_lats->m_value_range.m_vr.type (),
					   op_vr.type ())
	      || !handler.fold_range (op_res, operand_type,
				      src_lats->m_value_range.m_vr, op_vr))
	    op_res.set_varying (param_type);

	  ipa_vr_operation_and_type_effects (vr,
					     op_res,
					     NOP_EXPR, param_type,
					     operand_type);
	}
      if (!vr.undefined_p () && !vr.varying_p ())
	{
	  if (jfunc->m_vr)
	    {
	      value_range jvr (param_type);
	      if (ipa_vr_operation_and_type_effects (jvr, *jfunc->m_vr,
						     NOP_EXPR,
						     param_type,
						     jfunc->m_vr->type ()))
		vr.intersect (jvr);
	    }
	  return dest_lat->meet_with (vr);
	}
    }
  else if (jfunc->type == IPA_JF_CONST)
    {
      tree val = ipa_get_jf_constant (jfunc);
      if (TREE_CODE (val) == INTEGER_CST)
	{
	  val = fold_convert (param_type, val);
	  if (TREE_OVERFLOW_P (val))
	    val = drop_tree_overflow (val);

	  value_range tmpvr (val, val);
	  return dest_lat->meet_with (tmpvr);
	}
    }

  value_range vr (param_type);
  if (jfunc->m_vr
      && ipa_vr_operation_and_type_effects (vr, *jfunc->m_vr, NOP_EXPR,
					    param_type,
					    jfunc->m_vr->type ()))
    return dest_lat->meet_with (vr);
  else
    return dest_lat->set_to_bottom ();
}

/* If DEST_PLATS already has aggregate items, check that aggs_by_ref matches
   NEW_AGGS_BY_REF and if not, mark all aggs as bottoms and return true (in all
   other cases, return false).  If there are no aggregate items, set
   aggs_by_ref to NEW_AGGS_BY_REF.  */

static bool
set_check_aggs_by_ref (class ipcp_param_lattices *dest_plats,
		       bool new_aggs_by_ref)
{
  if (dest_plats->aggs)
    {
      if (dest_plats->aggs_by_ref != new_aggs_by_ref)
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return true;
	}
    }
  else
    dest_plats->aggs_by_ref = new_aggs_by_ref;
  return false;
}

/* Walk aggregate lattices in DEST_PLATS from ***AGLAT on, until ***aglat is an
   already existing lattice for the given OFFSET and SIZE, marking all skipped
   lattices as containing variable and checking for overlaps.  If there is no
   already existing lattice for the OFFSET and VAL_SIZE, create one, initialize
   it with offset, size and contains_variable to PRE_EXISTING, and return true,
   unless there are too many already.  If there are two many, return false.  If
   there are overlaps turn whole DEST_PLATS to bottom and return false.  If any
   skipped lattices were newly marked as containing variable, set *CHANGE to
   true.  MAX_AGG_ITEMS is the maximum number of lattices.  */

static bool
merge_agg_lats_step (class ipcp_param_lattices *dest_plats,
		     HOST_WIDE_INT offset, HOST_WIDE_INT val_size,
		     struct ipcp_agg_lattice ***aglat,
		     bool pre_existing, bool *change, int max_agg_items)
{
  gcc_checking_assert (offset >= 0);

  while (**aglat && (**aglat)->offset < offset)
    {
      if ((**aglat)->offset + (**aglat)->size > offset)
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return false;
	}
      *change |= (**aglat)->set_contains_variable ();
      *aglat = &(**aglat)->next;
    }

  if (**aglat && (**aglat)->offset == offset)
    {
      if ((**aglat)->size != val_size)
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return false;
	}
      gcc_assert (!(**aglat)->next
		  || (**aglat)->next->offset >= offset + val_size);
      return true;
    }
  else
    {
      struct ipcp_agg_lattice *new_al;

      if (**aglat && (**aglat)->offset < offset + val_size)
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return false;
	}
      if (dest_plats->aggs_count == max_agg_items)
	return false;
      dest_plats->aggs_count++;
      new_al = ipcp_agg_lattice_pool.allocate ();

      new_al->offset = offset;
      new_al->size = val_size;
      new_al->contains_variable = pre_existing;

      new_al->next = **aglat;
      **aglat = new_al;
      return true;
    }
}

/* Set all AGLAT and all other aggregate lattices reachable by next pointers as
   containing an unknown value.  */

static bool
set_chain_of_aglats_contains_variable (struct ipcp_agg_lattice *aglat)
{
  bool ret = false;
  while (aglat)
    {
      ret |= aglat->set_contains_variable ();
      aglat = aglat->next;
    }
  return ret;
}

/* Merge existing aggregate lattices in SRC_PLATS to DEST_PLATS, subtracting
   DELTA_OFFSET.  CS is the call graph edge and SRC_IDX the index of the source
   parameter used for lattice value sources.  Return true if DEST_PLATS changed
   in any way.  */

static bool
merge_aggregate_lattices (struct cgraph_edge *cs,
			  class ipcp_param_lattices *dest_plats,
			  class ipcp_param_lattices *src_plats,
			  int src_idx, HOST_WIDE_INT offset_delta)
{
  bool pre_existing = dest_plats->aggs != NULL;
  struct ipcp_agg_lattice **dst_aglat;
  bool ret = false;

  if (set_check_aggs_by_ref (dest_plats, src_plats->aggs_by_ref))
    return true;
  if (src_plats->aggs_bottom)
    return set_agg_lats_contain_variable (dest_plats);
  if (src_plats->aggs_contain_variable)
    ret |= set_agg_lats_contain_variable (dest_plats);
  dst_aglat = &dest_plats->aggs;

  int max_agg_items = opt_for_fn (cs->callee->function_symbol ()->decl,
				  param_ipa_max_agg_items);
  for (struct ipcp_agg_lattice *src_aglat = src_plats->aggs;
       src_aglat;
       src_aglat = src_aglat->next)
    {
      HOST_WIDE_INT new_offset = src_aglat->offset - offset_delta;

      if (new_offset < 0)
	continue;
      if (merge_agg_lats_step (dest_plats, new_offset, src_aglat->size,
			       &dst_aglat, pre_existing, &ret, max_agg_items))
	{
	  struct ipcp_agg_lattice *new_al = *dst_aglat;

	  dst_aglat = &(*dst_aglat)->next;
	  if (src_aglat->bottom)
	    {
	      ret |= new_al->set_contains_variable ();
	      continue;
	    }
	  if (src_aglat->contains_variable)
	    ret |= new_al->set_contains_variable ();
	  for (ipcp_value<tree> *val = src_aglat->values;
	       val;
	       val = val->next)
	    ret |= new_al->add_value (val->value, cs, val, src_idx,
				      src_aglat->offset);
	}
      else if (dest_plats->aggs_bottom)
	return true;
    }
  ret |= set_chain_of_aglats_contains_variable (*dst_aglat);
  return ret;
}

/* Determine whether there is anything to propagate FROM SRC_PLATS through a
   pass-through JFUNC and if so, whether it has conform and conforms to the
   rules about propagating values passed by reference.  */

static bool
agg_pass_through_permissible_p (class ipcp_param_lattices *src_plats,
				struct ipa_jump_func *jfunc)
{
  return src_plats->aggs
    && (!src_plats->aggs_by_ref
	|| ipa_get_jf_pass_through_agg_preserved (jfunc));
}

/* Propagate values through ITEM, jump function for a part of an aggregate,
   into corresponding aggregate lattice AGLAT.  CS is the call graph edge
   associated with the jump function.  Return true if AGLAT changed in any
   way.  */

static bool
propagate_aggregate_lattice (struct cgraph_edge *cs,
			     struct ipa_agg_jf_item *item,
			     struct ipcp_agg_lattice *aglat)
{
  class ipa_node_params *caller_info;
  class ipcp_param_lattices *src_plats;
  struct ipcp_lattice<tree> *src_lat;
  HOST_WIDE_INT src_offset;
  int src_idx;
  tree load_type;
  bool ret;

  if (item->jftype == IPA_JF_CONST)
    {
      tree value = item->value.constant;

      gcc_checking_assert (is_gimple_ip_invariant (value));
      return aglat->add_value (value, cs, NULL, 0);
    }

  gcc_checking_assert (item->jftype == IPA_JF_PASS_THROUGH
		       || item->jftype == IPA_JF_LOAD_AGG);

  caller_info = ipa_node_params_sum->get (cs->caller);
  src_idx = item->value.pass_through.formal_id;
  src_plats = ipa_get_parm_lattices (caller_info, src_idx);

  if (item->jftype == IPA_JF_PASS_THROUGH)
    {
      load_type = NULL_TREE;
      src_lat = &src_plats->itself;
      src_offset = -1;
    }
  else
    {
      HOST_WIDE_INT load_offset = item->value.load_agg.offset;
      struct ipcp_agg_lattice *src_aglat;

      for (src_aglat = src_plats->aggs; src_aglat; src_aglat = src_aglat->next)
	if (src_aglat->offset >= load_offset)
	  break;

      load_type = item->value.load_agg.type;
      if (!src_aglat
	  || src_aglat->offset > load_offset
	  || src_aglat->size != tree_to_shwi (TYPE_SIZE (load_type))
	  || src_plats->aggs_by_ref != item->value.load_agg.by_ref)
	return aglat->set_contains_variable ();

      src_lat = src_aglat;
      src_offset = load_offset;
    }

  if (src_lat->bottom
      || (!ipcp_versionable_function_p (cs->caller)
	  && !src_lat->is_single_const ()))
    return aglat->set_contains_variable ();

  ret = propagate_vals_across_arith_jfunc (cs,
					   item->value.pass_through.operation,
					   load_type,
					   item->value.pass_through.operand,
					   src_lat, aglat,
					   src_offset,
					   src_idx,
					   item->type);

  if (src_lat->contains_variable)
    ret |= aglat->set_contains_variable ();

  return ret;
}

/* Propagate scalar values across jump function JFUNC that is associated with
   edge CS and put the values into DEST_LAT.  */

static bool
propagate_aggs_across_jump_function (struct cgraph_edge *cs,
				     struct ipa_jump_func *jfunc,
				     class ipcp_param_lattices *dest_plats)
{
  bool ret = false;

  if (dest_plats->aggs_bottom)
    return false;

  if (jfunc->type == IPA_JF_PASS_THROUGH
      && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    {
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      int src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      class ipcp_param_lattices *src_plats;

      src_plats = ipa_get_parm_lattices (caller_info, src_idx);
      if (agg_pass_through_permissible_p (src_plats, jfunc))
	{
	  /* Currently we do not produce clobber aggregate jump
	     functions, replace with merging when we do.  */
	  gcc_assert (!jfunc->agg.items);
	  ret |= merge_aggregate_lattices (cs, dest_plats, src_plats,
					   src_idx, 0);
	  return ret;
	}
    }
  else if (jfunc->type == IPA_JF_ANCESTOR
	   && ipa_get_jf_ancestor_agg_preserved (jfunc))
    {
      ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
      int src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
      class ipcp_param_lattices *src_plats;

      src_plats = ipa_get_parm_lattices (caller_info, src_idx);
      if (src_plats->aggs && src_plats->aggs_by_ref)
	{
	  /* Currently we do not produce clobber aggregate jump
	     functions, replace with merging when we do.  */
	  gcc_assert (!jfunc->agg.items);
	  ret |= merge_aggregate_lattices (cs, dest_plats, src_plats, src_idx,
					   ipa_get_jf_ancestor_offset (jfunc));
	}
      else if (!src_plats->aggs_by_ref)
	ret |= set_agg_lats_to_bottom (dest_plats);
      else
	ret |= set_agg_lats_contain_variable (dest_plats);
      return ret;
    }

  if (jfunc->agg.items)
    {
      bool pre_existing = dest_plats->aggs != NULL;
      struct ipcp_agg_lattice **aglat = &dest_plats->aggs;
      struct ipa_agg_jf_item *item;
      int i;

      if (set_check_aggs_by_ref (dest_plats, jfunc->agg.by_ref))
	return true;

      int max_agg_items = opt_for_fn (cs->callee->function_symbol ()->decl,
				      param_ipa_max_agg_items);
      FOR_EACH_VEC_ELT (*jfunc->agg.items, i, item)
	{
	  HOST_WIDE_INT val_size;

	  if (item->offset < 0 || item->jftype == IPA_JF_UNKNOWN)
	    continue;
	  val_size = tree_to_shwi (TYPE_SIZE (item->type));

	  if (merge_agg_lats_step (dest_plats, item->offset, val_size,
				   &aglat, pre_existing, &ret, max_agg_items))
	    {
	      ret |= propagate_aggregate_lattice (cs, item, *aglat);
	      aglat = &(*aglat)->next;
	    }
	  else if (dest_plats->aggs_bottom)
	    return true;
	}

      ret |= set_chain_of_aglats_contains_variable (*aglat);
    }
  else
    ret |= set_agg_lats_contain_variable (dest_plats);

  return ret;
}

/* Return true if on the way cfrom CS->caller to the final (non-alias and
   non-thunk) destination, the call passes through a thunk.  */

static bool
call_passes_through_thunk (cgraph_edge *cs)
{
  cgraph_node *alias_or_thunk = cs->callee;
  while (alias_or_thunk->alias)
    alias_or_thunk = alias_or_thunk->get_alias_target ();
  return alias_or_thunk->thunk;
}

/* Propagate constants from the caller to the callee of CS.  INFO describes the
   caller.  */

static bool
propagate_constants_across_call (struct cgraph_edge *cs)
{
  class ipa_node_params *callee_info;
  enum availability availability;
  cgraph_node *callee;
  class ipa_edge_args *args;
  bool ret = false;
  int i, args_count, parms_count;

  callee = cs->callee->function_symbol (&availability);
  if (!callee->definition)
    return false;
  gcc_checking_assert (callee->has_gimple_body_p ());
  callee_info = ipa_node_params_sum->get (callee);
  if (!callee_info)
    return false;

  args = ipa_edge_args_sum->get (cs);
  parms_count = ipa_get_param_count (callee_info);
  if (parms_count == 0)
    return false;
  if (!args
      || !opt_for_fn (cs->caller->decl, flag_ipa_cp)
      || !opt_for_fn (cs->caller->decl, optimize))
    {
      for (i = 0; i < parms_count; i++)
	ret |= set_all_contains_variable (ipa_get_parm_lattices (callee_info,
								 i));
      return ret;
    }
  args_count = ipa_get_cs_argument_count (args);

  /* If this call goes through a thunk we must not propagate to the first (0th)
     parameter.  However, we might need to uncover a thunk from below a series
     of aliases first.  */
  if (call_passes_through_thunk (cs))
    {
      ret |= set_all_contains_variable (ipa_get_parm_lattices (callee_info,
							       0));
      i = 1;
    }
  else
    i = 0;

  for (; (i < args_count) && (i < parms_count); i++)
    {
      struct ipa_jump_func *jump_func = ipa_get_ith_jump_func (args, i);
      class ipcp_param_lattices *dest_plats;
      tree param_type = ipa_get_type (callee_info, i);

      dest_plats = ipa_get_parm_lattices (callee_info, i);
      if (availability == AVAIL_INTERPOSABLE)
	ret |= set_all_contains_variable (dest_plats);
      else
	{
	  ret |= propagate_scalar_across_jump_function (cs, jump_func,
							&dest_plats->itself,
							param_type);
	  ret |= propagate_context_across_jump_function (cs, jump_func, i,
							 &dest_plats->ctxlat);
	  ret
	    |= propagate_bits_across_jump_function (cs, i, jump_func,
						    &dest_plats->bits_lattice);
	  ret |= propagate_aggs_across_jump_function (cs, jump_func,
						      dest_plats);
	  if (opt_for_fn (callee->decl, flag_ipa_vrp))
	    ret |= propagate_vr_across_jump_function (cs, jump_func,
						      dest_plats, param_type);
	  else
	    ret |= dest_plats->m_value_range.set_to_bottom ();
	}
    }
  for (; i < parms_count; i++)
    ret |= set_all_contains_variable (ipa_get_parm_lattices (callee_info, i));

  return ret;
}

/* If an indirect edge IE can be turned into a direct one based on KNOWN_VALS
   KNOWN_CONTEXTS, and known aggregates either in AVS or KNOWN_AGGS return
   the destination.  The latter three can be NULL.  If AGG_REPS is not NULL,
   KNOWN_AGGS is ignored.  */

static tree
ipa_get_indirect_edge_target_1 (struct cgraph_edge *ie,
				const vec<tree> &known_csts,
				const vec<ipa_polymorphic_call_context> &known_contexts,
				const ipa_argagg_value_list &avs,
				bool *speculative)
{
  int param_index = ie->indirect_info->param_index;
  HOST_WIDE_INT anc_offset;
  tree t = NULL;
  tree target = NULL;

  *speculative = false;

  if (param_index == -1)
    return NULL_TREE;

  if (!ie->indirect_info->polymorphic)
    {
      tree t = NULL;

      if (ie->indirect_info->agg_contents)
	{
	  t = NULL;
	  if ((unsigned) param_index < known_csts.length ()
	      && known_csts[param_index])
	    t = ipa_find_agg_cst_from_init (known_csts[param_index],
					    ie->indirect_info->offset,
					    ie->indirect_info->by_ref);

	  if (!t && ie->indirect_info->guaranteed_unmodified)
	    t = avs.get_value (param_index,
			       ie->indirect_info->offset / BITS_PER_UNIT,
			       ie->indirect_info->by_ref);
	}
      else if ((unsigned) param_index < known_csts.length ())
	t = known_csts[param_index];

      if (t
	  && TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL)
	return TREE_OPERAND (t, 0);
      else
	return NULL_TREE;
    }

  if (!opt_for_fn (ie->caller->decl, flag_devirtualize))
    return NULL_TREE;

  gcc_assert (!ie->indirect_info->agg_contents);
  gcc_assert (!ie->indirect_info->by_ref);
  anc_offset = ie->indirect_info->offset;

  t = NULL;

  if ((unsigned) param_index < known_csts.length ()
      && known_csts[param_index])
    t = ipa_find_agg_cst_from_init (known_csts[param_index],
				    ie->indirect_info->offset, true);

  /* Try to work out value of virtual table pointer value in replacements.  */
  /* or known aggregate values.  */
  if (!t)
    t = avs.get_value (param_index,
		       ie->indirect_info->offset / BITS_PER_UNIT,
		       true);

  /* If we found the virtual table pointer, lookup the target.  */
  if (t)
    {
      tree vtable;
      unsigned HOST_WIDE_INT offset;
      if (vtable_pointer_value_to_vtable (t, &vtable, &offset))
	{
	  bool can_refer;
	  target = gimple_get_virt_method_for_vtable (ie->indirect_info->otr_token,
						      vtable, offset, &can_refer);
	  if (can_refer)
	    {
	      if (!target
		  || fndecl_built_in_p (target, BUILT_IN_UNREACHABLE)
		  || !possible_polymorphic_call_target_p
		       (ie, cgraph_node::get (target)))
		{
		  /* Do not speculate builtin_unreachable, it is stupid!  */
		  if (ie->indirect_info->vptr_changed)
		    return NULL;
		  target = ipa_impossible_devirt_target (ie, target);
		}
	      *speculative = ie->indirect_info->vptr_changed;
	      if (!*speculative)
		return target;
	    }
	}
    }

  /* Do we know the constant value of pointer?  */
  if (!t && (unsigned) param_index < known_csts.length ())
    t = known_csts[param_index];

  gcc_checking_assert (!t || TREE_CODE (t) != TREE_BINFO);

  ipa_polymorphic_call_context context;
  if (known_contexts.length () > (unsigned int) param_index)
    {
      context = known_contexts[param_index];
      context.offset_by (anc_offset);
      if (ie->indirect_info->vptr_changed)
	context.possible_dynamic_type_change (ie->in_polymorphic_cdtor,
					      ie->indirect_info->otr_type);
      if (t)
	{
	  ipa_polymorphic_call_context ctx2 = ipa_polymorphic_call_context
	    (t, ie->indirect_info->otr_type, anc_offset);
	  if (!ctx2.useless_p ())
	    context.combine_with (ctx2, ie->indirect_info->otr_type);
	}
    }
  else if (t)
    {
      context = ipa_polymorphic_call_context (t, ie->indirect_info->otr_type,
					      anc_offset);
      if (ie->indirect_info->vptr_changed)
	context.possible_dynamic_type_change (ie->in_polymorphic_cdtor,
					      ie->indirect_info->otr_type);
    }
  else
    return NULL_TREE;

  vec <cgraph_node *>targets;
  bool final;

  targets = possible_polymorphic_call_targets
    (ie->indirect_info->otr_type,
     ie->indirect_info->otr_token,
     context, &final);
  if (!final || targets.length () > 1)
    {
      struct cgraph_node *node;
      if (*speculative)
	return target;
      if (!opt_for_fn (ie->caller->decl, flag_devirtualize_speculatively)
	  || ie->speculative || !ie->maybe_hot_p ())
	return NULL;
      node = try_speculative_devirtualization (ie->indirect_info->otr_type,
					       ie->indirect_info->otr_token,
					       context);
      if (node)
	{
	  *speculative = true;
	  target = node->decl;
	}
      else
	return NULL;
    }
  else
    {
      *speculative = false;
      if (targets.length () == 1)
	target = targets[0]->decl;
      else
	target = ipa_impossible_devirt_target (ie, NULL_TREE);
    }

  if (target && !possible_polymorphic_call_target_p (ie,
						     cgraph_node::get (target)))
    {
      if (*speculative)
	return NULL;
      target = ipa_impossible_devirt_target (ie, target);
    }

  return target;
}

/* If an indirect edge IE can be turned into a direct one based on data in
   AVALS, return the destination.  Store into *SPECULATIVE a boolean determinig
   whether the discovered target is only speculative guess.  */

tree
ipa_get_indirect_edge_target (struct cgraph_edge *ie,
			      ipa_call_arg_values *avals,
			      bool *speculative)
{
  ipa_argagg_value_list avl (avals);
  return ipa_get_indirect_edge_target_1 (ie, avals->m_known_vals,
					 avals->m_known_contexts,
					 avl, speculative);
}

/* Calculate devirtualization time bonus for NODE, assuming we know information
   about arguments stored in AVALS.  */

static int
devirtualization_time_bonus (struct cgraph_node *node,
			     ipa_auto_call_arg_values *avals)
{
  struct cgraph_edge *ie;
  int res = 0;

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    {
      struct cgraph_node *callee;
      class ipa_fn_summary *isummary;
      enum availability avail;
      tree target;
      bool speculative;

      ipa_argagg_value_list avl (avals);
      target = ipa_get_indirect_edge_target_1 (ie, avals->m_known_vals,
					       avals->m_known_contexts,
					       avl, &speculative);
      if (!target)
	continue;

      /* Only bare minimum benefit for clearly un-inlineable targets.  */
      res += 1;
      callee = cgraph_node::get (target);
      if (!callee || !callee->definition)
	continue;
      callee = callee->function_symbol (&avail);
      if (avail < AVAIL_AVAILABLE)
	continue;
      isummary = ipa_fn_summaries->get (callee);
      if (!isummary || !isummary->inlinable)
	continue;

      int size = ipa_size_summaries->get (callee)->size;
      /* FIXME: The values below need re-considering and perhaps also
	 integrating into the cost metrics, at lest in some very basic way.  */
      int max_inline_insns_auto
	= opt_for_fn (callee->decl, param_max_inline_insns_auto);
      if (size <= max_inline_insns_auto / 4)
	res += 31 / ((int)speculative + 1);
      else if (size <= max_inline_insns_auto / 2)
	res += 15 / ((int)speculative + 1);
      else if (size <= max_inline_insns_auto
	       || DECL_DECLARED_INLINE_P (callee->decl))
	res += 7 / ((int)speculative + 1);
    }

  return res;
}

/* Return time bonus incurred because of hints stored in ESTIMATES.  */

static int
hint_time_bonus (cgraph_node *node, const ipa_call_estimates &estimates)
{
  int result = 0;
  ipa_hints hints = estimates.hints;
  if (hints & (INLINE_HINT_loop_iterations | INLINE_HINT_loop_stride))
    result += opt_for_fn (node->decl, param_ipa_cp_loop_hint_bonus);

  sreal bonus_for_one = opt_for_fn (node->decl, param_ipa_cp_loop_hint_bonus);

  if (hints & INLINE_HINT_loop_iterations)
    result += (estimates.loops_with_known_iterations * bonus_for_one).to_int ();

  if (hints & INLINE_HINT_loop_stride)
    result += (estimates.loops_with_known_strides * bonus_for_one).to_int ();

  return result;
}

/* If there is a reason to penalize the function described by INFO in the
   cloning goodness evaluation, do so.  */

static inline sreal
incorporate_penalties (cgraph_node *node, ipa_node_params *info,
		       sreal evaluation)
{
  if (info->node_within_scc && !info->node_is_self_scc)
    evaluation = (evaluation
		  * (100 - opt_for_fn (node->decl,
				       param_ipa_cp_recursion_penalty))) / 100;

  if (info->node_calling_single_call)
    evaluation = (evaluation
		  * (100 - opt_for_fn (node->decl,
				       param_ipa_cp_single_call_penalty)))
      / 100;

  return evaluation;
}

/* Return true if cloning NODE is a good idea, given the estimated TIME_BENEFIT
   and SIZE_COST and with the sum of frequencies of incoming edges to the
   potential new clone in FREQUENCIES.  */

static bool
good_cloning_opportunity_p (struct cgraph_node *node, sreal time_benefit,
			    sreal freq_sum, profile_count count_sum,
			    int size_cost)
{
  if (time_benefit == 0
      || !opt_for_fn (node->decl, flag_ipa_cp_clone)
      || node->optimize_for_size_p ())
    return false;

  gcc_assert (size_cost > 0);

  ipa_node_params *info = ipa_node_params_sum->get (node);
  int eval_threshold = opt_for_fn (node->decl, param_ipa_cp_eval_threshold);
  if (count_sum.nonzero_p ())
    {
      gcc_assert (base_count.nonzero_p ());
      sreal factor = count_sum.probability_in (base_count).to_sreal ();
      sreal evaluation = (time_benefit * factor) / size_cost;
      evaluation = incorporate_penalties (node, info, evaluation);
      evaluation *= 1000;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "     good_cloning_opportunity_p (time: %g, "
		   "size: %i, count_sum: ", time_benefit.to_double (),
		   size_cost);
	  count_sum.dump (dump_file);
	  fprintf (dump_file, "%s%s) -> evaluation: %.2f, threshold: %i\n",
		 info->node_within_scc
		   ? (info->node_is_self_scc ? ", self_scc" : ", scc") : "",
		 info->node_calling_single_call ? ", single_call" : "",
		   evaluation.to_double (), eval_threshold);
	}

      return evaluation.to_int () >= eval_threshold;
    }
  else
    {
      sreal evaluation = (time_benefit * freq_sum) / size_cost;
      evaluation = incorporate_penalties (node, info, evaluation);
      evaluation *= 1000;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %g, "
		 "size: %i, freq_sum: %g%s%s) -> evaluation: %.2f, "
		 "threshold: %i\n",
		 time_benefit.to_double (), size_cost, freq_sum.to_double (),
		 info->node_within_scc
		   ? (info->node_is_self_scc ? ", self_scc" : ", scc") : "",
		 info->node_calling_single_call ? ", single_call" : "",
		 evaluation.to_double (), eval_threshold);

      return evaluation.to_int () >= eval_threshold;
    }
}

/* Grow vectors in AVALS and fill them with information about values of
   parameters that are known to be independent of the context.  Only calculate
   m_known_aggs if CALCULATE_AGGS is true.  INFO describes the function.  If
   REMOVABLE_PARAMS_COST is non-NULL, the movement cost of all removable
   parameters will be stored in it.

   TODO: Also grow context independent value range vectors.  */

static bool
gather_context_independent_values (class ipa_node_params *info,
				   ipa_auto_call_arg_values *avals,
				   bool calculate_aggs,
				   int *removable_params_cost)
{
  int i, count = ipa_get_param_count (info);
  bool ret = false;

  avals->m_known_vals.safe_grow_cleared (count, true);
  avals->m_known_contexts.safe_grow_cleared (count, true);

  if (removable_params_cost)
    *removable_params_cost = 0;

  for (i = 0; i < count; i++)
    {
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;

      if (lat->is_single_const ())
	{
	  ipcp_value<tree> *val = lat->values;
	  gcc_checking_assert (TREE_CODE (val->value) != TREE_BINFO);
	  avals->m_known_vals[i] = val->value;
	  if (removable_params_cost)
	    *removable_params_cost
	      += estimate_move_cost (TREE_TYPE (val->value), false);
	  ret = true;
	}
      else if (removable_params_cost
	       && !ipa_is_param_used (info, i))
	*removable_params_cost
	  += ipa_get_param_move_cost (info, i);

      if (!ipa_is_param_used (info, i))
	continue;

      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;
      /* Do not account known context as reason for cloning.  We can see
	 if it permits devirtualization.  */
      if (ctxlat->is_single_const ())
	avals->m_known_contexts[i] = ctxlat->values->value;

      if (calculate_aggs)
	ret |= push_agg_values_from_plats (plats, i, 0, &avals->m_known_aggs);
    }

  return ret;
}

/* Perform time and size measurement of NODE with the context given in AVALS,
   calculate the benefit compared to the node without specialization and store
   it into VAL.  Take into account REMOVABLE_PARAMS_COST of all
   context-independent or unused removable parameters and EST_MOVE_COST, the
   estimated movement of the considered parameter.  */

static void
perform_estimation_of_a_value (cgraph_node *node,
			       ipa_auto_call_arg_values *avals,
			       int removable_params_cost, int est_move_cost,
			       ipcp_value_base *val)
{
  sreal time_benefit;
  ipa_call_estimates estimates;

  estimate_ipcp_clone_size_and_time (node, avals, &estimates);

  /* Extern inline functions have no cloning local time benefits because they
     will be inlined anyway.  The only reason to clone them is if it enables
     optimization in any of the functions they call.  */
  if (DECL_EXTERNAL (node->decl) && DECL_DECLARED_INLINE_P (node->decl))
    time_benefit = 0;
  else
    time_benefit = (estimates.nonspecialized_time - estimates.time)
      + (devirtualization_time_bonus (node, avals)
	 + hint_time_bonus (node, estimates)
	 + removable_params_cost + est_move_cost);

  int size = estimates.size;
  gcc_checking_assert (size >=0);
  /* The inliner-heuristics based estimates may think that in certain
     contexts some functions do not have any size at all but we want
     all specializations to have at least a tiny cost, not least not to
     divide by zero.  */
  if (size == 0)
    size = 1;

  val->local_time_benefit = time_benefit;
  val->local_size_cost = size;
}

/* Get the overall limit oof growth based on parameters extracted from growth.
   it does not really make sense to mix functions with different overall growth
   limits but it is possible and if it happens, we do not want to select one
   limit at random.  */

static long
get_max_overall_size (cgraph_node *node)
{
  long max_new_size = orig_overall_size;
  long large_unit = opt_for_fn (node->decl, param_ipa_cp_large_unit_insns);
  if (max_new_size < large_unit)
    max_new_size = large_unit;
  int unit_growth = opt_for_fn (node->decl, param_ipa_cp_unit_growth);
  max_new_size += max_new_size * unit_growth / 100 + 1;
  return max_new_size;
}

/* Return true if NODE should be cloned just for a parameter removal, possibly
   dumping a reason if not.  */

static bool
clone_for_param_removal_p (cgraph_node *node)
{
  if (!node->can_change_signature)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Not considering cloning to remove parameters, "
		 "function cannot change signature.\n");
      return false;
    }
  if (node->can_be_local_p ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Not considering cloning to remove parameters, "
		 "IPA-SRA can do it potentially better.\n");
      return false;
    }
  return true;
}

/* Iterate over known values of parameters of NODE and estimate the local
   effects in terms of time and size they have.  */

static void
estimate_local_effects (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int count = ipa_get_param_count (info);
  bool always_const;
  int removable_params_cost;

  if (!count || !ipcp_versionable_function_p (node))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEstimating effects for %s.\n", node->dump_name ());

  ipa_auto_call_arg_values avals;
  always_const = gather_context_independent_values (info, &avals, true,
						    &removable_params_cost);
  int devirt_bonus = devirtualization_time_bonus (node, &avals);
  if (always_const || devirt_bonus
      || (removable_params_cost && clone_for_param_removal_p (node)))
    {
      struct caller_statistics stats;
      ipa_call_estimates estimates;

      init_caller_stats (&stats);
      node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats,
					      false);
      estimate_ipcp_clone_size_and_time (node, &avals, &estimates);
      sreal time = estimates.nonspecialized_time - estimates.time;
      time += devirt_bonus;
      time += hint_time_bonus (node, estimates);
      time += removable_params_cost;
      int size = estimates.size - stats.n_calls * removable_params_cost;

      if (dump_file)
	fprintf (dump_file, " - context independent values, size: %i, "
		 "time_benefit: %f\n", size, (time).to_double ());

      if (size <= 0 || node->local)
	{
	  info->do_clone_for_all_contexts = true;

	  if (dump_file)
	    fprintf (dump_file, "     Decided to specialize for all "
		     "known contexts, code not going to grow.\n");
	}
      else if (good_cloning_opportunity_p (node, time, stats.freq_sum,
					   stats.count_sum, size))
	{
	  if (size + overall_size <= get_max_overall_size (node))
	    {
	      info->do_clone_for_all_contexts = true;
	      overall_size += size;

	      if (dump_file)
		fprintf (dump_file, "     Decided to specialize for all "
			 "known contexts, growth (to %li) deemed "
			 "beneficial.\n", overall_size);
	    }
	  else if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Not cloning for all contexts because "
		     "maximum unit size would be reached with %li.\n",
		     size + overall_size);
	}
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   Not cloning for all contexts because "
		 "!good_cloning_opportunity_p.\n");

    }

  for (int i = 0; i < count; i++)
    {
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;
      ipcp_value<tree> *val;

      if (lat->bottom
	  || !lat->values
	  || avals.m_known_vals[i])
	continue;

      for (val = lat->values; val; val = val->next)
	{
	  gcc_checking_assert (TREE_CODE (val->value) != TREE_BINFO);
	  avals.m_known_vals[i] = val->value;

	  int emc = estimate_move_cost (TREE_TYPE (val->value), true);
	  perform_estimation_of_a_value (node, &avals, removable_params_cost,
					 emc, val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for value ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, ": time_benefit: %g, size: %i\n",
		       val->local_time_benefit.to_double (),
		       val->local_size_cost);
	    }
	}
      avals.m_known_vals[i] = NULL_TREE;
    }

  for (int i = 0; i < count; i++)
    {
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);

      if (!plats->virt_call)
	continue;

      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;
      ipcp_value<ipa_polymorphic_call_context> *val;

      if (ctxlat->bottom
	  || !ctxlat->values
	  || !avals.m_known_contexts[i].useless_p ())
	continue;

      for (val = ctxlat->values; val; val = val->next)
	{
	  avals.m_known_contexts[i] = val->value;
	  perform_estimation_of_a_value (node, &avals, removable_params_cost,
					 0, val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for polymorphic context ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, ": time_benefit: %g, size: %i\n",
		       val->local_time_benefit.to_double (),
		       val->local_size_cost);
	    }
	}
      avals.m_known_contexts[i] = ipa_polymorphic_call_context ();
    }

  unsigned all_ctx_len = avals.m_known_aggs.length ();
  auto_vec<ipa_argagg_value, 32> all_ctx;
  all_ctx.reserve_exact (all_ctx_len);
  all_ctx.splice (avals.m_known_aggs);
  avals.m_known_aggs.safe_grow_cleared (all_ctx_len + 1);

  unsigned j = 0;
  for (int index = 0; index < count; index++)
    {
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, index);

      if (plats->aggs_bottom || !plats->aggs)
	continue;

      for (ipcp_agg_lattice *aglat = plats->aggs; aglat; aglat = aglat->next)
	{
	  ipcp_value<tree> *val;
	  if (aglat->bottom || !aglat->values
	      /* If the following is true, the one value is already part of all
		 context estimations.  */
	      || (!plats->aggs_contain_variable
		  && aglat->is_single_const ()))
	    continue;

	  unsigned unit_offset = aglat->offset / BITS_PER_UNIT;
	  while (j < all_ctx_len
		 && (all_ctx[j].index < index
		     || (all_ctx[j].index == index
			 && all_ctx[j].unit_offset < unit_offset)))
	    {
	      avals.m_known_aggs[j] = all_ctx[j];
	      j++;
	    }

	  for (unsigned k = j; k < all_ctx_len; k++)
	    avals.m_known_aggs[k+1] = all_ctx[k];

	  for (val = aglat->values; val; val = val->next)
	    {
	      avals.m_known_aggs[j].value = val->value;
	      avals.m_known_aggs[j].unit_offset = unit_offset;
	      avals.m_known_aggs[j].index = index;
	      avals.m_known_aggs[j].by_ref = plats->aggs_by_ref;
	      avals.m_known_aggs[j].killed = false;

	      perform_estimation_of_a_value (node, &avals,
					     removable_params_cost, 0, val);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, " - estimates for value ");
		  print_ipcp_constant_value (dump_file, val->value);
		  fprintf (dump_file, " for ");
		  ipa_dump_param (dump_file, info, index);
		  fprintf (dump_file, "[%soffset: " HOST_WIDE_INT_PRINT_DEC
			   "]: time_benefit: %g, size: %i\n",
			   plats->aggs_by_ref ? "ref " : "",
			   aglat->offset,
			   val->local_time_benefit.to_double (),
			   val->local_size_cost);
		}
	    }
	}
    }
}


/* Add value CUR_VAL and all yet-unsorted values it is dependent on to the
   topological sort of values.  */

template <typename valtype>
void
value_topo_info<valtype>::add_val (ipcp_value<valtype> *cur_val)
{
  ipcp_value_source<valtype> *src;

  if (cur_val->dfs)
    return;

  dfs_counter++;
  cur_val->dfs = dfs_counter;
  cur_val->low_link = dfs_counter;

  cur_val->topo_next = stack;
  stack = cur_val;
  cur_val->on_stack = true;

  for (src = cur_val->sources; src; src = src->next)
    if (src->val)
      {
	if (src->val->dfs == 0)
	  {
	    add_val (src->val);
	    if (src->val->low_link < cur_val->low_link)
	      cur_val->low_link = src->val->low_link;
	  }
	else if (src->val->on_stack
		 && src->val->dfs < cur_val->low_link)
	  cur_val->low_link = src->val->dfs;
      }

  if (cur_val->dfs == cur_val->low_link)
    {
      ipcp_value<valtype> *v, *scc_list = NULL;

      do
	{
	  v = stack;
	  stack = v->topo_next;
	  v->on_stack = false;
	  v->scc_no = cur_val->dfs;

	  v->scc_next = scc_list;
	  scc_list = v;
	}
      while (v != cur_val);

      cur_val->topo_next = values_topo;
      values_topo = cur_val;
    }
}

/* Add all values in lattices associated with NODE to the topological sort if
   they are not there yet.  */

static void
add_all_node_vals_to_toposort (cgraph_node *node, ipa_topo_info *topo)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count; i++)
    {
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;
      struct ipcp_agg_lattice *aglat;

      if (!lat->bottom)
	{
	  ipcp_value<tree> *val;
	  for (val = lat->values; val; val = val->next)
	    topo->constants.add_val (val);
	}

      if (!plats->aggs_bottom)
	for (aglat = plats->aggs; aglat; aglat = aglat->next)
	  if (!aglat->bottom)
	    {
	      ipcp_value<tree> *val;
	      for (val = aglat->values; val; val = val->next)
		topo->constants.add_val (val);
	    }

      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;
      if (!ctxlat->bottom)
	{
	  ipcp_value<ipa_polymorphic_call_context> *ctxval;
	  for (ctxval = ctxlat->values; ctxval; ctxval = ctxval->next)
	    topo->contexts.add_val (ctxval);
	}
    }
}

/* One pass of constants propagation along the call graph edges, from callers
   to callees (requires topological ordering in TOPO), iterate over strongly
   connected components.  */

static void
propagate_constants_topo (class ipa_topo_info *topo)
{
  int i;

  for (i = topo->nnodes - 1; i >= 0; i--)
    {
      unsigned j;
      struct cgraph_node *v, *node = topo->order[i];
      vec<cgraph_node *> cycle_nodes = ipa_get_nodes_in_cycle (node);

      /* First, iteratively propagate within the strongly connected component
	 until all lattices stabilize.  */
      FOR_EACH_VEC_ELT (cycle_nodes, j, v)
	if (v->has_gimple_body_p ())
	  {
	    if (opt_for_fn (v->decl, flag_ipa_cp)
		&& opt_for_fn (v->decl, optimize))
	      push_node_to_stack (topo, v);
	    /* When V is not optimized, we can not push it to stack, but
	       still we need to set all its callees lattices to bottom.  */
	    else
	      {
		for (cgraph_edge *cs = v->callees; cs; cs = cs->next_callee)
	           propagate_constants_across_call (cs);
	      }
	  }

      v = pop_node_from_stack (topo);
      while (v)
	{
	  struct cgraph_edge *cs;
	  class ipa_node_params *info = NULL;
	  bool self_scc = true;

	  for (cs = v->callees; cs; cs = cs->next_callee)
	    if (ipa_edge_within_scc (cs))
	      {
		cgraph_node *callee = cs->callee->function_symbol ();

		if (v != callee)
		  self_scc = false;

		if (!info)
		  {
		    info = ipa_node_params_sum->get (v);
		    info->node_within_scc = true;
		  }

		if (propagate_constants_across_call (cs))
		  push_node_to_stack (topo, callee);
	      }

	  if (info)
	    info->node_is_self_scc = self_scc;

	  v = pop_node_from_stack (topo);
	}

      /* Afterwards, propagate along edges leading out of the SCC, calculates
	 the local effects of the discovered constants and all valid values to
	 their topological sort.  */
      FOR_EACH_VEC_ELT (cycle_nodes, j, v)
	if (v->has_gimple_body_p ()
	    && opt_for_fn (v->decl, flag_ipa_cp)
	    && opt_for_fn (v->decl, optimize))
	  {
	    struct cgraph_edge *cs;

	    estimate_local_effects (v);
	    add_all_node_vals_to_toposort (v, topo);
	    for (cs = v->callees; cs; cs = cs->next_callee)
	      if (!ipa_edge_within_scc (cs))
		propagate_constants_across_call (cs);
	  }
      cycle_nodes.release ();
    }
}

/* Propagate the estimated effects of individual values along the topological
   from the dependent values to those they depend on.  */

template <typename valtype>
void
value_topo_info<valtype>::propagate_effects ()
{
  ipcp_value<valtype> *base;
  hash_set<ipcp_value<valtype> *> processed_srcvals;

  for (base = values_topo; base; base = base->topo_next)
    {
      ipcp_value_source<valtype> *src;
      ipcp_value<valtype> *val;
      sreal time = 0;
      HOST_WIDE_INT size = 0;

      for (val = base; val; val = val->scc_next)
	{
	  time = time + val->local_time_benefit + val->prop_time_benefit;
	  size = size + val->local_size_cost + val->prop_size_cost;
	}

      for (val = base; val; val = val->scc_next)
	{
	  processed_srcvals.empty ();
	  for (src = val->sources; src; src = src->next)
	    if (src->val
		&& src->cs->maybe_hot_p ())
	      {
		if (!processed_srcvals.add (src->val))
		  {
		    HOST_WIDE_INT prop_size = size + src->val->prop_size_cost;
		    if (prop_size < INT_MAX)
		      src->val->prop_size_cost = prop_size;
		    else
		      continue;
		  }

		int special_factor = 1;
		if (val->same_scc (src->val))
		  special_factor
		    = opt_for_fn(src->cs->caller->decl,
				 param_ipa_cp_recursive_freq_factor);
		else if (val->self_recursion_generated_p ()
			 && (src->cs->callee->function_symbol ()
			     == src->cs->caller))
		  {
		    int max_recur_gen_depth
		      = opt_for_fn(src->cs->caller->decl,
				   param_ipa_cp_max_recursive_depth);
		    special_factor = max_recur_gen_depth
		      - val->self_recursion_generated_level + 1;
		  }

		src->val->prop_time_benefit
		  += time * special_factor * src->cs->sreal_frequency ();
	      }

	  if (size < INT_MAX)
	    {
	      val->prop_time_benefit = time;
	      val->prop_size_cost = size;
	    }
	  else
	    {
	      val->prop_time_benefit = 0;
	      val->prop_size_cost = 0;
	    }
	}
    }
}

/* Callback for qsort to sort counts of all edges.  */

static int
compare_edge_profile_counts (const void *a, const void *b)
{
  const profile_count *cnt1 = (const profile_count *) a;
  const profile_count *cnt2 = (const profile_count *) b;

  if (*cnt1 < *cnt2)
    return 1;
  if (*cnt1 > *cnt2)
    return -1;
  return 0;
}


/* Propagate constants, polymorphic contexts and their effects from the
   summaries interprocedurally.  */

static void
ipcp_propagate_stage (class ipa_topo_info *topo)
{
  struct cgraph_node *node;

  if (dump_file)
    fprintf (dump_file, "\n Propagating constants:\n\n");

  base_count = profile_count::uninitialized ();

  bool compute_count_base = false;
  unsigned base_count_pos_percent = 0;
  FOR_EACH_DEFINED_FUNCTION (node)
  {
    if (node->has_gimple_body_p ()
	&& opt_for_fn (node->decl, flag_ipa_cp)
	&& opt_for_fn (node->decl, optimize))
      {
        ipa_node_params *info = ipa_node_params_sum->get (node);
        determine_versionability (node, info);

	unsigned nlattices = ipa_get_param_count (info);
	info->lattices.safe_grow_cleared (nlattices, true);
	initialize_node_lattices (node);
      }
    ipa_size_summary *s = ipa_size_summaries->get (node);
    if (node->definition && !node->alias && s != NULL)
      overall_size += s->self_size;
    if (node->count.ipa ().initialized_p ())
      {
	compute_count_base = true;
	unsigned pos_percent = opt_for_fn (node->decl,
					   param_ipa_cp_profile_count_base);
	base_count_pos_percent = MAX (base_count_pos_percent, pos_percent);
      }
  }

  if (compute_count_base)
    {
      auto_vec<profile_count> all_edge_counts;
      all_edge_counts.reserve_exact (symtab->edges_count);
      FOR_EACH_DEFINED_FUNCTION (node)
	for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
	  {
	    profile_count count = cs->count.ipa ();
	    if (!count.nonzero_p ())
	      continue;

	    enum availability avail;
	    cgraph_node *tgt
	      = cs->callee->function_or_virtual_thunk_symbol (&avail);
	    ipa_node_params *info = ipa_node_params_sum->get (tgt);
	    if (info && info->versionable)
	      all_edge_counts.quick_push (count);
	  }

      if (!all_edge_counts.is_empty ())
	{
	  gcc_assert (base_count_pos_percent <= 100);
	  all_edge_counts.qsort (compare_edge_profile_counts);

	  unsigned base_count_pos
	    = ((all_edge_counts.length () * (base_count_pos_percent)) / 100);
	  base_count = all_edge_counts[base_count_pos];

	  if (dump_file)
	    {
	      fprintf (dump_file, "\nSelected base_count from %u edges at "
		       "position %u, arriving at: ", all_edge_counts.length (),
		       base_count_pos);
	      base_count.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
      else if (dump_file)
	fprintf (dump_file, "\nNo candidates with non-zero call count found, "
		 "continuing as if without profile feedback.\n");
    }

  orig_overall_size = overall_size;

  if (dump_file)
    fprintf (dump_file, "\noverall_size: %li\n", overall_size);

  propagate_constants_topo (topo);
  if (flag_checking)
    ipcp_verify_propagated_values ();
  topo->constants.propagate_effects ();
  topo->contexts.propagate_effects ();

  if (dump_file)
    {
      fprintf (dump_file, "\nIPA lattices after all propagation:\n");
      print_all_lattices (dump_file, (dump_flags & TDF_DETAILS), true);
    }
}

/* Discover newly direct outgoing edges from NODE which is a new clone with
   known KNOWN_CSTS and make them direct.  */

static void
ipcp_discover_new_direct_edges (struct cgraph_node *node,
				vec<tree> known_csts,
				vec<ipa_polymorphic_call_context>
				known_contexts,
				vec<ipa_argagg_value, va_gc> *aggvals)
{
  struct cgraph_edge *ie, *next_ie;
  bool found = false;

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      tree target;
      bool speculative;

      next_ie = ie->next_callee;
      ipa_argagg_value_list avs (aggvals);
      target = ipa_get_indirect_edge_target_1 (ie, known_csts, known_contexts,
					       avs, &speculative);
      if (target)
	{
	  bool agg_contents = ie->indirect_info->agg_contents;
	  bool polymorphic = ie->indirect_info->polymorphic;
	  int param_index = ie->indirect_info->param_index;
	  struct cgraph_edge *cs = ipa_make_edge_direct_to_target (ie, target,
								   speculative);
	  found = true;

	  if (cs && !agg_contents && !polymorphic)
	    {
	      ipa_node_params *info = ipa_node_params_sum->get (node);
	      int c = ipa_get_controlled_uses (info, param_index);
	      if (c != IPA_UNDESCRIBED_USE
		  && !ipa_get_param_load_dereferenced (info, param_index))
		{
		  struct ipa_ref *to_del;

		  c--;
		  ipa_set_controlled_uses (info, param_index, c);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "     controlled uses count of param "
			     "%i bumped down to %i\n", param_index, c);
		  if (c == 0
		      && (to_del = node->find_reference (cs->callee, NULL, 0,
							 IPA_REF_ADDR)))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "       and even removing its "
				 "cloning-created reference\n");
		      to_del->remove_reference ();
		    }
		}
	    }
	}
    }
  /* Turning calls to direct calls will improve overall summary.  */
  if (found)
    ipa_update_overall_fn_summary (node);
}

class edge_clone_summary;
static call_summary <edge_clone_summary *> *edge_clone_summaries = NULL;

/* Edge clone summary.  */

class edge_clone_summary
{
public:
  /* Default constructor.  */
  edge_clone_summary (): prev_clone (NULL), next_clone (NULL) {}

  /* Default destructor.  */
  ~edge_clone_summary ()
  {
    if (prev_clone)
      edge_clone_summaries->get (prev_clone)->next_clone = next_clone;
    if (next_clone)
      edge_clone_summaries->get (next_clone)->prev_clone = prev_clone;
  }

  cgraph_edge *prev_clone;
  cgraph_edge *next_clone;
};

class edge_clone_summary_t:
  public call_summary <edge_clone_summary *>
{
public:
  edge_clone_summary_t (symbol_table *symtab):
    call_summary <edge_clone_summary *> (symtab)
    {
      m_initialize_when_cloning = true;
    }

  void duplicate (cgraph_edge *src_edge, cgraph_edge *dst_edge,
		  edge_clone_summary *src_data,
		  edge_clone_summary *dst_data) final override;
};

/* Edge duplication hook.  */

void
edge_clone_summary_t::duplicate (cgraph_edge *src_edge, cgraph_edge *dst_edge,
				 edge_clone_summary *src_data,
				 edge_clone_summary *dst_data)
{
  if (src_data->next_clone)
    edge_clone_summaries->get (src_data->next_clone)->prev_clone = dst_edge;
  dst_data->prev_clone = src_edge;
  dst_data->next_clone = src_data->next_clone;
  src_data->next_clone = dst_edge;
}

/* Return true is CS calls DEST or its clone for all contexts.  When
   ALLOW_RECURSION_TO_CLONE is false, also return false for self-recursive
   edges from/to an all-context clone.  */

static bool
calls_same_node_or_its_all_contexts_clone_p (cgraph_edge *cs, cgraph_node *dest,
					     bool allow_recursion_to_clone)
{
  enum availability availability;
  cgraph_node *callee = cs->callee->function_symbol (&availability);

  if (availability <= AVAIL_INTERPOSABLE)
    return false;
  if (callee == dest)
    return true;
  if (!allow_recursion_to_clone && cs->caller == callee)
    return false;

  ipa_node_params *info = ipa_node_params_sum->get (callee);
  return info->is_all_contexts_clone && info->ipcp_orig_node == dest;
}

/* Return true if edge CS does bring about the value described by SRC to
   DEST_VAL of node DEST or its clone for all contexts.  */

static bool
cgraph_edge_brings_value_p (cgraph_edge *cs, ipcp_value_source<tree> *src,
			    cgraph_node *dest, ipcp_value<tree> *dest_val)
{
  ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);

  if (!calls_same_node_or_its_all_contexts_clone_p (cs, dest, !src->val)
      || caller_info->node_dead)
    return false;

  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    {
      tree t = NULL_TREE;
      if (src->offset == -1)
	t = caller_info->known_csts[src->index];
      else if (ipcp_transformation *ts
	       = ipcp_get_transformation_summary (cs->caller))
	{
	  ipa_argagg_value_list avl (ts);
	  t = avl.get_value (src->index, src->offset / BITS_PER_UNIT);
	}
      return (t != NULL_TREE
	      && values_equal_for_ipcp_p (src->val->value, t));
    }
  else
    {
      if (src->val == dest_val)
	return true;

      struct ipcp_agg_lattice *aglat;
      class ipcp_param_lattices *plats = ipa_get_parm_lattices (caller_info,
								 src->index);
      if (src->offset == -1)
	return (plats->itself.is_single_const ()
		&& values_equal_for_ipcp_p (src->val->value,
					    plats->itself.values->value));
      else
	{
	  if (plats->aggs_bottom || plats->aggs_contain_variable)
	    return false;
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    if (aglat->offset == src->offset)
	      return  (aglat->is_single_const ()
		       && values_equal_for_ipcp_p (src->val->value,
						   aglat->values->value));
	}
      return false;
    }
}

/* Return true if edge CS does bring about the value described by SRC to
   DST_VAL of node DEST or its clone for all contexts.  */

static bool
cgraph_edge_brings_value_p (cgraph_edge *cs,
			    ipcp_value_source<ipa_polymorphic_call_context> *src,
			    cgraph_node *dest,
			    ipcp_value<ipa_polymorphic_call_context> *)
{
  ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);

  if (!calls_same_node_or_its_all_contexts_clone_p (cs, dest, true)
      || caller_info->node_dead)
    return false;
  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    return (caller_info->known_contexts.length () > (unsigned) src->index)
      && values_equal_for_ipcp_p (src->val->value,
				  caller_info->known_contexts[src->index]);

  class ipcp_param_lattices *plats = ipa_get_parm_lattices (caller_info,
							     src->index);
  return plats->ctxlat.is_single_const ()
    && values_equal_for_ipcp_p (src->val->value,
				plats->ctxlat.values->value);
}

/* Get the next clone in the linked list of clones of an edge.  */

static inline struct cgraph_edge *
get_next_cgraph_edge_clone (struct cgraph_edge *cs)
{
  edge_clone_summary *s = edge_clone_summaries->get (cs);
  return s != NULL ? s->next_clone : NULL;
}

/* Given VAL that is intended for DEST, iterate over all its sources and if any
   of them is viable and hot, return true.  In that case, for those that still
   hold, add their edge frequency and their number and cumulative profile
   counts of self-ecursive and other edges into *FREQUENCY, *CALLER_COUNT,
   REC_COUNT_SUM and NONREC_COUNT_SUM respectively.  */

template <typename valtype>
static bool
get_info_about_necessary_edges (ipcp_value<valtype> *val, cgraph_node *dest,
				sreal *freq_sum, int *caller_count,
				profile_count *rec_count_sum,
				profile_count *nonrec_count_sum)
{
  ipcp_value_source<valtype> *src;
  sreal freq = 0;
  int count = 0;
  profile_count rec_cnt = profile_count::zero ();
  profile_count nonrec_cnt = profile_count::zero ();
  bool hot = false;
  bool non_self_recursive = false;

  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src, dest, val))
	    {
	      count++;
	      freq += cs->sreal_frequency ();
	      hot |= cs->maybe_hot_p ();
	      if (cs->caller != dest)
		{
		  non_self_recursive = true;
		  if (cs->count.ipa ().initialized_p ())
		    rec_cnt += cs->count.ipa ();
		}
	      else if (cs->count.ipa ().initialized_p ())
	        nonrec_cnt += cs->count.ipa ();
	    }
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  /* If the only edges bringing a value are self-recursive ones, do not bother
     evaluating it.  */
  if (!non_self_recursive)
    return false;

  *freq_sum = freq;
  *caller_count = count;
  *rec_count_sum = rec_cnt;
  *nonrec_count_sum = nonrec_cnt;

  if (!hot && ipa_node_params_sum->get (dest)->node_within_scc)
    {
      struct cgraph_edge *cs;

      /* Cold non-SCC source edge could trigger hot recursive execution of
	 function. Consider the case as hot and rely on following cost model
	 computation to further select right one.  */
      for (cs = dest->callers; cs; cs = cs->next_caller)
	if (cs->caller == dest && cs->maybe_hot_p ())
	  return true;
    }

  return hot;
}

/* Given a NODE, and a set of its CALLERS, try to adjust order of the callers
   to let a non-self-recursive caller be the first element.  Thus, we can
   simplify intersecting operations on values that arrive from all of these
   callers, especially when there exists self-recursive call.  Return true if
   this kind of adjustment is possible.  */

static bool
adjust_callers_for_value_intersection (vec<cgraph_edge *> &callers,
				       cgraph_node *node)
{
  for (unsigned i = 0; i < callers.length (); i++)
    {
      cgraph_edge *cs = callers[i];

      if (cs->caller != node)
	{
	  if (i > 0)
	    {
	      callers[i] = callers[0];
	      callers[0] = cs;
	    }
	  return true;
	}
    }
  return false;
}

/* Return a vector of incoming edges that do bring value VAL to node DEST.  It
   is assumed their number is known and equal to CALLER_COUNT.  */

template <typename valtype>
static vec<cgraph_edge *>
gather_edges_for_value (ipcp_value<valtype> *val, cgraph_node *dest,
			int caller_count)
{
  ipcp_value_source<valtype> *src;
  vec<cgraph_edge *> ret;

  ret.create (caller_count);
  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src, dest, val))
	    ret.quick_push (cs);
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  if (caller_count > 1)
    adjust_callers_for_value_intersection (ret, dest);

  return ret;
}

/* Construct a replacement map for a know VALUE for a formal parameter PARAM.
   Return it or NULL if for some reason it cannot be created.  FORCE_LOAD_REF
   should be set to true when the reference created for the constant should be
   a load one and not an address one because the corresponding parameter p is
   only used as *p.  */

static struct ipa_replace_map *
get_replacement_map (class ipa_node_params *info, tree value, int parm_num,
		     bool force_load_ref)
{
  struct ipa_replace_map *replace_map;

  replace_map = ggc_alloc<ipa_replace_map> ();
  if (dump_file)
    {
      fprintf (dump_file, "    replacing ");
      ipa_dump_param (dump_file, info, parm_num);

      fprintf (dump_file, " with const ");
      print_generic_expr (dump_file, value);

      if (force_load_ref)
	fprintf (dump_file, " - forcing load reference\n");
      else
	fprintf (dump_file, "\n");
    }
  replace_map->parm_num = parm_num;
  replace_map->new_tree = value;
  replace_map->force_load_ref = force_load_ref;
  return replace_map;
}

/* Dump new profiling counts of NODE.  SPEC is true when NODE is a specialzied
   one, otherwise it will be referred to as the original node.  */

static void
dump_profile_updates (cgraph_node *node, bool spec)
{
  if (spec)
    fprintf (dump_file, "     setting count of the specialized node %s to ",
	     node->dump_name ());
  else
    fprintf (dump_file, "     setting count of the original node %s to ",
	     node->dump_name ());

  node->count.dump (dump_file);
  fprintf (dump_file, "\n");
  for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
    {
      fprintf (dump_file, "       edge to %s has count ",
	       cs->callee->dump_name ());
      cs->count.dump (dump_file);
      fprintf (dump_file, "\n");
    }
}

/* With partial train run we do not want to assume that original's count is
   zero whenever we redurect all executed edges to clone.  Simply drop profile
   to local one in this case.  In eany case, return the new value.  ORIG_NODE
   is the original node and its count has not been updaed yet.  */

profile_count
lenient_count_portion_handling (profile_count remainder, cgraph_node *orig_node)
{
  if (remainder.ipa_p () && !remainder.ipa ().nonzero_p ()
      && orig_node->count.ipa_p () && orig_node->count.ipa ().nonzero_p ()
      && opt_for_fn (orig_node->decl, flag_profile_partial_training))
    remainder = remainder.guessed_local ();

  return remainder;
}

/* Structure to sum counts coming from nodes other than the original node and
   its clones.  */

struct gather_other_count_struct
{
  cgraph_node *orig;
  profile_count other_count;
};

/* Worker callback of call_for_symbol_thunks_and_aliases summing the number of
   counts that come from non-self-recursive calls..  */

static bool
gather_count_of_non_rec_edges (cgraph_node *node, void *data)
{
  gather_other_count_struct *desc = (gather_other_count_struct *) data;
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller != desc->orig && cs->caller->clone_of != desc->orig)
      desc->other_count += cs->count.ipa ();
  return false;
}

/* Structure to help analyze if we need to boost counts of some clones of some
   non-recursive edges to match the new callee count.  */

struct desc_incoming_count_struct
{
  cgraph_node *orig;
  hash_set <cgraph_edge *> *processed_edges;
  profile_count count;
  unsigned unproc_orig_rec_edges;
};

/* Go over edges calling NODE and its thunks and gather information about
   incoming counts so that we know if we need to make any adjustments.  */

static void
analyze_clone_icoming_counts (cgraph_node *node,
			      desc_incoming_count_struct *desc)
{
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk)
      {
	analyze_clone_icoming_counts (cs->caller, desc);
	continue;
      }
    else
      {
	if (cs->count.initialized_p ())
	  desc->count += cs->count.ipa ();
	if (!desc->processed_edges->contains (cs)
	    && cs->caller->clone_of == desc->orig)
	  desc->unproc_orig_rec_edges++;
      }
}

/* If caller edge counts of a clone created for a self-recursive arithmetic
   jump function must be adjusted because it is coming from a the "seed" clone
   for the first value and so has been excessively scaled back as if it was not
   a recursive call, adjust it so that the incoming counts of NODE match its
   count. NODE is the node or its thunk.  */

static void
adjust_clone_incoming_counts (cgraph_node *node,
			      desc_incoming_count_struct *desc)
{
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk)
      {
	adjust_clone_incoming_counts (cs->caller, desc);
	profile_count sum = profile_count::zero ();
	for (cgraph_edge *e = cs->caller->callers; e; e = e->next_caller)
	  if (e->count.initialized_p ())
	    sum += e->count.ipa ();
	cs->count = cs->count.combine_with_ipa_count (sum);
      }
    else if (!desc->processed_edges->contains (cs)
	     && cs->caller->clone_of == desc->orig)
      {
	cs->count += desc->count;
	if (dump_file)
	  {
	    fprintf (dump_file, "       Adjusted count of an incoming edge of "
		     "a clone %s -> %s to ", cs->caller->dump_name (),
		     cs->callee->dump_name ());
	    cs->count.dump (dump_file);
	    fprintf (dump_file, "\n");
	  }
      }
}

/* When ORIG_NODE has been cloned for values which have been generated fora
   self-recursive call as a result of an arithmetic pass-through
   jump-functions, adjust its count together with counts of all such clones in
   SELF_GEN_CLONES which also at this point contains ORIG_NODE itself.

   The function sums the counts of the original node and all its clones that
   cannot be attributed to a specific clone because it comes from a
   non-recursive edge.  This sum is then evenly divided between the clones and
   on top of that each one gets all the counts which can be attributed directly
   to it.  */

static void
update_counts_for_self_gen_clones (cgraph_node *orig_node,
				   const vec<cgraph_node *> &self_gen_clones)
{
  profile_count redist_sum = orig_node->count.ipa ();
  if (!(redist_sum > profile_count::zero ()))
    return;

  if (dump_file)
    fprintf (dump_file, "     Updating profile of self recursive clone "
	     "series\n");

  gather_other_count_struct gocs;
  gocs.orig = orig_node;
  gocs.other_count = profile_count::zero ();

  auto_vec <profile_count, 8> other_edges_count;
  for (cgraph_node *n : self_gen_clones)
    {
      gocs.other_count = profile_count::zero ();
      n->call_for_symbol_thunks_and_aliases (gather_count_of_non_rec_edges,
					     &gocs, false);
      other_edges_count.safe_push (gocs.other_count);
      redist_sum -= gocs.other_count;
    }

  hash_set<cgraph_edge *> processed_edges;
  unsigned i = 0;
  for (cgraph_node *n : self_gen_clones)
    {
      profile_count orig_count = n->count;
      profile_count new_count
	= (redist_sum / self_gen_clones.length () + other_edges_count[i]);
      new_count = lenient_count_portion_handling (new_count, orig_node);
      n->count = new_count;
      profile_count::adjust_for_ipa_scaling (&new_count, &orig_count);
      for (cgraph_edge *cs = n->callees; cs; cs = cs->next_callee)
	{
	  cs->count = cs->count.apply_scale (new_count, orig_count);
	  processed_edges.add (cs);
	}
      for (cgraph_edge *cs = n->indirect_calls; cs; cs = cs->next_callee)
	cs->count = cs->count.apply_scale (new_count, orig_count);

      i++;
    }

  /* There are still going to be edges to ORIG_NODE that have one or more
     clones coming from another node clone in SELF_GEN_CLONES and which we
     scaled by the same amount, which means that the total incoming sum of
     counts to ORIG_NODE will be too high, scale such edges back.  */
  for (cgraph_edge *cs = orig_node->callees; cs; cs = cs->next_callee)
    {
      if (cs->callee->ultimate_alias_target () == orig_node)
	{
	  unsigned den = 0;
	  for (cgraph_edge *e = cs; e; e = get_next_cgraph_edge_clone (e))
	    if (e->callee->ultimate_alias_target () == orig_node
		&& processed_edges.contains (e))
	      den++;
	  if (den > 0)
	    for (cgraph_edge *e = cs; e; e = get_next_cgraph_edge_clone (e))
	      if (e->callee->ultimate_alias_target () == orig_node
		  && processed_edges.contains (e))
		e->count /= den;
	}
    }

  /* Edges from the seeds of the valus generated for arithmetic jump-functions
     along self-recursive edges are likely to have fairly low count and so
     edges from them to nodes in the self_gen_clones do not correspond to the
     artificially distributed count of the nodes, the total sum of incoming
     edges to some clones might be too low.  Detect this situation and correct
     it.  */
  for (cgraph_node *n : self_gen_clones)
    {
      if (!(n->count.ipa () > profile_count::zero ()))
	continue;

      desc_incoming_count_struct desc;
      desc.orig = orig_node;
      desc.processed_edges = &processed_edges;
      desc.count = profile_count::zero ();
      desc.unproc_orig_rec_edges = 0;
      analyze_clone_icoming_counts (n, &desc);

      if (n->count.differs_from_p (desc.count))
	{
	  if (n->count > desc.count
	      && desc.unproc_orig_rec_edges > 0)
	    {
	      desc.count = n->count - desc.count;
	      desc.count = desc.count /= desc.unproc_orig_rec_edges;
	      adjust_clone_incoming_counts (n, &desc);
	    }
	  else if (dump_file)
	    fprintf (dump_file,
		     "       Unable to fix up incoming counts for %s.\n",
		     n->dump_name ());
	}
    }

  if (dump_file)
    for (cgraph_node *n : self_gen_clones)
      dump_profile_updates (n, n != orig_node);
  return;
}

/* After a specialized NEW_NODE version of ORIG_NODE has been created, update
   their profile information to reflect this.  This function should not be used
   for clones generated for arithmetic pass-through jump functions on a
   self-recursive call graph edge, that situation is handled by
   update_counts_for_self_gen_clones.  */

static void
update_profiling_info (struct cgraph_node *orig_node,
		       struct cgraph_node *new_node)
{
  struct caller_statistics stats;
  profile_count new_sum;
  profile_count remainder, orig_node_count = orig_node->count.ipa ();

  if (!(orig_node_count > profile_count::zero ()))
    return;

  if (dump_file)
    {
      fprintf (dump_file, "     Updating profile from original count: ");
      orig_node_count.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  init_caller_stats (&stats, new_node);
  new_node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats,
					      false);
  new_sum = stats.count_sum;

  bool orig_edges_processed = false;
  if (new_sum > orig_node_count)
    {
      /* TODO: Profile has alreay gone astray, keep what we have but lower it
	 to global0 category.  */
      remainder = orig_node->count.global0 ();

      for (cgraph_edge *cs = orig_node->callees; cs; cs = cs->next_callee)
	cs->count = cs->count.global0 ();
      for (cgraph_edge *cs = orig_node->indirect_calls;
	   cs;
	   cs = cs->next_callee)
	cs->count = cs->count.global0 ();
      orig_edges_processed = true;
    }
  else if (stats.rec_count_sum.nonzero_p ())
    {
      int new_nonrec_calls = stats.n_nonrec_calls;
      /* There are self-recursive edges which are likely to bring in the
	 majority of calls but which we must divide in between the original and
	 new node.  */
      init_caller_stats (&stats, orig_node);
      orig_node->call_for_symbol_thunks_and_aliases (gather_caller_stats,
						     &stats, false);
      int orig_nonrec_calls = stats.n_nonrec_calls;
      profile_count orig_nonrec_call_count = stats.count_sum;

      if (orig_node->local)
	{
	  if (!orig_nonrec_call_count.nonzero_p ())
	    {
	      if (dump_file)
		fprintf (dump_file, "       The original is local and the only "
			 "incoming edges from non-dead callers with nonzero "
			 "counts are self-recursive, assuming it is cold.\n");
	      /* The NEW_NODE count and counts of all its outgoing edges
		 are still unmodified copies of ORIG_NODE's.  Just clear
		 the latter and bail out.  */
	      profile_count zero;
              if (opt_for_fn (orig_node->decl, flag_profile_partial_training))
                zero = profile_count::zero ().guessed_local ();
	      else
		zero = profile_count::adjusted_zero ();
	      orig_node->count = zero;
	      for (cgraph_edge *cs = orig_node->callees;
		   cs;
		   cs = cs->next_callee)
		cs->count = zero;
	      for (cgraph_edge *cs = orig_node->indirect_calls;
		   cs;
		   cs = cs->next_callee)
		cs->count = zero;
	      return;
	    }
	}
      else
	{
	  /* Let's behave as if there was another caller that accounts for all
	     the calls that were either indirect or from other compilation
	     units. */
	  orig_nonrec_calls++;
	  profile_count pretend_caller_count
	    = (orig_node_count - new_sum - orig_nonrec_call_count
	       - stats.rec_count_sum);
	  orig_nonrec_call_count += pretend_caller_count;
	}

      /* Divide all "unexplained" counts roughly proportionally to sums of
	 counts of non-recursive calls.

	 We put rather arbitrary limits on how many counts we claim because the
	 number of non-self-recursive incoming count is only a rough guideline
	 and there are cases (such as mcf) where using it blindly just takes
	 too many.  And if lattices are considered in the opposite order we
	 could also take too few.  */
      profile_count unexp = orig_node_count - new_sum - orig_nonrec_call_count;

      int limit_den = 2 * (orig_nonrec_calls + new_nonrec_calls);
      profile_count new_part
	= MAX(MIN (unexp.apply_scale (new_sum,
				      new_sum + orig_nonrec_call_count),
		   unexp.apply_scale (limit_den - 1, limit_den)),
	      unexp.apply_scale (new_nonrec_calls, limit_den));
      if (dump_file)
	{
	  fprintf (dump_file, "       Claiming ");
	  new_part.dump (dump_file);
	  fprintf (dump_file, " of unexplained ");
	  unexp.dump (dump_file);
	  fprintf (dump_file, " counts because of self-recursive "
		   "calls\n");
	}
      new_sum += new_part;
      remainder = lenient_count_portion_handling (orig_node_count - new_sum,
						  orig_node);
    }
  else
    remainder = lenient_count_portion_handling (orig_node_count - new_sum,
						orig_node);

  new_sum = orig_node_count.combine_with_ipa_count (new_sum);
  new_node->count = new_sum;
  orig_node->count = remainder;

  profile_count orig_new_node_count = orig_node_count;
  profile_count::adjust_for_ipa_scaling (&new_sum, &orig_new_node_count);
  for (cgraph_edge *cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (new_sum, orig_new_node_count);
  for (cgraph_edge *cs = new_node->indirect_calls; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (new_sum, orig_new_node_count);

  if (!orig_edges_processed)
    {
      profile_count::adjust_for_ipa_scaling (&remainder, &orig_node_count);
      for (cgraph_edge *cs = orig_node->callees; cs; cs = cs->next_callee)
	cs->count = cs->count.apply_scale (remainder, orig_node_count);
      for (cgraph_edge *cs = orig_node->indirect_calls;
	   cs;
	   cs = cs->next_callee)
	cs->count = cs->count.apply_scale (remainder, orig_node_count);
    }

  if (dump_file)
    {
      dump_profile_updates (new_node, true);
      dump_profile_updates (orig_node, false);
    }
}

/* Update the respective profile of specialized NEW_NODE and the original
   ORIG_NODE after additional edges with cumulative count sum REDIRECTED_SUM
   have been redirected to the specialized version.  */

static void
update_specialized_profile (struct cgraph_node *new_node,
			    struct cgraph_node *orig_node,
			    profile_count redirected_sum)
{
  struct cgraph_edge *cs;
  profile_count new_node_count, orig_node_count = orig_node->count.ipa ();

  if (dump_file)
    {
      fprintf (dump_file, "    the sum of counts of redirected  edges is ");
      redirected_sum.dump (dump_file);
      fprintf (dump_file, "\n    old ipa count of the original node is ");
      orig_node_count.dump (dump_file);
      fprintf (dump_file, "\n");
    }
  if (!(orig_node_count > profile_count::zero ()))
    return;

  new_node_count = new_node->count;
  new_node->count += redirected_sum;
  orig_node->count
    = lenient_count_portion_handling (orig_node->count - redirected_sum,
				      orig_node);

  for (cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count += cs->count.apply_scale (redirected_sum, new_node_count);

  for (cs = orig_node->callees; cs; cs = cs->next_callee)
    {
      profile_count dec = cs->count.apply_scale (redirected_sum,
						 orig_node_count);
      cs->count -= dec;
    }

  if (dump_file)
    {
      dump_profile_updates (new_node, true);
      dump_profile_updates (orig_node, false);
    }
}

static void adjust_references_in_caller (cgraph_edge *cs,
					 symtab_node *symbol, int index);

/* Simple structure to pass a symbol and index (with same meaning as parameters
   of adjust_references_in_caller) through a void* parameter of a
   call_for_symbol_thunks_and_aliases callback. */
struct symbol_and_index_together
{
  symtab_node *symbol;
  int index;
};

/* Worker callback of call_for_symbol_thunks_and_aliases to recursively call
   adjust_references_in_caller on edges up in the call-graph, if necessary. */
static bool
adjust_refs_in_act_callers (struct cgraph_node *node, void *data)
{
  symbol_and_index_together *pack = (symbol_and_index_together *) data;
  for (cgraph_edge *cs = node->callers; cs; cs = cs->next_caller)
    if (!cs->caller->thunk)
      adjust_references_in_caller (cs, pack->symbol, pack->index);
  return false;
}

/* At INDEX of a function being called by CS there is an ADDR_EXPR of a
   variable which is only dereferenced and which is represented by SYMBOL.  See
   if we can remove ADDR reference in callers assosiated witht the call. */

static void
adjust_references_in_caller (cgraph_edge *cs, symtab_node *symbol, int index)
{
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, index);
  if (jfunc->type == IPA_JF_CONST)
    {
      ipa_ref *to_del = cs->caller->find_reference (symbol, cs->call_stmt,
						    cs->lto_stmt_uid,
						    IPA_REF_ADDR);
      if (!to_del)
	return;
      to_del->remove_reference ();
      ipa_zap_jf_refdesc (jfunc);
      if (dump_file)
	fprintf (dump_file, "    Removed a reference from %s to %s.\n",
		 cs->caller->dump_name (), symbol->dump_name ());
      return;
    }

  if (jfunc->type != IPA_JF_PASS_THROUGH
      || ipa_get_jf_pass_through_operation (jfunc) != NOP_EXPR
      || ipa_get_jf_pass_through_refdesc_decremented (jfunc))
    return;

  int fidx = ipa_get_jf_pass_through_formal_id (jfunc);
  cgraph_node *caller = cs->caller;
  ipa_node_params *caller_info = ipa_node_params_sum->get (caller);
  /* TODO: This consistency check may be too big and not really
     that useful.  Consider removing it.  */
  tree cst;
  if (caller_info->ipcp_orig_node)
    cst = caller_info->known_csts[fidx];
  else
    {
      ipcp_lattice<tree> *lat = ipa_get_scalar_lat (caller_info, fidx);
      gcc_assert (lat->is_single_const ());
      cst = lat->values->value;
    }
  gcc_assert (TREE_CODE (cst) == ADDR_EXPR
	      && (symtab_node::get (get_base_address (TREE_OPERAND (cst, 0)))
		  == symbol));

  int cuses = ipa_get_controlled_uses (caller_info, fidx);
  if (cuses == IPA_UNDESCRIBED_USE)
    return;
  gcc_assert (cuses > 0);
  cuses--;
  ipa_set_controlled_uses (caller_info, fidx, cuses);
  ipa_set_jf_pass_through_refdesc_decremented (jfunc, true);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "    Controlled uses of parameter %i of %s dropped "
	     "to %i.\n", fidx, caller->dump_name (), cuses);
  if (cuses)
    return;

  if (caller_info->ipcp_orig_node)
    {
      /* Cloning machinery has created a reference here, we need to either
	 remove it or change it to a read one.  */
      ipa_ref *to_del = caller->find_reference (symbol, NULL, 0, IPA_REF_ADDR);
      if (to_del)
	{
	  to_del->remove_reference ();
	  if (dump_file)
	    fprintf (dump_file, "    Removed a reference from %s to %s.\n",
		     cs->caller->dump_name (), symbol->dump_name ());
	  if (ipa_get_param_load_dereferenced (caller_info, fidx))
	    {
	      caller->create_reference (symbol, IPA_REF_LOAD, NULL);
	      if (dump_file)
		fprintf (dump_file,
			 "      ...and replaced it with LOAD one.\n");
	    }
	}
    }

  symbol_and_index_together pack;
  pack.symbol = symbol;
  pack.index = fidx;
  if (caller->can_change_signature)
    caller->call_for_symbol_thunks_and_aliases (adjust_refs_in_act_callers,
						&pack, true);
}


/* Return true if we would like to remove a parameter from NODE when cloning it
   with KNOWN_CSTS scalar constants.  */

static bool
want_remove_some_param_p (cgraph_node *node, vec<tree> known_csts)
{
  auto_vec<bool, 16> surviving;
  bool filled_vec = false;
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count; i++)
    {
      if (!known_csts[i] && ipa_is_param_used (info, i))
       continue;

      if (!filled_vec)
       {
	 clone_info *info = clone_info::get (node);
	 if (!info || !info->param_adjustments)
           return true;
	 info->param_adjustments->get_surviving_params (&surviving);
         filled_vec = true;
       }
      if (surviving.length() < (unsigned) i &&  surviving[i])
       return true;
    }
  return false;
}

/* Create a specialized version of NODE with known constants in KNOWN_CSTS,
   known contexts in KNOWN_CONTEXTS and known aggregate values in AGGVALS and
   redirect all edges in CALLERS to it.  */

static struct cgraph_node *
create_specialized_node (struct cgraph_node *node,
			 vec<tree> known_csts,
			 vec<ipa_polymorphic_call_context> known_contexts,
			 vec<ipa_argagg_value, va_gc> *aggvals,
			 vec<cgraph_edge *> &callers)
{
  ipa_node_params *new_info, *info = ipa_node_params_sum->get (node);
  vec<ipa_replace_map *, va_gc> *replace_trees = NULL;
  vec<ipa_adjusted_param, va_gc> *new_params = NULL;
  struct cgraph_node *new_node;
  int i, count = ipa_get_param_count (info);
  clone_info *cinfo = clone_info::get (node);
  ipa_param_adjustments *old_adjustments = cinfo
					   ? cinfo->param_adjustments : NULL;
  ipa_param_adjustments *new_adjustments;
  gcc_assert (!info->ipcp_orig_node);
  gcc_assert (node->can_change_signature
	      || !old_adjustments);

  if (old_adjustments)
    {
      /* At the moment all IPA optimizations should use the number of
	 parameters of the prevailing decl as the m_always_copy_start.
	 Handling any other value would complicate the code below, so for the
	 time bing let's only assert it is so.  */
      gcc_assert (old_adjustments->m_always_copy_start == count
		  || old_adjustments->m_always_copy_start < 0);
      int old_adj_count = vec_safe_length (old_adjustments->m_adj_params);
      for (i = 0; i < old_adj_count; i++)
	{
	  ipa_adjusted_param *old_adj = &(*old_adjustments->m_adj_params)[i];
	  if (!node->can_change_signature
	      || old_adj->op != IPA_PARAM_OP_COPY
	      || (!known_csts[old_adj->base_index]
		  && ipa_is_param_used (info, old_adj->base_index)))
	    {
	      ipa_adjusted_param new_adj = *old_adj;

	      new_adj.prev_clone_adjustment = true;
	      new_adj.prev_clone_index = i;
	      vec_safe_push (new_params, new_adj);
	    }
	}
      bool skip_return = old_adjustments->m_skip_return;
      new_adjustments = (new (ggc_alloc <ipa_param_adjustments> ())
			 ipa_param_adjustments (new_params, count,
						skip_return));
    }
  else if (node->can_change_signature
	   && want_remove_some_param_p (node, known_csts))
    {
      ipa_adjusted_param adj;
      memset (&adj, 0, sizeof (adj));
      adj.op = IPA_PARAM_OP_COPY;
      for (i = 0; i < count; i++)
	if (!known_csts[i] && ipa_is_param_used (info, i))
	  {
	    adj.base_index = i;
	    adj.prev_clone_index = i;
	    vec_safe_push (new_params, adj);
	  }
      new_adjustments = (new (ggc_alloc <ipa_param_adjustments> ())
			 ipa_param_adjustments (new_params, count, false));
    }
  else
    new_adjustments = NULL;

  auto_vec<cgraph_edge *, 2> self_recursive_calls;
  for (i = callers.length () - 1; i >= 0; i--)
    {
      cgraph_edge *cs = callers[i];
      if (cs->caller == node)
	{
	  self_recursive_calls.safe_push (cs);
	  callers.unordered_remove (i);
	}
    }
  replace_trees = cinfo ? vec_safe_copy (cinfo->tree_map) : NULL;
  for (i = 0; i < count; i++)
    {
      tree t = known_csts[i];
      if (!t)
	continue;

      gcc_checking_assert (TREE_CODE (t) != TREE_BINFO);

      bool load_ref = false;
      symtab_node *ref_symbol;
      if (TREE_CODE (t) == ADDR_EXPR)
	{
	  tree base = get_base_address (TREE_OPERAND (t, 0));
	  if (TREE_CODE (base) == VAR_DECL
	      && ipa_get_controlled_uses (info, i) == 0
	      && ipa_get_param_load_dereferenced (info, i)
	      && (ref_symbol = symtab_node::get (base)))
	    {
	      load_ref = true;
	      if (node->can_change_signature)
		for (cgraph_edge *caller : callers)
		  adjust_references_in_caller (caller, ref_symbol, i);
	    }
	}

      ipa_replace_map *replace_map = get_replacement_map (info, t, i, load_ref);
      if (replace_map)
	vec_safe_push (replace_trees, replace_map);
    }

  unsigned &suffix_counter = clone_num_suffixes->get_or_insert (
			       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (
				 node->decl)));
  new_node = node->create_virtual_clone (callers, replace_trees,
					 new_adjustments, "constprop",
					 suffix_counter);
  suffix_counter++;

  bool have_self_recursive_calls = !self_recursive_calls.is_empty ();
  for (unsigned j = 0; j < self_recursive_calls.length (); j++)
    {
      cgraph_edge *cs = get_next_cgraph_edge_clone (self_recursive_calls[j]);
      /* Cloned edges can disappear during cloning as speculation can be
	 resolved, check that we have one and that it comes from the last
	 cloning.  */
      if (cs && cs->caller == new_node)
	cs->redirect_callee_duplicating_thunks (new_node);
      /* Any future code that would make more than one clone of an outgoing
	 edge would confuse this mechanism, so let's check that does not
	 happen.  */
      gcc_checking_assert (!cs
			   || !get_next_cgraph_edge_clone (cs)
			   || get_next_cgraph_edge_clone (cs)->caller != new_node);
    }
  if (have_self_recursive_calls)
    new_node->expand_all_artificial_thunks ();

  ipa_set_node_agg_value_chain (new_node, aggvals);
  for (const ipa_argagg_value &av : aggvals)
    new_node->maybe_create_reference (av.value, NULL);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "     the new node is %s.\n", new_node->dump_name ());
      if (known_contexts.exists ())
	{
	  for (i = 0; i < count; i++)
	    if (!known_contexts[i].useless_p ())
	      {
		fprintf (dump_file, "     known ctx %i is ", i);
		known_contexts[i].dump (dump_file);
	      }
	}
      if (aggvals)
	{
	  fprintf (dump_file, "     Aggregate replacements:");
	  ipa_argagg_value_list avs (aggvals);
	  avs.dump (dump_file);
	}
    }

  new_info = ipa_node_params_sum->get (new_node);
  new_info->ipcp_orig_node = node;
  new_node->ipcp_clone = true;
  new_info->known_csts = known_csts;
  new_info->known_contexts = known_contexts;

  ipcp_discover_new_direct_edges (new_node, known_csts, known_contexts,
				  aggvals);

  return new_node;
}

/* Return true if JFUNC, which describes a i-th parameter of call CS, is a
   pass-through function to itself when the cgraph_node involved is not an
   IPA-CP clone.  When SIMPLE is true, further check if JFUNC is a simple
   no-operation pass-through.  */

static bool
self_recursive_pass_through_p (cgraph_edge *cs, ipa_jump_func *jfunc, int i,
			       bool simple = true)
{
  enum availability availability;
  if (cs->caller == cs->callee->function_symbol (&availability)
      && availability > AVAIL_INTERPOSABLE
      && jfunc->type == IPA_JF_PASS_THROUGH
      && (!simple || ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
      && ipa_get_jf_pass_through_formal_id (jfunc) == i
      && ipa_node_params_sum->get (cs->caller)
      && !ipa_node_params_sum->get (cs->caller)->ipcp_orig_node)
    return true;
  return false;
}

/* Return true if JFUNC, which describes a part of an aggregate represented or
   pointed to by the i-th parameter of call CS, is a pass-through function to
   itself when the cgraph_node involved is not an IPA-CP clone..  When
   SIMPLE is true, further check if JFUNC is a simple no-operation
   pass-through.  */

static bool
self_recursive_agg_pass_through_p (const cgraph_edge *cs,
				   const ipa_agg_jf_item *jfunc,
				   int i, bool simple = true)
{
  enum availability availability;
  if (cs->caller == cs->callee->function_symbol (&availability)
      && availability > AVAIL_INTERPOSABLE
      && jfunc->jftype == IPA_JF_LOAD_AGG
      && jfunc->offset == jfunc->value.load_agg.offset
      && (!simple || jfunc->value.pass_through.operation == NOP_EXPR)
      && jfunc->value.pass_through.formal_id == i
      && useless_type_conversion_p (jfunc->value.load_agg.type, jfunc->type)
      && ipa_node_params_sum->get (cs->caller)
      && !ipa_node_params_sum->get (cs->caller)->ipcp_orig_node)
    return true;
  return false;
}

/* Given a NODE, and a subset of its CALLERS, try to populate blanks slots in
   KNOWN_CSTS with constants that are also known for all of the CALLERS.  */

static void
find_more_scalar_values_for_callers_subset (struct cgraph_node *node,
					    vec<tree> &known_csts,
					    const vec<cgraph_edge *> &callers)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count; i++)
    {
      struct cgraph_edge *cs;
      tree newval = NULL_TREE;
      int j;
      bool first = true;
      tree type = ipa_get_type (info, i);

      if (ipa_get_scalar_lat (info, i)->bottom || known_csts[i])
	continue;

      FOR_EACH_VEC_ELT (callers, j, cs)
	{
	  struct ipa_jump_func *jump_func;
	  tree t;

	  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
	  if (!args
	      || i >= ipa_get_cs_argument_count (args)
	      || (i == 0
		  && call_passes_through_thunk (cs)))
	    {
	      newval = NULL_TREE;
	      break;
	    }
	  jump_func = ipa_get_ith_jump_func (args, i);

	  /* Besides simple pass-through jump function, arithmetic jump
	     function could also introduce argument-direct-pass-through for
	     self-feeding recursive call.  For example,

	        fn (int i)
	        {
	          fn (i & 1);
	        }

	     Given that i is 0, recursive propagation via (i & 1) also gets
	     0.  */
	  if (self_recursive_pass_through_p (cs, jump_func, i, false))
	    {
	      gcc_assert (newval);
	      t = ipa_get_jf_arith_result (
				ipa_get_jf_pass_through_operation (jump_func),
				newval,
				ipa_get_jf_pass_through_operand (jump_func),
				type);
	    }
	  else
	    t = ipa_value_from_jfunc (ipa_node_params_sum->get (cs->caller),
				      jump_func, type);
	  if (!t
	      || (newval
		  && !values_equal_for_ipcp_p (t, newval))
	      || (!first && !newval))
	    {
	      newval = NULL_TREE;
	      break;
	    }
	  else
	    newval = t;
	  first = false;
	}

      if (newval)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    adding an extra known scalar value ");
	      print_ipcp_constant_value (dump_file, newval);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, "\n");
	    }

	  known_csts[i] = newval;
	}
    }
}

/* Given a NODE and a subset of its CALLERS, try to populate plank slots in
   KNOWN_CONTEXTS with polymorphic contexts that are also known for all of the
   CALLERS.  */

static void
find_more_contexts_for_caller_subset (cgraph_node *node,
				      vec<ipa_polymorphic_call_context>
				      *known_contexts,
				      const vec<cgraph_edge *> &callers)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count; i++)
    {
      cgraph_edge *cs;

      if (ipa_get_poly_ctx_lat (info, i)->bottom
	  || (known_contexts->exists ()
	      && !(*known_contexts)[i].useless_p ()))
	continue;

      ipa_polymorphic_call_context newval;
      bool first = true;
      int j;

      FOR_EACH_VEC_ELT (callers, j, cs)
	{
	  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
	  if (!args
	      || i >= ipa_get_cs_argument_count (args))
	    return;
	  ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, i);
	  ipa_polymorphic_call_context ctx;
	  ctx = ipa_context_from_jfunc (ipa_node_params_sum->get (cs->caller),
					cs, i, jfunc);
	  if (first)
	    {
	      newval = ctx;
	      first = false;
	    }
	  else
	    newval.meet_with (ctx);
	  if (newval.useless_p ())
	    break;
	}

      if (!newval.useless_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "    adding an extra known polymorphic "
		       "context ");
	      print_ipcp_constant_value (dump_file, newval);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, "\n");
	    }

	  if (!known_contexts->exists ())
	    known_contexts->safe_grow_cleared (ipa_get_param_count (info),
					       true);
	  (*known_contexts)[i] = newval;
	}

    }
}

/* Push all aggregate values coming along edge CS for parameter number INDEX to
   RES.  If INTERIM is non-NULL, it contains the current interim state of
   collected aggregate values which can be used to compute values passed over
   self-recursive edges.

   This basically one iteration of push_agg_values_from_edge over one
   parameter, which allows for simpler early returns.  */

static void
push_agg_values_for_index_from_edge (struct cgraph_edge *cs, int index,
				     vec<ipa_argagg_value> *res,
				     const ipa_argagg_value_list *interim)
{
  bool agg_values_from_caller = false;
  bool agg_jf_preserved = false;
  unsigned unit_delta = UINT_MAX;
  int src_idx = -1;
  ipa_jump_func *jfunc = ipa_get_ith_jump_func (ipa_edge_args_sum->get (cs),
						index);

  if (jfunc->type == IPA_JF_PASS_THROUGH
      && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    {
      agg_values_from_caller = true;
      agg_jf_preserved = ipa_get_jf_pass_through_agg_preserved (jfunc);
      src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      unit_delta = 0;
    }
  else if (jfunc->type == IPA_JF_ANCESTOR
	   && ipa_get_jf_ancestor_agg_preserved (jfunc))
    {
      agg_values_from_caller = true;
      agg_jf_preserved = true;
      src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
      unit_delta = ipa_get_jf_ancestor_offset (jfunc) / BITS_PER_UNIT;
    }

  ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
  if (agg_values_from_caller)
    {
      if (caller_info->ipcp_orig_node)
	{
	  struct cgraph_node *orig_node = caller_info->ipcp_orig_node;
	  ipcp_transformation *ts
	    = ipcp_get_transformation_summary (cs->caller);
	  ipa_node_params *orig_info = ipa_node_params_sum->get (orig_node);
	  ipcp_param_lattices *orig_plats
	    = ipa_get_parm_lattices (orig_info, src_idx);
	  if (ts
	      && orig_plats->aggs
	      && (agg_jf_preserved || !orig_plats->aggs_by_ref))
	    {
	      ipa_argagg_value_list src (ts);
	      src.push_adjusted_values (src_idx, index, unit_delta, res);
	      return;
	    }
	}
      else
	{
	  ipcp_param_lattices *src_plats
	    = ipa_get_parm_lattices (caller_info, src_idx);
	  if (src_plats->aggs
	      && !src_plats->aggs_bottom
	      && (agg_jf_preserved || !src_plats->aggs_by_ref))
	    {
	      if (interim && self_recursive_pass_through_p (cs, jfunc, index))
		{
		  interim->push_adjusted_values (src_idx, index, unit_delta,
						 res);
		  return;
		}
	      if (!src_plats->aggs_contain_variable)
		{
		  push_agg_values_from_plats (src_plats, index, unit_delta,
					      res);
		  return;
		}
	    }
	}
    }

  if (!jfunc->agg.items)
    return;
  bool first = true;
  unsigned prev_unit_offset = 0;
  for (const ipa_agg_jf_item &agg_jf : *jfunc->agg.items)
    {
      tree value, srcvalue;
      /* Besides simple pass-through aggregate jump function, arithmetic
	 aggregate jump function could also bring same aggregate value as
	 parameter passed-in for self-feeding recursive call.  For example,

	 fn (int *i)
	 {
	   int j = *i & 1;
	   fn (&j);
	 }

	 Given that *i is 0, recursive propagation via (*i & 1) also gets 0.  */
      if (interim
	  && self_recursive_agg_pass_through_p (cs, &agg_jf, index, false)
	  && (srcvalue = interim->get_value(index,
					    agg_jf.offset / BITS_PER_UNIT)))
	value = ipa_get_jf_arith_result (agg_jf.value.pass_through.operation,
					 srcvalue,
					 agg_jf.value.pass_through.operand,
					 agg_jf.type);
      else
	value = ipa_agg_value_from_jfunc (caller_info, cs->caller,
					  &agg_jf);
      if (value)
	{
	  struct ipa_argagg_value iav;
	  iav.value = value;
	  iav.unit_offset = agg_jf.offset / BITS_PER_UNIT;
	  iav.index = index;
	  iav.by_ref = jfunc->agg.by_ref;
	  iav.killed = false;

	  gcc_assert (first
		      || iav.unit_offset > prev_unit_offset);
	  prev_unit_offset = iav.unit_offset;
	  first = false;

	  res->safe_push (iav);
	}
    }
  return;
}

/* Push all aggregate values coming along edge CS to RES.  DEST_INFO is the
   description of ultimate callee of CS or the one it was cloned from (the
   summary where lattices are).  If INTERIM is non-NULL, it contains the
   current interim state of collected aggregate values which can be used to
   compute values passed over self-recursive edges (if OPTIMIZE_SELF_RECURSION
   is true) and to skip values which clearly will not be part of intersection
   with INTERIM.  */

static void
push_agg_values_from_edge (struct cgraph_edge *cs,
			   ipa_node_params *dest_info,
			   vec<ipa_argagg_value> *res,
			   const ipa_argagg_value_list *interim,
			   bool optimize_self_recursion)
{
  ipa_edge_args *args = ipa_edge_args_sum->get (cs);
  if (!args)
    return;

  int count = MIN (ipa_get_param_count (dest_info),
		   ipa_get_cs_argument_count (args));

  unsigned interim_index = 0;
  for (int index = 0; index < count; index++)
    {
      if (interim)
	{
	  while (interim_index < interim->m_elts.size ()
		 && interim->m_elts[interim_index].value
		 && interim->m_elts[interim_index].index < index)
	    interim_index++;
	  if (interim_index >= interim->m_elts.size ()
	      || interim->m_elts[interim_index].index > index)
	    continue;
	}

      ipcp_param_lattices *plats = ipa_get_parm_lattices (dest_info, index);
      if (!ipa_is_param_used (dest_info, index)
	  || plats->aggs_bottom)
	continue;
      push_agg_values_for_index_from_edge (cs, index, res,
					   optimize_self_recursion ? interim
					   : NULL);
    }
}


/* Look at edges in CALLERS and collect all known aggregate values that arrive
   from all of them.  Return nullptr if there are none.  */

static struct vec<ipa_argagg_value, va_gc> *
find_aggregate_values_for_callers_subset (struct cgraph_node *node,
					  const vec<cgraph_edge *> &callers)
{
  ipa_node_params *dest_info = ipa_node_params_sum->get (node);
  if (dest_info->ipcp_orig_node)
    dest_info = ipa_node_params_sum->get (dest_info->ipcp_orig_node);

  /* gather_edges_for_value puts a non-recursive call into the first element of
     callers if it can.  */
  auto_vec<ipa_argagg_value, 32> interim;
  push_agg_values_from_edge (callers[0], dest_info, &interim, NULL, true);

  unsigned valid_entries = interim.length ();
  if (!valid_entries)
    return nullptr;

  unsigned caller_count = callers.length();
  for (unsigned i = 1; i < caller_count; i++)
    {
      auto_vec<ipa_argagg_value, 32> last;
      ipa_argagg_value_list avs (&interim);
      push_agg_values_from_edge (callers[i], dest_info, &last, &avs, true);

      valid_entries = intersect_argaggs_with (interim, last);
      if (!valid_entries)
	return nullptr;
    }

  vec<ipa_argagg_value, va_gc> *res = NULL;
  vec_safe_reserve_exact (res, valid_entries);
  for (const ipa_argagg_value &av : interim)
    if (av.value)
      res->quick_push(av);
  gcc_checking_assert (res->length () == valid_entries);
  return res;
}

/* Determine whether CS also brings all scalar values that the NODE is
   specialized for.  */

static bool
cgraph_edge_brings_all_scalars_for_node (struct cgraph_edge *cs,
					 struct cgraph_node *node)
{
  ipa_node_params *dest_info = ipa_node_params_sum->get (node);
  int count = ipa_get_param_count (dest_info);
  class ipa_node_params *caller_info;
  class ipa_edge_args *args;
  int i;

  caller_info = ipa_node_params_sum->get (cs->caller);
  args = ipa_edge_args_sum->get (cs);
  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *jump_func;
      tree val, t;

      val = dest_info->known_csts[i];
      if (!val)
	continue;

      if (i >= ipa_get_cs_argument_count (args))
	return false;
      jump_func = ipa_get_ith_jump_func (args, i);
      t = ipa_value_from_jfunc (caller_info, jump_func,
				ipa_get_type (dest_info, i));
      if (!t || !values_equal_for_ipcp_p (val, t))
	return false;
    }
  return true;
}

/* Determine whether CS also brings all aggregate values that NODE is
   specialized for.  */

static bool
cgraph_edge_brings_all_agg_vals_for_node (struct cgraph_edge *cs,
					  struct cgraph_node *node)
{
  ipcp_transformation *ts = ipcp_get_transformation_summary (node);
  if (!ts || vec_safe_is_empty (ts->m_agg_values))
    return true;

  const ipa_argagg_value_list existing (ts->m_agg_values);
  auto_vec<ipa_argagg_value, 32> edge_values;
  ipa_node_params *dest_info = ipa_node_params_sum->get (node);
  gcc_checking_assert (dest_info->ipcp_orig_node);
  dest_info = ipa_node_params_sum->get (dest_info->ipcp_orig_node);
  push_agg_values_from_edge (cs, dest_info, &edge_values, &existing, false);
  const ipa_argagg_value_list avl (&edge_values);
  return avl.superset_of_p (existing);
}

/* Given an original NODE and a VAL for which we have already created a
   specialized clone, look whether there are incoming edges that still lead
   into the old node but now also bring the requested value and also conform to
   all other criteria such that they can be redirected the special node.
   This function can therefore redirect the final edge in a SCC.  */

template <typename valtype>
static void
perhaps_add_new_callers (cgraph_node *node, ipcp_value<valtype> *val)
{
  ipcp_value_source<valtype> *src;
  profile_count redirected_sum = profile_count::zero ();

  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src, node, val)
	      && cgraph_edge_brings_all_scalars_for_node (cs, val->spec_node)
	      && cgraph_edge_brings_all_agg_vals_for_node (cs, val->spec_node))
	    {
	      if (dump_file)
		fprintf (dump_file, " - adding an extra caller %s of %s\n",
			 cs->caller->dump_name (),
			 val->spec_node->dump_name ());

	      cs->redirect_callee_duplicating_thunks (val->spec_node);
	      val->spec_node->expand_all_artificial_thunks ();
	      if (cs->count.ipa ().initialized_p ())
	        redirected_sum = redirected_sum + cs->count.ipa ();
	    }
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  if (redirected_sum.nonzero_p ())
    update_specialized_profile (val->spec_node, node, redirected_sum);
}

/* Return true if KNOWN_CONTEXTS contain at least one useful context.  */

static bool
known_contexts_useful_p (vec<ipa_polymorphic_call_context> known_contexts)
{
  ipa_polymorphic_call_context *ctx;
  int i;

  FOR_EACH_VEC_ELT (known_contexts, i, ctx)
    if (!ctx->useless_p ())
      return true;
  return false;
}

/* Return a copy of KNOWN_CSTS if it is not empty, otherwise return vNULL.  */

static vec<ipa_polymorphic_call_context>
copy_useful_known_contexts (const vec<ipa_polymorphic_call_context> &known_contexts)
{
  if (known_contexts_useful_p (known_contexts))
    return known_contexts.copy ();
  else
    return vNULL;
}

/* Copy known scalar values from AVALS into KNOWN_CSTS and modify the copy
   according to VAL and INDEX.  If non-empty, replace KNOWN_CONTEXTS with its
   copy too.  */

static void
copy_known_vectors_add_val (ipa_auto_call_arg_values *avals,
			    vec<tree> *known_csts,
			    vec<ipa_polymorphic_call_context> *known_contexts,
			    ipcp_value<tree> *val, int index)
{
  *known_csts = avals->m_known_vals.copy ();
  *known_contexts = copy_useful_known_contexts (avals->m_known_contexts);
  (*known_csts)[index] = val->value;
}

/* Copy known scalar values from AVALS into KNOWN_CSTS.  Similarly, copy
   contexts to KNOWN_CONTEXTS and modify the copy according to VAL and
   INDEX.  */

static void
copy_known_vectors_add_val (ipa_auto_call_arg_values *avals,
			    vec<tree> *known_csts,
			    vec<ipa_polymorphic_call_context> *known_contexts,
			    ipcp_value<ipa_polymorphic_call_context> *val,
			    int index)
{
  *known_csts = avals->m_known_vals.copy ();
  *known_contexts = avals->m_known_contexts.copy ();
  (*known_contexts)[index] = val->value;
}

/* Return true if OFFSET indicates this was not an aggregate value or there is
   a replacement equivalent to VALUE, INDEX and OFFSET among those in the
   AGGVALS list.  */

DEBUG_FUNCTION bool
ipcp_val_agg_replacement_ok_p (vec<ipa_argagg_value, va_gc> *aggvals,
			       int index, HOST_WIDE_INT offset, tree value)
{
  if (offset == -1)
    return true;

  const ipa_argagg_value_list avl (aggvals);
  tree v = avl.get_value (index, offset / BITS_PER_UNIT);
  return v && values_equal_for_ipcp_p (v, value);
}

/* Return true if offset is minus one because source of a polymorphic context
   cannot be an aggregate value.  */

DEBUG_FUNCTION bool
ipcp_val_agg_replacement_ok_p (vec<ipa_argagg_value, va_gc> *,
			       int , HOST_WIDE_INT offset,
			       ipa_polymorphic_call_context)
{
  return offset == -1;
}

/* Decide whether to create a special version of NODE for value VAL of
   parameter at the given INDEX.  If OFFSET is -1, the value is for the
   parameter itself, otherwise it is stored at the given OFFSET of the
   parameter.  AVALS describes the other already known values.  SELF_GEN_CLONES
   is a vector which contains clones created for self-recursive calls with an
   arithmetic pass-through jump function.  */

template <typename valtype>
static bool
decide_about_value (struct cgraph_node *node, int index, HOST_WIDE_INT offset,
		    ipcp_value<valtype> *val, ipa_auto_call_arg_values *avals,
		    vec<cgraph_node *> *self_gen_clones)
{
  int caller_count;
  sreal freq_sum;
  profile_count count_sum, rec_count_sum;
  vec<cgraph_edge *> callers;

  if (val->spec_node)
    {
      perhaps_add_new_callers (node, val);
      return false;
    }
  else if (val->local_size_cost + overall_size > get_max_overall_size (node))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   Ignoring candidate value because "
		 "maximum unit size would be reached with %li.\n",
		 val->local_size_cost + overall_size);
      return false;
    }
  else if (!get_info_about_necessary_edges (val, node, &freq_sum, &caller_count,
					    &rec_count_sum, &count_sum))
    return false;

  if (!dbg_cnt (ipa_cp_values))
    return false;

  if (val->self_recursion_generated_p ())
    {
      /* The edge counts in this case might not have been adjusted yet.
	 Nevertleless, even if they were it would be only a guesswork which we
	 can do now.  The recursive part of the counts can be derived from the
	 count of the original node anyway.  */
      if (node->count.ipa ().nonzero_p ())
	{
	  unsigned dem = self_gen_clones->length () + 1;
	  rec_count_sum = node->count.ipa () / dem;
	}
      else
	rec_count_sum = profile_count::zero ();
    }

  /* get_info_about_necessary_edges only sums up ipa counts.  */
  count_sum += rec_count_sum;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, " - considering value ");
      print_ipcp_constant_value (dump_file, val->value);
      fprintf (dump_file, " for ");
      ipa_dump_param (dump_file, ipa_node_params_sum->get (node), index);
      if (offset != -1)
	fprintf (dump_file, ", offset: " HOST_WIDE_INT_PRINT_DEC, offset);
      fprintf (dump_file, " (caller_count: %i)\n", caller_count);
    }

  if (!good_cloning_opportunity_p (node, val->local_time_benefit,
				   freq_sum, count_sum,
				   val->local_size_cost)
      && !good_cloning_opportunity_p (node, val->prop_time_benefit,
				      freq_sum, count_sum, val->prop_size_cost))
    return false;

  if (dump_file)
    fprintf (dump_file, "  Creating a specialized node of %s.\n",
	     node->dump_name ());

  vec<tree> known_csts;
  vec<ipa_polymorphic_call_context> known_contexts;

  callers = gather_edges_for_value (val, node, caller_count);
  if (offset == -1)
    copy_known_vectors_add_val (avals, &known_csts, &known_contexts, val, index);
  else
    {
      known_csts = avals->m_known_vals.copy ();
      known_contexts = copy_useful_known_contexts (avals->m_known_contexts);
    }
  find_more_scalar_values_for_callers_subset (node, known_csts, callers);
  find_more_contexts_for_caller_subset (node, &known_contexts, callers);
  vec<ipa_argagg_value, va_gc> *aggvals
    = find_aggregate_values_for_callers_subset (node, callers);
  gcc_checking_assert (ipcp_val_agg_replacement_ok_p (aggvals, index,
						      offset, val->value));
  val->spec_node = create_specialized_node (node, known_csts, known_contexts,
					    aggvals, callers);

  if (val->self_recursion_generated_p ())
    self_gen_clones->safe_push (val->spec_node);
  else
    update_profiling_info (node, val->spec_node);

  callers.release ();
  overall_size += val->local_size_cost;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "     overall size reached %li\n",
	     overall_size);

  /* TODO: If for some lattice there is only one other known value
     left, make a special node for it too. */

  return true;
}

/* Like irange::contains_p(), but convert VAL to the range of R if
   necessary.  */

static inline bool
ipa_range_contains_p (const vrange &r, tree val)
{
  if (r.undefined_p ())
    return false;

  tree type = r.type ();
  if (!wi::fits_to_tree_p (wi::to_wide (val), type))
    return false;

  val = fold_convert (type, val);
  return r.contains_p (val);
}

/* Decide whether and what specialized clones of NODE should be created.  */

static bool
decide_whether_version_node (struct cgraph_node *node)
{
  ipa_node_params *info = ipa_node_params_sum->get (node);
  int i, count = ipa_get_param_count (info);
  bool ret = false;

  if (count == 0)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEvaluating opportunities for %s.\n",
	     node->dump_name ());

  auto_vec <cgraph_node *, 9> self_gen_clones;
  ipa_auto_call_arg_values avals;
  gather_context_independent_values (info, &avals, false, NULL);

  for (i = 0; i < count;i++)
    {
      if (!ipa_is_param_used (info, i))
	continue;

      class ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;
      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;

      if (!lat->bottom
	  && !avals.m_known_vals[i])
	{
	  ipcp_value<tree> *val;
	  for (val = lat->values; val; val = val->next)
	    {
	      /* If some values generated for self-recursive calls with
		 arithmetic jump functions fall outside of the known
		 range for the parameter, we can skip them.  */
	      if (TREE_CODE (val->value) == INTEGER_CST
		  && !plats->m_value_range.bottom_p ()
		  && !ipa_range_contains_p (plats->m_value_range.m_vr,
					    val->value))
		{
		  /* This can happen also if a constant present in the source
		     code falls outside of the range of parameter's type, so we
		     cannot assert.  */
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, " - skipping%s value ",
			       val->self_recursion_generated_p ()
			       ? " self_recursion_generated" : "");
		      print_ipcp_constant_value (dump_file, val->value);
		      fprintf (dump_file, " because it is outside known "
			       "value range.\n");
		    }
		  continue;
		}
	      ret |= decide_about_value (node, i, -1, val, &avals,
					 &self_gen_clones);
	    }
	}

      if (!plats->aggs_bottom)
	{
	  struct ipcp_agg_lattice *aglat;
	  ipcp_value<tree> *val;
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    if (!aglat->bottom && aglat->values
		/* If the following is false, the one value has been considered
		   for cloning for all contexts.  */
		&& (plats->aggs_contain_variable
		    || !aglat->is_single_const ()))
	      for (val = aglat->values; val; val = val->next)
		ret |= decide_about_value (node, i, aglat->offset, val, &avals,
					   &self_gen_clones);
	}

      if (!ctxlat->bottom
	  && avals.m_known_contexts[i].useless_p ())
	{
	  ipcp_value<ipa_polymorphic_call_context> *val;
	  for (val = ctxlat->values; val; val = val->next)
	    ret |= decide_about_value (node, i, -1, val, &avals,
				       &self_gen_clones);
	}
    }

  if (!self_gen_clones.is_empty ())
    {
      self_gen_clones.safe_push (node);
      update_counts_for_self_gen_clones (node, self_gen_clones);
    }

  if (info->do_clone_for_all_contexts)
    {
      if (!dbg_cnt (ipa_cp_values))
	{
	  info->do_clone_for_all_contexts = false;
	  return ret;
	}

      struct cgraph_node *clone;
      auto_vec<cgraph_edge *> callers = node->collect_callers ();

      for (int i = callers.length () - 1; i >= 0; i--)
	{
	  cgraph_edge *cs = callers[i];
	  ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);

	  if (caller_info && caller_info->node_dead)
	    callers.unordered_remove (i);
	}

      if (!adjust_callers_for_value_intersection (callers, node))
	{
	  /* If node is not called by anyone, or all its caller edges are
	     self-recursive, the node is not really in use, no need to do
	     cloning.  */
	  info->do_clone_for_all_contexts = false;
	  return ret;
	}

      if (dump_file)
	fprintf (dump_file, " - Creating a specialized node of %s "
		 "for all known contexts.\n", node->dump_name ());

      vec<tree> known_csts = avals.m_known_vals.copy ();
      vec<ipa_polymorphic_call_context> known_contexts
	= copy_useful_known_contexts (avals.m_known_contexts);
      find_more_scalar_values_for_callers_subset (node, known_csts, callers);
      find_more_contexts_for_caller_subset (node, &known_contexts, callers);
      vec<ipa_argagg_value, va_gc> *aggvals
	= find_aggregate_values_for_callers_subset (node, callers);

      if (!known_contexts_useful_p (known_contexts))
	{
	  known_contexts.release ();
	  known_contexts = vNULL;
	}
      clone = create_specialized_node (node, known_csts, known_contexts,
				       aggvals, callers);
      info->do_clone_for_all_contexts = false;
      ipa_node_params_sum->get (clone)->is_all_contexts_clone = true;
      ret = true;
    }

  return ret;
}

/* Transitively mark all callees of NODE within the same SCC as not dead.  */

static void
spread_undeadness (struct cgraph_node *node)
{
  struct cgraph_edge *cs;

  for (cs = node->callees; cs; cs = cs->next_callee)
    if (ipa_edge_within_scc (cs))
      {
	struct cgraph_node *callee;
	class ipa_node_params *info;

	callee = cs->callee->function_symbol (NULL);
	info = ipa_node_params_sum->get (callee);

	if (info && info->node_dead)
	  {
	    info->node_dead = 0;
	    spread_undeadness (callee);
	  }
      }
}

/* Return true if NODE has a caller from outside of its SCC that is not
   dead.  Worker callback for cgraph_for_node_and_aliases.  */

static bool
has_undead_caller_from_outside_scc_p (struct cgraph_node *node,
				      void *data ATTRIBUTE_UNUSED)
{
  struct cgraph_edge *cs;

  for (cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk
	&& cs->caller->call_for_symbol_thunks_and_aliases
	  (has_undead_caller_from_outside_scc_p, NULL, true))
      return true;
    else if (!ipa_edge_within_scc (cs))
      {
	ipa_node_params *caller_info = ipa_node_params_sum->get (cs->caller);
	if (!caller_info /* Unoptimized caller are like dead ones.  */
	    || !caller_info->node_dead)
	  return true;
      }
  return false;
}


/* Identify nodes within the same SCC as NODE which are no longer needed
   because of new clones and will be removed as unreachable.  */

static void
identify_dead_nodes (struct cgraph_node *node)
{
  struct cgraph_node *v;
  for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    if (v->local)
      {
	ipa_node_params *info = ipa_node_params_sum->get (v);
	if (info
	    && !v->call_for_symbol_thunks_and_aliases
	      (has_undead_caller_from_outside_scc_p, NULL, true))
	  info->node_dead = 1;
      }

  for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    {
      ipa_node_params *info = ipa_node_params_sum->get (v);
      if (info && !info->node_dead)
	spread_undeadness (v);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
	if (ipa_node_params_sum->get (v)
	    && ipa_node_params_sum->get (v)->node_dead)
	  fprintf (dump_file, "  Marking node as dead: %s.\n",
		   v->dump_name ());
    }
}

/* The decision stage.  Iterate over the topological order of call graph nodes
   TOPO and make specialized clones if deemed beneficial.  */

static void
ipcp_decision_stage (class ipa_topo_info *topo)
{
  int i;

  if (dump_file)
    fprintf (dump_file, "\nIPA decision stage:\n\n");

  for (i = topo->nnodes - 1; i >= 0; i--)
    {
      struct cgraph_node *node = topo->order[i];
      bool change = false, iterate = true;

      while (iterate)
	{
	  struct cgraph_node *v;
	  iterate = false;
	  for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
	    if (v->has_gimple_body_p ()
		&& ipcp_versionable_function_p (v))
	      iterate |= decide_whether_version_node (v);

	  change |= iterate;
	}
      if (change)
	identify_dead_nodes (node);
    }
}

/* Look up all VR and bits information that we have discovered and copy it
   over to the transformation summary.  */

static void
ipcp_store_vr_results (void)
{
  cgraph_node *node;

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      ipa_node_params *info = ipa_node_params_sum->get (node);
      bool dumped_sth = false;
      bool found_useful_result = false;
      bool do_vr = true;
      bool do_bits = true;

      if (!info || !opt_for_fn (node->decl, flag_ipa_vrp))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not considering %s for VR discovery "
		     "and propagate; -fipa-ipa-vrp: disabled.\n",
		     node->dump_name ());
	  do_vr = false;
	}
      if (!info || !opt_for_fn (node->decl, flag_ipa_bit_cp))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not considering %s for ipa bitwise "
				"propagation ; -fipa-bit-cp: disabled.\n",
				node->dump_name ());
	  do_bits = false;
	}
      if (!do_bits && !do_vr)
	continue;

      if (info->ipcp_orig_node)
	info = ipa_node_params_sum->get (info->ipcp_orig_node);
      if (info->lattices.is_empty ())
	/* Newly expanded artificial thunks do not have lattices.  */
	continue;

      unsigned count = ipa_get_param_count (info);
      for (unsigned i = 0; i < count; i++)
	{
	  ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  if (do_vr
	      && !plats->m_value_range.bottom_p ()
	      && !plats->m_value_range.top_p ())
	    {
	      found_useful_result = true;
	      break;
	    }
	  if (do_bits && plats->bits_lattice.constant_p ())
	    {
	      found_useful_result = true;
	      break;
	    }
	}
      if (!found_useful_result)
	continue;

      ipcp_transformation_initialize ();
      ipcp_transformation *ts = ipcp_transformation_sum->get_create (node);
      vec_safe_reserve_exact (ts->m_vr, count);

      for (unsigned i = 0; i < count; i++)
	{
	  ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  ipcp_bits_lattice *bits = NULL;

	  if (do_bits
	      && plats->bits_lattice.constant_p ()
	      && dbg_cnt (ipa_cp_bits))
	    bits = &plats->bits_lattice;

	  if (do_vr
	      && !plats->m_value_range.bottom_p ()
	      && !plats->m_value_range.top_p ()
	      && dbg_cnt (ipa_cp_vr))
	    {
	      if (bits)
		{
		  value_range tmp = plats->m_value_range.m_vr;
		  tree type = ipa_get_type (info, i);
		  irange_bitmask bm (wide_int::from (bits->get_value (),
						     TYPE_PRECISION (type),
						     TYPE_SIGN (type)),
				     wide_int::from (bits->get_mask (),
						     TYPE_PRECISION (type),
						     TYPE_SIGN (type)));
		  tmp.update_bitmask (bm);
		  ipa_vr vr (tmp);
		  ts->m_vr->quick_push (vr);
		}
	      else
		{
		  ipa_vr vr (plats->m_value_range.m_vr);
		  ts->m_vr->quick_push (vr);
		}
	    }
	  else if (bits)
	    {
	      tree type = ipa_get_type (info, i);
	      value_range tmp;
	      tmp.set_varying (type);
	      irange_bitmask bm (wide_int::from (bits->get_value (),
						 TYPE_PRECISION (type),
						 TYPE_SIGN (type)),
				 wide_int::from (bits->get_mask (),
						 TYPE_PRECISION (type),
						 TYPE_SIGN (type)));
	      tmp.update_bitmask (bm);
	      ipa_vr vr (tmp);
	      ts->m_vr->quick_push (vr);
	    }
	  else
	    {
	      ipa_vr vr;
	      ts->m_vr->quick_push (vr);
	    }

	  if (!dump_file || !bits)
	    continue;

	  if (!dumped_sth)
	    {
	      fprintf (dump_file, "Propagated bits info for function %s:\n",
		       node->dump_name ());
	      dumped_sth = true;
	    }
	  fprintf (dump_file, " param %i: value = ", i);
	  print_hex (bits->get_value (), dump_file);
	  fprintf (dump_file, ", mask = ");
	  print_hex (bits->get_mask (), dump_file);
	  fprintf (dump_file, "\n");
	}
    }
}

/* The IPCP driver.  */

static unsigned int
ipcp_driver (void)
{
  class ipa_topo_info topo;

  if (edge_clone_summaries == NULL)
    edge_clone_summaries = new edge_clone_summary_t (symtab);

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  clone_num_suffixes = new hash_map<const char *, unsigned>;

  if (dump_file)
    {
      fprintf (dump_file, "\nIPA structures before propagation:\n");
      if (dump_flags & TDF_DETAILS)
	ipa_print_all_params (dump_file);
      ipa_print_all_jump_functions (dump_file);
    }

  /* Topological sort.  */
  build_toporder_info (&topo);
  /* Do the interprocedural propagation.  */
  ipcp_propagate_stage (&topo);
  /* Decide what constant propagation and cloning should be performed.  */
  ipcp_decision_stage (&topo);
  /* Store results of value range and bits propagation.  */
  ipcp_store_vr_results ();

  /* Free all IPCP structures.  */
  delete clone_num_suffixes;
  free_toporder_info (&topo);
  delete edge_clone_summaries;
  edge_clone_summaries = NULL;
  ipa_free_all_structures_after_ipa_cp ();
  if (dump_file)
    fprintf (dump_file, "\nIPA constant propagation end\n");
  return 0;
}

/* Initialization and computation of IPCP data structures.  This is the initial
   intraprocedural analysis of functions, which gathers information to be
   propagated later on.  */

static void
ipcp_generate_summary (void)
{
  struct cgraph_node *node;

  if (dump_file)
    fprintf (dump_file, "\nIPA constant propagation start:\n");
  ipa_register_cgraph_hooks ();

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    ipa_analyze_node (node);
}

namespace {

const pass_data pass_data_ipa_cp =
{
  IPA_PASS, /* type */
  "cp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_CONSTANT_PROP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab | TODO_remove_functions ), /* todo_flags_finish */
};

class pass_ipa_cp : public ipa_opt_pass_d
{
public:
  pass_ipa_cp (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_cp, ctxt,
		      ipcp_generate_summary, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      ipcp_write_transformation_summaries, /*
		      write_optimization_summary */
		      ipcp_read_transformation_summaries, /*
		      read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      ipcp_transform_function, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      /* FIXME: We should remove the optimize check after we ensure we never run
	 IPA passes when not optimizing.  */
      return (flag_ipa_cp && optimize) || in_lto_p;
    }

  unsigned int execute (function *) final override { return ipcp_driver (); }

}; // class pass_ipa_cp

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_cp (gcc::context *ctxt)
{
  return new pass_ipa_cp (ctxt);
}

/* Reset all state within ipa-cp.cc so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_cp_cc_finalize (void)
{
  base_count = profile_count::uninitialized ();
  overall_size = 0;
  orig_overall_size = 0;
  ipcp_free_transformation_sum ();
}

/* Given PARAM which must be a parameter of function FNDECL described by THIS,
   return its index in the DECL_ARGUMENTS chain, using a pre-computed
   DECL_UID-sorted vector if available (which is pre-computed only if there are
   many parameters).  Can return -1 if param is static chain not represented
   among DECL_ARGUMENTS. */

int
ipcp_transformation::get_param_index (const_tree fndecl, const_tree param) const
{
  gcc_assert (TREE_CODE (param) == PARM_DECL);
  if (m_uid_to_idx)
    {
      unsigned puid = DECL_UID (param);
      const ipa_uid_to_idx_map_elt *res
	= std::lower_bound (m_uid_to_idx->begin(), m_uid_to_idx->end (), puid,
			    [] (const ipa_uid_to_idx_map_elt &elt, unsigned uid)
			    {
			      return elt.uid < uid;
			    });
      if (res == m_uid_to_idx->end ()
	  || res->uid != puid)
	{
	  gcc_assert (DECL_STATIC_CHAIN (fndecl));
	  return -1;
	}
      return res->index;
    }

  unsigned index = 0;
  for (tree p = DECL_ARGUMENTS (fndecl); p; p = DECL_CHAIN (p), index++)
    if (p == param)
      return (int) index;

  gcc_assert (DECL_STATIC_CHAIN (fndecl));
  return -1;
}

/* Helper function to qsort a vector of ipa_uid_to_idx_map_elt elements
   according to the uid.  */

static int
compare_uids (const void *a, const void *b)
{
  const ipa_uid_to_idx_map_elt *e1 = (const ipa_uid_to_idx_map_elt *) a;
  const ipa_uid_to_idx_map_elt *e2 = (const ipa_uid_to_idx_map_elt *) b;
  if (e1->uid < e2->uid)
    return -1;
  if (e1->uid > e2->uid)
    return 1;
  gcc_unreachable ();
}

/* Assuming THIS describes FNDECL and it has sufficiently many parameters to
   justify the overhead, create a DECL_UID-sorted vector to speed up mapping
   from parameters to their indices in DECL_ARGUMENTS chain.  */

void
ipcp_transformation::maybe_create_parm_idx_map (tree fndecl)
{
  int c = count_formal_params (fndecl);
  if (c < 32)
    return;

  m_uid_to_idx = NULL;
  vec_safe_reserve (m_uid_to_idx, c, true);
  unsigned index = 0;
  for (tree p = DECL_ARGUMENTS (fndecl); p; p = DECL_CHAIN (p), index++)
    {
      ipa_uid_to_idx_map_elt elt;
      elt.uid = DECL_UID (p);
      elt.index = index;
      m_uid_to_idx->quick_push (elt);
    }
  m_uid_to_idx->qsort (compare_uids);
}
