/* Interprocedural constant propagation
   Copyright (C) 2005-2014 Free Software Foundation, Inc.

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

   This stage is currently performed by call graph code (mainly in cgraphunit.c
   and tree-inline.c) according to instructions inserted to the call graph by
   the second stage.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple-fold.h"
#include "gimple-expr.h"
#include "target.h"
#include "ipa-prop.h"
#include "bitmap.h"
#include "tree-pass.h"
#include "flags.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "params.h"
#include "ipa-inline.h"
#include "ipa-utils.h"

struct ipcp_value;

/* Describes a particular source for an IPA-CP value.  */

struct ipcp_value_source
{
  /* Aggregate offset of the source, negative if the source is scalar value of
     the argument itself.  */
  HOST_WIDE_INT offset;
  /* The incoming edge that brought the value.  */
  struct cgraph_edge *cs;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the ipcp_value of the caller from which the described
     value has been derived.  Otherwise it is NULL.  */
  struct ipcp_value *val;
  /* Next pointer in a linked list of sources of a value.  */
  struct ipcp_value_source *next;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the index of the parameter of the caller the jump
     function references.  */
  int index;
};

/* Describes one particular value stored in struct ipcp_lattice.  */

struct ipcp_value
{
  /* The actual value for the given parameter.  This is either an IPA invariant
     or a TREE_BINFO describing a type that can be used for
     devirtualization.  */
  tree value;
  /* The list of sources from which this value originates.  */
  struct ipcp_value_source *sources;
  /* Next pointers in a linked list of all values in a lattice.  */
  struct ipcp_value *next;
  /* Next pointers in a linked list of values in a strongly connected component
     of values. */
  struct ipcp_value *scc_next;
  /* Next pointers in a linked list of SCCs of values sorted topologically
     according their sources.  */
  struct ipcp_value  *topo_next;
  /* A specialized node created for this value, NULL if none has been (so far)
     created.  */
  struct cgraph_node *spec_node;
  /* Depth first search number and low link for topological sorting of
     values.  */
  int dfs, low_link;
  /* Time benefit and size cost that specializing the function for this value
     would bring about in this function alone.  */
  int local_time_benefit, local_size_cost;
  /* Time benefit and size cost that specializing the function for this value
     can bring about in it's callees (transitively).  */
  int prop_time_benefit, prop_size_cost;
  /* True if this valye is currently on the topo-sort stack.  */
  bool on_stack;
};

/* Lattice describing potential values of a formal parameter of a function, or
   a part of an aggreagate.  TOP is represented by a lattice with zero values
   and with contains_variable and bottom flags cleared.  BOTTOM is represented
   by a lattice with the bottom flag set.  In that case, values and
   contains_variable flag should be disregarded.  */

struct ipcp_lattice
{
  /* The list of known values and types in this lattice.  Note that values are
     not deallocated if a lattice is set to bottom because there may be value
     sources referencing them.  */
  struct ipcp_value *values;
  /* Number of known values and types in this lattice.  */
  int values_count;
  /* The lattice contains a variable component (in addition to values).  */
  bool contains_variable;
  /* The value of the lattice is bottom (i.e. variable and unusable for any
     propagation).  */
  bool bottom;
};

/* Lattice with an offset to describe a part of an aggregate.  */

struct ipcp_agg_lattice : public ipcp_lattice
{
  /* Offset that is being described by this lattice. */
  HOST_WIDE_INT offset;
  /* Size so that we don't have to re-compute it every time we traverse the
     list.  Must correspond to TYPE_SIZE of all lat values.  */
  HOST_WIDE_INT size;
  /* Next element of the linked list.  */
  struct ipcp_agg_lattice *next;
};

/* Structure containing lattices for a parameter itself and for pieces of
   aggregates that are passed in the parameter or by a reference in a parameter
   plus some other useful flags.  */

struct ipcp_param_lattices
{
  /* Lattice describing the value of the parameter itself.  */
  struct ipcp_lattice itself;
  /* Lattices describing aggregate parts.  */
  struct ipcp_agg_lattice *aggs;
  /* Number of aggregate lattices */
  int aggs_count;
  /* True if aggregate data were passed by reference (as opposed to by
     value).  */
  bool aggs_by_ref;
  /* All aggregate lattices contain a variable component (in addition to
     values).  */
  bool aggs_contain_variable;
  /* The value of all aggregate lattices is bottom (i.e. variable and unusable
     for any propagation).  */
  bool aggs_bottom;

  /* There is a virtual call based on this parameter.  */
  bool virt_call;
};

/* Allocation pools for values and their sources in ipa-cp.  */

alloc_pool ipcp_values_pool;
alloc_pool ipcp_sources_pool;
alloc_pool ipcp_agg_lattice_pool;

/* Maximal count found in program.  */

static gcov_type max_count;

/* Original overall size of the program.  */

static long overall_size, max_new_size;

/* Head of the linked list of topologically sorted values. */

static struct ipcp_value *values_topo;

/* Return the param lattices structure corresponding to the Ith formal
   parameter of the function described by INFO.  */
static inline struct ipcp_param_lattices *
ipa_get_parm_lattices (struct ipa_node_params *info, int i)
{
  gcc_assert (i >= 0 && i < ipa_get_param_count (info));
  gcc_checking_assert (!info->ipcp_orig_node);
  gcc_checking_assert (info->lattices);
  return &(info->lattices[i]);
}

/* Return the lattice corresponding to the scalar value of the Ith formal
   parameter of the function described by INFO.  */
static inline struct ipcp_lattice *
ipa_get_scalar_lat (struct ipa_node_params *info, int i)
{
  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
  return &plats->itself;
}

/* Return whether LAT is a lattice with a single constant and without an
   undefined value.  */

static inline bool
ipa_lat_is_single_const (struct ipcp_lattice *lat)
{
  if (lat->bottom
      || lat->contains_variable
      || lat->values_count != 1)
    return false;
  else
    return true;
}

/* Print V which is extracted from a value in a lattice to F.  */

static void
print_ipcp_constant_value (FILE * f, tree v)
{
  if (TREE_CODE (v) == TREE_BINFO)
    {
      fprintf (f, "BINFO ");
      print_generic_expr (f, BINFO_TYPE (v), 0);
    }
  else if (TREE_CODE (v) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (v, 0)) == CONST_DECL)
    {
      fprintf (f, "& ");
      print_generic_expr (f, DECL_INITIAL (TREE_OPERAND (v, 0)), 0);
    }
  else
    print_generic_expr (f, v, 0);
}

/* Print a lattice LAT to F.  */

static void
print_lattice (FILE * f, struct ipcp_lattice *lat,
	       bool dump_sources, bool dump_benefits)
{
  struct ipcp_value *val;
  bool prev = false;

  if (lat->bottom)
    {
      fprintf (f, "BOTTOM\n");
      return;
    }

  if (!lat->values_count && !lat->contains_variable)
    {
      fprintf (f, "TOP\n");
      return;
    }

  if (lat->contains_variable)
    {
      fprintf (f, "VARIABLE");
      prev = true;
      if (dump_benefits)
	fprintf (f, "\n");
    }

  for (val = lat->values; val; val = val->next)
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
	  struct ipcp_value_source *s;

	  fprintf (f, " [from:");
	  for (s = val->sources; s; s = s->next)
	    fprintf (f, " %i(%i)", s->cs->caller->order,
		     s->cs->frequency);
	  fprintf (f, "]");
	}

      if (dump_benefits)
	fprintf (f, " [loc_time: %i, loc_size: %i, "
		 "prop_time: %i, prop_size: %i]\n",
		 val->local_time_benefit, val->local_size_cost,
		 val->prop_time_benefit, val->prop_size_cost);
    }
  if (!dump_benefits)
    fprintf (f, "\n");
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
      struct ipa_node_params *info;

      info = IPA_NODE_REF (node);
      fprintf (f, "  Node: %s/%i:\n", node->name (),
	       node->order);
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_agg_lattice *aglat;
	  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  fprintf (f, "    param [%d]: ", i);
	  print_lattice (f, &plats->itself, dump_sources, dump_benefits);

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
	      print_lattice (f, aglat, dump_sources, dump_benefits);
	    }
	}
    }
}

/* Determine whether it is at all technically possible to create clones of NODE
   and store this information in the ipa_node_params structure associated
   with NODE.  */

static void
determine_versionability (struct cgraph_node *node)
{
  const char *reason = NULL;

  /* There are a number of generic reasons functions cannot be versioned.  We
     also cannot remove parameters if there are type attributes such as fnspec
     present.  */
  if (node->alias || node->thunk.thunk_p)
    reason = "alias or thunk";
  else if (!node->local.versionable)
    reason = "not a tree_versionable_function";
  else if (cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
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
  /* Don't clone decls local to a comdat group; it breaks and for C++
     decloned constructors, inlining is always better anyway.  */
  else if (symtab_comdat_local_p (node))
    reason = "comdat-local function";

  if (reason && dump_file && !node->alias && !node->thunk.thunk_p)
    fprintf (dump_file, "Function %s/%i is not versionable, reason: %s.\n",
	     node->name (), node->order, reason);

  node->local.versionable = (reason == NULL);
}

/* Return true if it is at all technically possible to create clones of a
   NODE.  */

static bool
ipcp_versionable_function_p (struct cgraph_node *node)
{
  return node->local.versionable;
}

/* Structure holding accumulated information about callers of a node.  */

struct caller_statistics
{
  gcov_type count_sum;
  int n_calls, n_hot_calls, freq_sum;
};

/* Initialize fields of STAT to zeroes.  */

static inline void
init_caller_stats (struct caller_statistics *stats)
{
  stats->count_sum = 0;
  stats->n_calls = 0;
  stats->n_hot_calls = 0;
  stats->freq_sum = 0;
}

/* Worker callback of cgraph_for_node_and_aliases accumulating statistics of
   non-thunk incoming edges to NODE.  */

static bool
gather_caller_stats (struct cgraph_node *node, void *data)
{
  struct caller_statistics *stats = (struct caller_statistics *) data;
  struct cgraph_edge *cs;

  for (cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk.thunk_p)
      cgraph_for_node_and_aliases (cs->caller, gather_caller_stats,
				   stats, false);
    else
      {
	stats->count_sum += cs->count;
	stats->freq_sum += cs->frequency;
	stats->n_calls++;
	if (cgraph_maybe_hot_edge_p (cs))
	  stats->n_hot_calls ++;
      }
  return false;

}

/* Return true if this NODE is viable candidate for cloning.  */

static bool
ipcp_cloning_candidate_p (struct cgraph_node *node)
{
  struct caller_statistics stats;

  gcc_checking_assert (cgraph_function_with_gimple_body_p (node));

  if (!flag_ipa_cp_clone)
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; "
		 "-fipa-cp-clone disabled.\n",
 	         node->name ());
      return false;
    }

  if (!optimize_function_for_speed_p (DECL_STRUCT_FUNCTION (node->decl)))
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; "
		 "optimizing it for size.\n",
 	         node->name ());
      return false;
    }

  init_caller_stats (&stats);
  cgraph_for_node_and_aliases (node, gather_caller_stats, &stats, false);

  if (inline_summary (node)->self_size < stats.n_calls)
    {
      if (dump_file)
        fprintf (dump_file, "Considering %s for cloning; code might shrink.\n",
 	         node->name ());
      return true;
    }

  /* When profile is available and function is hot, propagate into it even if
     calls seems cold; constant propagation can improve function's speed
     significantly.  */
  if (max_count)
    {
      if (stats.count_sum > node->count * 90 / 100)
	{
	  if (dump_file)
	    fprintf (dump_file, "Considering %s for cloning; "
		     "usually called directly.\n",
		     node->name ());
	  return true;
        }
    }
  if (!stats.n_hot_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; no hot calls.\n",
		 node->name ());
      return false;
    }
  if (dump_file)
    fprintf (dump_file, "Considering %s for cloning.\n",
	     node->name ());
  return true;
}

/* Arrays representing a topological ordering of call graph nodes and a stack
   of noes used during constant propagation.  */

struct topo_info
{
  struct cgraph_node **order;
  struct cgraph_node **stack;
  int nnodes, stack_top;
};

/* Allocate the arrays in TOPO and topologically sort the nodes into order.  */

static void
build_toporder_info (struct topo_info *topo)
{
  topo->order = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  topo->stack = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  topo->stack_top = 0;
  topo->nnodes = ipa_reduced_postorder (topo->order, true, true, NULL);
}

/* Free information about strongly connected components and the arrays in
   TOPO.  */

static void
free_toporder_info (struct topo_info *topo)
{
  ipa_free_postorder_info ();
  free (topo->order);
  free (topo->stack);
}

/* Add NODE to the stack in TOPO, unless it is already there.  */

static inline void
push_node_to_stack (struct topo_info *topo, struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  if (info->node_enqueued)
    return;
  info->node_enqueued = 1;
  topo->stack[topo->stack_top++] = node;
}

/* Pop a node from the stack in TOPO and return it or return NULL if the stack
   is empty.  */

static struct cgraph_node *
pop_node_from_stack (struct topo_info *topo)
{
  if (topo->stack_top)
    {
      struct cgraph_node *node;
      topo->stack_top--;
      node = topo->stack[topo->stack_top];
      IPA_NODE_REF (node)->node_enqueued = 0;
      return node;
    }
  else
    return NULL;
}

/* Set lattice LAT to bottom and return true if it previously was not set as
   such.  */

static inline bool
set_lattice_to_bottom (struct ipcp_lattice *lat)
{
  bool ret = !lat->bottom;
  lat->bottom = true;
  return ret;
}

/* Mark lattice as containing an unknown value and return true if it previously
   was not marked as such.  */

static inline bool
set_lattice_contains_variable (struct ipcp_lattice *lat)
{
  bool ret = !lat->contains_variable;
  lat->contains_variable = true;
  return ret;
}

/* Set all aggegate lattices in PLATS to bottom and return true if they were
   not previously set as such.  */

static inline bool
set_agg_lats_to_bottom (struct ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_bottom;
  plats->aggs_bottom = true;
  return ret;
}

/* Mark all aggegate lattices in PLATS as containing an unknown value and
   return true if they were not previously marked as such.  */

static inline bool
set_agg_lats_contain_variable (struct ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_contain_variable;
  plats->aggs_contain_variable = true;
  return ret;
}

/* Mark bot aggregate and scalar lattices as containing an unknown variable,
   return true is any of them has not been marked as such so far.  */

static inline bool
set_all_contains_variable (struct ipcp_param_lattices *plats)
{
  bool ret = !plats->itself.contains_variable || !plats->aggs_contain_variable;
  plats->itself.contains_variable = true;
  plats->aggs_contain_variable = true;
  return ret;
}

/* Initialize ipcp_lattices.  */

static void
initialize_node_lattices (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  struct cgraph_edge *ie;
  bool disable = false, variable = false;
  int i;

  gcc_checking_assert (cgraph_function_with_gimple_body_p (node));
  if (!node->local.local)
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

  if (disable || variable)
    {
      for (i = 0; i < ipa_get_param_count (info) ; i++)
	{
	  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  if (disable)
	    {
	      set_lattice_to_bottom (&plats->itself);
	      set_agg_lats_to_bottom (plats);
	    }
	  else
	    set_all_contains_variable (plats);
	}
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && !node->alias && !node->thunk.thunk_p)
	fprintf (dump_file, "Marking all lattices of %s/%i as %s\n",
		 node->name (), node->order,
		 disable ? "BOTTOM" : "VARIABLE");
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

/* Return the result of a (possibly arithmetic) pass through jump function
   JFUNC on the constant value INPUT.  Return NULL_TREE if that cannot be
   determined or be considered an interprocedural invariant.  */

static tree
ipa_get_jf_pass_through_result (struct ipa_jump_func *jfunc, tree input)
{
  tree restype, res;

  if (TREE_CODE (input) == TREE_BINFO)
    {
      if (ipa_get_jf_pass_through_type_preserved (jfunc))
	{
	  gcc_checking_assert (ipa_get_jf_pass_through_operation (jfunc)
			       == NOP_EXPR);
	  return input;
	}
      return NULL_TREE;
    }

  if (ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    return input;

  gcc_checking_assert (is_gimple_ip_invariant (input));
  if (TREE_CODE_CLASS (ipa_get_jf_pass_through_operation (jfunc))
      == tcc_comparison)
    restype = boolean_type_node;
  else
    restype = TREE_TYPE (input);
  res = fold_binary (ipa_get_jf_pass_through_operation (jfunc), restype,
		     input, ipa_get_jf_pass_through_operand (jfunc));

  if (res && !is_gimple_ip_invariant (res))
    return NULL_TREE;

  return res;
}

/* Return the result of an ancestor jump function JFUNC on the constant value
   INPUT.  Return NULL_TREE if that cannot be determined.  */

static tree
ipa_get_jf_ancestor_result (struct ipa_jump_func *jfunc, tree input)
{
  if (TREE_CODE (input) == TREE_BINFO)
    {
      if (!ipa_get_jf_ancestor_type_preserved (jfunc))
	return NULL;
      return get_binfo_at_offset (input,
				  ipa_get_jf_ancestor_offset (jfunc),
				  ipa_get_jf_ancestor_type (jfunc));
    }
  else if (TREE_CODE (input) == ADDR_EXPR)
    {
      tree t = TREE_OPERAND (input, 0);
      t = build_ref_for_offset (EXPR_LOCATION (t), t,
				ipa_get_jf_ancestor_offset (jfunc),
				ipa_get_jf_ancestor_type (jfunc)
				? ipa_get_jf_ancestor_type (jfunc)
				: ptr_type_node, NULL, false);
      return build_fold_addr_expr (t);
    }
  else
    return NULL_TREE;
}

/* Determine whether JFUNC evaluates to a known value (that is either a
   constant or a binfo) and if so, return it.  Otherwise return NULL. INFO
   describes the caller node so that pass-through jump functions can be
   evaluated.  */

tree
ipa_value_from_jfunc (struct ipa_node_params *info, struct ipa_jump_func *jfunc)
{
  if (jfunc->type == IPA_JF_CONST)
    return ipa_get_jf_constant (jfunc);
  else if (jfunc->type == IPA_JF_KNOWN_TYPE)
    return ipa_binfo_from_known_type_jfunc (jfunc);
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
	input = info->known_vals[idx];
      else
	{
	  struct ipcp_lattice *lat;

	  if (!info->lattices)
	    {
	      gcc_checking_assert (!flag_ipa_cp);
	      return NULL_TREE;
	    }
	  lat = ipa_get_scalar_lat (info, idx);
	  if (!ipa_lat_is_single_const (lat))
	    return NULL_TREE;
	  input = lat->values->value;
	}

      if (!input)
	return NULL_TREE;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	return ipa_get_jf_pass_through_result (jfunc, input);
      else
	return ipa_get_jf_ancestor_result (jfunc, input);
    }
  else
    return NULL_TREE;
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
      struct ipa_node_params *info = IPA_NODE_REF (node);
      int i, count = ipa_get_param_count (info);

      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipa_get_scalar_lat (info, i);

	  if (!lat->bottom
	      && !lat->contains_variable
	      && lat->values_count == 0)
	    {
	      if (dump_file)
		{
		  dump_symtab (dump_file);
		  fprintf (dump_file, "\nIPA lattices after constant "
			   "propagation, before gcc_unreachable:\n");
		  print_all_lattices (dump_file, true, false);
		}

	      gcc_unreachable ();
	    }
	}
    }
}

/* Return true iff X and Y should be considered equal values by IPA-CP.  */

static bool
values_equal_for_ipcp_p (tree x, tree y)
{
  gcc_checking_assert (x != NULL_TREE && y != NULL_TREE);

  if (x == y)
    return true;

  if (TREE_CODE (x) == TREE_BINFO || TREE_CODE (y) == TREE_BINFO)
    return false;

  if (TREE_CODE (x) ==  ADDR_EXPR
      && TREE_CODE (y) ==  ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (x, 0)) == CONST_DECL
      && TREE_CODE (TREE_OPERAND (y, 0)) == CONST_DECL)
    return operand_equal_p (DECL_INITIAL (TREE_OPERAND (x, 0)),
			    DECL_INITIAL (TREE_OPERAND (y, 0)), 0);
  else
    return operand_equal_p (x, y, 0);
}

/* Add a new value source to VAL, marking that a value comes from edge CS and
   (if the underlying jump function is a pass-through or an ancestor one) from
   a caller value SRC_VAL of a caller parameter described by SRC_INDEX.  OFFSET
   is negative if the source was the scalar value of the parameter itself or
   the offset within an aggregate.  */

static void
add_value_source (struct ipcp_value *val, struct cgraph_edge *cs,
		  struct ipcp_value *src_val, int src_idx, HOST_WIDE_INT offset)
{
  struct ipcp_value_source *src;

  src = (struct ipcp_value_source *) pool_alloc (ipcp_sources_pool);
  src->offset = offset;
  src->cs = cs;
  src->val = src_val;
  src->index = src_idx;

  src->next = val->sources;
  val->sources = src;
}

/* Try to add NEWVAL to LAT, potentially creating a new struct ipcp_value for
   it.  CS, SRC_VAL SRC_INDEX and OFFSET are meant for add_value_source and
   have the same meaning.  */

static bool
add_value_to_lattice (struct ipcp_lattice *lat, tree newval,
		      struct cgraph_edge *cs, struct ipcp_value *src_val,
		      int src_idx, HOST_WIDE_INT offset)
{
  struct ipcp_value *val;

  if (lat->bottom)
    return false;

  for (val = lat->values; val; val = val->next)
    if (values_equal_for_ipcp_p (val->value, newval))
      {
	if (ipa_edge_within_scc (cs))
	  {
	    struct ipcp_value_source *s;
	    for (s = val->sources; s ; s = s->next)
	      if (s->cs == cs)
		break;
	    if (s)
	      return false;
	  }

	add_value_source (val, cs, src_val, src_idx, offset);
	return false;
      }

  if (lat->values_count == PARAM_VALUE (PARAM_IPA_CP_VALUE_LIST_SIZE))
    {
      /* We can only free sources, not the values themselves, because sources
	 of other values in this this SCC might point to them.   */
      for (val = lat->values; val; val = val->next)
	{
	  while (val->sources)
	    {
	      struct ipcp_value_source *src = val->sources;
	      val->sources = src->next;
	      pool_free (ipcp_sources_pool, src);
	    }
	}

      lat->values = NULL;
      return set_lattice_to_bottom (lat);
    }

  lat->values_count++;
  val = (struct ipcp_value *) pool_alloc (ipcp_values_pool);
  memset (val, 0, sizeof (*val));

  add_value_source (val, cs, src_val, src_idx, offset);
  val->value = newval;
  val->next = lat->values;
  lat->values = val;
  return true;
}

/* Like above but passes a special value of offset to distinguish that the
   origin is the scalar value of the parameter rather than a part of an
   aggregate.  */

static inline bool
add_scalar_value_to_lattice (struct ipcp_lattice *lat, tree newval,
			     struct cgraph_edge *cs,
			     struct ipcp_value *src_val, int src_idx)
{
  return add_value_to_lattice (lat, newval, cs, src_val, src_idx, -1);
}

/* Propagate values through a pass-through jump function JFUNC associated with
   edge CS, taking values from SRC_LAT and putting them into DEST_LAT.  SRC_IDX
   is the index of the source parameter.  */

static bool
propagate_vals_accross_pass_through (struct cgraph_edge *cs,
				     struct ipa_jump_func *jfunc,
				     struct ipcp_lattice *src_lat,
				     struct ipcp_lattice *dest_lat,
				     int src_idx)
{
  struct ipcp_value *src_val;
  bool ret = false;

  /* Do not create new values when propagating within an SCC because if there
     are arithmetic functions with circular dependencies, there is infinite
     number of them and we would just make lattices bottom.  */
  if ((ipa_get_jf_pass_through_operation (jfunc) != NOP_EXPR)
      && ipa_edge_within_scc (cs))
    ret = set_lattice_contains_variable (dest_lat);
  else
    for (src_val = src_lat->values; src_val; src_val = src_val->next)
      {
	tree cstval = ipa_get_jf_pass_through_result (jfunc, src_val->value);

	if (cstval)
	  ret |= add_scalar_value_to_lattice (dest_lat, cstval, cs, src_val,
					      src_idx);
	else
	  ret |= set_lattice_contains_variable (dest_lat);
      }

  return ret;
}

/* Propagate values through an ancestor jump function JFUNC associated with
   edge CS, taking values from SRC_LAT and putting them into DEST_LAT.  SRC_IDX
   is the index of the source parameter.  */

static bool
propagate_vals_accross_ancestor (struct cgraph_edge *cs,
				 struct ipa_jump_func *jfunc,
				 struct ipcp_lattice *src_lat,
				 struct ipcp_lattice *dest_lat,
				 int src_idx)
{
  struct ipcp_value *src_val;
  bool ret = false;

  if (ipa_edge_within_scc (cs))
    return set_lattice_contains_variable (dest_lat);

  for (src_val = src_lat->values; src_val; src_val = src_val->next)
    {
      tree t = ipa_get_jf_ancestor_result (jfunc, src_val->value);

      if (t)
	ret |= add_scalar_value_to_lattice (dest_lat, t, cs, src_val, src_idx);
      else
	ret |= set_lattice_contains_variable (dest_lat);
    }

  return ret;
}

/* Propagate scalar values across jump function JFUNC that is associated with
   edge CS and put the values into DEST_LAT.  */

static bool
propagate_scalar_accross_jump_function (struct cgraph_edge *cs,
					struct ipa_jump_func *jfunc,
					struct ipcp_lattice *dest_lat)
{
  if (dest_lat->bottom)
    return false;

  if (jfunc->type == IPA_JF_CONST
      || jfunc->type == IPA_JF_KNOWN_TYPE)
    {
      tree val;

      if (jfunc->type == IPA_JF_KNOWN_TYPE)
	{
	  val = ipa_binfo_from_known_type_jfunc (jfunc);
	  if (!val)
	    return set_lattice_contains_variable (dest_lat);
	}
      else
	val = ipa_get_jf_constant (jfunc);
      return add_scalar_value_to_lattice (dest_lat, val, cs, NULL, 0);
    }
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      struct ipcp_lattice *src_lat;
      int src_idx;
      bool ret;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      else
	src_idx = ipa_get_jf_ancestor_formal_id (jfunc);

      src_lat = ipa_get_scalar_lat (caller_info, src_idx);
      if (src_lat->bottom)
	return set_lattice_contains_variable (dest_lat);

      /* If we would need to clone the caller and cannot, do not propagate.  */
      if (!ipcp_versionable_function_p (cs->caller)
	  && (src_lat->contains_variable
	      || (src_lat->values_count > 1)))
	return set_lattice_contains_variable (dest_lat);

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	ret = propagate_vals_accross_pass_through (cs, jfunc, src_lat,
						   dest_lat, src_idx);
      else
	ret = propagate_vals_accross_ancestor (cs, jfunc, src_lat, dest_lat,
					       src_idx);

      if (src_lat->contains_variable)
	ret |= set_lattice_contains_variable (dest_lat);

      return ret;
    }

  /* TODO: We currently do not handle member method pointers in IPA-CP (we only
     use it for indirect inlining), we should propagate them too.  */
  return set_lattice_contains_variable (dest_lat);
}

/* If DEST_PLATS already has aggregate items, check that aggs_by_ref matches
   NEW_AGGS_BY_REF and if not, mark all aggs as bottoms and return true (in all
   other cases, return false).  If there are no aggregate items, set
   aggs_by_ref to NEW_AGGS_BY_REF.  */

static bool
set_check_aggs_by_ref (struct ipcp_param_lattices *dest_plats,
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
   true.  */

static bool
merge_agg_lats_step (struct ipcp_param_lattices *dest_plats,
		     HOST_WIDE_INT offset, HOST_WIDE_INT val_size,
		     struct ipcp_agg_lattice ***aglat,
		     bool pre_existing, bool *change)
{
  gcc_checking_assert (offset >= 0);

  while (**aglat && (**aglat)->offset < offset)
    {
      if ((**aglat)->offset + (**aglat)->size > offset)
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return false;
	}
      *change |= set_lattice_contains_variable (**aglat);
      *aglat = &(**aglat)->next;
    }

  if (**aglat && (**aglat)->offset == offset)
    {
      if ((**aglat)->size != val_size
          || ((**aglat)->next
              && (**aglat)->next->offset < offset + val_size))
	{
	  set_agg_lats_to_bottom (dest_plats);
	  return false;
	}
      gcc_checking_assert (!(**aglat)->next
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
      if (dest_plats->aggs_count == PARAM_VALUE (PARAM_IPA_MAX_AGG_ITEMS))
	return false;
      dest_plats->aggs_count++;
      new_al = (struct ipcp_agg_lattice *) pool_alloc (ipcp_agg_lattice_pool);
      memset (new_al, 0, sizeof (*new_al));

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
      ret |= set_lattice_contains_variable (aglat);
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
			  struct ipcp_param_lattices *dest_plats,
			  struct ipcp_param_lattices *src_plats,
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

  for (struct ipcp_agg_lattice *src_aglat = src_plats->aggs;
       src_aglat;
       src_aglat = src_aglat->next)
    {
      HOST_WIDE_INT new_offset = src_aglat->offset - offset_delta;

      if (new_offset < 0)
	continue;
      if (merge_agg_lats_step (dest_plats, new_offset, src_aglat->size,
			       &dst_aglat, pre_existing, &ret))
	{
	  struct ipcp_agg_lattice *new_al = *dst_aglat;

	  dst_aglat = &(*dst_aglat)->next;
	  if (src_aglat->bottom)
	    {
	      ret |= set_lattice_contains_variable (new_al);
	      continue;
	    }
	  if (src_aglat->contains_variable)
	    ret |= set_lattice_contains_variable (new_al);
	  for (struct ipcp_value *val = src_aglat->values;
	       val;
	       val = val->next)
	    ret |= add_value_to_lattice (new_al, val->value, cs, val, src_idx,
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
agg_pass_through_permissible_p (struct ipcp_param_lattices *src_plats,
				struct ipa_jump_func *jfunc)
{
  return src_plats->aggs
    && (!src_plats->aggs_by_ref
	|| ipa_get_jf_pass_through_agg_preserved (jfunc));
}

/* Propagate scalar values across jump function JFUNC that is associated with
   edge CS and put the values into DEST_LAT.  */

static bool
propagate_aggs_accross_jump_function (struct cgraph_edge *cs,
				      struct ipa_jump_func *jfunc,
				      struct ipcp_param_lattices *dest_plats)
{
  bool ret = false;

  if (dest_plats->aggs_bottom)
    return false;

  if (jfunc->type == IPA_JF_PASS_THROUGH
      && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      int src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
      struct ipcp_param_lattices *src_plats;

      src_plats = ipa_get_parm_lattices (caller_info, src_idx);
      if (agg_pass_through_permissible_p (src_plats, jfunc))
	{
	  /* Currently we do not produce clobber aggregate jump
	     functions, replace with merging when we do.  */
	  gcc_assert (!jfunc->agg.items);
	  ret |= merge_aggregate_lattices (cs, dest_plats, src_plats,
					   src_idx, 0);
	}
      else
	ret |= set_agg_lats_contain_variable (dest_plats);
    }
  else if (jfunc->type == IPA_JF_ANCESTOR
	   && ipa_get_jf_ancestor_agg_preserved (jfunc))
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      int src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
      struct ipcp_param_lattices *src_plats;

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
    }
  else if (jfunc->agg.items)
    {
      bool pre_existing = dest_plats->aggs != NULL;
      struct ipcp_agg_lattice **aglat = &dest_plats->aggs;
      struct ipa_agg_jf_item *item;
      int i;

      if (set_check_aggs_by_ref (dest_plats, jfunc->agg.by_ref))
	return true;

      FOR_EACH_VEC_ELT (*jfunc->agg.items, i, item)
	{
	  HOST_WIDE_INT val_size;

	  if (item->offset < 0)
	    continue;
	  gcc_checking_assert (is_gimple_ip_invariant (item->value));
	  val_size = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (item->value)));

	  if (merge_agg_lats_step (dest_plats, item->offset, val_size,
				   &aglat, pre_existing, &ret))
	    {
	      ret |= add_value_to_lattice (*aglat, item->value, cs, NULL, 0, 0);
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

/* Propagate constants from the caller to the callee of CS.  INFO describes the
   caller.  */

static bool
propagate_constants_accross_call (struct cgraph_edge *cs)
{
  struct ipa_node_params *callee_info;
  enum availability availability;
  struct cgraph_node *callee, *alias_or_thunk;
  struct ipa_edge_args *args;
  bool ret = false;
  int i, args_count, parms_count;

  callee = cgraph_function_node (cs->callee, &availability);
  if (!callee->definition)
    return false;
  gcc_checking_assert (cgraph_function_with_gimple_body_p (callee));
  callee_info = IPA_NODE_REF (callee);

  args = IPA_EDGE_REF (cs);
  args_count = ipa_get_cs_argument_count (args);
  parms_count = ipa_get_param_count (callee_info);
  if (parms_count == 0)
    return false;

  /* If this call goes through a thunk we must not propagate to the first (0th)
     parameter.  However, we might need to uncover a thunk from below a series
     of aliases first.  */
  alias_or_thunk = cs->callee;
  while (alias_or_thunk->alias)
    alias_or_thunk = cgraph_alias_target (alias_or_thunk);
  if (alias_or_thunk->thunk.thunk_p)
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
      struct ipcp_param_lattices *dest_plats;

      dest_plats = ipa_get_parm_lattices (callee_info, i);
      if (availability == AVAIL_OVERWRITABLE)
	ret |= set_all_contains_variable (dest_plats);
      else
	{
	  ret |= propagate_scalar_accross_jump_function (cs, jump_func,
							 &dest_plats->itself);
	  ret |= propagate_aggs_accross_jump_function (cs, jump_func,
						       dest_plats);
	}
    }
  for (; i < parms_count; i++)
    ret |= set_all_contains_variable (ipa_get_parm_lattices (callee_info, i));

  return ret;
}

/* If an indirect edge IE can be turned into a direct one based on KNOWN_VALS
   (which can contain both constants and binfos), KNOWN_BINFOS, KNOWN_AGGS or
   AGG_REPS return the destination.  The latter three can be NULL.  If AGG_REPS
   is not NULL, KNOWN_AGGS is ignored.  */

static tree
ipa_get_indirect_edge_target_1 (struct cgraph_edge *ie,
				vec<tree> known_vals,
				vec<tree> known_binfos,
				vec<ipa_agg_jump_function_p> known_aggs,
				struct ipa_agg_replacement_value *agg_reps)
{
  int param_index = ie->indirect_info->param_index;
  HOST_WIDE_INT token, anc_offset;
  tree otr_type;
  tree t;
  tree target = NULL;

  if (param_index == -1
      || known_vals.length () <= (unsigned int) param_index)
    return NULL_TREE;

  if (!ie->indirect_info->polymorphic)
    {
      tree t;

      if (ie->indirect_info->agg_contents)
	{
	  if (agg_reps)
	    {
	      t = NULL;
	      while (agg_reps)
		{
		  if (agg_reps->index == param_index
		      && agg_reps->offset == ie->indirect_info->offset
		      && agg_reps->by_ref == ie->indirect_info->by_ref)
		    {
		      t = agg_reps->value;
		      break;
		    }
		  agg_reps = agg_reps->next;
		}
	    }
	  else if (known_aggs.length () > (unsigned int) param_index)
	    {
	      struct ipa_agg_jump_function *agg;
	      agg = known_aggs[param_index];
	      t = ipa_find_agg_cst_for_param (agg, ie->indirect_info->offset,
					      ie->indirect_info->by_ref);
	    }
	  else
	    t = NULL;
	}
      else
	t = known_vals[param_index];

      if (t &&
	  TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL)
	return TREE_OPERAND (t, 0);
      else
	return NULL_TREE;
    }

  if (!flag_devirtualize)
    return NULL_TREE;

  gcc_assert (!ie->indirect_info->agg_contents);
  token = ie->indirect_info->otr_token;
  anc_offset = ie->indirect_info->offset;
  otr_type = ie->indirect_info->otr_type;

  t = NULL;

  /* Try to work out value of virtual table pointer value in replacemnets.  */
  if (!t && agg_reps && !ie->indirect_info->by_ref)
    {
      while (agg_reps)
	{
	  if (agg_reps->index == param_index
	      && agg_reps->offset == ie->indirect_info->offset
	      && agg_reps->by_ref)
	    {
	      t = agg_reps->value;
	      break;
	    }
	  agg_reps = agg_reps->next;
	}
    }

  /* Try to work out value of virtual table pointer value in known
     aggregate values.  */
  if (!t && known_aggs.length () > (unsigned int) param_index
      && !ie->indirect_info->by_ref)
    {
       struct ipa_agg_jump_function *agg;
       agg = known_aggs[param_index];
       t = ipa_find_agg_cst_for_param (agg, ie->indirect_info->offset,
				       true);
    }

  /* If we found the virtual table pointer, lookup the target.  */
  if (t)
    {
      tree vtable;
      unsigned HOST_WIDE_INT offset;
      if (vtable_pointer_value_to_vtable (t, &vtable, &offset))
	{
	  target = gimple_get_virt_method_for_vtable (ie->indirect_info->otr_token,
						      vtable, offset);
	  if (target)
	    {
	      if ((TREE_CODE (TREE_TYPE (target)) == FUNCTION_TYPE
		   && DECL_FUNCTION_CODE (target) == BUILT_IN_UNREACHABLE)
		  || !possible_polymorphic_call_target_p
		       (ie, cgraph_get_node (target)))
		{
		  if (dump_file)
		    fprintf (dump_file,
			     "Type inconsident devirtualization: %s/%i->%s\n",
			     ie->caller->name (), ie->caller->order,
			     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (target)));
		  target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
		  cgraph_get_create_node (target);
		}
	      return target;
	    }
	}
    }

  /* Did we work out BINFO via type propagation?  */
  if (!t && known_binfos.length () > (unsigned int) param_index)
    t = known_binfos[param_index];
  /* Or do we know the constant value of pointer?  */
  if (!t)
    t = known_vals[param_index];
  if (!t)
    return NULL_TREE;

  if (TREE_CODE (t) != TREE_BINFO)
    {
      ipa_polymorphic_call_context context;
      vec <cgraph_node *>targets;
      bool final;

      if (!get_polymorphic_call_info_from_invariant
	     (&context, t, ie->indirect_info->otr_type,
	      anc_offset))
	return NULL_TREE;
      targets = possible_polymorphic_call_targets
		 (ie->indirect_info->otr_type,
		  ie->indirect_info->otr_token,
		  context, &final);
      if (!final || targets.length () > 1)
	return NULL_TREE;
      if (targets.length () == 1)
	target = targets[0]->decl;
      else
	target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
    }
  else
    {
      tree binfo;

      binfo = get_binfo_at_offset (t, anc_offset, otr_type);
      if (!binfo)
	return NULL_TREE;
      target = gimple_get_virt_method_for_binfo (token, binfo);
    }

  if (target && !possible_polymorphic_call_target_p (ie,
						     cgraph_get_node (target)))
    {
      if (dump_file)
	fprintf (dump_file,
		 "Type inconsident devirtualization: %s/%i->%s\n",
		 ie->caller->name (), ie->caller->order,
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (target)));
      target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
      cgraph_get_create_node (target);
    }

  return target;
}


/* If an indirect edge IE can be turned into a direct one based on KNOWN_VALS
   (which can contain both constants and binfos), KNOWN_BINFOS (which can be
   NULL) or KNOWN_AGGS (which also can be NULL) return the destination.  */

tree
ipa_get_indirect_edge_target (struct cgraph_edge *ie,
			      vec<tree> known_vals,
			      vec<tree> known_binfos,
			      vec<ipa_agg_jump_function_p> known_aggs)
{
  return ipa_get_indirect_edge_target_1 (ie, known_vals, known_binfos,
					 known_aggs, NULL);
}

/* Calculate devirtualization time bonus for NODE, assuming we know KNOWN_CSTS
   and KNOWN_BINFOS.  */

static int
devirtualization_time_bonus (struct cgraph_node *node,
			     vec<tree> known_csts,
			     vec<tree> known_binfos,
			     vec<ipa_agg_jump_function_p> known_aggs)
{
  struct cgraph_edge *ie;
  int res = 0;

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    {
      struct cgraph_node *callee;
      struct inline_summary *isummary;
      tree target;

      target = ipa_get_indirect_edge_target (ie, known_csts, known_binfos,
					     known_aggs);
      if (!target)
	continue;

      /* Only bare minimum benefit for clearly un-inlineable targets.  */
      res += 1;
      callee = cgraph_get_node (target);
      if (!callee || !callee->definition)
	continue;
      isummary = inline_summary (callee);
      if (!isummary->inlinable)
	continue;

      /* FIXME: The values below need re-considering and perhaps also
	 integrating into the cost metrics, at lest in some very basic way.  */
      if (isummary->size <= MAX_INLINE_INSNS_AUTO / 4)
	res += 31;
      else if (isummary->size <= MAX_INLINE_INSNS_AUTO / 2)
	res += 15;
      else if (isummary->size <= MAX_INLINE_INSNS_AUTO
	       || DECL_DECLARED_INLINE_P (callee->decl))
	res += 7;
    }

  return res;
}

/* Return time bonus incurred because of HINTS.  */

static int
hint_time_bonus (inline_hints hints)
{
  int result = 0;
  if (hints & (INLINE_HINT_loop_iterations | INLINE_HINT_loop_stride))
    result += PARAM_VALUE (PARAM_IPA_CP_LOOP_HINT_BONUS);
  if (hints & INLINE_HINT_array_index)
    result += PARAM_VALUE (PARAM_IPA_CP_ARRAY_INDEX_HINT_BONUS);
  return result;
}

/* Return true if cloning NODE is a good idea, given the estimated TIME_BENEFIT
   and SIZE_COST and with the sum of frequencies of incoming edges to the
   potential new clone in FREQUENCIES.  */

static bool
good_cloning_opportunity_p (struct cgraph_node *node, int time_benefit,
			    int freq_sum, gcov_type count_sum, int size_cost)
{
  if (time_benefit == 0
      || !flag_ipa_cp_clone
      || !optimize_function_for_speed_p (DECL_STRUCT_FUNCTION (node->decl)))
    return false;

  gcc_assert (size_cost > 0);

  if (max_count)
    {
      int factor = (count_sum * 1000) / max_count;
      HOST_WIDEST_INT evaluation = (((HOST_WIDEST_INT) time_benefit * factor)
				    / size_cost);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		 "size: %i, count_sum: " HOST_WIDE_INT_PRINT_DEC
		 ") -> evaluation: " HOST_WIDEST_INT_PRINT_DEC
		 ", threshold: %i\n",
		 time_benefit, size_cost, (HOST_WIDE_INT) count_sum,
		 evaluation, PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD));

      return evaluation >= PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD);
    }
  else
    {
      HOST_WIDEST_INT evaluation = (((HOST_WIDEST_INT) time_benefit * freq_sum)
				    / size_cost);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		 "size: %i, freq_sum: %i) -> evaluation: "
		 HOST_WIDEST_INT_PRINT_DEC ", threshold: %i\n",
		 time_benefit, size_cost, freq_sum, evaluation,
		 PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD));

      return evaluation >= PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD);
    }
}

/* Return all context independent values from aggregate lattices in PLATS in a
   vector.  Return NULL if there are none.  */

static vec<ipa_agg_jf_item, va_gc> *
context_independent_aggregate_values (struct ipcp_param_lattices *plats)
{
  vec<ipa_agg_jf_item, va_gc> *res = NULL;

  if (plats->aggs_bottom
      || plats->aggs_contain_variable
      || plats->aggs_count == 0)
    return NULL;

  for (struct ipcp_agg_lattice *aglat = plats->aggs;
       aglat;
       aglat = aglat->next)
    if (ipa_lat_is_single_const (aglat))
      {
	struct ipa_agg_jf_item item;
	item.offset = aglat->offset;
	item.value = aglat->values->value;
	vec_safe_push (res, item);
      }
  return res;
}

/* Allocate KNOWN_CSTS, KNOWN_BINFOS and, if non-NULL, KNOWN_AGGS and populate
   them with values of parameters that are known independent of the context.
   INFO describes the function.  If REMOVABLE_PARAMS_COST is non-NULL, the
   movement cost of all removable parameters will be stored in it.  */

static bool
gather_context_independent_values (struct ipa_node_params *info,
			       vec<tree> *known_csts,
			       vec<tree> *known_binfos,
			       vec<ipa_agg_jump_function> *known_aggs,
			       int *removable_params_cost)
{
  int i, count = ipa_get_param_count (info);
  bool ret = false;

  known_csts->create (0);
  known_binfos->create (0);
  known_csts->safe_grow_cleared (count);
  known_binfos->safe_grow_cleared (count);
  if (known_aggs)
    {
      known_aggs->create (0);
      known_aggs->safe_grow_cleared (count);
    }

  if (removable_params_cost)
    *removable_params_cost = 0;

  for (i = 0; i < count ; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipcp_lattice *lat = &plats->itself;

      if (ipa_lat_is_single_const (lat))
	{
	  struct ipcp_value *val = lat->values;
	  if (TREE_CODE (val->value) != TREE_BINFO)
	    {
	      (*known_csts)[i] = val->value;
	      if (removable_params_cost)
		*removable_params_cost
		  += estimate_move_cost (TREE_TYPE (val->value));
	      ret = true;
	    }
	  else if (plats->virt_call)
	    {
	      (*known_binfos)[i] = val->value;
	      ret = true;
	    }
	  else if (removable_params_cost
		   && !ipa_is_param_used (info, i))
	    *removable_params_cost += ipa_get_param_move_cost (info, i);
	}
      else if (removable_params_cost
	       && !ipa_is_param_used (info, i))
	*removable_params_cost
	  += ipa_get_param_move_cost (info, i);

      if (known_aggs)
	{
	  vec<ipa_agg_jf_item, va_gc> *agg_items;
	  struct ipa_agg_jump_function *ajf;

	  agg_items = context_independent_aggregate_values (plats);
	  ajf = &(*known_aggs)[i];
	  ajf->items = agg_items;
	  ajf->by_ref = plats->aggs_by_ref;
	  ret |= agg_items != NULL;
	}
    }

  return ret;
}

/* The current interface in ipa-inline-analysis requires a pointer vector.
   Create it.

   FIXME: That interface should be re-worked, this is slightly silly.  Still,
   I'd like to discuss how to change it first and this demonstrates the
   issue.  */

static vec<ipa_agg_jump_function_p>
agg_jmp_p_vec_for_t_vec (vec<ipa_agg_jump_function> known_aggs)
{
  vec<ipa_agg_jump_function_p> ret;
  struct ipa_agg_jump_function *ajf;
  int i;

  ret.create (known_aggs.length ());
  FOR_EACH_VEC_ELT (known_aggs, i, ajf)
    ret.quick_push (ajf);
  return ret;
}

/* Iterate over known values of parameters of NODE and estimate the local
   effects in terms of time and size they have.  */

static void
estimate_local_effects (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);
  vec<tree> known_csts, known_binfos;
  vec<ipa_agg_jump_function> known_aggs;
  vec<ipa_agg_jump_function_p> known_aggs_ptrs;
  bool always_const;
  int base_time = inline_summary (node)->time;
  int removable_params_cost;

  if (!count || !ipcp_versionable_function_p (node))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEstimating effects for %s/%i, base_time: %i.\n",
	     node->name (), node->order, base_time);

  always_const = gather_context_independent_values (info, &known_csts,
						    &known_binfos, &known_aggs,
						    &removable_params_cost);
  known_aggs_ptrs = agg_jmp_p_vec_for_t_vec (known_aggs);
  if (always_const)
    {
      struct caller_statistics stats;
      inline_hints hints;
      int time, size;

      init_caller_stats (&stats);
      cgraph_for_node_and_aliases (node, gather_caller_stats, &stats, false);
      estimate_ipcp_clone_size_and_time (node, known_csts, known_binfos,
					 known_aggs_ptrs, &size, &time, &hints);
      time -= devirtualization_time_bonus (node, known_csts, known_binfos,
					   known_aggs_ptrs);
      time -= hint_time_bonus (hints);
      time -= removable_params_cost;
      size -= stats.n_calls * removable_params_cost;

      if (dump_file)
	fprintf (dump_file, " - context independent values, size: %i, "
		 "time_benefit: %i\n", size, base_time - time);

      if (size <= 0
	  || cgraph_will_be_removed_from_program_if_no_direct_calls (node))
	{
	  info->do_clone_for_all_contexts = true;
	  base_time = time;

	  if (dump_file)
	    fprintf (dump_file, "     Decided to specialize for all "
		     "known contexts, code not going to grow.\n");
	}
      else if (good_cloning_opportunity_p (node, base_time - time,
					   stats.freq_sum, stats.count_sum,
					   size))
	{
	  if (size + overall_size <= max_new_size)
	    {
	      info->do_clone_for_all_contexts = true;
	      base_time = time;
	      overall_size += size;

	      if (dump_file)
		fprintf (dump_file, "     Decided to specialize for all "
			 "known contexts, growth deemed beneficial.\n");
	    }
	  else if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "   Not cloning for all contexts because "
		     "max_new_size would be reached with %li.\n",
		     size + overall_size);
	}
    }

  for (i = 0; i < count ; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipcp_lattice *lat = &plats->itself;
      struct ipcp_value *val;
      int emc;

      if (lat->bottom
	  || !lat->values
	  || known_csts[i]
	  || known_binfos[i])
	continue;

      for (val = lat->values; val; val = val->next)
	{
	  int time, size, time_benefit;
	  inline_hints hints;

	  if (TREE_CODE (val->value) != TREE_BINFO)
	    {
	      known_csts[i] = val->value;
	      known_binfos[i] = NULL_TREE;
	      emc = estimate_move_cost (TREE_TYPE (val->value));
	    }
	  else if (plats->virt_call)
	    {
	      known_csts[i] = NULL_TREE;
	      known_binfos[i] = val->value;
	      emc = 0;
	    }
	  else
	    continue;

	  estimate_ipcp_clone_size_and_time (node, known_csts, known_binfos,
					     known_aggs_ptrs, &size, &time,
					     &hints);
	  time_benefit = base_time - time
	    + devirtualization_time_bonus (node, known_csts, known_binfos,
					   known_aggs_ptrs)
	    + hint_time_bonus (hints)
	    + removable_params_cost + emc;

	  gcc_checking_assert (size >=0);
	  /* The inliner-heuristics based estimates may think that in certain
	     contexts some functions do not have any size at all but we want
	     all specializations to have at least a tiny cost, not least not to
	     divide by zero.  */
	  if (size == 0)
	    size = 1;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for value ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, ": time_benefit: %i, size: %i\n",
		       time_benefit, size);
	    }

	  val->local_time_benefit = time_benefit;
	  val->local_size_cost = size;
	}
      known_binfos[i] = NULL_TREE;
      known_csts[i] = NULL_TREE;
    }

  for (i = 0; i < count ; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipa_agg_jump_function *ajf;
      struct ipcp_agg_lattice *aglat;

      if (plats->aggs_bottom || !plats->aggs)
	continue;

      ajf = &known_aggs[i];
      for (aglat = plats->aggs; aglat; aglat = aglat->next)
	{
	  struct ipcp_value *val;
	  if (aglat->bottom || !aglat->values
	      /* If the following is true, the one value is in known_aggs.  */
	      || (!plats->aggs_contain_variable
		  && ipa_lat_is_single_const (aglat)))
	    continue;

	  for (val = aglat->values; val; val = val->next)
	    {
	      int time, size, time_benefit;
	      struct ipa_agg_jf_item item;
	      inline_hints hints;

	      item.offset = aglat->offset;
	      item.value = val->value;
	      vec_safe_push (ajf->items, item);

	      estimate_ipcp_clone_size_and_time (node, known_csts, known_binfos,
						 known_aggs_ptrs, &size, &time,
						 &hints);
	      time_benefit = base_time - time
		+ devirtualization_time_bonus (node, known_csts, known_binfos,
					       known_aggs_ptrs)
		+ hint_time_bonus (hints);
	      gcc_checking_assert (size >=0);
	      if (size == 0)
		size = 1;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, " - estimates for value ");
		  print_ipcp_constant_value (dump_file, val->value);
		  fprintf (dump_file, " for ");
	          ipa_dump_param (dump_file, info, i);
		  fprintf (dump_file, "[%soffset: " HOST_WIDE_INT_PRINT_DEC
				       "]: time_benefit: %i, size: %i\n",
				       plats->aggs_by_ref ? "ref " : "",
				       aglat->offset, time_benefit, size);
		}

	      val->local_time_benefit = time_benefit;
	      val->local_size_cost = size;
	      ajf->items->pop ();
	    }
	}
    }

  for (i = 0; i < count ; i++)
    vec_free (known_aggs[i].items);

  known_csts.release ();
  known_binfos.release ();
  known_aggs.release ();
  known_aggs_ptrs.release ();
}


/* Add value CUR_VAL and all yet-unsorted values it is dependent on to the
   topological sort of values.  */

static void
add_val_to_toposort (struct ipcp_value *cur_val)
{
  static int dfs_counter = 0;
  static struct ipcp_value *stack;
  struct ipcp_value_source *src;

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
	    add_val_to_toposort (src->val);
	    if (src->val->low_link < cur_val->low_link)
	      cur_val->low_link = src->val->low_link;
	  }
	else if (src->val->on_stack
		 && src->val->dfs < cur_val->low_link)
	  cur_val->low_link = src->val->dfs;
      }

  if (cur_val->dfs == cur_val->low_link)
    {
      struct ipcp_value *v, *scc_list = NULL;

      do
	{
	  v = stack;
	  stack = v->topo_next;
	  v->on_stack = false;

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
add_all_node_vals_to_toposort (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count ; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipcp_lattice *lat = &plats->itself;
      struct ipcp_agg_lattice *aglat;
      struct ipcp_value *val;

      if (!lat->bottom)
	for (val = lat->values; val; val = val->next)
	  add_val_to_toposort (val);

      if (!plats->aggs_bottom)
	for (aglat = plats->aggs; aglat; aglat = aglat->next)
	  if (!aglat->bottom)
	    for (val = aglat->values; val; val = val->next)
	      add_val_to_toposort (val);
    }
}

/* One pass of constants propagation along the call graph edges, from callers
   to callees (requires topological ordering in TOPO), iterate over strongly
   connected components.  */

static void
propagate_constants_topo (struct topo_info *topo)
{
  int i;

  for (i = topo->nnodes - 1; i >= 0; i--)
    {
      unsigned j;
      struct cgraph_node *v, *node = topo->order[i];
      vec<cgraph_node_ptr> cycle_nodes = ipa_get_nodes_in_cycle (node);

      /* First, iteratively propagate within the strongly connected component
	 until all lattices stabilize.  */
      FOR_EACH_VEC_ELT (cycle_nodes, j, v)
	if (cgraph_function_with_gimple_body_p (v))
	  push_node_to_stack (topo, v);

      v = pop_node_from_stack (topo);
      while (v)
	{
	  struct cgraph_edge *cs;

	  for (cs = v->callees; cs; cs = cs->next_callee)
	    if (ipa_edge_within_scc (cs)
		&& propagate_constants_accross_call (cs))
	      push_node_to_stack (topo, cs->callee);
	  v = pop_node_from_stack (topo);
	}

      /* Afterwards, propagate along edges leading out of the SCC, calculates
	 the local effects of the discovered constants and all valid values to
	 their topological sort.  */
      FOR_EACH_VEC_ELT (cycle_nodes, j, v)
	if (cgraph_function_with_gimple_body_p (v))
	  {
	    struct cgraph_edge *cs;

	    estimate_local_effects (v);
	    add_all_node_vals_to_toposort (v);
	    for (cs = v->callees; cs; cs = cs->next_callee)
	      if (!ipa_edge_within_scc (cs))
		propagate_constants_accross_call (cs);
	  }
      cycle_nodes.release ();
    }
}


/* Return the sum of A and B if none of them is bigger than INT_MAX/2, return
   the bigger one if otherwise.  */

static int
safe_add (int a, int b)
{
  if (a > INT_MAX/2 || b > INT_MAX/2)
    return a > b ? a : b;
  else
    return a + b;
}


/* Propagate the estimated effects of individual values along the topological
   from the dependent values to those they depend on.  */

static void
propagate_effects (void)
{
  struct ipcp_value *base;

  for (base = values_topo; base; base = base->topo_next)
    {
      struct ipcp_value_source *src;
      struct ipcp_value *val;
      int time = 0, size = 0;

      for (val = base; val; val = val->scc_next)
	{
	  time = safe_add (time,
			   val->local_time_benefit + val->prop_time_benefit);
	  size = safe_add (size, val->local_size_cost + val->prop_size_cost);
	}

      for (val = base; val; val = val->scc_next)
	for (src = val->sources; src; src = src->next)
	  if (src->val
	      && cgraph_maybe_hot_edge_p (src->cs))
	    {
	      src->val->prop_time_benefit = safe_add (time,
						src->val->prop_time_benefit);
	      src->val->prop_size_cost = safe_add (size,
						   src->val->prop_size_cost);
	    }
    }
}


/* Propagate constants, binfos and their effects from the summaries
   interprocedurally.  */

static void
ipcp_propagate_stage (struct topo_info *topo)
{
  struct cgraph_node *node;

  if (dump_file)
    fprintf (dump_file, "\n Propagating constants:\n\n");

  if (in_lto_p)
    ipa_update_after_lto_read ();


  FOR_EACH_DEFINED_FUNCTION (node)
  {
    struct ipa_node_params *info = IPA_NODE_REF (node);

    determine_versionability (node);
    if (cgraph_function_with_gimple_body_p (node))
      {
	info->lattices = XCNEWVEC (struct ipcp_param_lattices,
				   ipa_get_param_count (info));
	initialize_node_lattices (node);
      }
    if (node->definition && !node->alias)
      overall_size += inline_summary (node)->self_size;
    if (node->count > max_count)
      max_count = node->count;
  }

  max_new_size = overall_size;
  if (max_new_size < PARAM_VALUE (PARAM_LARGE_UNIT_INSNS))
    max_new_size = PARAM_VALUE (PARAM_LARGE_UNIT_INSNS);
  max_new_size += max_new_size * PARAM_VALUE (PARAM_IPCP_UNIT_GROWTH) / 100 + 1;

  if (dump_file)
    fprintf (dump_file, "\noverall_size: %li, max_new_size: %li\n",
	     overall_size, max_new_size);

  propagate_constants_topo (topo);
#ifdef ENABLE_CHECKING
  ipcp_verify_propagated_values ();
#endif
  propagate_effects ();

  if (dump_file)
    {
      fprintf (dump_file, "\nIPA lattices after all propagation:\n");
      print_all_lattices (dump_file, (dump_flags & TDF_DETAILS), true);
    }
}

/* Discover newly direct outgoing edges from NODE which is a new clone with
   known KNOWN_VALS and make them direct.  */

static void
ipcp_discover_new_direct_edges (struct cgraph_node *node,
				vec<tree> known_vals,
				struct ipa_agg_replacement_value *aggvals)
{
  struct cgraph_edge *ie, *next_ie;
  bool found = false;

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      tree target;

      next_ie = ie->next_callee;
      target = ipa_get_indirect_edge_target_1 (ie, known_vals, vNULL, vNULL,
					       aggvals);
      if (target)
	{
	  bool agg_contents = ie->indirect_info->agg_contents;
	  bool polymorphic = ie->indirect_info->polymorphic;
	  int param_index = ie->indirect_info->param_index;
	  struct cgraph_edge *cs = ipa_make_edge_direct_to_target (ie, target);
	  found = true;

	  if (cs && !agg_contents && !polymorphic)
	    {
	      struct ipa_node_params *info = IPA_NODE_REF (node);
	      int c = ipa_get_controlled_uses (info, param_index);
	      if (c != IPA_UNDESCRIBED_USE)
		{
		  struct ipa_ref *to_del;

		  c--;
		  ipa_set_controlled_uses (info, param_index, c);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "     controlled uses count of param "
			     "%i bumped down to %i\n", param_index, c);
		  if (c == 0
		      && (to_del = ipa_find_reference (node,
						       cs->callee,
						       NULL, 0)))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file, "       and even removing its "
				 "cloning-created reference\n");
		      ipa_remove_reference (to_del);
		    }
		}
	    }
	}
    }
  /* Turning calls to direct calls will improve overall summary.  */
  if (found)
    inline_update_overall_summary (node);
}

/* Vector of pointers which for linked lists of clones of an original crgaph
   edge. */

static vec<cgraph_edge_p> next_edge_clone;
static vec<cgraph_edge_p> prev_edge_clone;

static inline void
grow_edge_clone_vectors (void)
{
  if (next_edge_clone.length ()
      <=  (unsigned) cgraph_edge_max_uid)
    next_edge_clone.safe_grow_cleared (cgraph_edge_max_uid + 1);
  if (prev_edge_clone.length ()
      <=  (unsigned) cgraph_edge_max_uid)
    prev_edge_clone.safe_grow_cleared (cgraph_edge_max_uid + 1);
}

/* Edge duplication hook to grow the appropriate linked list in
   next_edge_clone. */

static void
ipcp_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			    void *)
{
  grow_edge_clone_vectors ();

  struct cgraph_edge *old_next = next_edge_clone[src->uid];
  if (old_next)
    prev_edge_clone[old_next->uid] = dst;
  prev_edge_clone[dst->uid] = src;

  next_edge_clone[dst->uid] = old_next;
  next_edge_clone[src->uid] = dst;
}

/* Hook that is called by cgraph.c when an edge is removed.  */

static void
ipcp_edge_removal_hook (struct cgraph_edge *cs, void *)
{
  grow_edge_clone_vectors ();

  struct cgraph_edge *prev = prev_edge_clone[cs->uid];
  struct cgraph_edge *next = next_edge_clone[cs->uid];
  if (prev)
    next_edge_clone[prev->uid] = next;
  if (next)
    prev_edge_clone[next->uid] = prev;
}

/* See if NODE is a clone with a known aggregate value at a given OFFSET of a
   parameter with the given INDEX.  */

static tree
get_clone_agg_value (struct cgraph_node *node, HOST_WIDEST_INT offset,
		     int index)
{
  struct ipa_agg_replacement_value *aggval;

  aggval = ipa_get_agg_replacements_for_node (node);
  while (aggval)
    {
      if (aggval->offset == offset
	  && aggval->index == index)
	return aggval->value;
      aggval = aggval->next;
    }
  return NULL_TREE;
}

/* Return true if edge CS does bring about the value described by SRC.  */

static bool
cgraph_edge_brings_value_p (struct cgraph_edge *cs,
			    struct ipcp_value_source *src)
{
  struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
  struct ipa_node_params *dst_info = IPA_NODE_REF (cs->callee);

  if ((dst_info->ipcp_orig_node && !dst_info->is_all_contexts_clone)
      || caller_info->node_dead)
    return false;
  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    {
      tree t;
      if (src->offset == -1)
	t = caller_info->known_vals[src->index];
      else
	t = get_clone_agg_value (cs->caller, src->offset, src->index);
      return (t != NULL_TREE
	      && values_equal_for_ipcp_p (src->val->value, t));
    }
  else
    {
      struct ipcp_agg_lattice *aglat;
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (caller_info,
								 src->index);
      if (src->offset == -1)
	return (ipa_lat_is_single_const (&plats->itself)
		&& values_equal_for_ipcp_p (src->val->value,
					    plats->itself.values->value));
      else
	{
	  if (plats->aggs_bottom || plats->aggs_contain_variable)
	    return false;
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    if (aglat->offset == src->offset)
	      return  (ipa_lat_is_single_const (aglat)
		       && values_equal_for_ipcp_p (src->val->value,
						   aglat->values->value));
	}
      return false;
    }
}

/* Get the next clone in the linked list of clones of an edge.  */

static inline struct cgraph_edge *
get_next_cgraph_edge_clone (struct cgraph_edge *cs)
{
  return next_edge_clone[cs->uid];
}

/* Given VAL, iterate over all its sources and if they still hold, add their
   edge frequency and their number into *FREQUENCY and *CALLER_COUNT
   respectively.  */

static bool
get_info_about_necessary_edges (struct ipcp_value *val, int *freq_sum,
				gcov_type *count_sum, int *caller_count)
{
  struct ipcp_value_source *src;
  int freq = 0, count = 0;
  gcov_type cnt = 0;
  bool hot = false;

  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src))
	    {
	      count++;
	      freq += cs->frequency;
	      cnt += cs->count;
	      hot |= cgraph_maybe_hot_edge_p (cs);
	    }
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  *freq_sum = freq;
  *count_sum = cnt;
  *caller_count = count;
  return hot;
}

/* Return a vector of incoming edges that do bring value VAL.  It is assumed
   their number is known and equal to CALLER_COUNT.  */

static vec<cgraph_edge_p> 
gather_edges_for_value (struct ipcp_value *val, int caller_count)
{
  struct ipcp_value_source *src;
  vec<cgraph_edge_p> ret;

  ret.create (caller_count);
  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src))
	    ret.quick_push (cs);
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  return ret;
}

/* Construct a replacement map for a know VALUE for a formal parameter PARAM.
   Return it or NULL if for some reason it cannot be created.  */

static struct ipa_replace_map *
get_replacement_map (struct ipa_node_params *info, tree value, int parm_num)
{
  struct ipa_replace_map *replace_map;


  replace_map = ggc_alloc_ipa_replace_map ();
  if (dump_file)
    {
      fprintf (dump_file, "    replacing ");
      ipa_dump_param (dump_file, info, parm_num);
  
      fprintf (dump_file, " with const ");
      print_generic_expr (dump_file, value, 0);
      fprintf (dump_file, "\n");
    }
  replace_map->old_tree = NULL;
  replace_map->parm_num = parm_num;
  replace_map->new_tree = value;
  replace_map->replace_p = true;
  replace_map->ref_p = false;

  return replace_map;
}

/* Dump new profiling counts */

static void
dump_profile_updates (struct cgraph_node *orig_node,
		      struct cgraph_node *new_node)
{
  struct cgraph_edge *cs;

  fprintf (dump_file, "    setting count of the specialized node to "
	   HOST_WIDE_INT_PRINT_DEC "\n", (HOST_WIDE_INT) new_node->count);
  for (cs = new_node->callees; cs ; cs = cs->next_callee)
    fprintf (dump_file, "      edge to %s has count "
	     HOST_WIDE_INT_PRINT_DEC "\n",
	     cs->callee->name (), (HOST_WIDE_INT) cs->count);

  fprintf (dump_file, "    setting count of the original node to "
	   HOST_WIDE_INT_PRINT_DEC "\n", (HOST_WIDE_INT) orig_node->count);
  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    fprintf (dump_file, "      edge to %s is left with "
	     HOST_WIDE_INT_PRINT_DEC "\n",
	     cs->callee->name (), (HOST_WIDE_INT) cs->count);
}

/* After a specialized NEW_NODE version of ORIG_NODE has been created, update
   their profile information to reflect this.  */

static void
update_profiling_info (struct cgraph_node *orig_node,
		       struct cgraph_node *new_node)
{
  struct cgraph_edge *cs;
  struct caller_statistics stats;
  gcov_type new_sum, orig_sum;
  gcov_type remainder, orig_node_count = orig_node->count;

  if (orig_node_count == 0)
    return;

  init_caller_stats (&stats);
  cgraph_for_node_and_aliases (orig_node, gather_caller_stats, &stats, false);
  orig_sum = stats.count_sum;
  init_caller_stats (&stats);
  cgraph_for_node_and_aliases (new_node, gather_caller_stats, &stats, false);
  new_sum = stats.count_sum;

  if (orig_node_count < orig_sum + new_sum)
    {
      if (dump_file)
	fprintf (dump_file, "    Problem: node %s/%i has too low count "
		 HOST_WIDE_INT_PRINT_DEC " while the sum of incoming "
		 "counts is " HOST_WIDE_INT_PRINT_DEC "\n",
		 orig_node->name (), orig_node->order,
		 (HOST_WIDE_INT) orig_node_count,
		 (HOST_WIDE_INT) (orig_sum + new_sum));

      orig_node_count = (orig_sum + new_sum) * 12 / 10;
      if (dump_file)
	fprintf (dump_file, "      proceeding by pretending it was "
		 HOST_WIDE_INT_PRINT_DEC "\n",
		 (HOST_WIDE_INT) orig_node_count);
    }

  new_node->count = new_sum;
  remainder = orig_node_count - new_sum;
  orig_node->count = remainder;

  for (cs = new_node->callees; cs ; cs = cs->next_callee)
    if (cs->frequency)
      cs->count = apply_probability (cs->count,
                                     GCOV_COMPUTE_SCALE (new_sum,
                                                         orig_node_count));
    else
      cs->count = 0;

  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    cs->count = apply_probability (cs->count,
                                   GCOV_COMPUTE_SCALE (remainder,
                                                       orig_node_count));

  if (dump_file)
    dump_profile_updates (orig_node, new_node);
}

/* Update the respective profile of specialized NEW_NODE and the original
   ORIG_NODE after additional edges with cumulative count sum REDIRECTED_SUM
   have been redirected to the specialized version.  */

static void
update_specialized_profile (struct cgraph_node *new_node,
			    struct cgraph_node *orig_node,
			    gcov_type redirected_sum)
{
  struct cgraph_edge *cs;
  gcov_type new_node_count, orig_node_count = orig_node->count;

  if (dump_file)
    fprintf (dump_file, "    the sum of counts of redirected  edges is "
	     HOST_WIDE_INT_PRINT_DEC "\n", (HOST_WIDE_INT) redirected_sum);
  if (orig_node_count == 0)
    return;

  gcc_assert (orig_node_count >= redirected_sum);

  new_node_count = new_node->count;
  new_node->count += redirected_sum;
  orig_node->count -= redirected_sum;

  for (cs = new_node->callees; cs ; cs = cs->next_callee)
    if (cs->frequency)
      cs->count += apply_probability (cs->count,
                                      GCOV_COMPUTE_SCALE (redirected_sum,
                                                          new_node_count));
    else
      cs->count = 0;

  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    {
      gcov_type dec = apply_probability (cs->count,
                                         GCOV_COMPUTE_SCALE (redirected_sum,
                                                             orig_node_count));
      if (dec < cs->count)
	cs->count -= dec;
      else
	cs->count = 0;
    }

  if (dump_file)
    dump_profile_updates (orig_node, new_node);
}

/* Create a specialized version of NODE with known constants and types of
   parameters in KNOWN_VALS and redirect all edges in CALLERS to it.  */

static struct cgraph_node *
create_specialized_node (struct cgraph_node *node,
			 vec<tree> known_vals,
			 struct ipa_agg_replacement_value *aggvals,
			 vec<cgraph_edge_p> callers)
{
  struct ipa_node_params *new_info, *info = IPA_NODE_REF (node);
  vec<ipa_replace_map_p, va_gc> *replace_trees = NULL;
  struct ipa_agg_replacement_value *av;
  struct cgraph_node *new_node;
  int i, count = ipa_get_param_count (info);
  bitmap args_to_skip;

  gcc_assert (!info->ipcp_orig_node);

  if (node->local.can_change_signature)
    {
      args_to_skip = BITMAP_GGC_ALLOC ();
      for (i = 0; i < count; i++)
	{
	  tree t = known_vals[i];

	  if ((t && TREE_CODE (t) != TREE_BINFO)
	      || !ipa_is_param_used (info, i))
	    bitmap_set_bit (args_to_skip, i);
	}
    }
  else
    {
      args_to_skip = NULL;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "      cannot change function signature\n");
    }

  for (i = 0; i < count ; i++)
    {
      tree t = known_vals[i];
      if (t && TREE_CODE (t) != TREE_BINFO)
	{
	  struct ipa_replace_map *replace_map;

	  replace_map = get_replacement_map (info, t, i);
	  if (replace_map)
	    vec_safe_push (replace_trees, replace_map);
	}
    }

  new_node = cgraph_create_virtual_clone (node, callers, replace_trees,
					  args_to_skip, "constprop");
  ipa_set_node_agg_value_chain (new_node, aggvals);
  for (av = aggvals; av; av = av->next)
    ipa_maybe_record_reference (new_node, av->value,
				IPA_REF_ADDR, NULL);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "     the new node is %s/%i.\n",
	       new_node->name (), new_node->order);
      if (aggvals)
	ipa_dump_agg_replacement_values (dump_file, aggvals);
    }
  ipa_check_create_node_params ();
  update_profiling_info (node, new_node);
  new_info = IPA_NODE_REF (new_node);
  new_info->ipcp_orig_node = node;
  new_info->known_vals = known_vals;

  ipcp_discover_new_direct_edges (new_node, known_vals, aggvals);

  callers.release ();
  return new_node;
}

/* Given a NODE, and a subset of its CALLERS, try to populate blanks slots in
   KNOWN_VALS with constants and types that are also known for all of the
   CALLERS.  */

static void
find_more_scalar_values_for_callers_subset (struct cgraph_node *node,
					    vec<tree> known_vals,
					    vec<cgraph_edge_p> callers)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count ; i++)
    {
      struct cgraph_edge *cs;
      tree newval = NULL_TREE;
      int j;

      if (ipa_get_scalar_lat (info, i)->bottom || known_vals[i])
	continue;

      FOR_EACH_VEC_ELT (callers, j, cs)
	{
	  struct ipa_jump_func *jump_func;
	  tree t;

          if (i >= ipa_get_cs_argument_count (IPA_EDGE_REF (cs)))
            {
              newval = NULL_TREE;
              break;
            }
	  jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  t = ipa_value_from_jfunc (IPA_NODE_REF (cs->caller), jump_func);
	  if (!t
	      || (newval
		  && !values_equal_for_ipcp_p (t, newval)))
	    {
	      newval = NULL_TREE;
	      break;
	    }
	  else
	    newval = t;
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

	  known_vals[i] = newval;
	}
    }
}

/* Go through PLATS and create a vector of values consisting of values and
   offsets (minus OFFSET) of lattices that contain only a single value.  */

static vec<ipa_agg_jf_item>
copy_plats_to_inter (struct ipcp_param_lattices *plats, HOST_WIDE_INT offset)
{
  vec<ipa_agg_jf_item> res = vNULL;

  if (!plats->aggs || plats->aggs_contain_variable || plats->aggs_bottom)
    return vNULL;

  for (struct ipcp_agg_lattice *aglat = plats->aggs; aglat; aglat = aglat->next)
    if (ipa_lat_is_single_const (aglat))
      {
	struct ipa_agg_jf_item ti;
	ti.offset = aglat->offset - offset;
	ti.value = aglat->values->value;
	res.safe_push (ti);
      }
  return res;
}

/* Intersect all values in INTER with single value lattices in PLATS (while
   subtracting OFFSET).  */

static void
intersect_with_plats (struct ipcp_param_lattices *plats,
		      vec<ipa_agg_jf_item> *inter,
		      HOST_WIDE_INT offset)
{
  struct ipcp_agg_lattice *aglat;
  struct ipa_agg_jf_item *item;
  int k;

  if (!plats->aggs || plats->aggs_contain_variable || plats->aggs_bottom)
    {
      inter->release ();
      return;
    }

  aglat = plats->aggs;
  FOR_EACH_VEC_ELT (*inter, k, item)
    {
      bool found = false;
      if (!item->value)
	continue;
      while (aglat)
	{
	  if (aglat->offset - offset > item->offset)
	    break;
	  if (aglat->offset - offset == item->offset)
	    {
	      gcc_checking_assert (item->value);
	      if (values_equal_for_ipcp_p (item->value, aglat->values->value))
		found = true;
	      break;
	    }
	  aglat = aglat->next;
	}
      if (!found)
	item->value = NULL_TREE;
    }
}

/* Copy agggregate replacement values of NODE (which is an IPA-CP clone) to the
   vector result while subtracting OFFSET from the individual value offsets.  */

static vec<ipa_agg_jf_item>
agg_replacements_to_vector (struct cgraph_node *node, int index,
			    HOST_WIDE_INT offset)
{
  struct ipa_agg_replacement_value *av;
  vec<ipa_agg_jf_item> res = vNULL;

  for (av = ipa_get_agg_replacements_for_node (node); av; av = av->next)
    if (av->index == index
	&& (av->offset - offset) >= 0)
    {
      struct ipa_agg_jf_item item;
      gcc_checking_assert (av->value);
      item.offset = av->offset - offset;
      item.value = av->value;
      res.safe_push (item);
    }

  return res;
}

/* Intersect all values in INTER with those that we have already scheduled to
   be replaced in parameter number INDEX of NODE, which is an IPA-CP clone
   (while subtracting OFFSET).  */

static void
intersect_with_agg_replacements (struct cgraph_node *node, int index,
				 vec<ipa_agg_jf_item> *inter,
				 HOST_WIDE_INT offset)
{
  struct ipa_agg_replacement_value *srcvals;
  struct ipa_agg_jf_item *item;
  int i;

  srcvals = ipa_get_agg_replacements_for_node (node);
  if (!srcvals)
    {
      inter->release ();
      return;
    }

  FOR_EACH_VEC_ELT (*inter, i, item)
    {
      struct ipa_agg_replacement_value *av;
      bool found = false;
      if (!item->value)
	continue;
      for (av = srcvals; av; av = av->next)
	{
	  gcc_checking_assert (av->value);
	  if (av->index == index
	      && av->offset - offset == item->offset)
	    {
	      if (values_equal_for_ipcp_p (item->value, av->value))
		found = true;
	      break;
	    }
	}
      if (!found)
	item->value = NULL_TREE;
    }
}

/* Intersect values in INTER with aggregate values that come along edge CS to
   parameter number INDEX and return it.  If INTER does not actually exist yet,
   copy all incoming values to it.  If we determine we ended up with no values
   whatsoever, return a released vector.  */

static vec<ipa_agg_jf_item>
intersect_aggregates_with_edge (struct cgraph_edge *cs, int index,
				vec<ipa_agg_jf_item> inter)
{
  struct ipa_jump_func *jfunc;
  jfunc = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), index);
  if (jfunc->type == IPA_JF_PASS_THROUGH
      && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      int src_idx = ipa_get_jf_pass_through_formal_id (jfunc);

      if (caller_info->ipcp_orig_node)
	{
	  struct cgraph_node *orig_node = caller_info->ipcp_orig_node;
	  struct ipcp_param_lattices *orig_plats;
	  orig_plats = ipa_get_parm_lattices (IPA_NODE_REF (orig_node),
					      src_idx);
	  if (agg_pass_through_permissible_p (orig_plats, jfunc))
	    {
	      if (!inter.exists ())
		inter = agg_replacements_to_vector (cs->caller, src_idx, 0);
	      else
		intersect_with_agg_replacements (cs->caller, src_idx,
						 &inter, 0);
	    }
	}
      else
	{
	  struct ipcp_param_lattices *src_plats;
	  src_plats = ipa_get_parm_lattices (caller_info, src_idx);
	  if (agg_pass_through_permissible_p (src_plats, jfunc))
	    {
	      /* Currently we do not produce clobber aggregate jump
		 functions, adjust when we do.  */
	      gcc_checking_assert (!jfunc->agg.items);
	      if (!inter.exists ())
		inter = copy_plats_to_inter (src_plats, 0);
	      else
		intersect_with_plats (src_plats, &inter, 0);
	    }
	}
    }
  else if (jfunc->type == IPA_JF_ANCESTOR
	   && ipa_get_jf_ancestor_agg_preserved (jfunc))
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      int src_idx = ipa_get_jf_ancestor_formal_id (jfunc);
      struct ipcp_param_lattices *src_plats;
      HOST_WIDE_INT delta = ipa_get_jf_ancestor_offset (jfunc);

      if (caller_info->ipcp_orig_node)
	{
	  if (!inter.exists ())
	    inter = agg_replacements_to_vector (cs->caller, src_idx, delta);
	  else
	    intersect_with_agg_replacements (cs->caller, src_idx, &inter,
					     delta);
	}
      else
	{
	  src_plats = ipa_get_parm_lattices (caller_info, src_idx);;
	  /* Currently we do not produce clobber aggregate jump
	     functions, adjust when we do.  */
	  gcc_checking_assert (!src_plats->aggs || !jfunc->agg.items);
	  if (!inter.exists ())
	    inter = copy_plats_to_inter (src_plats, delta);
	  else
	    intersect_with_plats (src_plats, &inter, delta);
	}
    }
  else if (jfunc->agg.items)
    {
      struct ipa_agg_jf_item *item;
      int k;

      if (!inter.exists ())
	for (unsigned i = 0; i < jfunc->agg.items->length (); i++)
	  inter.safe_push ((*jfunc->agg.items)[i]);
      else
	FOR_EACH_VEC_ELT (inter, k, item)
	  {
	    int l = 0;
	    bool found = false;;

	    if (!item->value)
	      continue;

	    while ((unsigned) l < jfunc->agg.items->length ())
	      {
		struct ipa_agg_jf_item *ti;
		ti = &(*jfunc->agg.items)[l];
		if (ti->offset > item->offset)
		  break;
		if (ti->offset == item->offset)
		  {
		    gcc_checking_assert (ti->value);
		    if (values_equal_for_ipcp_p (item->value,
						 ti->value))
		      found = true;
		    break;
		  }
		l++;
	      }
	    if (!found)
	      item->value = NULL;
	  }
    }
  else
    {
      inter.release ();
      return vec<ipa_agg_jf_item>();
    }
  return inter;
}

/* Look at edges in CALLERS and collect all known aggregate values that arrive
   from all of them.  */

static struct ipa_agg_replacement_value *
find_aggregate_values_for_callers_subset (struct cgraph_node *node,
					  vec<cgraph_edge_p> callers)
{
  struct ipa_node_params *dest_info = IPA_NODE_REF (node);
  struct ipa_agg_replacement_value *res = NULL;
  struct cgraph_edge *cs;
  int i, j, count = ipa_get_param_count (dest_info);

  FOR_EACH_VEC_ELT (callers, j, cs)
    {
      int c = ipa_get_cs_argument_count (IPA_EDGE_REF (cs));
      if (c < count)
	count = c;
    }

  for (i = 0; i < count ; i++)
    {
      struct cgraph_edge *cs;
      vec<ipa_agg_jf_item> inter = vNULL;
      struct ipa_agg_jf_item *item;
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (dest_info, i);
      int j;

      /* Among other things, the following check should deal with all by_ref
	 mismatches.  */
      if (plats->aggs_bottom)
	continue;

      FOR_EACH_VEC_ELT (callers, j, cs)
	{
	  inter = intersect_aggregates_with_edge (cs, i, inter);

	  if (!inter.exists ())
	    goto next_param;
	}

      FOR_EACH_VEC_ELT (inter, j, item)
	{
	  struct ipa_agg_replacement_value *v;

	  if (!item->value)
	    continue;

	  v = ggc_alloc_ipa_agg_replacement_value ();
	  v->index = i;
	  v->offset = item->offset;
	  v->value = item->value;
	  v->by_ref = plats->aggs_by_ref;
	  v->next = res;
	  res = v;
	}

    next_param:
      if (inter.exists ())
	inter.release ();
    }
  return res;
}

/* Turn KNOWN_AGGS into a list of aggreate replacement values.  */

static struct ipa_agg_replacement_value *
known_aggs_to_agg_replacement_list (vec<ipa_agg_jump_function> known_aggs)
{
  struct ipa_agg_replacement_value *res = NULL;
  struct ipa_agg_jump_function *aggjf;
  struct ipa_agg_jf_item *item;
  int i, j;

  FOR_EACH_VEC_ELT (known_aggs, i, aggjf)
    FOR_EACH_VEC_SAFE_ELT (aggjf->items, j, item)
      {
	struct ipa_agg_replacement_value *v;
	v = ggc_alloc_ipa_agg_replacement_value ();
	v->index = i;
	v->offset = item->offset;
	v->value = item->value;
	v->by_ref = aggjf->by_ref;
	v->next = res;
	res = v;
      }
  return res;
}

/* Determine whether CS also brings all scalar values that the NODE is
   specialized for.  */

static bool
cgraph_edge_brings_all_scalars_for_node (struct cgraph_edge *cs,
					 struct cgraph_node *node)
{
  struct ipa_node_params *dest_info = IPA_NODE_REF (node);
  int count = ipa_get_param_count (dest_info);
  struct ipa_node_params *caller_info;
  struct ipa_edge_args *args;
  int i;

  caller_info = IPA_NODE_REF (cs->caller);
  args = IPA_EDGE_REF (cs);
  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *jump_func;
      tree val, t;

      val = dest_info->known_vals[i];
      if (!val)
	continue;

      if (i >= ipa_get_cs_argument_count (args))
	return false;
      jump_func = ipa_get_ith_jump_func (args, i);
      t = ipa_value_from_jfunc (caller_info, jump_func);
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
  struct ipa_node_params *orig_caller_info = IPA_NODE_REF (cs->caller);
  struct ipa_node_params *orig_node_info;
  struct ipa_agg_replacement_value *aggval;
  int i, ec, count;

  aggval = ipa_get_agg_replacements_for_node (node);
  if (!aggval)
    return true;

  count = ipa_get_param_count (IPA_NODE_REF (node));
  ec = ipa_get_cs_argument_count (IPA_EDGE_REF (cs));
  if (ec < count)
    for (struct ipa_agg_replacement_value *av = aggval; av; av = av->next)
      if (aggval->index >= ec)
	return false;

  orig_node_info = IPA_NODE_REF (IPA_NODE_REF (node)->ipcp_orig_node);
  if (orig_caller_info->ipcp_orig_node)
    orig_caller_info = IPA_NODE_REF (orig_caller_info->ipcp_orig_node);

  for (i = 0; i < count; i++)
    {
      static vec<ipa_agg_jf_item> values = vec<ipa_agg_jf_item>();
      struct ipcp_param_lattices *plats;
      bool interesting = false;
      for (struct ipa_agg_replacement_value *av = aggval; av; av = av->next)
	if (aggval->index == i)
	  {
	    interesting = true;
	    break;
	  }
      if (!interesting)
	continue;

      plats = ipa_get_parm_lattices (orig_node_info, aggval->index);
      if (plats->aggs_bottom)
	return false;

      values = intersect_aggregates_with_edge (cs, i, values);
      if (!values.exists ())
	return false;

      for (struct ipa_agg_replacement_value *av = aggval; av; av = av->next)
	if (aggval->index == i)
	  {
	    struct ipa_agg_jf_item *item;
	    int j;
	    bool found = false;
	    FOR_EACH_VEC_ELT (values, j, item)
	      if (item->value
		  && item->offset == av->offset
		  && values_equal_for_ipcp_p (item->value, av->value))
		{
		  found = true;
		  break;
		}
	    if (!found)
	      {
		values.release ();
		return false;
	      }
	  }
    }
  return true;
}

/* Given an original NODE and a VAL for which we have already created a
   specialized clone, look whether there are incoming edges that still lead
   into the old node but now also bring the requested value and also conform to
   all other criteria such that they can be redirected the the special node.
   This function can therefore redirect the final edge in a SCC.  */

static void
perhaps_add_new_callers (struct cgraph_node *node, struct ipcp_value *val)
{
  struct ipcp_value_source *src;
  gcov_type redirected_sum = 0;

  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  enum availability availability;
	  struct cgraph_node *dst = cgraph_function_node (cs->callee,
							  &availability);
	  if ((dst == node || IPA_NODE_REF (dst)->is_all_contexts_clone)
	      && availability > AVAIL_OVERWRITABLE
	      && cgraph_edge_brings_value_p (cs, src))
	    {
	      if (cgraph_edge_brings_all_scalars_for_node (cs, val->spec_node)
		  && cgraph_edge_brings_all_agg_vals_for_node (cs,
							       val->spec_node))
		{
		  if (dump_file)
		    fprintf (dump_file, " - adding an extra caller %s/%i"
			     " of %s/%i\n",
			     xstrdup (cs->caller->name ()),
			     cs->caller->order,
			     xstrdup (val->spec_node->name ()),
			     val->spec_node->order);

		  cgraph_redirect_edge_callee (cs, val->spec_node);
		  redirected_sum += cs->count;
		}
	    }
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  if (redirected_sum)
    update_specialized_profile (val->spec_node, node, redirected_sum);
}


/* Copy KNOWN_BINFOS to KNOWN_VALS.  */

static void
move_binfos_to_values (vec<tree> known_vals,
		       vec<tree> known_binfos)
{
  tree t;
  int i;

  for (i = 0; known_binfos.iterate (i, &t); i++)
    if (t)
      known_vals[i] = t;
}

/* Return true if there is a replacement equivalent to VALUE, INDEX and OFFSET
   among those in the AGGVALS list.  */

DEBUG_FUNCTION bool
ipcp_val_in_agg_replacements_p (struct ipa_agg_replacement_value *aggvals,
				int index, HOST_WIDE_INT offset, tree value)
{
  while (aggvals)
    {
      if (aggvals->index == index
	  && aggvals->offset == offset
	  && values_equal_for_ipcp_p (aggvals->value, value))
	return true;
      aggvals = aggvals->next;
    }
  return false;
}

/* Decide wheter to create a special version of NODE for value VAL of parameter
   at the given INDEX.  If OFFSET is -1, the value is for the parameter itself,
   otherwise it is stored at the given OFFSET of the parameter.  KNOWN_CSTS,
   KNOWN_BINFOS and KNOWN_AGGS describe the other already known values.  */

static bool
decide_about_value (struct cgraph_node *node, int index, HOST_WIDE_INT offset,
		    struct ipcp_value *val, vec<tree> known_csts,
		    vec<tree> known_binfos)
{
  struct ipa_agg_replacement_value *aggvals;
  int freq_sum, caller_count;
  gcov_type count_sum;
  vec<cgraph_edge_p> callers;
  vec<tree> kv;

  if (val->spec_node)
    {
      perhaps_add_new_callers (node, val);
      return false;
    }
  else if (val->local_size_cost + overall_size > max_new_size)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   Ignoring candidate value because "
		 "max_new_size would be reached with %li.\n",
		 val->local_size_cost + overall_size);
      return false;
    }
  else if (!get_info_about_necessary_edges (val, &freq_sum, &count_sum,
					    &caller_count))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, " - considering value ");
      print_ipcp_constant_value (dump_file, val->value);
      fprintf (dump_file, " for ");
      ipa_dump_param (dump_file, IPA_NODE_REF (node), index);
      if (offset != -1)
	fprintf (dump_file, ", offset: " HOST_WIDE_INT_PRINT_DEC, offset);
      fprintf (dump_file, " (caller_count: %i)\n", caller_count);
    }

  if (!good_cloning_opportunity_p (node, val->local_time_benefit,
				   freq_sum, count_sum,
				   val->local_size_cost)
      && !good_cloning_opportunity_p (node,
				      val->local_time_benefit
				      + val->prop_time_benefit,
				      freq_sum, count_sum,
				      val->local_size_cost
				      + val->prop_size_cost))
    return false;

  if (dump_file)
    fprintf (dump_file, "  Creating a specialized node of %s/%i.\n",
	     node->name (), node->order);

  callers = gather_edges_for_value (val, caller_count);
  kv = known_csts.copy ();
  move_binfos_to_values (kv, known_binfos);
  if (offset == -1)
    kv[index] = val->value;
  find_more_scalar_values_for_callers_subset (node, kv, callers);
  aggvals = find_aggregate_values_for_callers_subset (node, callers);
  gcc_checking_assert (offset == -1
		       || ipcp_val_in_agg_replacements_p (aggvals, index,
							  offset, val->value));
  val->spec_node = create_specialized_node (node, kv, aggvals, callers);
  overall_size += val->local_size_cost;

  /* TODO: If for some lattice there is only one other known value
     left, make a special node for it too. */

  return true;
}

/* Decide whether and what specialized clones of NODE should be created.  */

static bool
decide_whether_version_node (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);
  vec<tree> known_csts, known_binfos;
  vec<ipa_agg_jump_function> known_aggs = vNULL;
  bool ret = false;

  if (count == 0)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEvaluating opportunities for %s/%i.\n",
	     node->name (), node->order);

  gather_context_independent_values (info, &known_csts, &known_binfos,
				  info->do_clone_for_all_contexts ? &known_aggs
				  : NULL, NULL);

  for (i = 0; i < count ;i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipcp_lattice *lat = &plats->itself;
      struct ipcp_value *val;

      if (!lat->bottom
	  && !known_csts[i]
	  && !known_binfos[i])
	for (val = lat->values; val; val = val->next)
	  ret |= decide_about_value (node, i, -1, val, known_csts,
				     known_binfos);

      if (!plats->aggs_bottom)
	{
	  struct ipcp_agg_lattice *aglat;
	  struct ipcp_value *val;
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    if (!aglat->bottom && aglat->values
		/* If the following is false, the one value is in
		   known_aggs.  */
		&& (plats->aggs_contain_variable
		    || !ipa_lat_is_single_const (aglat)))
	      for (val = aglat->values; val; val = val->next)
		ret |= decide_about_value (node, i, aglat->offset, val,
					   known_csts, known_binfos);
	}
        info = IPA_NODE_REF (node);
    }

  if (info->do_clone_for_all_contexts)
    {
      struct cgraph_node *clone;
      vec<cgraph_edge_p> callers;

      if (dump_file)
	fprintf (dump_file, " - Creating a specialized node of %s/%i "
		 "for all known contexts.\n", node->name (),
		 node->order);

      callers = collect_callers_of_node (node);
      move_binfos_to_values (known_csts, known_binfos);
      clone = create_specialized_node (node, known_csts,
			       known_aggs_to_agg_replacement_list (known_aggs),
			       callers);
      info = IPA_NODE_REF (node);
      info->do_clone_for_all_contexts = false;
      IPA_NODE_REF (clone)->is_all_contexts_clone = true;
      for (i = 0; i < count ; i++)
	vec_free (known_aggs[i].items);
      known_aggs.release ();
      ret = true;
    }
  else
    known_csts.release ();

  known_binfos.release ();
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
	struct ipa_node_params *info;

	callee = cgraph_function_node (cs->callee, NULL);
	info = IPA_NODE_REF (callee);

	if (info->node_dead)
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
    if (cs->caller->thunk.thunk_p
	&& cgraph_for_node_and_aliases (cs->caller,
					has_undead_caller_from_outside_scc_p,
					NULL, true))
      return true;
    else if (!ipa_edge_within_scc (cs)
	     && !IPA_NODE_REF (cs->caller)->node_dead)
      return true;
  return false;
}


/* Identify nodes within the same SCC as NODE which are no longer needed
   because of new clones and will be removed as unreachable.  */

static void
identify_dead_nodes (struct cgraph_node *node)
{
  struct cgraph_node *v;
  for (v = node; v ; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    if (cgraph_will_be_removed_from_program_if_no_direct_calls (v)
	&& !cgraph_for_node_and_aliases (v,
					 has_undead_caller_from_outside_scc_p,
					 NULL, true))
      IPA_NODE_REF (v)->node_dead = 1;

  for (v = node; v ; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    if (!IPA_NODE_REF (v)->node_dead)
      spread_undeadness (v);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      for (v = node; v ; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
	if (IPA_NODE_REF (v)->node_dead)
	  fprintf (dump_file, "  Marking node as dead: %s/%i.\n",
		   v->name (), v->order);
    }
}

/* The decision stage.  Iterate over the topological order of call graph nodes
   TOPO and make specialized clones if deemed beneficial.  */

static void
ipcp_decision_stage (struct topo_info *topo)
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
	  for (v = node; v ; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
	    if (cgraph_function_with_gimple_body_p (v)
		&& ipcp_versionable_function_p (v))
	      iterate |= decide_whether_version_node (v);

	  change |= iterate;
	}
      if (change)
	identify_dead_nodes (node);
    }
}

/* The IPCP driver.  */

static unsigned int
ipcp_driver (void)
{
  struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
  struct cgraph_edge_hook_list *edge_removal_hook_holder;
  struct topo_info topo;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  grow_edge_clone_vectors ();
  edge_duplication_hook_holder =
    cgraph_add_edge_duplication_hook (&ipcp_edge_duplication_hook, NULL);
  edge_removal_hook_holder =
    cgraph_add_edge_removal_hook (&ipcp_edge_removal_hook, NULL);

  ipcp_values_pool = create_alloc_pool ("IPA-CP values",
					sizeof (struct ipcp_value), 32);
  ipcp_sources_pool = create_alloc_pool ("IPA-CP value sources",
					 sizeof (struct ipcp_value_source), 64);
  ipcp_agg_lattice_pool = create_alloc_pool ("IPA_CP aggregate lattices",
					     sizeof (struct ipcp_agg_lattice),
					     32);
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

  /* Free all IPCP structures.  */
  free_toporder_info (&topo);
  next_edge_clone.release ();
  cgraph_remove_edge_removal_hook (edge_removal_hook_holder);
  cgraph_remove_edge_duplication_hook (edge_duplication_hook_holder);
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
      {
	node->local.versionable
	  = tree_versionable_function_p (node->decl);
	ipa_analyze_node (node);
      }
}

/* Write ipcp summary for nodes in SET.  */

static void
ipcp_write_summary (void)
{
  ipa_prop_write_jump_functions ();
}

/* Read ipcp summary.  */

static void
ipcp_read_summary (void)
{
  ipa_prop_read_jump_functions ();
}

/* Gate for IPCP optimization.  */

static bool
cgraph_gate_cp (void)
{
  /* FIXME: We should remove the optimize check after we ensure we never run
     IPA passes when not optimizing.  */
  return flag_ipa_cp && optimize;
}

namespace {

const pass_data pass_data_ipa_cp =
{
  IPA_PASS, /* type */
  "cp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
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
		      ipcp_write_summary, /* write_summary */
		      ipcp_read_summary, /* read_summary */
		      ipa_prop_write_all_agg_replacement, /*
		      write_optimization_summary */
		      ipa_prop_read_all_agg_replacement, /*
		      read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      ipcp_transform_function, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate () { return cgraph_gate_cp (); }
  unsigned int execute () { return ipcp_driver (); }

}; // class pass_ipa_cp

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_cp (gcc::context *ctxt)
{
  return new pass_ipa_cp (ctxt);
}
