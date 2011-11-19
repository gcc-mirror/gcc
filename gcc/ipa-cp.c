/* Interprocedural constant propagation
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
   propagate cumulative information about these effects from dependant values
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
#include "target.h"
#include "gimple.h"
#include "cgraph.h"
#include "ipa-prop.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "tree-dump.h"
#include "tree-inline.h"
#include "fibheap.h"
#include "params.h"
#include "ipa-inline.h"
#include "ipa-utils.h"

struct ipcp_value;

/* Describes a particular source for an IPA-CP value.  */

struct ipcp_value_source
{
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

/* Allocation pools for values and their sources in ipa-cp.  */

alloc_pool ipcp_values_pool;
alloc_pool ipcp_sources_pool;

/* Lattice describing potential values of a formal parameter of a function and
   some of their other properties.  TOP is represented by a lattice with zero
   values and with contains_variable and bottom flags cleared.  BOTTOM is
   represented by a lattice with the bottom flag set.  In that case, values and
   contains_variable flag should be disregarded.  */

struct ipcp_lattice
{
  /* The list of known values and types in this lattice.  Note that values are
     not deallocated if a lattice is set to bottom because there may be value
     sources referencing them.  */
  struct ipcp_value *values;
  /* Number of known values and types in this lattice.  */
  int values_count;
  /* The lattice contains a variable component  (in addition to values).  */
  bool contains_variable;
  /* The value of the lattice is bottom (i.e. variable and unusable for any
     propagation).  */
  bool bottom;
  /* There is a virtual call based on this parameter.  */
  bool virt_call;
};

/* Maximal count found in program.  */

static gcov_type max_count;

/* Original overall size of the program.  */

static long overall_size, max_new_size;

/* Head of the linked list of topologically sorted values. */

static struct ipcp_value *values_topo;

/* Return the lattice corresponding to the Ith formal parameter of the function
   described by INFO.  */
static inline struct ipcp_lattice *
ipa_get_lattice (struct ipa_node_params *info, int i)
{
  gcc_assert (i >= 0 && i < ipa_get_param_count (info));
  gcc_checking_assert (!info->ipcp_orig_node);
  gcc_checking_assert (info->lattices);
  return &(info->lattices[i]);
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

/* Return true iff the CS is an edge within a strongly connected component as
   computed by ipa_reduced_postorder.  */

static inline bool
edge_within_scc (struct cgraph_edge *cs)
{
  struct ipa_dfs_info *caller_dfs = (struct ipa_dfs_info *) cs->caller->aux;
  struct ipa_dfs_info *callee_dfs;
  struct cgraph_node *callee = cgraph_function_node (cs->callee, NULL);

  callee_dfs = (struct ipa_dfs_info *) callee->aux;
  return (caller_dfs
	  && callee_dfs
	  && caller_dfs->scc_no == callee_dfs->scc_no);
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
      fprintf (f, "  Node: %s/%i:\n", cgraph_node_name (node), node->uid);
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipa_get_lattice (info, i);
	  struct ipcp_value *val;
	  bool prev = false;

	  fprintf (f, "    param [%d]: ", i);
	  if (lat->bottom)
	    {
	      fprintf (f, "BOTTOM\n");
	      continue;
	    }

	  if (!lat->values_count && !lat->contains_variable)
	    {
	      fprintf (f, "TOP\n");
	      continue;
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
		    fprintf (f, " %i(%i)", s->cs->caller->uid,s->cs->frequency);
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

  if (reason && dump_file && !node->alias && !node->thunk.thunk_p)
    fprintf (dump_file, "Function %s/%i is not versionable, reason: %s.\n",
	     cgraph_node_name (node), node->uid, reason);

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
 	         cgraph_node_name (node));
      return false;
    }

  if (!optimize_function_for_speed_p (DECL_STRUCT_FUNCTION (node->decl)))
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; "
		 "optimizing it for size.\n",
 	         cgraph_node_name (node));
      return false;
    }

  init_caller_stats (&stats);
  cgraph_for_node_and_aliases (node, gather_caller_stats, &stats, false);

  if (inline_summary (node)->self_size < stats.n_calls)
    {
      if (dump_file)
        fprintf (dump_file, "Considering %s for cloning; code might shrink.\n",
 	         cgraph_node_name (node));
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
		     cgraph_node_name (node));
	  return true;
        }
    }
  if (!stats.n_hot_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; no hot calls.\n",
		 cgraph_node_name (node));
      return false;
    }
  if (dump_file)
    fprintf (dump_file, "Considering %s for cloning.\n",
	     cgraph_node_name (node));
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
	  struct ipcp_lattice *lat = ipa_get_lattice (info, i);
	  if (disable)
	    set_lattice_to_bottom (lat);
	  else
	    set_lattice_contains_variable (lat);
	}
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && node->alias && node->thunk.thunk_p)
	fprintf (dump_file, "Marking all lattices of %s/%i as %s\n",
		 cgraph_node_name (node), node->uid,
		 disable ? "BOTTOM" : "VARIABLE");
    }

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    if (ie->indirect_info->polymorphic)
      {
	gcc_checking_assert (ie->indirect_info->param_index >= 0);
	ipa_get_lattice (info, ie->indirect_info->param_index)->virt_call = 1;
      }
}

/* Return the result of a (possibly arithmetic) pass through jump function
   JFUNC on the constant value INPUT.  Return NULL_TREE if that cannot be
   determined or itself is considered an interprocedural invariant.  */

static tree
ipa_get_jf_pass_through_result (struct ipa_jump_func *jfunc, tree input)
{
  tree restype, res;

  gcc_checking_assert (is_gimple_ip_invariant (input));
  if (jfunc->value.pass_through.operation == NOP_EXPR)
    return input;

  if (TREE_CODE_CLASS (jfunc->value.pass_through.operation)
      == tcc_comparison)
    restype = boolean_type_node;
  else
    restype = TREE_TYPE (input);
  res = fold_binary (jfunc->value.pass_through.operation, restype,
		     input, jfunc->value.pass_through.operand);

  if (res && !is_gimple_ip_invariant (res))
    return NULL_TREE;

  return res;
}

/* Return the result of an ancestor jump function JFUNC on the constant value
   INPUT.  Return NULL_TREE if that cannot be determined.  */

static tree
ipa_get_jf_ancestor_result (struct ipa_jump_func *jfunc, tree input)
{
  if (TREE_CODE (input) == ADDR_EXPR)
    {
      tree t = TREE_OPERAND (input, 0);
      t = build_ref_for_offset (EXPR_LOCATION (t), t,
				jfunc->value.ancestor.offset,
				jfunc->value.ancestor.type, NULL, false);
      return build_fold_addr_expr (t);
    }
  else
    return NULL_TREE;
}

/* Extract the acual BINFO being described by JFUNC which must be a known type
   jump function.  */

static tree
ipa_value_from_known_type_jfunc (struct ipa_jump_func *jfunc)
{
  tree base_binfo = TYPE_BINFO (jfunc->value.known_type.base_type);
  if (!base_binfo)
    return NULL_TREE;
  return get_binfo_at_offset (base_binfo,
			      jfunc->value.known_type.offset,
			      jfunc->value.known_type.component_type);
}

/* Determine whether JFUNC evaluates to a known value (that is either a
   constant or a binfo) and if so, return it.  Otherwise return NULL. INFO
   describes the caller node so that pass-through jump functions can be
   evaluated.  */

tree
ipa_value_from_jfunc (struct ipa_node_params *info, struct ipa_jump_func *jfunc)
{
  if (jfunc->type == IPA_JF_CONST)
    return jfunc->value.constant;
  else if (jfunc->type == IPA_JF_KNOWN_TYPE)
    return ipa_value_from_known_type_jfunc (jfunc);
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      tree input;
      int idx;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	idx = jfunc->value.pass_through.formal_id;
      else
	idx = jfunc->value.ancestor.formal_id;

      if (info->ipcp_orig_node)
	input = VEC_index (tree, info->known_vals, idx);
      else
	{
	  struct ipcp_lattice *lat;

	  if (!info->lattices)
	    {
	      gcc_checking_assert (!flag_ipa_cp);
	      return NULL_TREE;
	    }
	  lat = ipa_get_lattice (info, idx);
	  if (!ipa_lat_is_single_const (lat))
	    return NULL_TREE;
	  input = lat->values->value;
	}

      if (!input)
	return NULL_TREE;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	{
	  if (jfunc->value.pass_through.operation == NOP_EXPR)
	    return input;
	  else if (TREE_CODE (input) == TREE_BINFO)
	    return NULL_TREE;
	  else
	    return ipa_get_jf_pass_through_result (jfunc, input);
	}
      else
	{
	  if (TREE_CODE (input) == TREE_BINFO)
	    return get_binfo_at_offset (input, jfunc->value.ancestor.offset,
					jfunc->value.ancestor.type);
	  else
	    return ipa_get_jf_ancestor_result (jfunc, input);
	}
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
	  struct ipcp_lattice *lat = ipa_get_lattice (info, i);

	  if (!lat->bottom
	      && !lat->contains_variable
	      && lat->values_count == 0)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "\nIPA lattices after constant "
			   "propagation:\n");
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
   a caller value SRC_VAL of a caller parameter described by SRC_INDEX.  */

static void
add_value_source (struct ipcp_value *val, struct cgraph_edge *cs,
		  struct ipcp_value *src_val, int src_idx)
{
  struct ipcp_value_source *src;

  src = (struct ipcp_value_source *) pool_alloc (ipcp_sources_pool);
  src->cs = cs;
  src->val = src_val;
  src->index = src_idx;

  src->next = val->sources;
  val->sources = src;
}


/* Try to add NEWVAL to LAT, potentially creating a new struct ipcp_value for
   it.  CS, SRC_VAL and SRC_INDEX are meant for add_value_source and have the
   same meaning.  */

static bool
add_value_to_lattice (struct ipcp_lattice *lat, tree newval,
		      struct cgraph_edge *cs, struct ipcp_value *src_val,
		      int src_idx)
{
  struct ipcp_value *val;

  if (lat->bottom)
    return false;


  for (val = lat->values; val; val = val->next)
    if (values_equal_for_ipcp_p (val->value, newval))
      {
	if (edge_within_scc (cs))
	  {
	    struct ipcp_value_source *s;
	    for (s = val->sources; s ; s = s->next)
	      if (s->cs == cs)
		break;
	    if (s)
	      return false;
	  }

	add_value_source (val, cs, src_val, src_idx);
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

  add_value_source (val, cs, src_val, src_idx);
  val->value = newval;
  val->next = lat->values;
  lat->values = val;
  return true;
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

  if (jfunc->value.pass_through.operation == NOP_EXPR)
    for (src_val = src_lat->values; src_val; src_val = src_val->next)
      ret |= add_value_to_lattice (dest_lat, src_val->value, cs,
				   src_val, src_idx);
  /* Do not create new values when propagating within an SCC because if there
     arithmetic functions with circular dependencies, there is infinite number
     of them and we would just make lattices bottom.  */
  else if (edge_within_scc (cs))
    ret = set_lattice_contains_variable (dest_lat);
  else
    for (src_val = src_lat->values; src_val; src_val = src_val->next)
      {
	tree cstval = src_val->value;

	if (TREE_CODE (cstval) == TREE_BINFO)
	  {
	    ret |= set_lattice_contains_variable (dest_lat);
	    continue;
	  }
	cstval = ipa_get_jf_pass_through_result (jfunc, cstval);

	if (cstval)
	  ret |= add_value_to_lattice (dest_lat, cstval, cs, src_val, src_idx);
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

  if (edge_within_scc (cs))
    return set_lattice_contains_variable (dest_lat);

  for (src_val = src_lat->values; src_val; src_val = src_val->next)
    {
      tree t = src_val->value;

      if (TREE_CODE (t) == TREE_BINFO)
	t = get_binfo_at_offset (t, jfunc->value.ancestor.offset,
				 jfunc->value.ancestor.type);
      else
	t = ipa_get_jf_ancestor_result (jfunc, t);

      if (t)
	ret |= add_value_to_lattice (dest_lat, t, cs, src_val, src_idx);
      else
	ret |= set_lattice_contains_variable (dest_lat);
    }

  return ret;
}

/* Propagate values across jump function JFUNC that is associated with edge CS
   and put the values into DEST_LAT.  */

static bool
propagate_accross_jump_function (struct cgraph_edge *cs,
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
	  val = ipa_value_from_known_type_jfunc (jfunc);
	  if (!val)
	    return set_lattice_contains_variable (dest_lat);
	}
      else
	val = jfunc->value.constant;
      return add_value_to_lattice (dest_lat, val, cs, NULL, 0);
    }
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      struct ipcp_lattice *src_lat;
      int src_idx;
      bool ret;

      if (jfunc->type == IPA_JF_PASS_THROUGH)
	src_idx = jfunc->value.pass_through.formal_id;
      else
	src_idx = jfunc->value.ancestor.formal_id;

      src_lat = ipa_get_lattice (caller_info, src_idx);
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
  if (!callee->analyzed)
    return false;
  gcc_checking_assert (cgraph_function_with_gimple_body_p (callee));
  callee_info = IPA_NODE_REF (callee);

  args = IPA_EDGE_REF (cs);
  args_count = ipa_get_cs_argument_count (args);
  parms_count = ipa_get_param_count (callee_info);

  /* If this call goes through a thunk we must not propagate to the first (0th)
     parameter.  However, we might need to uncover a thunk from below a series
     of aliases first.  */
  alias_or_thunk = cs->callee;
  while (alias_or_thunk->alias)
    alias_or_thunk = cgraph_alias_aliased_node (alias_or_thunk);
  if (alias_or_thunk->thunk.thunk_p)
    {
      ret |= set_lattice_contains_variable (ipa_get_lattice (callee_info, 0));
      i = 1;
    }
  else
    i = 0;

  for (; (i < args_count) && (i < parms_count); i++)
    {
      struct ipa_jump_func *jump_func = ipa_get_ith_jump_func (args, i);
      struct ipcp_lattice *dest_lat = ipa_get_lattice (callee_info, i);

      if (availability == AVAIL_OVERWRITABLE)
	ret |= set_lattice_contains_variable (dest_lat);
      else
	ret |= propagate_accross_jump_function (cs, jump_func, dest_lat);
    }
  for (; i < parms_count; i++)
    ret |= set_lattice_contains_variable (ipa_get_lattice (callee_info, i));

  return ret;
}

/* If an indirect edge IE can be turned into a direct one based on KNOWN_VALS
   (which can contain both constants and binfos) or KNOWN_BINFOS (which can be
   NULL) return the destination.  */

tree
ipa_get_indirect_edge_target (struct cgraph_edge *ie,
			      VEC (tree, heap) *known_vals,
			      VEC (tree, heap) *known_binfos)
{
  int param_index = ie->indirect_info->param_index;
  HOST_WIDE_INT token, anc_offset;
  tree otr_type;
  tree t;

  if (param_index == -1)
    return NULL_TREE;

  if (!ie->indirect_info->polymorphic)
    {
      tree t = VEC_index (tree, known_vals, param_index);
      if (t &&
	  TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL)
	return TREE_OPERAND (t, 0);
      else
	return NULL_TREE;
    }

  token = ie->indirect_info->otr_token;
  anc_offset = ie->indirect_info->anc_offset;
  otr_type = ie->indirect_info->otr_type;

  t = VEC_index (tree, known_vals, param_index);
  if (!t && known_binfos)
    t = VEC_index (tree, known_binfos, param_index);
  if (!t)
    return NULL_TREE;

  if (TREE_CODE (t) != TREE_BINFO)
    {
      tree binfo;
      binfo = gimple_extract_devirt_binfo_from_cst (t);
      if (!binfo)
	return NULL_TREE;
      binfo = get_binfo_at_offset (binfo, anc_offset, otr_type);
      if (!binfo)
	return NULL_TREE;
      return gimple_get_virt_method_for_binfo (token, binfo);
    }
  else
    {
      tree binfo;

      binfo = get_binfo_at_offset (t, anc_offset, otr_type);
      if (!binfo)
	return NULL_TREE;
      return gimple_get_virt_method_for_binfo (token, binfo);
    }
}

/* Calculate devirtualization time bonus for NODE, assuming we know KNOWN_CSTS
   and KNOWN_BINFOS.  */

static int
devirtualization_time_bonus (struct cgraph_node *node,
			     VEC (tree, heap) *known_csts,
			     VEC (tree, heap) *known_binfos)
{
  struct cgraph_edge *ie;
  int res = 0;

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    {
      struct cgraph_node *callee;
      struct inline_summary *isummary;
      tree target;

      target = ipa_get_indirect_edge_target (ie, known_csts, known_binfos);
      if (!target)
	continue;

      /* Only bare minimum benefit for clearly un-inlineable targets.  */
      res += 1;
      callee = cgraph_get_node (target);
      if (!callee || !callee->analyzed)
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

  gcc_checking_assert (size_cost >= 0);

  /* FIXME:  These decisions need tuning.  */
  if (max_count)
    {
      int evaluation, factor = (count_sum * 1000) / max_count;

      evaluation = (time_benefit * factor) / size_cost;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		 "size: %i, count_sum: " HOST_WIDE_INT_PRINT_DEC
		 ") -> evaluation: %i, threshold: %i\n",
		 time_benefit, size_cost, (HOST_WIDE_INT) count_sum,
		 evaluation, 500);

      return evaluation >= PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD);
    }
  else
    {
      int evaluation = (time_benefit * freq_sum) / size_cost;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		 "size: %i, freq_sum: %i) -> evaluation: %i, threshold: %i\n",
		 time_benefit, size_cost, freq_sum, evaluation,
		 CGRAPH_FREQ_BASE /2);

      return evaluation >= PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD);
    }
}


/* Allocate KNOWN_CSTS and KNOWN_BINFOS and populate them with values of
   parameters that are known independent of the context.  INFO describes the
   function.  If REMOVABLE_PARAMS_COST is non-NULL, the movement cost of all
   removable parameters will be stored in it.  */

static bool
gather_context_independent_values (struct ipa_node_params *info,
				   VEC (tree, heap) **known_csts,
				   VEC (tree, heap) **known_binfos,
				   int *removable_params_cost)
{
  int i, count = ipa_get_param_count (info);
  bool ret = false;

  *known_csts = NULL;
  *known_binfos = NULL;
  VEC_safe_grow_cleared (tree, heap, *known_csts, count);
  VEC_safe_grow_cleared (tree, heap, *known_binfos, count);

  if (removable_params_cost)
    *removable_params_cost = 0;

  for (i = 0; i < count ; i++)
    {
      struct ipcp_lattice *lat = ipa_get_lattice (info, i);

      if (ipa_lat_is_single_const (lat))
	{
	  struct ipcp_value *val = lat->values;
	  if (TREE_CODE (val->value) != TREE_BINFO)
	    {
	      VEC_replace (tree, *known_csts, i, val->value);
	      if (removable_params_cost)
		*removable_params_cost
		  += estimate_move_cost (TREE_TYPE (val->value));
	      ret = true;
	    }
	  else if (lat->virt_call)
	    {
	      VEC_replace (tree, *known_binfos, i, val->value);
	      ret = true;
	    }
	  else if (removable_params_cost
		   && !ipa_is_param_used (info, i))
	    *removable_params_cost
	      += estimate_move_cost (TREE_TYPE (ipa_get_param (info, i)));
	}
      else if (removable_params_cost
	       && !ipa_is_param_used (info, i))
	*removable_params_cost
	  +=  estimate_move_cost (TREE_TYPE (ipa_get_param (info, i)));
    }

  return ret;
}

/* Iterate over known values of parameters of NODE and estimate the local
   effects in terms of time and size they have.  */

static void
estimate_local_effects (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);
  VEC (tree, heap) *known_csts, *known_binfos;
  bool always_const;
  int base_time = inline_summary (node)->time;
  int removable_params_cost;

  if (!count || !ipcp_versionable_function_p (node))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEstimating effects for %s/%i, base_time: %i.\n",
	     cgraph_node_name (node), node->uid, base_time);

  always_const = gather_context_independent_values (info, &known_csts,
						    &known_binfos,
						    &removable_params_cost);
  if (always_const)
    {
      struct caller_statistics stats;
      int time, size;

      init_caller_stats (&stats);
      cgraph_for_node_and_aliases (node, gather_caller_stats, &stats, false);
      estimate_ipcp_clone_size_and_time (node, known_csts, known_binfos,
					 &size, &time);
      time -= devirtualization_time_bonus (node, known_csts, known_binfos);
      time -= removable_params_cost;
      size -= stats.n_calls * removable_params_cost;

      if (dump_file)
	fprintf (dump_file, " - context independent values, size: %i, "
		 "time_benefit: %i\n", size, base_time - time);

      if (size <= 0
	  || cgraph_will_be_removed_from_program_if_no_direct_calls (node))
	{
	  info->clone_for_all_contexts = true;
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
	      info->clone_for_all_contexts = true;
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
      struct ipcp_lattice *lat = ipa_get_lattice (info, i);
      struct ipcp_value *val;
      int emc;

      if (lat->bottom
	  || !lat->values
	  || VEC_index (tree, known_csts, i)
	  || VEC_index (tree, known_binfos, i))
	continue;

      for (val = lat->values; val; val = val->next)
	{
	  int time, size, time_benefit;

	  if (TREE_CODE (val->value) != TREE_BINFO)
	    {
	      VEC_replace (tree, known_csts, i, val->value);
	      VEC_replace (tree, known_binfos, i, NULL_TREE);
	      emc = estimate_move_cost (TREE_TYPE (val->value));
	    }
	  else if (lat->virt_call)
	    {
	      VEC_replace (tree, known_csts, i, NULL_TREE);
	      VEC_replace (tree, known_binfos, i, val->value);
	      emc = 0;
	    }
	  else
	    continue;

	  estimate_ipcp_clone_size_and_time (node, known_csts, known_binfos,
					     &size, &time);
	  time_benefit = base_time - time
	    + devirtualization_time_bonus (node, known_csts, known_binfos)
	    + removable_params_cost + emc;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for value ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for parameter ");
	      print_generic_expr (dump_file, ipa_get_param (info, i), 0);
	      fprintf (dump_file, ": time_benefit: %i, size: %i\n",
		       time_benefit, size);
	    }

	  val->local_time_benefit = time_benefit;
	  val->local_size_cost = size;
	}
    }

  VEC_free (tree, heap, known_csts);
  VEC_free (tree, heap, known_binfos);
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
      struct ipcp_lattice *lat = ipa_get_lattice (info, i);
      struct ipcp_value *val;

      if (lat->bottom || !lat->values)
	continue;
      for (val = lat->values; val; val = val->next)
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
      struct cgraph_node *v, *node = topo->order[i];
      struct ipa_dfs_info *node_dfs_info;

      if (!cgraph_function_with_gimple_body_p (node))
	continue;

      node_dfs_info = (struct ipa_dfs_info *) node->aux;
      /* First, iteratively propagate within the strongly connected component
	 until all lattices stabilize.  */
      v = node_dfs_info->next_cycle;
      while (v)
	{
	  push_node_to_stack (topo, v);
	  v = ((struct ipa_dfs_info *) v->aux)->next_cycle;
	}

      v = node;
      while (v)
	{
	  struct cgraph_edge *cs;

	  for (cs = v->callees; cs; cs = cs->next_callee)
	    if (edge_within_scc (cs)
		&& propagate_constants_accross_call (cs))
	      push_node_to_stack (topo, cs->callee);
	  v = pop_node_from_stack (topo);
	}

      /* Afterwards, propagate along edges leading out of the SCC, calculates
	 the local effects of the discovered constants and all valid values to
	 their topological sort.  */
      v = node;
      while (v)
	{
	  struct cgraph_edge *cs;

	  estimate_local_effects (v);
	  add_all_node_vals_to_toposort (v);
	  for (cs = v->callees; cs; cs = cs->next_callee)
	    if (!edge_within_scc (cs))
	      propagate_constants_accross_call (cs);

	  v = ((struct ipa_dfs_info *) v->aux)->next_cycle;
	}
    }
}

/* Propagate the estimated effects of individual values along the topological
   from the dependant values to those they depend on.  */

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
	  time += val->local_time_benefit + val->prop_time_benefit;
	  size += val->local_size_cost + val->prop_size_cost;
	}

      for (val = base; val; val = val->scc_next)
	for (src = val->sources; src; src = src->next)
	  if (src->val
	      && cgraph_maybe_hot_edge_p (src->cs))
	    {
	      src->val->prop_time_benefit += time;
	      src->val->prop_size_cost += size;
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
	info->lattices = XCNEWVEC (struct ipcp_lattice,
				   ipa_get_param_count (info));
	initialize_node_lattices (node);
      }
    if (node->count > max_count)
      max_count = node->count;
    overall_size += inline_summary (node)->self_size;
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
				VEC (tree, heap) *known_vals)
{
  struct cgraph_edge *ie, *next_ie;

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      tree target;

      next_ie = ie->next_callee;
      target = ipa_get_indirect_edge_target (ie, known_vals, NULL);
      if (target)
	ipa_make_edge_direct_to_target (ie, target);
    }
}

/* Vector of pointers which for linked lists of clones of an original crgaph
   edge. */

static VEC (cgraph_edge_p, heap) *next_edge_clone;

static inline void
grow_next_edge_clone_vector (void)
{
  if (VEC_length (cgraph_edge_p, next_edge_clone)
      <=  (unsigned) cgraph_edge_max_uid)
    VEC_safe_grow_cleared (cgraph_edge_p, heap, next_edge_clone,
			   cgraph_edge_max_uid + 1);
}

/* Edge duplication hook to grow the appropriate linked list in
   next_edge_clone. */

static void
ipcp_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			    __attribute__((unused)) void *data)
{
  grow_next_edge_clone_vector ();
  VEC_replace (cgraph_edge_p, next_edge_clone, dst->uid,
	       VEC_index (cgraph_edge_p, next_edge_clone, src->uid));
  VEC_replace (cgraph_edge_p, next_edge_clone, src->uid, dst);
}

/* Get the next clone in the linked list of clones of an edge.  */

static inline struct cgraph_edge *
get_next_cgraph_edge_clone (struct cgraph_edge *cs)
{
  return VEC_index (cgraph_edge_p, next_edge_clone, cs->uid);
}

/* Return true if edge CS does bring about the value described by SRC.  */

static bool
cgraph_edge_brings_value_p (struct cgraph_edge *cs,
			    struct ipcp_value_source *src)
{
  struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);

  if (IPA_NODE_REF (cs->callee)->ipcp_orig_node
      || caller_info->node_dead)
    return false;
  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    {
      tree t = VEC_index (tree, caller_info->known_vals, src->index);
      return (t != NULL_TREE
	      && values_equal_for_ipcp_p (src->val->value, t));
    }
  else
    {
      struct ipcp_lattice *lat = ipa_get_lattice (caller_info, src->index);
      if (ipa_lat_is_single_const (lat)
	  && values_equal_for_ipcp_p (src->val->value, lat->values->value))
	return true;
      else
	return false;
    }
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

static VEC (cgraph_edge_p,heap) *
gather_edges_for_value (struct ipcp_value *val, int caller_count)
{
  struct ipcp_value_source *src;
  VEC (cgraph_edge_p,heap) *ret;

  ret = VEC_alloc (cgraph_edge_p, heap, caller_count);
  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  if (cgraph_edge_brings_value_p (cs, src))
	    VEC_quick_push (cgraph_edge_p, ret, cs);
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  return ret;
}

/* Construct a replacement map for a know VALUE for a formal parameter PARAM.
   Return it or NULL if for some reason it cannot be created.  */

static struct ipa_replace_map *
get_replacement_map (tree value, tree parm)
{
  tree req_type = TREE_TYPE (parm);
  struct ipa_replace_map *replace_map;

  if (!useless_type_conversion_p (req_type, TREE_TYPE (value)))
    {
      if (fold_convertible_p (req_type, value))
	value = fold_build1 (NOP_EXPR, req_type, value);
      else if (TYPE_SIZE (req_type) == TYPE_SIZE (TREE_TYPE (value)))
	value = fold_build1 (VIEW_CONVERT_EXPR, req_type, value);
      else
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "    const ");
	      print_generic_expr (dump_file, value, 0);
	      fprintf (dump_file, "  can't be converted to param ");
	      print_generic_expr (dump_file, parm, 0);
	      fprintf (dump_file, "\n");
	    }
	  return NULL;
	}
    }

  replace_map = ggc_alloc_ipa_replace_map ();
  if (dump_file)
    {
      fprintf (dump_file, "    replacing param ");
      print_generic_expr (dump_file, parm, 0);
      fprintf (dump_file, " with const ");
      print_generic_expr (dump_file, value, 0);
      fprintf (dump_file, "\n");
    }
  replace_map->old_tree = parm;
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
	     cgraph_node_name (cs->callee), (HOST_WIDE_INT) cs->count);

  fprintf (dump_file, "    setting count of the original node to "
	   HOST_WIDE_INT_PRINT_DEC "\n", (HOST_WIDE_INT) orig_node->count);
  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    fprintf (dump_file, "      edge to %s is left with "
	     HOST_WIDE_INT_PRINT_DEC "\n",
	     cgraph_node_name (cs->callee), (HOST_WIDE_INT) cs->count);
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
		 cgraph_node_name (orig_node), orig_node->uid,
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
      cs->count = cs->count * (new_sum * REG_BR_PROB_BASE
			       / orig_node_count) / REG_BR_PROB_BASE;
    else
      cs->count = 0;

  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    cs->count = cs->count * (remainder * REG_BR_PROB_BASE
			     / orig_node_count) / REG_BR_PROB_BASE;

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
      cs->count += cs->count * redirected_sum / new_node_count;
    else
      cs->count = 0;

  for (cs = orig_node->callees; cs ; cs = cs->next_callee)
    {
      gcov_type dec = cs->count * (redirected_sum * REG_BR_PROB_BASE
				   / orig_node_count) / REG_BR_PROB_BASE;
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
			 VEC (tree, heap) *known_vals,
			 VEC (cgraph_edge_p,heap) *callers)
{
  struct ipa_node_params *new_info, *info = IPA_NODE_REF (node);
  VEC (ipa_replace_map_p,gc)* replace_trees = NULL;
  struct cgraph_node *new_node;
  int i, count = ipa_get_param_count (info);
  bitmap args_to_skip;

  gcc_assert (!info->ipcp_orig_node);

  if (node->local.can_change_signature)
    {
      args_to_skip = BITMAP_GGC_ALLOC ();
      for (i = 0; i < count; i++)
	{
	  tree t = VEC_index (tree, known_vals, i);

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
      tree t = VEC_index (tree, known_vals, i);
      if (t && TREE_CODE (t) != TREE_BINFO)
	{
	  struct ipa_replace_map *replace_map;

	  replace_map = get_replacement_map (t, ipa_get_param (info, i));
	  if (replace_map)
	    VEC_safe_push (ipa_replace_map_p, gc, replace_trees, replace_map);
	}
    }

  new_node = cgraph_create_virtual_clone (node, callers, replace_trees,
					  args_to_skip, "constprop");
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "     the new node is %s/%i.\n",
	     cgraph_node_name (new_node), new_node->uid);
  gcc_checking_assert (ipa_node_params_vector
		       && (VEC_length (ipa_node_params_t,
				       ipa_node_params_vector)
			   > (unsigned) cgraph_max_uid));
  update_profiling_info (node, new_node);
  new_info = IPA_NODE_REF (new_node);
  new_info->ipcp_orig_node = node;
  new_info->known_vals = known_vals;

  ipcp_discover_new_direct_edges (new_node, known_vals);

  VEC_free (cgraph_edge_p, heap, callers);
  return new_node;
}

/* Given a NODE, and a subset of its CALLERS, try to populate blanks slots in
   KNOWN_VALS with constants and types that are also known for all of the
   CALLERS.  */

static void
find_more_values_for_callers_subset (struct cgraph_node *node,
				     VEC (tree, heap) *known_vals,
				     VEC (cgraph_edge_p,heap) *callers)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count ; i++)
    {
      struct cgraph_edge *cs;
      tree newval = NULL_TREE;
      int j;

      if (ipa_get_lattice (info, i)->bottom
	  || VEC_index (tree, known_vals, i))
	continue;

      FOR_EACH_VEC_ELT (cgraph_edge_p, callers, j, cs)
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
	      fprintf (dump_file, "    adding an extra known value ");
	      print_ipcp_constant_value (dump_file, newval);
	      fprintf (dump_file, " for parameter ");
	      print_generic_expr (dump_file, ipa_get_param (info, i), 0);
	      fprintf (dump_file, "\n");
	    }

	  VEC_replace (tree, known_vals, i, newval);
	}
    }
}

/* Given an original NODE and a VAL for which we have already created a
   specialized clone, look whether there are incoming edges that still lead
   into the old node but now also bring the requested value and also conform to
   all other criteria such that they can be redirected the the special node.
   This function can therefore redirect the final edge in a SCC.  */

static void
perhaps_add_new_callers (struct cgraph_node *node, struct ipcp_value *val)
{
  struct ipa_node_params *dest_info = IPA_NODE_REF (val->spec_node);
  struct ipcp_value_source *src;
  int count = ipa_get_param_count (dest_info);
  gcov_type redirected_sum = 0;

  for (src = val->sources; src; src = src->next)
    {
      struct cgraph_edge *cs = src->cs;
      while (cs)
	{
	  enum availability availability;
	  bool insufficient = false;

	  if (cgraph_function_node (cs->callee, &availability) == node
	      && availability > AVAIL_OVERWRITABLE
	      && cgraph_edge_brings_value_p (cs, src))
	    {
	      struct ipa_node_params *caller_info;
	      struct ipa_edge_args *args;
	      int i;

	      caller_info = IPA_NODE_REF (cs->caller);
	      args = IPA_EDGE_REF (cs);
	      for (i = 0; i < count; i++)
		{
		  struct ipa_jump_func *jump_func;
		  tree val, t;

		  val = VEC_index (tree, dest_info->known_vals, i);
		  if (!val)
		    continue;

		  if (i >= ipa_get_cs_argument_count (args))
		    {
		      insufficient = true;
		      break;
		    }
		  jump_func = ipa_get_ith_jump_func (args, i);
		  t = ipa_value_from_jfunc (caller_info, jump_func);
		  if (!t || !values_equal_for_ipcp_p (val, t))
		    {
		      insufficient = true;
		      break;
		    }
		}

	      if (!insufficient)
		{
		  if (dump_file)
		    fprintf (dump_file, " - adding an extra caller %s/%i"
			     " of %s/%i\n",
			     cgraph_node_name (cs->caller), cs->caller->uid,
			     cgraph_node_name (val->spec_node),
			     val->spec_node->uid);

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
move_binfos_to_values (VEC (tree, heap) *known_vals,
		       VEC (tree, heap) *known_binfos)
{
  tree t;
  int i;

  for (i = 0; VEC_iterate (tree, known_binfos, i, t); i++)
    if (t)
      VEC_replace (tree, known_vals, i, t);
}


/* Decide whether and what specialized clones of NODE should be created.  */

static bool
decide_whether_version_node (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);
  VEC (tree, heap) *known_csts, *known_binfos;
  bool ret = false;

  if (count == 0)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEvaluating opportunities for %s/%i.\n",
	     cgraph_node_name (node), node->uid);

  gather_context_independent_values (info, &known_csts, &known_binfos,
				     NULL);

  for (i = 0; i < count ; i++)
    {
      struct ipcp_lattice *lat = ipa_get_lattice (info, i);
      struct ipcp_value *val;

      if (lat->bottom
	  || VEC_index (tree, known_csts, i)
	  || VEC_index (tree, known_binfos, i))
	continue;

      for (val = lat->values; val; val = val->next)
	{
	  int freq_sum, caller_count;
	  gcov_type count_sum;
	  VEC (cgraph_edge_p, heap) *callers;
	  VEC (tree, heap) *kv;

	  if (val->spec_node)
	    {
	      perhaps_add_new_callers (node, val);
	      continue;
	    }
	  else if (val->local_size_cost + overall_size > max_new_size)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "   Ignoring candidate value because "
			 "max_new_size would be reached with %li.\n",
			 val->local_size_cost + overall_size);
	      continue;
	    }
	  else if (!get_info_about_necessary_edges (val, &freq_sum, &count_sum,
						    &caller_count))
	    continue;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - considering value ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for parameter ");
	      print_generic_expr (dump_file, ipa_get_param (info, i), 0);
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
	    continue;

	  if (dump_file)
	    fprintf (dump_file, "  Creating a specialized node of %s/%i.\n",
		     cgraph_node_name (node), node->uid);

	  callers = gather_edges_for_value (val, caller_count);
	  kv = VEC_copy (tree, heap, known_csts);
	  move_binfos_to_values (kv, known_binfos);
	  VEC_replace (tree, kv, i, val->value);
	  find_more_values_for_callers_subset (node, kv, callers);
	  val->spec_node = create_specialized_node (node, kv, callers);
	  overall_size += val->local_size_cost;
	  info = IPA_NODE_REF (node);

	  /* TODO: If for some lattice there is only one other known value
	     left, make a special node for it too. */
	  ret = true;

	  VEC_replace (tree, kv, i, val->value);
	}
    }

  if (info->clone_for_all_contexts)
    {
      VEC (cgraph_edge_p, heap) *callers;

      if (dump_file)
	fprintf (dump_file, " - Creating a specialized node of %s/%i "
		 "for all known contexts.\n", cgraph_node_name (node),
		 node->uid);

      callers = collect_callers_of_node (node);
      move_binfos_to_values (known_csts, known_binfos);
      create_specialized_node (node, known_csts, callers);
      info = IPA_NODE_REF (node);
      info->clone_for_all_contexts = false;
      ret = true;
    }
  else
    VEC_free (tree, heap, known_csts);

  VEC_free (tree, heap, known_binfos);
  return ret;
}

/* Transitively mark all callees of NODE within the same SCC as not dead.  */

static void
spread_undeadness (struct cgraph_node *node)
{
  struct cgraph_edge *cs;

  for (cs = node->callees; cs; cs = cs->next_callee)
    if (edge_within_scc (cs))
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
    else if (!edge_within_scc (cs)
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
		   cgraph_node_name (v), v->uid);
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
  struct topo_info topo;

  cgraph_remove_unreachable_nodes (true,dump_file);
  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  grow_next_edge_clone_vector ();
  edge_duplication_hook_holder =
    cgraph_add_edge_duplication_hook (&ipcp_edge_duplication_hook, NULL);
  ipcp_values_pool = create_alloc_pool ("IPA-CP values",
					sizeof (struct ipcp_value), 32);
  ipcp_sources_pool = create_alloc_pool ("IPA-CP value sources",
					 sizeof (struct ipcp_value_source), 64);
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
  VEC_free (cgraph_edge_p, heap, next_edge_clone);
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
	/* Unreachable nodes should have been eliminated before ipcp.  */
	gcc_assert (node->needed || node->reachable);
	node->local.versionable = tree_versionable_function_p (node->decl);
	ipa_analyze_node (node);
      }
}

/* Write ipcp summary for nodes in SET.  */

static void
ipcp_write_summary (cgraph_node_set set,
		    varpool_node_set vset ATTRIBUTE_UNUSED)
{
  ipa_prop_write_jump_functions (set);
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

struct ipa_opt_pass_d pass_ipa_cp =
{
 {
  IPA_PASS,
  "cp",				/* name */
  cgraph_gate_cp,		/* gate */
  ipcp_driver,			/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_IPA_CONSTANT_PROP,		/* tv_id */
  0,				/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_cgraph |
  TODO_remove_functions | TODO_ggc_collect /* todo_flags_finish */
 },
 ipcp_generate_summary,			/* generate_summary */
 ipcp_write_summary,			/* write_summary */
 ipcp_read_summary,			/* read_summary */
 NULL,					/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,			 		/* stmt_fixup */
 0,					/* TODOs */
 NULL,					/* function_transform */
 NULL,					/* variable_transform */
};
