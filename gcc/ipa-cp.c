/* Interprocedural constant propagation
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple-expr.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "params.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "tree-ssa-ccp.h"
#include "stringpool.h"
#include "attribs.h"

template <typename valtype> class ipcp_value;

/* Describes a particular source for an IPA-CP value.  */

template <typename valtype>
class ipcp_value_source
{
public:
  /* Aggregate offset of the source, negative if the source is scalar value of
     the argument itself.  */
  HOST_WIDE_INT offset;
  /* The incoming edge that brought the value.  */
  cgraph_edge *cs;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the ipcp_value of the caller from which the described
     value has been derived.  Otherwise it is NULL.  */
  ipcp_value<valtype> *val;
  /* Next pointer in a linked list of sources of a value.  */
  ipcp_value_source *next;
  /* If the jump function that resulted into his value was a pass-through or an
     ancestor, this is the index of the parameter of the caller the jump
     function references.  */
  int index;
};

/* Common ancestor for all ipcp_value instantiations.  */

class ipcp_value_base
{
public:
  /* Time benefit and size cost that specializing the function for this value
     would bring about in this function alone.  */
  int local_time_benefit, local_size_cost;
  /* Time benefit and size cost that specializing the function for this value
     can bring about in it's callees (transitively).  */
  int prop_time_benefit, prop_size_cost;

  ipcp_value_base ()
    : local_time_benefit (0), local_size_cost (0),
      prop_time_benefit (0), prop_size_cost (0) {}
};

/* Describes one particular value stored in struct ipcp_lattice.  */

template <typename valtype>
class ipcp_value : public ipcp_value_base
{
public:
  /* The actual value for the given parameter.  */
  valtype value;
  /* The list of sources from which this value originates.  */
  ipcp_value_source <valtype> *sources;
  /* Next pointers in a linked list of all values in a lattice.  */
  ipcp_value *next;
  /* Next pointers in a linked list of values in a strongly connected component
     of values. */
  ipcp_value *scc_next;
  /* Next pointers in a linked list of SCCs of values sorted topologically
     according their sources.  */
  ipcp_value  *topo_next;
  /* A specialized node created for this value, NULL if none has been (so far)
     created.  */
  cgraph_node *spec_node;
  /* Depth first search number and low link for topological sorting of
     values.  */
  int dfs, low_link;
  /* True if this value is currently on the topo-sort stack.  */
  bool on_stack;

  ipcp_value()
    : sources (0), next (0), scc_next (0), topo_next (0),
      spec_node (0), dfs (0), low_link (0), on_stack (false) {}

  void add_source (cgraph_edge *cs, ipcp_value *src_val, int src_idx,
		   HOST_WIDE_INT offset);
};

/* Lattice describing potential values of a formal parameter of a function, or
   a part of an aggregate.  TOP is represented by a lattice with zero values
   and with contains_variable and bottom flags cleared.  BOTTOM is represented
   by a lattice with the bottom flag set.  In that case, values and
   contains_variable flag should be disregarded.  */

template <typename valtype>
class ipcp_lattice
{
public:
  /* The list of known values and types in this lattice.  Note that values are
     not deallocated if a lattice is set to bottom because there may be value
     sources referencing them.  */
  ipcp_value<valtype> *values;
  /* Number of known values and types in this lattice.  */
  int values_count;
  /* The lattice contains a variable component (in addition to values).  */
  bool contains_variable;
  /* The value of the lattice is bottom (i.e. variable and unusable for any
     propagation).  */
  bool bottom;

  inline bool is_single_const ();
  inline bool set_to_bottom ();
  inline bool set_contains_variable ();
  bool add_value (valtype newval, cgraph_edge *cs,
		  ipcp_value<valtype> *src_val = NULL,
		  int src_idx = 0, HOST_WIDE_INT offset = -1);
  void print (FILE * f, bool dump_sources, bool dump_benefits);
};

/* Lattice of tree values with an offset to describe a part of an
   aggregate.  */

class ipcp_agg_lattice : public ipcp_lattice<tree>
{
public:
  /* Offset that is being described by this lattice. */
  HOST_WIDE_INT offset;
  /* Size so that we don't have to re-compute it every time we traverse the
     list.  Must correspond to TYPE_SIZE of all lat values.  */
  HOST_WIDE_INT size;
  /* Next element of the linked list.  */
  struct ipcp_agg_lattice *next;
};

/* Lattice of known bits, only capable of holding one value.
   Bitwise constant propagation propagates which bits of a
   value are constant.
   For eg:
   int f(int x)
   {
     return some_op (x);
   }

   int f1(int y)
   {
     if (cond)
      return f (y & 0xff);
     else
      return f (y & 0xf);
   }

   In the above case, the param 'x' will always have all
   the bits (except the bits in lsb) set to 0.
   Hence the mask of 'x' would be 0xff. The mask
   reflects that the bits in lsb are unknown.
   The actual propagated value is given by m_value & ~m_mask.  */

class ipcp_bits_lattice
{
public:
  bool bottom_p () { return m_lattice_val == IPA_BITS_VARYING; }
  bool top_p () { return m_lattice_val == IPA_BITS_UNDEFINED; }
  bool constant_p () { return m_lattice_val == IPA_BITS_CONSTANT; }
  bool set_to_bottom ();
  bool set_to_constant (widest_int, widest_int);

  widest_int get_value () { return m_value; }
  widest_int get_mask () { return m_mask; }

  bool meet_with (ipcp_bits_lattice& other, unsigned, signop,
		  enum tree_code, tree);

  bool meet_with (widest_int, widest_int, unsigned);

  void print (FILE *);

private:
  enum { IPA_BITS_UNDEFINED, IPA_BITS_CONSTANT, IPA_BITS_VARYING } m_lattice_val;

  /* Similar to ccp_lattice_t, mask represents which bits of value are constant.
     If a bit in mask is set to 0, then the corresponding bit in
     value is known to be constant.  */
  widest_int m_value, m_mask;

  bool meet_with_1 (widest_int, widest_int, unsigned);
  void get_value_and_mask (tree, widest_int *, widest_int *);
};

/* Lattice of value ranges.  */

class ipcp_vr_lattice
{
public:
  value_range_base m_vr;

  inline bool bottom_p () const;
  inline bool top_p () const;
  inline bool set_to_bottom ();
  bool meet_with (const value_range_base *p_vr);
  bool meet_with (const ipcp_vr_lattice &other);
  void init () { gcc_assert (m_vr.undefined_p ()); }
  void print (FILE * f);

private:
  bool meet_with_1 (const value_range_base *other_vr);
};

/* Structure containing lattices for a parameter itself and for pieces of
   aggregates that are passed in the parameter or by a reference in a parameter
   plus some other useful flags.  */

class ipcp_param_lattices
{
public:
  /* Lattice describing the value of the parameter itself.  */
  ipcp_lattice<tree> itself;
  /* Lattice describing the polymorphic contexts of a parameter.  */
  ipcp_lattice<ipa_polymorphic_call_context> ctxlat;
  /* Lattices describing aggregate parts.  */
  ipcp_agg_lattice *aggs;
  /* Lattice describing known bits.  */
  ipcp_bits_lattice bits_lattice;
  /* Lattice describing value range.  */
  ipcp_vr_lattice m_value_range;
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

object_allocator<ipcp_value<tree> > ipcp_cst_values_pool
  ("IPA-CP constant values");

object_allocator<ipcp_value<ipa_polymorphic_call_context> >
  ipcp_poly_ctx_values_pool ("IPA-CP polymorphic contexts");

object_allocator<ipcp_value_source<tree> > ipcp_sources_pool
  ("IPA-CP value sources");

object_allocator<ipcp_agg_lattice> ipcp_agg_lattice_pool
  ("IPA_CP aggregate lattices");

/* Maximal count found in program.  */

static profile_count max_count;

/* Original overall size of the program.  */

static long overall_size, max_new_size;

/* Node name to unique clone suffix number map.  */
static hash_map<const char *, unsigned> *clone_num_suffixes;

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
static inline ipcp_lattice<tree> *
ipa_get_scalar_lat (struct ipa_node_params *info, int i)
{
  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
  return &plats->itself;
}

/* Return the lattice corresponding to the scalar value of the Ith formal
   parameter of the function described by INFO.  */
static inline ipcp_lattice<ipa_polymorphic_call_context> *
ipa_get_poly_ctx_lat (struct ipa_node_params *info, int i)
{
  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
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

/* Print V which is extracted from a value in a lattice to F.  */

static void
print_ipcp_constant_value (FILE * f, tree v)
{
  if (TREE_CODE (v) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (v, 0)) == CONST_DECL)
    {
      fprintf (f, "& ");
      print_generic_expr (f, DECL_INITIAL (TREE_OPERAND (v, 0)));
    }
  else
    print_generic_expr (f, v);
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

	  fprintf (f, " [from:");
	  for (s = val->sources; s; s = s->next)
	    fprintf (f, " %i(%f)", s->cs->caller->order,
		     s->cs->sreal_frequency ().to_double ());
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
  dump_value_range (f, &m_vr);
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
      /* Skip constprop clones since we don't make lattices for them.  */
      if (info->ipcp_orig_node)
	continue;
      fprintf (f, "  Node: %s:\n", node->dump_name ());
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_agg_lattice *aglat;
	  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
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
			  struct ipa_node_params *info)
{
  const char *reason = NULL;

  /* There are a number of generic reasons functions cannot be versioned.  We
     also cannot remove parameters if there are type attributes such as fnspec
     present.  */
  if (node->alias || node->thunk.thunk_p)
    reason = "alias or thunk";
  else if (!node->local.versionable)
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

  if (reason && dump_file && !node->alias && !node->thunk.thunk_p)
    fprintf (dump_file, "Function %s is not versionable, reason: %s.\n",
	     node->dump_name (), reason);

  info->versionable = (reason == NULL);
}

/* Return true if it is at all technically possible to create clones of a
   NODE.  */

static bool
ipcp_versionable_function_p (struct cgraph_node *node)
{
  return IPA_NODE_REF (node)->versionable;
}

/* Structure holding accumulated information about callers of a node.  */

struct caller_statistics
{
  profile_count count_sum;
  int n_calls, n_hot_calls, freq_sum;
};

/* Initialize fields of STAT to zeroes.  */

static inline void
init_caller_stats (struct caller_statistics *stats)
{
  stats->count_sum = profile_count::zero ();
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
    if (!cs->caller->thunk.thunk_p)
      {
        if (cs->count.ipa ().initialized_p ())
	  stats->count_sum += cs->count.ipa ();
	stats->freq_sum += cs->frequency ();
	stats->n_calls++;
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
 		 node->name ());
      return false;
    }

  if (node->optimize_for_size_p ())
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; "
		 "optimizing it for size.\n",
 		 node->name ());
      return false;
    }

  init_caller_stats (&stats);
  node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats, false);

  if (ipa_fn_summaries->get (node)->self_size < stats.n_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Considering %s for cloning; code might shrink.\n",
 		 node->name ());
      return true;
    }

  /* When profile is available and function is hot, propagate into it even if
     calls seems cold; constant propagation can improve function's speed
     significantly.  */
  if (max_count > profile_count::zero ())
    {
      if (stats.count_sum > node->count.ipa ().apply_scale (90, 100))
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

/* Allocate the arrays in TOPO and topologically sort the nodes into order.  */

static void
build_toporder_info (struct ipa_topo_info *topo)
{
  topo->order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  topo->stack = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);

  gcc_checking_assert (topo->stack_top == 0);
  topo->nnodes = ipa_reduced_postorder (topo->order, true, NULL);
}

/* Free information about strongly connected components and the arrays in
   TOPO.  */

static void
free_toporder_info (struct ipa_topo_info *topo)
{
  ipa_free_postorder_info ();
  free (topo->order);
  free (topo->stack);
}

/* Add NODE to the stack in TOPO, unless it is already there.  */

static inline void
push_node_to_stack (struct ipa_topo_info *topo, struct cgraph_node *node)
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
pop_node_from_stack (struct ipa_topo_info *topo)
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
set_agg_lats_to_bottom (struct ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_bottom;
  plats->aggs_bottom = true;
  return ret;
}

/* Mark all aggregate lattices in PLATS as containing an unknown value and
   return true if they were not previously marked as such.  */

static inline bool
set_agg_lats_contain_variable (struct ipcp_param_lattices *plats)
{
  bool ret = !plats->aggs_contain_variable;
  plats->aggs_contain_variable = true;
  return ret;
}

bool
ipcp_vr_lattice::meet_with (const ipcp_vr_lattice &other)
{
  return meet_with_1 (&other.m_vr);
}

/* Meet the current value of the lattice with value range described by VR
   lattice.  */

bool
ipcp_vr_lattice::meet_with (const value_range_base *p_vr)
{
  return meet_with_1 (p_vr);
}

/* Meet the current value of the lattice with value range described by
   OTHER_VR lattice.  Return TRUE if anything changed.  */

bool
ipcp_vr_lattice::meet_with_1 (const value_range_base *other_vr)
{
  if (bottom_p ())
    return false;

  if (other_vr->varying_p ())
    return set_to_bottom ();

  value_range_base save (m_vr);
  m_vr.union_ (other_vr);
  return !m_vr.equal_p (save);
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
  m_vr.set_varying ();
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
  m_value = value;
  m_mask = mask;
  return true;
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
   This function assumes that the lattice value is in CONSTANT state  */

bool
ipcp_bits_lattice::meet_with_1 (widest_int value, widest_int mask,
				unsigned precision)
{
  gcc_assert (constant_p ());

  widest_int old_mask = m_mask;
  m_mask = (m_mask | mask) | (m_value ^ value);

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

  return meet_with_1 (value, mask, precision);
}

/* Meet bits lattice with the result of bit_value_binop (other, operand)
   if code is binary operation or bit_value_unop (other) if code is unary op.
   In the case when code is nop_expr, no adjustment is required. */

bool
ipcp_bits_lattice::meet_with (ipcp_bits_lattice& other, unsigned precision,
			      signop sgn, enum tree_code code, tree operand)
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
      if (wi::sext (adjusted_mask, precision) == -1)
	return set_to_bottom ();
      return set_to_constant (adjusted_value, adjusted_mask);
    }
  else
    return meet_with_1 (adjusted_value, adjusted_mask, precision);
}

/* Mark bot aggregate and scalar lattices as containing an unknown variable,
   return true is any of them has not been marked as such so far.  */

static inline bool
set_all_contains_variable (struct ipcp_param_lattices *plats)
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
    if (!cs->caller->thunk.thunk_p || !cs->caller->local.local)
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
  while (cs && cs->caller->thunk.thunk_p && cs->caller->local.local)
    cs = cs->next_caller;
  if (cs)
    {
      IPA_NODE_REF (cs->caller)->node_calling_single_call = true;
      return true;
    }
  return false;
}

/* Initialize ipcp_lattices.  */

static void
initialize_node_lattices (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  struct cgraph_edge *ie;
  bool disable = false, variable = false;
  int i;

  gcc_checking_assert (node->has_gimple_body_p ());
  if (node->local.local)
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

  for (i = 0; i < ipa_get_param_count (info); i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      plats->m_value_range.init ();
    }

  if (disable || variable)
    {
      for (i = 0; i < ipa_get_param_count (info); i++)
	{
	  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  if (disable)
	    {
	      plats->itself.set_to_bottom ();
	      plats->ctxlat.set_to_bottom ();
	      set_agg_lats_to_bottom (plats);
	      plats->bits_lattice.set_to_bottom ();
	      plats->m_value_range.set_to_bottom ();
	    }
	  else
	    set_all_contains_variable (plats);
	}
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && !node->alias && !node->thunk.thunk_p)
	fprintf (dump_file, "Marking all lattices of %s as %s\n",
		 node->dump_name (), disable ? "BOTTOM" : "VARIABLE");
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
   JFUNC on the constant value INPUT.  RES_TYPE is the type of the parameter
   to which the result is passed.  Return NULL_TREE if that cannot be
   determined or be considered an interprocedural invariant.  */

static tree
ipa_get_jf_pass_through_result (struct ipa_jump_func *jfunc, tree input,
				tree res_type)
{
  tree res;

  if (ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
    return input;
  if (!is_gimple_ip_invariant (input))
    return NULL_TREE;

  tree_code opcode = ipa_get_jf_pass_through_operation (jfunc);
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
    res = fold_binary (opcode, res_type, input,
		       ipa_get_jf_pass_through_operand (jfunc));

  if (res && !is_gimple_ip_invariant (res))
    return NULL_TREE;

  return res;
}

/* Return the result of an ancestor jump function JFUNC on the constant value
   INPUT.  Return NULL_TREE if that cannot be determined.  */

static tree
ipa_get_jf_ancestor_result (struct ipa_jump_func *jfunc, tree input)
{
  gcc_checking_assert (TREE_CODE (input) != TREE_BINFO);
  if (TREE_CODE (input) == ADDR_EXPR)
    {
      tree t = TREE_OPERAND (input, 0);
      t = build_ref_for_offset (EXPR_LOCATION (t), t,
				ipa_get_jf_ancestor_offset (jfunc), false,
				ptr_type_node, NULL, false);
      return build_fold_addr_expr (t);
    }
  else
    return NULL_TREE;
}

/* Determine whether JFUNC evaluates to a single known constant value and if
   so, return it.  Otherwise return NULL.  INFO describes the caller node or
   the one it is inlined to, so that pass-through jump functions can be
   evaluated.  PARM_TYPE is the type of the parameter to which the result is
   passed.  */

tree
ipa_value_from_jfunc (struct ipa_node_params *info, struct ipa_jump_func *jfunc,
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

	  if (!info->lattices
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
  ipa_edge_args *args = IPA_EDGE_REF (cs);
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
	  if (!info->lattices
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

/* Return true iff X and Y should be considered equal values by IPA-CP.  */

static bool
values_equal_for_ipcp_p (tree x, tree y)
{
  gcc_checking_assert (x != NULL_TREE && y != NULL_TREE);

  if (x == y)
    return true;

  if (TREE_CODE (x) ==  ADDR_EXPR
      && TREE_CODE (y) ==  ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (x, 0)) == CONST_DECL
      && TREE_CODE (TREE_OPERAND (y, 0)) == CONST_DECL)
    return operand_equal_p (DECL_INITIAL (TREE_OPERAND (x, 0)),
			    DECL_INITIAL (TREE_OPERAND (y, 0)), 0);
  else
    return operand_equal_p (x, y, 0);
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
allocate_and_init_ipcp_value (tree source)
{
  ipcp_value<tree> *val;

  val = new (ipcp_cst_values_pool.allocate ()) ipcp_value<tree>();
  val->value = source;
  return val;
}

/* Allocate a new ipcp_value holding a polymorphic context, initialize its
   value to SOURCE and clear all other fields.  */

static ipcp_value<ipa_polymorphic_call_context> *
allocate_and_init_ipcp_value (ipa_polymorphic_call_context source)
{
  ipcp_value<ipa_polymorphic_call_context> *val;

  // TODO
  val = new (ipcp_poly_ctx_values_pool.allocate ())
    ipcp_value<ipa_polymorphic_call_context>();
  val->value = source;
  return val;
}

/* Try to add NEWVAL to LAT, potentially creating a new ipcp_value for it.  CS,
   SRC_VAL SRC_INDEX and OFFSET are meant for add_source and have the same
   meaning.  OFFSET -1 means the source is scalar and not a part of an
   aggregate.  */

template <typename valtype>
bool
ipcp_lattice<valtype>::add_value (valtype newval, cgraph_edge *cs,
				  ipcp_value<valtype> *src_val,
				  int src_idx, HOST_WIDE_INT offset)
{
  ipcp_value<valtype> *val;

  if (bottom)
    return false;

  for (val = values; val; val = val->next)
    if (values_equal_for_ipcp_p (val->value, newval))
      {
	if (ipa_edge_within_scc (cs))
	  {
	    ipcp_value_source<valtype> *s;
	    for (s = val->sources; s; s = s->next)
	      if (s->cs == cs)
		break;
	    if (s)
	      return false;
	  }

	val->add_source (cs, src_val, src_idx, offset);
	return false;
      }

  if (values_count == PARAM_VALUE (PARAM_IPA_CP_VALUE_LIST_SIZE))
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
  val = allocate_and_init_ipcp_value (newval);
  val->add_source (cs, src_val, src_idx, offset);
  val->next = values;
  values = val;
  return true;
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
  ipcp_value<tree> *src_val;
  bool ret = false;

  /* Do not create new values when propagating within an SCC because if there
     are arithmetic functions with circular dependencies, there is infinite
     number of them and we would just make lattices bottom.  If this condition
     is ever relaxed we have to detect self-feeding recursive calls in
     cgraph_edge_brings_value_p in a smarter way.  */
  if ((ipa_get_jf_pass_through_operation (jfunc) != NOP_EXPR)
      && ipa_edge_within_scc (cs))
    ret = dest_lat->set_contains_variable ();
  else
    for (src_val = src_lat->values; src_val; src_val = src_val->next)
      {
	tree cstval = ipa_get_jf_pass_through_result (jfunc, src_val->value,
						      parm_type);

	if (cstval)
	  ret |= dest_lat->add_value (cstval, cs, src_val, src_idx);
	else
	  ret |= dest_lat->set_contains_variable ();
      }

  return ret;
}

/* Propagate values through an ancestor jump function JFUNC associated with
   edge CS, taking values from SRC_LAT and putting them into DEST_LAT.  SRC_IDX
   is the index of the source parameter.  */

static bool
propagate_vals_across_ancestor (struct cgraph_edge *cs,
				struct ipa_jump_func *jfunc,
				ipcp_lattice<tree> *src_lat,
				ipcp_lattice<tree> *dest_lat, int src_idx)
{
  ipcp_value<tree> *src_val;
  bool ret = false;

  if (ipa_edge_within_scc (cs))
    return dest_lat->set_contains_variable ();

  for (src_val = src_lat->values; src_val; src_val = src_val->next)
    {
      tree t = ipa_get_jf_ancestor_result (jfunc, src_val->value);

      if (t)
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
      return dest_lat->add_value (val, cs, NULL, 0);
    }
  else if (jfunc->type == IPA_JF_PASS_THROUGH
	   || jfunc->type == IPA_JF_ANCESTOR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
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
						  dest_lat, src_idx, param_type);
      else
	ret = propagate_vals_across_ancestor (cs, jfunc, src_lat, dest_lat,
					      src_idx);

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
  ipa_edge_args *args = IPA_EDGE_REF (cs);
  if (dest_lat->bottom)
    return false;
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
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
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
  struct ipa_node_params *callee_info = IPA_NODE_REF (callee);
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
		 idx, cs->callee->name ());

      return dest_lattice->set_to_bottom ();
    }

  unsigned precision = TYPE_PRECISION (parm_type);
  signop sgn = TYPE_SIGN (parm_type);

  if (jfunc->type == IPA_JF_PASS_THROUGH
      || jfunc->type == IPA_JF_ANCESTOR)
    {
      struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
      tree operand = NULL_TREE;
      enum tree_code code;
      unsigned src_idx;

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
	  unsigned HOST_WIDE_INT offset = ipa_get_jf_ancestor_offset (jfunc) / BITS_PER_UNIT;
	  operand = build_int_cstu (size_type_node, offset);
	}

      struct ipcp_param_lattices *src_lats
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

      if (src_lats->bits_lattice.bottom_p ()
	  && jfunc->bits)
	return dest_lattice->meet_with (jfunc->bits->value, jfunc->bits->mask,
					precision);
      else
	return dest_lattice->meet_with (src_lats->bits_lattice, precision, sgn,
					code, operand);
    }

  else if (jfunc->type == IPA_JF_ANCESTOR)
    return dest_lattice->set_to_bottom ();
  else if (jfunc->bits)
    return dest_lattice->meet_with (jfunc->bits->value, jfunc->bits->mask,
				    precision);
  else
    return dest_lattice->set_to_bottom ();
}

/* Emulate effects of unary OPERATION and/or conversion from SRC_TYPE to
   DST_TYPE on value range in SRC_VR and store it to DST_VR.  Return true if
   the result is a range or an anti-range.  */

static bool
ipa_vr_operation_and_type_effects (value_range_base *dst_vr,
				   value_range_base *src_vr,
				   enum tree_code operation,
				   tree dst_type, tree src_type)
{
  extract_range_from_unary_expr (dst_vr, operation, dst_type,
				 src_vr, src_type);
  if (dst_vr->varying_p () || dst_vr->undefined_p ())
    return false;
  return true;
}

/* Propagate value range across jump function JFUNC that is associated with
   edge CS with param of callee of PARAM_TYPE and update DEST_PLATS
   accordingly.  */

static bool
propagate_vr_across_jump_function (cgraph_edge *cs, ipa_jump_func *jfunc,
				   struct ipcp_param_lattices *dest_plats,
				   tree param_type)
{
  ipcp_vr_lattice *dest_lat = &dest_plats->m_value_range;

  if (dest_lat->bottom_p ())
    return false;

  if (!param_type
      || (!INTEGRAL_TYPE_P (param_type)
	  && !POINTER_TYPE_P (param_type)))
    return dest_lat->set_to_bottom ();

  if (jfunc->type == IPA_JF_PASS_THROUGH)
    {
      enum tree_code operation = ipa_get_jf_pass_through_operation (jfunc);

      if (TREE_CODE_CLASS (operation) == tcc_unary)
	{
	  struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
	  int src_idx = ipa_get_jf_pass_through_formal_id (jfunc);
	  tree operand_type = ipa_get_type (caller_info, src_idx);
	  struct ipcp_param_lattices *src_lats
	    = ipa_get_parm_lattices (caller_info, src_idx);

	  if (src_lats->m_value_range.bottom_p ())
	    return dest_lat->set_to_bottom ();
	  value_range_base vr;
	  if (ipa_vr_operation_and_type_effects (&vr,
						 &src_lats->m_value_range.m_vr,
						 operation, param_type,
						 operand_type))
	    return dest_lat->meet_with (&vr);
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

	  value_range_base tmpvr (VR_RANGE, val, val);
	  return dest_lat->meet_with (&tmpvr);
	}
    }

  value_range_base vr;
  if (jfunc->m_vr
      && ipa_vr_operation_and_type_effects (&vr, jfunc->m_vr, NOP_EXPR,
					    param_type,
					    jfunc->m_vr->type ()))
    return dest_lat->meet_with (&vr);
  else
    return dest_lat->set_to_bottom ();
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
      *change |= (**aglat)->set_contains_variable ();
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
      new_al = ipcp_agg_lattice_pool.allocate ();
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
propagate_aggs_across_jump_function (struct cgraph_edge *cs,
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
	      ret |= (*aglat)->add_value (item->value, cs, NULL, 0, 0);
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
call_passes_through_thunk_p (cgraph_edge *cs)
{
  cgraph_node *alias_or_thunk = cs->callee;
  while (alias_or_thunk->alias)
    alias_or_thunk = alias_or_thunk->get_alias_target ();
  return alias_or_thunk->thunk.thunk_p;
}

/* Propagate constants from the caller to the callee of CS.  INFO describes the
   caller.  */

static bool
propagate_constants_across_call (struct cgraph_edge *cs)
{
  struct ipa_node_params *callee_info;
  enum availability availability;
  cgraph_node *callee;
  struct ipa_edge_args *args;
  bool ret = false;
  int i, args_count, parms_count;

  callee = cs->callee->function_symbol (&availability);
  if (!callee->definition)
    return false;
  gcc_checking_assert (callee->has_gimple_body_p ());
  callee_info = IPA_NODE_REF (callee);

  args = IPA_EDGE_REF (cs);
  args_count = ipa_get_cs_argument_count (args);
  parms_count = ipa_get_param_count (callee_info);
  if (parms_count == 0)
    return false;

  /* If this call goes through a thunk we must not propagate to the first (0th)
     parameter.  However, we might need to uncover a thunk from below a series
     of aliases first.  */
  if (call_passes_through_thunk_p (cs))
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
   KNOWN_CONTEXTS, KNOWN_AGGS or AGG_REPS return the destination.  The latter
   three can be NULL.  If AGG_REPS is not NULL, KNOWN_AGGS is ignored.  */

static tree
ipa_get_indirect_edge_target_1 (struct cgraph_edge *ie,
				vec<tree> known_csts,
				vec<ipa_polymorphic_call_context> known_contexts,
				vec<ipa_agg_jump_function_p> known_aggs,
				struct ipa_agg_replacement_value *agg_reps,
				bool *speculative)
{
  int param_index = ie->indirect_info->param_index;
  HOST_WIDE_INT anc_offset;
  tree t;
  tree target = NULL;

  *speculative = false;

  if (param_index == -1
      || known_csts.length () <= (unsigned int) param_index)
    return NULL_TREE;

  if (!ie->indirect_info->polymorphic)
    {
      tree t;

      if (ie->indirect_info->agg_contents)
	{
	  t = NULL;
	  if (agg_reps && ie->indirect_info->guaranteed_unmodified)
	    {
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
	  if (!t)
	    {
	      struct ipa_agg_jump_function *agg;
	      if (known_aggs.length () > (unsigned int) param_index)
		agg = known_aggs[param_index];
	      else
		agg = NULL;
	      bool from_global_constant;
	      t = ipa_find_agg_cst_for_param (agg, known_csts[param_index],
					      ie->indirect_info->offset,
					      ie->indirect_info->by_ref,
					      &from_global_constant);
	      if (t
		  && !from_global_constant
		  && !ie->indirect_info->guaranteed_unmodified)
		t = NULL_TREE;
	    }
	}
      else
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
  anc_offset = ie->indirect_info->offset;

  t = NULL;

  /* Try to work out value of virtual table pointer value in replacements.  */
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
      t = ipa_find_agg_cst_for_param (agg, known_csts[param_index],
				      ie->indirect_info->offset, true);
    }

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
		  || (TREE_CODE (TREE_TYPE (target)) == FUNCTION_TYPE
		      && DECL_FUNCTION_CODE (target) == BUILT_IN_UNREACHABLE)
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
  if (!t)
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


/* If an indirect edge IE can be turned into a direct one based on KNOWN_CSTS,
   KNOWN_CONTEXTS (which can be vNULL) or KNOWN_AGGS (which also can be vNULL)
   return the destination.  */

tree
ipa_get_indirect_edge_target (struct cgraph_edge *ie,
			      vec<tree> known_csts,
			      vec<ipa_polymorphic_call_context> known_contexts,
			      vec<ipa_agg_jump_function_p> known_aggs,
			      bool *speculative)
{
  return ipa_get_indirect_edge_target_1 (ie, known_csts, known_contexts,
					 known_aggs, NULL, speculative);
}

/* Calculate devirtualization time bonus for NODE, assuming we know KNOWN_CSTS
   and KNOWN_CONTEXTS.  */

static int
devirtualization_time_bonus (struct cgraph_node *node,
			     vec<tree> known_csts,
			     vec<ipa_polymorphic_call_context> known_contexts,
			     vec<ipa_agg_jump_function_p> known_aggs)
{
  struct cgraph_edge *ie;
  int res = 0;

  for (ie = node->indirect_calls; ie; ie = ie->next_callee)
    {
      struct cgraph_node *callee;
      struct ipa_fn_summary *isummary;
      enum availability avail;
      tree target;
      bool speculative;

      target = ipa_get_indirect_edge_target (ie, known_csts, known_contexts,
					     known_aggs, &speculative);
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
      if (!isummary->inlinable)
	continue;

      /* FIXME: The values below need re-considering and perhaps also
	 integrating into the cost metrics, at lest in some very basic way.  */
      if (isummary->size <= MAX_INLINE_INSNS_AUTO / 4)
	res += 31 / ((int)speculative + 1);
      else if (isummary->size <= MAX_INLINE_INSNS_AUTO / 2)
	res += 15 / ((int)speculative + 1);
      else if (isummary->size <= MAX_INLINE_INSNS_AUTO
	       || DECL_DECLARED_INLINE_P (callee->decl))
	res += 7 / ((int)speculative + 1);
    }

  return res;
}

/* Return time bonus incurred because of HINTS.  */

static int
hint_time_bonus (ipa_hints hints)
{
  int result = 0;
  if (hints & (INLINE_HINT_loop_iterations | INLINE_HINT_loop_stride))
    result += PARAM_VALUE (PARAM_IPA_CP_LOOP_HINT_BONUS);
  if (hints & INLINE_HINT_array_index)
    result += PARAM_VALUE (PARAM_IPA_CP_ARRAY_INDEX_HINT_BONUS);
  return result;
}

/* If there is a reason to penalize the function described by INFO in the
   cloning goodness evaluation, do so.  */

static inline int64_t
incorporate_penalties (ipa_node_params *info, int64_t evaluation)
{
  if (info->node_within_scc)
    evaluation = (evaluation
		  * (100 - PARAM_VALUE (PARAM_IPA_CP_RECURSION_PENALTY))) / 100;

  if (info->node_calling_single_call)
    evaluation = (evaluation
		  * (100 - PARAM_VALUE (PARAM_IPA_CP_SINGLE_CALL_PENALTY)))
      / 100;

  return evaluation;
}

/* Return true if cloning NODE is a good idea, given the estimated TIME_BENEFIT
   and SIZE_COST and with the sum of frequencies of incoming edges to the
   potential new clone in FREQUENCIES.  */

static bool
good_cloning_opportunity_p (struct cgraph_node *node, int time_benefit,
			    int freq_sum, profile_count count_sum, int size_cost)
{
  if (time_benefit == 0
      || !opt_for_fn (node->decl, flag_ipa_cp_clone)
      || node->optimize_for_size_p ())
    return false;

  gcc_assert (size_cost > 0);

  struct ipa_node_params *info = IPA_NODE_REF (node);
  if (max_count > profile_count::zero ())
    {
      int factor = RDIV (count_sum.probability_in
				 (max_count).to_reg_br_prob_base ()
		         * 1000, REG_BR_PROB_BASE);
      int64_t evaluation = (((int64_t) time_benefit * factor)
				    / size_cost);
      evaluation = incorporate_penalties (info, evaluation);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		   "size: %i, count_sum: ", time_benefit, size_cost);
	  count_sum.dump (dump_file);
	  fprintf (dump_file, "%s%s) -> evaluation: " "%" PRId64
		 ", threshold: %i\n",
		 info->node_within_scc ? ", scc" : "",
		 info->node_calling_single_call ? ", single_call" : "",
		 evaluation, PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD));
	}

      return evaluation >= PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD);
    }
  else
    {
      int64_t evaluation = (((int64_t) time_benefit * freq_sum)
				    / size_cost);
      evaluation = incorporate_penalties (info, evaluation);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "     good_cloning_opportunity_p (time: %i, "
		 "size: %i, freq_sum: %i%s%s) -> evaluation: "
		 "%" PRId64 ", threshold: %i\n",
		 time_benefit, size_cost, freq_sum,
		 info->node_within_scc ? ", scc" : "",
		 info->node_calling_single_call ? ", single_call" : "",
		 evaluation, PARAM_VALUE (PARAM_IPA_CP_EVAL_THRESHOLD));

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
    if (aglat->is_single_const ())
      {
	struct ipa_agg_jf_item item;
	item.offset = aglat->offset;
	item.value = aglat->values->value;
	vec_safe_push (res, item);
      }
  return res;
}

/* Allocate KNOWN_CSTS, KNOWN_CONTEXTS and, if non-NULL, KNOWN_AGGS and
   populate them with values of parameters that are known independent of the
   context.  INFO describes the function.  If REMOVABLE_PARAMS_COST is
   non-NULL, the movement cost of all removable parameters will be stored in
   it.  */

static bool
gather_context_independent_values (struct ipa_node_params *info,
				   vec<tree> *known_csts,
				   vec<ipa_polymorphic_call_context>
				   *known_contexts,
				   vec<ipa_agg_jump_function> *known_aggs,
				   int *removable_params_cost)
{
  int i, count = ipa_get_param_count (info);
  bool ret = false;

  known_csts->create (0);
  known_contexts->create (0);
  known_csts->safe_grow_cleared (count);
  known_contexts->safe_grow_cleared (count);
  if (known_aggs)
    {
      known_aggs->create (0);
      known_aggs->safe_grow_cleared (count);
    }

  if (removable_params_cost)
    *removable_params_cost = 0;

  for (i = 0; i < count; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;

      if (lat->is_single_const ())
	{
	  ipcp_value<tree> *val = lat->values;
	  gcc_checking_assert (TREE_CODE (val->value) != TREE_BINFO);
	  (*known_csts)[i] = val->value;
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
	(*known_contexts)[i] = ctxlat->values->value;

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

/* Perform time and size measurement of NODE with the context given in
   KNOWN_CSTS, KNOWN_CONTEXTS and KNOWN_AGGS, calculate the benefit and cost
   given BASE_TIME of the node without specialization, REMOVABLE_PARAMS_COST of
   all context-independent removable parameters and EST_MOVE_COST of estimated
   movement of the considered parameter and store it into VAL.  */

static void
perform_estimation_of_a_value (cgraph_node *node, vec<tree> known_csts,
			       vec<ipa_polymorphic_call_context> known_contexts,
			       vec<ipa_agg_jump_function_p> known_aggs_ptrs,
			       int removable_params_cost,
			       int est_move_cost, ipcp_value_base *val)
{
  int size, time_benefit;
  sreal time, base_time;
  ipa_hints hints;

  estimate_ipcp_clone_size_and_time (node, known_csts, known_contexts,
				     known_aggs_ptrs, &size, &time,
				     &base_time, &hints);
  base_time -= time;
  if (base_time > 65535)
    base_time = 65535;

  /* Extern inline functions have no cloning local time benefits because they
     will be inlined anyway.  The only reason to clone them is if it enables
     optimization in any of the functions they call.  */
  if (DECL_EXTERNAL (node->decl) && DECL_DECLARED_INLINE_P (node->decl))
    time_benefit = 0;
  else
    time_benefit = base_time.to_int ()
      + devirtualization_time_bonus (node, known_csts, known_contexts,
				     known_aggs_ptrs)
      + hint_time_bonus (hints)
      + removable_params_cost + est_move_cost;

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

/* Iterate over known values of parameters of NODE and estimate the local
   effects in terms of time and size they have.  */

static void
estimate_local_effects (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);
  vec<tree> known_csts;
  vec<ipa_polymorphic_call_context> known_contexts;
  vec<ipa_agg_jump_function> known_aggs;
  vec<ipa_agg_jump_function_p> known_aggs_ptrs;
  bool always_const;
  int removable_params_cost;

  if (!count || !ipcp_versionable_function_p (node))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEstimating effects for %s.\n", node->dump_name ());

  always_const = gather_context_independent_values (info, &known_csts,
						    &known_contexts, &known_aggs,
						    &removable_params_cost);
  known_aggs_ptrs = agg_jmp_p_vec_for_t_vec (known_aggs);
  int devirt_bonus = devirtualization_time_bonus (node, known_csts,
					   known_contexts, known_aggs_ptrs);
  if (always_const || devirt_bonus
      || (removable_params_cost && node->local.can_change_signature))
    {
      struct caller_statistics stats;
      ipa_hints hints;
      sreal time, base_time;
      int size;

      init_caller_stats (&stats);
      node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats,
					      false);
      estimate_ipcp_clone_size_and_time (node, known_csts, known_contexts,
					 known_aggs_ptrs, &size, &time,
					 &base_time, &hints);
      time -= devirt_bonus;
      time -= hint_time_bonus (hints);
      time -= removable_params_cost;
      size -= stats.n_calls * removable_params_cost;

      if (dump_file)
	fprintf (dump_file, " - context independent values, size: %i, "
		 "time_benefit: %f\n", size, (base_time - time).to_double ());

      if (size <= 0 || node->local.local)
	{
	  info->do_clone_for_all_contexts = true;

	  if (dump_file)
	    fprintf (dump_file, "     Decided to specialize for all "
		     "known contexts, code not going to grow.\n");
	}
      else if (good_cloning_opportunity_p (node,
					   MIN ((base_time - time).to_int (),
						65536),
					   stats.freq_sum, stats.count_sum,
					   size))
	{
	  if (size + overall_size <= max_new_size)
	    {
	      info->do_clone_for_all_contexts = true;
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
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   Not cloning for all contexts because "
		 "!good_cloning_opportunity_p.\n");

    }

  for (i = 0; i < count; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;
      ipcp_value<tree> *val;

      if (lat->bottom
	  || !lat->values
	  || known_csts[i])
	continue;

      for (val = lat->values; val; val = val->next)
	{
	  gcc_checking_assert (TREE_CODE (val->value) != TREE_BINFO);
	  known_csts[i] = val->value;

	  int emc = estimate_move_cost (TREE_TYPE (val->value), true);
	  perform_estimation_of_a_value (node, known_csts, known_contexts,
					 known_aggs_ptrs,
					 removable_params_cost, emc, val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for value ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, ": time_benefit: %i, size: %i\n",
		       val->local_time_benefit, val->local_size_cost);
	    }
	}
      known_csts[i] = NULL_TREE;
    }

  for (i = 0; i < count; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);

      if (!plats->virt_call)
	continue;

      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;
      ipcp_value<ipa_polymorphic_call_context> *val;

      if (ctxlat->bottom
	  || !ctxlat->values
	  || !known_contexts[i].useless_p ())
	continue;

      for (val = ctxlat->values; val; val = val->next)
	{
	  known_contexts[i] = val->value;
	  perform_estimation_of_a_value (node, known_csts, known_contexts,
					 known_aggs_ptrs,
					 removable_params_cost, 0, val);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " - estimates for polymorphic context ");
	      print_ipcp_constant_value (dump_file, val->value);
	      fprintf (dump_file, " for ");
	      ipa_dump_param (dump_file, info, i);
	      fprintf (dump_file, ": time_benefit: %i, size: %i\n",
		       val->local_time_benefit, val->local_size_cost);
	    }
	}
      known_contexts[i] = ipa_polymorphic_call_context ();
    }

  for (i = 0; i < count; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      struct ipa_agg_jump_function *ajf;
      struct ipcp_agg_lattice *aglat;

      if (plats->aggs_bottom || !plats->aggs)
	continue;

      ajf = &known_aggs[i];
      for (aglat = plats->aggs; aglat; aglat = aglat->next)
	{
	  ipcp_value<tree> *val;
	  if (aglat->bottom || !aglat->values
	      /* If the following is true, the one value is in known_aggs.  */
	      || (!plats->aggs_contain_variable
		  && aglat->is_single_const ()))
	    continue;

	  for (val = aglat->values; val; val = val->next)
	    {
	      struct ipa_agg_jf_item item;

	      item.offset = aglat->offset;
	      item.value = val->value;
	      vec_safe_push (ajf->items, item);

	      perform_estimation_of_a_value (node, known_csts, known_contexts,
					     known_aggs_ptrs,
					     removable_params_cost, 0, val);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, " - estimates for value ");
		  print_ipcp_constant_value (dump_file, val->value);
		  fprintf (dump_file, " for ");
		  ipa_dump_param (dump_file, info, i);
		  fprintf (dump_file, "[%soffset: " HOST_WIDE_INT_PRINT_DEC
			   "]: time_benefit: %i, size: %i\n",
			   plats->aggs_by_ref ? "ref " : "",
			   aglat->offset,
			   val->local_time_benefit, val->local_size_cost);
		}

	      ajf->items->pop ();
	    }
	}
    }

  for (i = 0; i < count; i++)
    vec_free (known_aggs[i].items);

  known_csts.release ();
  known_contexts.release ();
  known_aggs.release ();
  known_aggs_ptrs.release ();
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
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i, count = ipa_get_param_count (info);

  for (i = 0; i < count; i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
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
propagate_constants_topo (struct ipa_topo_info *topo)
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
	  push_node_to_stack (topo, v);

      v = pop_node_from_stack (topo);
      while (v)
	{
	  struct cgraph_edge *cs;

	  for (cs = v->callees; cs; cs = cs->next_callee)
	    if (ipa_edge_within_scc (cs))
	      {
		IPA_NODE_REF (v)->node_within_scc = true;
		if (propagate_constants_across_call (cs))
		  push_node_to_stack (topo, cs->callee->function_symbol ());
	      }
	  v = pop_node_from_stack (topo);
	}

      /* Afterwards, propagate along edges leading out of the SCC, calculates
	 the local effects of the discovered constants and all valid values to
	 their topological sort.  */
      FOR_EACH_VEC_ELT (cycle_nodes, j, v)
	if (v->has_gimple_body_p ())
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

template <typename valtype>
void
value_topo_info<valtype>::propagate_effects ()
{
  ipcp_value<valtype> *base;

  for (base = values_topo; base; base = base->topo_next)
    {
      ipcp_value_source<valtype> *src;
      ipcp_value<valtype> *val;
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
	      && src->cs->maybe_hot_p ())
	    {
	      src->val->prop_time_benefit = safe_add (time,
						src->val->prop_time_benefit);
	      src->val->prop_size_cost = safe_add (size,
						   src->val->prop_size_cost);
	    }
    }
}


/* Propagate constants, polymorphic contexts and their effects from the
   summaries interprocedurally.  */

static void
ipcp_propagate_stage (struct ipa_topo_info *topo)
{
  struct cgraph_node *node;

  if (dump_file)
    fprintf (dump_file, "\n Propagating constants:\n\n");

  max_count = profile_count::uninitialized ();

  FOR_EACH_DEFINED_FUNCTION (node)
  {
    struct ipa_node_params *info = IPA_NODE_REF (node);

    determine_versionability (node, info);
    if (node->has_gimple_body_p ())
      {
	info->lattices = XCNEWVEC (struct ipcp_param_lattices,
				   ipa_get_param_count (info));
	initialize_node_lattices (node);
      }
    ipa_fn_summary *s = ipa_fn_summaries->get (node);
    if (node->definition && !node->alias && s != NULL)
      overall_size += s->self_size;
    max_count = max_count.max (node->count.ipa ());
  }

  max_new_size = overall_size;
  if (max_new_size < PARAM_VALUE (PARAM_LARGE_UNIT_INSNS))
    max_new_size = PARAM_VALUE (PARAM_LARGE_UNIT_INSNS);
  max_new_size += max_new_size * PARAM_VALUE (PARAM_IPCP_UNIT_GROWTH) / 100 + 1;

  if (dump_file)
    fprintf (dump_file, "\noverall_size: %li, max_new_size: %li\n",
	     overall_size, max_new_size);

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
				struct ipa_agg_replacement_value *aggvals)
{
  struct cgraph_edge *ie, *next_ie;
  bool found = false;

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      tree target;
      bool speculative;

      next_ie = ie->next_callee;
      target = ipa_get_indirect_edge_target_1 (ie, known_csts, known_contexts,
					       vNULL, aggvals, &speculative);
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
		      && (to_del = node->find_reference (cs->callee, NULL, 0)))
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

struct edge_clone_summary
{
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

  virtual void duplicate (cgraph_edge *src_edge, cgraph_edge *dst_edge,
			  edge_clone_summary *src_data,
			  edge_clone_summary *dst_data);
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

/* See if NODE is a clone with a known aggregate value at a given OFFSET of a
   parameter with the given INDEX.  */

static tree
get_clone_agg_value (struct cgraph_node *node, HOST_WIDE_INT offset,
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

/* Return true is NODE is DEST or its clone for all contexts.  */

static bool
same_node_or_its_all_contexts_clone_p (cgraph_node *node, cgraph_node *dest)
{
  if (node == dest)
    return true;

  struct ipa_node_params *info = IPA_NODE_REF (node);
  return info->is_all_contexts_clone && info->ipcp_orig_node == dest;
}

/* Return true if edge CS does bring about the value described by SRC to
   DEST_VAL of node DEST or its clone for all contexts.  */

static bool
cgraph_edge_brings_value_p (cgraph_edge *cs, ipcp_value_source<tree> *src,
			    cgraph_node *dest, ipcp_value<tree> *dest_val)
{
  struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
  enum availability availability;
  cgraph_node *real_dest = cs->callee->function_symbol (&availability);

  if (!same_node_or_its_all_contexts_clone_p (real_dest, dest)
      || availability <= AVAIL_INTERPOSABLE
      || caller_info->node_dead)
    return false;

  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    {
      tree t;
      if (src->offset == -1)
	t = caller_info->known_csts[src->index];
      else
	t = get_clone_agg_value (cs->caller, src->offset, src->index);
      return (t != NULL_TREE
	      && values_equal_for_ipcp_p (src->val->value, t));
    }
  else
    {
      /* At the moment we do not propagate over arithmetic jump functions in
	 SCCs, so it is safe to detect self-feeding recursive calls in this
	 way.  */
      if (src->val == dest_val)
	return true;

      struct ipcp_agg_lattice *aglat;
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (caller_info,
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
  struct ipa_node_params *caller_info = IPA_NODE_REF (cs->caller);
  cgraph_node *real_dest = cs->callee->function_symbol ();

  if (!same_node_or_its_all_contexts_clone_p (real_dest, dest)
      || caller_info->node_dead)
    return false;
  if (!src->val)
    return true;

  if (caller_info->ipcp_orig_node)
    return (caller_info->known_contexts.length () > (unsigned) src->index)
      && values_equal_for_ipcp_p (src->val->value,
				  caller_info->known_contexts[src->index]);

  struct ipcp_param_lattices *plats = ipa_get_parm_lattices (caller_info,
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
   hold, add their edge frequency and their number into *FREQUENCY and
   *CALLER_COUNT respectively.  */

template <typename valtype>
static bool
get_info_about_necessary_edges (ipcp_value<valtype> *val, cgraph_node *dest,
				int *freq_sum,
				profile_count *count_sum, int *caller_count)
{
  ipcp_value_source<valtype> *src;
  int freq = 0, count = 0;
  profile_count cnt = profile_count::zero ();
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
	      freq += cs->frequency ();
	      if (cs->count.ipa ().initialized_p ())
	        cnt += cs->count.ipa ();
	      hot |= cs->maybe_hot_p ();
	      if (cs->caller != dest)
		non_self_recursive = true;
	    }
	  cs = get_next_cgraph_edge_clone (cs);
	}
    }

  /* If the only edges bringing a value are self-recursive ones, do not bother
     evaluating it.  */
  if (!non_self_recursive)
    return false;

  *freq_sum = freq;
  *count_sum = cnt;
  *caller_count = count;
  return hot;
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

  return ret;
}

/* Construct a replacement map for a know VALUE for a formal parameter PARAM.
   Return it or NULL if for some reason it cannot be created.  */

static struct ipa_replace_map *
get_replacement_map (struct ipa_node_params *info, tree value, int parm_num)
{
  struct ipa_replace_map *replace_map;


  replace_map = ggc_alloc<ipa_replace_map> ();
  if (dump_file)
    {
      fprintf (dump_file, "    replacing ");
      ipa_dump_param (dump_file, info, parm_num);

      fprintf (dump_file, " with const ");
      print_generic_expr (dump_file, value);
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

  fprintf (dump_file, "    setting count of the specialized node to ");
  new_node->count.dump (dump_file);
  fprintf (dump_file, "\n");
  for (cs = new_node->callees; cs; cs = cs->next_callee)
    {
      fprintf (dump_file, "      edge to %s has count ",
	       cs->callee->name ());
      cs->count.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  fprintf (dump_file, "    setting count of the original node to ");
  orig_node->count.dump (dump_file);
  fprintf (dump_file, "\n");
  for (cs = orig_node->callees; cs; cs = cs->next_callee)
    {
      fprintf (dump_file, "      edge to %s is left with ",
	       cs->callee->name ());
      cs->count.dump (dump_file);
      fprintf (dump_file, "\n");
    }
}

/* After a specialized NEW_NODE version of ORIG_NODE has been created, update
   their profile information to reflect this.  */

static void
update_profiling_info (struct cgraph_node *orig_node,
		       struct cgraph_node *new_node)
{
  struct cgraph_edge *cs;
  struct caller_statistics stats;
  profile_count new_sum, orig_sum;
  profile_count remainder, orig_node_count = orig_node->count;

  if (!(orig_node_count.ipa () > profile_count::zero ()))
    return;

  init_caller_stats (&stats);
  orig_node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats,
					       false);
  orig_sum = stats.count_sum;
  init_caller_stats (&stats);
  new_node->call_for_symbol_thunks_and_aliases (gather_caller_stats, &stats,
					      false);
  new_sum = stats.count_sum;

  if (orig_node_count < orig_sum + new_sum)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "    Problem: node %s has too low count ",
		   orig_node->dump_name ());
	  orig_node_count.dump (dump_file);
	  fprintf (dump_file, "while the sum of incoming count is ");
	  (orig_sum + new_sum).dump (dump_file);
	  fprintf (dump_file, "\n");
	}

      orig_node_count = (orig_sum + new_sum).apply_scale (12, 10);
      if (dump_file)
	{
	  fprintf (dump_file, "      proceeding by pretending it was ");
	  orig_node_count.dump (dump_file);
	  fprintf (dump_file, "\n");
	}
    }

  remainder = orig_node_count.combine_with_ipa_count (orig_node_count.ipa ()
						      - new_sum.ipa ());
  new_sum = orig_node_count.combine_with_ipa_count (new_sum);
  orig_node->count = remainder;

  profile_count::adjust_for_ipa_scaling (&new_sum, &orig_node_count);
  for (cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (new_sum, orig_node_count);

  profile_count::adjust_for_ipa_scaling (&remainder, &orig_node_count);
  for (cs = orig_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (remainder, orig_node_count);

  if (dump_file)
    dump_profile_updates (orig_node, new_node);
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
  profile_count new_node_count, orig_node_count = orig_node->count;

  if (dump_file)
    {
      fprintf (dump_file, "    the sum of counts of redirected  edges is ");
      redirected_sum.dump (dump_file);
      fprintf (dump_file, "\n");
    }
  if (!(orig_node_count > profile_count::zero ()))
    return;

  gcc_assert (orig_node_count >= redirected_sum);

  new_node_count = new_node->count;
  new_node->count += redirected_sum;
  orig_node->count -= redirected_sum;

  for (cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count += cs->count.apply_scale (redirected_sum, new_node_count);

  for (cs = orig_node->callees; cs; cs = cs->next_callee)
    {
      profile_count dec = cs->count.apply_scale (redirected_sum,
						 orig_node_count);
      cs->count -= dec;
    }

  if (dump_file)
    dump_profile_updates (orig_node, new_node);
}

/* Create a specialized version of NODE with known constants in KNOWN_CSTS,
   known contexts in KNOWN_CONTEXTS and known aggregate values in AGGVALS and
   redirect all edges in CALLERS to it.  */

static struct cgraph_node *
create_specialized_node (struct cgraph_node *node,
			 vec<tree> known_csts,
			 vec<ipa_polymorphic_call_context> known_contexts,
			 struct ipa_agg_replacement_value *aggvals,
			 vec<cgraph_edge *> callers)
{
  struct ipa_node_params *new_info, *info = IPA_NODE_REF (node);
  vec<ipa_replace_map *, va_gc> *replace_trees = NULL;
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
	  tree t = known_csts[i];

	  if (t || !ipa_is_param_used (info, i))
	    bitmap_set_bit (args_to_skip, i);
	}
    }
  else
    {
      args_to_skip = NULL;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "      cannot change function signature\n");
    }

  for (i = 0; i < count; i++)
    {
      tree t = known_csts[i];
      if (t)
	{
	  struct ipa_replace_map *replace_map;

	  gcc_checking_assert (TREE_CODE (t) != TREE_BINFO);
	  replace_map = get_replacement_map (info, t, i);
	  if (replace_map)
	    vec_safe_push (replace_trees, replace_map);
	}
    }
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

  unsigned &suffix_counter = clone_num_suffixes->get_or_insert (
			       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (
				 node->decl)));
  new_node = node->create_virtual_clone (callers, replace_trees,
					 args_to_skip, "constprop",
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
  for (av = aggvals; av; av = av->next)
    new_node->maybe_create_reference (av->value, NULL);

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
	ipa_dump_agg_replacement_values (dump_file, aggvals);
    }
  ipa_check_create_node_params ();
  update_profiling_info (node, new_node);
  new_info = IPA_NODE_REF (new_node);
  new_info->ipcp_orig_node = node;
  new_info->known_csts = known_csts;
  new_info->known_contexts = known_contexts;

  ipcp_discover_new_direct_edges (new_node, known_csts, known_contexts, aggvals);

  callers.release ();
  return new_node;
}

/* Return true, if JFUNC, which describes a i-th parameter of call CS, is a
   simple no-operation pass-through function to itself.  */

static bool
self_recursive_pass_through_p (cgraph_edge *cs, ipa_jump_func *jfunc, int i)
{
  enum availability availability;
  if (cs->caller == cs->callee->function_symbol (&availability)
      && availability > AVAIL_INTERPOSABLE
      && jfunc->type == IPA_JF_PASS_THROUGH
      && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR
      && ipa_get_jf_pass_through_formal_id (jfunc) == i)
    return true;
  return false;
}

/* Given a NODE, and a subset of its CALLERS, try to populate blanks slots in
   KNOWN_CSTS with constants that are also known for all of the CALLERS.  */

static void
find_more_scalar_values_for_callers_subset (struct cgraph_node *node,
					    vec<tree> known_csts,
					    vec<cgraph_edge *> callers)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
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

	  if (IPA_NODE_REF (cs->caller)->node_dead)
	    continue;

	  if (i >= ipa_get_cs_argument_count (IPA_EDGE_REF (cs))
	      || (i == 0
		  && call_passes_through_thunk_p (cs)))
	    {
	      newval = NULL_TREE;
	      break;
	    }
	  jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  if (self_recursive_pass_through_p (cs, jump_func, i))
	    continue;

	  t = ipa_value_from_jfunc (IPA_NODE_REF (cs->caller), jump_func, type);
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
				      vec<cgraph_edge *> callers)
{
  ipa_node_params *info = IPA_NODE_REF (node);
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
	  if (i >= ipa_get_cs_argument_count (IPA_EDGE_REF (cs)))
	    return;
	  ipa_jump_func *jfunc = ipa_get_ith_jump_func (IPA_EDGE_REF (cs),
							    i);
	  ipa_polymorphic_call_context ctx;
	  ctx = ipa_context_from_jfunc (IPA_NODE_REF (cs->caller), cs, i,
					jfunc);
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
	    known_contexts->safe_grow_cleared (ipa_get_param_count (info));
	  (*known_contexts)[i] = newval;
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
    if (aglat->is_single_const ())
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
	      if (aglat->is_single_const ()
		  && values_equal_for_ipcp_p (item->value,
					      aglat->values->value))
		found = true;
	      break;
	    }
	  aglat = aglat->next;
	}
      if (!found)
	item->value = NULL_TREE;
    }
}

/* Copy aggregate replacement values of NODE (which is an IPA-CP clone) to the
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
	  else
	    {
	      inter.release ();
	      return vNULL;
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
	  else
	    {
	      inter.release ();
	      return vNULL;
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
	  src_plats = ipa_get_parm_lattices (caller_info, src_idx);
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
	    bool found = false;

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
					  vec<cgraph_edge *> callers)
{
  struct ipa_node_params *dest_info = IPA_NODE_REF (node);
  struct ipa_agg_replacement_value *res;
  struct ipa_agg_replacement_value **tail = &res;
  struct cgraph_edge *cs;
  int i, j, count = ipa_get_param_count (dest_info);

  FOR_EACH_VEC_ELT (callers, j, cs)
    {
      int c = ipa_get_cs_argument_count (IPA_EDGE_REF (cs));
      if (c < count)
	count = c;
    }

  for (i = 0; i < count; i++)
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
	  struct ipa_jump_func *jfunc
	    = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  if (self_recursive_pass_through_p (cs, jfunc, i)
	      && (!plats->aggs_by_ref
		  || ipa_get_jf_pass_through_agg_preserved (jfunc)))
	    continue;
	  inter = intersect_aggregates_with_edge (cs, i, inter);

	  if (!inter.exists ())
	    goto next_param;
	}

      FOR_EACH_VEC_ELT (inter, j, item)
	{
	  struct ipa_agg_replacement_value *v;

	  if (!item->value)
	    continue;

	  v = ggc_alloc<ipa_agg_replacement_value> ();
	  v->index = i;
	  v->offset = item->offset;
	  v->value = item->value;
	  v->by_ref = plats->aggs_by_ref;
	  *tail = v;
	  tail = &v->next;
	}

    next_param:
      if (inter.exists ())
	inter.release ();
    }
  *tail = NULL;
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
copy_useful_known_contexts (vec<ipa_polymorphic_call_context> known_contexts)
{
  if (known_contexts_useful_p (known_contexts))
    return known_contexts.copy ();
  else
    return vNULL;
}

/* Copy KNOWN_CSTS and modify the copy according to VAL and INDEX.  If
   non-empty, replace KNOWN_CONTEXTS with its copy too.  */

static void
modify_known_vectors_with_val (vec<tree> *known_csts,
			       vec<ipa_polymorphic_call_context> *known_contexts,
			       ipcp_value<tree> *val,
			       int index)
{
  *known_csts = known_csts->copy ();
  *known_contexts = copy_useful_known_contexts (*known_contexts);
  (*known_csts)[index] = val->value;
}

/* Replace KNOWN_CSTS with its copy.  Also copy KNOWN_CONTEXTS and modify the
   copy according to VAL and INDEX.  */

static void
modify_known_vectors_with_val (vec<tree> *known_csts,
			       vec<ipa_polymorphic_call_context> *known_contexts,
			       ipcp_value<ipa_polymorphic_call_context> *val,
			       int index)
{
  *known_csts = known_csts->copy ();
  *known_contexts = known_contexts->copy ();
  (*known_contexts)[index] = val->value;
}

/* Return true if OFFSET indicates this was not an aggregate value or there is
   a replacement equivalent to VALUE, INDEX and OFFSET among those in the
   AGGVALS list.  */

DEBUG_FUNCTION bool
ipcp_val_agg_replacement_ok_p (ipa_agg_replacement_value *aggvals,
			       int index, HOST_WIDE_INT offset, tree value)
{
  if (offset == -1)
    return true;

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

/* Return true if offset is minus one because source of a polymorphic context
   cannot be an aggregate value.  */

DEBUG_FUNCTION bool
ipcp_val_agg_replacement_ok_p (ipa_agg_replacement_value *,
			       int , HOST_WIDE_INT offset,
			       ipa_polymorphic_call_context)
{
  return offset == -1;
}

/* Decide whether to create a special version of NODE for value VAL of parameter
   at the given INDEX.  If OFFSET is -1, the value is for the parameter itself,
   otherwise it is stored at the given OFFSET of the parameter.  KNOWN_CSTS,
   KNOWN_CONTEXTS and KNOWN_AGGS describe the other already known values.  */

template <typename valtype>
static bool
decide_about_value (struct cgraph_node *node, int index, HOST_WIDE_INT offset,
		    ipcp_value<valtype> *val, vec<tree> known_csts,
		    vec<ipa_polymorphic_call_context> known_contexts)
{
  struct ipa_agg_replacement_value *aggvals;
  int freq_sum, caller_count;
  profile_count count_sum;
  vec<cgraph_edge *> callers;

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
  else if (!get_info_about_necessary_edges (val, node, &freq_sum, &count_sum,
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
    fprintf (dump_file, "  Creating a specialized node of %s.\n",
	     node->dump_name ());

  callers = gather_edges_for_value (val, node, caller_count);
  if (offset == -1)
    modify_known_vectors_with_val (&known_csts, &known_contexts, val, index);
  else
    {
      known_csts = known_csts.copy ();
      known_contexts = copy_useful_known_contexts (known_contexts);
    }
  find_more_scalar_values_for_callers_subset (node, known_csts, callers);
  find_more_contexts_for_caller_subset (node, &known_contexts, callers);
  aggvals = find_aggregate_values_for_callers_subset (node, callers);
  gcc_checking_assert (ipcp_val_agg_replacement_ok_p (aggvals, index,
						      offset, val->value));
  val->spec_node = create_specialized_node (node, known_csts, known_contexts,
					    aggvals, callers);
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
  vec<tree> known_csts;
  vec<ipa_polymorphic_call_context> known_contexts;
  vec<ipa_agg_jump_function> known_aggs = vNULL;
  bool ret = false;

  if (count == 0)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEvaluating opportunities for %s.\n",
	     node->dump_name ());

  gather_context_independent_values (info, &known_csts, &known_contexts,
				  info->do_clone_for_all_contexts ? &known_aggs
				  : NULL, NULL);

  for (i = 0; i < count;i++)
    {
      struct ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
      ipcp_lattice<tree> *lat = &plats->itself;
      ipcp_lattice<ipa_polymorphic_call_context> *ctxlat = &plats->ctxlat;

      if (!lat->bottom
	  && !known_csts[i])
	{
	  ipcp_value<tree> *val;
	  for (val = lat->values; val; val = val->next)
	    ret |= decide_about_value (node, i, -1, val, known_csts,
				       known_contexts);
	}

      if (!plats->aggs_bottom)
	{
	  struct ipcp_agg_lattice *aglat;
	  ipcp_value<tree> *val;
	  for (aglat = plats->aggs; aglat; aglat = aglat->next)
	    if (!aglat->bottom && aglat->values
		/* If the following is false, the one value is in
		   known_aggs.  */
		&& (plats->aggs_contain_variable
		    || !aglat->is_single_const ()))
	      for (val = aglat->values; val; val = val->next)
		ret |= decide_about_value (node, i, aglat->offset, val,
					   known_csts, known_contexts);
	}

      if (!ctxlat->bottom
	  && known_contexts[i].useless_p ())
	{
	  ipcp_value<ipa_polymorphic_call_context> *val;
	  for (val = ctxlat->values; val; val = val->next)
	    ret |= decide_about_value (node, i, -1, val, known_csts,
				       known_contexts);
	}

	info = IPA_NODE_REF (node);
    }

  if (info->do_clone_for_all_contexts)
    {
      struct cgraph_node *clone;
      vec<cgraph_edge *> callers;

      if (dump_file)
	fprintf (dump_file, " - Creating a specialized node of %s "
		 "for all known contexts.\n", node->dump_name ());

      callers = node->collect_callers ();
      find_more_scalar_values_for_callers_subset (node, known_csts, callers);
      find_more_contexts_for_caller_subset (node, &known_contexts, callers);
      ipa_agg_replacement_value *aggvals
	= find_aggregate_values_for_callers_subset (node, callers);

      if (!known_contexts_useful_p (known_contexts))
	{
	  known_contexts.release ();
	  known_contexts = vNULL;
	}
      clone = create_specialized_node (node, known_csts, known_contexts,
				       aggvals, callers);
      info = IPA_NODE_REF (node);
      info->do_clone_for_all_contexts = false;
      IPA_NODE_REF (clone)->is_all_contexts_clone = true;
      for (i = 0; i < count; i++)
	vec_free (known_aggs[i].items);
      known_aggs.release ();
      ret = true;
    }
  else
    {
      known_csts.release ();
      known_contexts.release ();
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
	struct ipa_node_params *info;

	callee = cs->callee->function_symbol (NULL);
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
	&& cs->caller->call_for_symbol_thunks_and_aliases
	  (has_undead_caller_from_outside_scc_p, NULL, true))
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
  for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    if (v->local.local
	&& !v->call_for_symbol_thunks_and_aliases
	     (has_undead_caller_from_outside_scc_p, NULL, true))
      IPA_NODE_REF (v)->node_dead = 1;

  for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
    if (!IPA_NODE_REF (v)->node_dead)
      spread_undeadness (v);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      for (v = node; v; v = ((struct ipa_dfs_info *) v->aux)->next_cycle)
	if (IPA_NODE_REF (v)->node_dead)
	  fprintf (dump_file, "  Marking node as dead: %s.\n", v->dump_name ());
    }
}

/* The decision stage.  Iterate over the topological order of call graph nodes
   TOPO and make specialized clones if deemed beneficial.  */

static void
ipcp_decision_stage (struct ipa_topo_info *topo)
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

/* Look up all the bits information that we have discovered and copy it over
   to the transformation summary.  */

static void
ipcp_store_bits_results (void)
{
  cgraph_node *node;

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      ipa_node_params *info = IPA_NODE_REF (node);
      bool dumped_sth = false;
      bool found_useful_result = false;

      if (!opt_for_fn (node->decl, flag_ipa_bit_cp))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not considering %s for ipa bitwise propagation "
				"; -fipa-bit-cp: disabled.\n",
				node->name ());
	  continue;
	}

      if (info->ipcp_orig_node)
	info = IPA_NODE_REF (info->ipcp_orig_node);

      unsigned count = ipa_get_param_count (info);
      for (unsigned i = 0; i < count; i++)
	{
	  ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  if (plats->bits_lattice.constant_p ())
	    {
	      found_useful_result = true;
	      break;
	    }
	}

      if (!found_useful_result)
	continue;

      ipcp_transformation_initialize ();
      ipcp_transformation *ts = ipcp_transformation_sum->get_create (node);
      vec_safe_reserve_exact (ts->bits, count);

      for (unsigned i = 0; i < count; i++)
	{
	  ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  ipa_bits *jfbits;

	  if (plats->bits_lattice.constant_p ())
	    jfbits
	      = ipa_get_ipa_bits_for_value (plats->bits_lattice.get_value (),
					    plats->bits_lattice.get_mask ());
	  else
	    jfbits = NULL;

	  ts->bits->quick_push (jfbits);
	  if (!dump_file || !jfbits)
	    continue;
	  if (!dumped_sth)
	    {
	      fprintf (dump_file, "Propagated bits info for function %s:\n",
		       node->dump_name ());
	      dumped_sth = true;
	    }
	  fprintf (dump_file, " param %i: value = ", i);
	  print_hex (jfbits->value, dump_file);
	  fprintf (dump_file, ", mask = ");
	  print_hex (jfbits->mask, dump_file);
	  fprintf (dump_file, "\n");
	}
    }
}

/* Look up all VR information that we have discovered and copy it over
   to the transformation summary.  */

static void
ipcp_store_vr_results (void)
{
  cgraph_node *node;

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      ipa_node_params *info = IPA_NODE_REF (node);
      bool found_useful_result = false;

      if (!opt_for_fn (node->decl, flag_ipa_vrp))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not considering %s for VR discovery "
		     "and propagate; -fipa-ipa-vrp: disabled.\n",
		     node->name ());
	  continue;
	}

      if (info->ipcp_orig_node)
	info = IPA_NODE_REF (info->ipcp_orig_node);

      unsigned count = ipa_get_param_count (info);
      for (unsigned i = 0; i < count; i++)
	{
	  ipcp_param_lattices *plats = ipa_get_parm_lattices (info, i);
	  if (!plats->m_value_range.bottom_p ()
	      && !plats->m_value_range.top_p ())
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
	  ipa_vr vr;

	  if (!plats->m_value_range.bottom_p ()
	      && !plats->m_value_range.top_p ())
	    {
	      vr.known = true;
	      vr.type = plats->m_value_range.m_vr.kind ();
	      vr.min = wi::to_wide (plats->m_value_range.m_vr.min ());
	      vr.max = wi::to_wide (plats->m_value_range.m_vr.max ());
	    }
	  else
	    {
	      vr.known = false;
	      vr.type = VR_VARYING;
	      vr.min = vr.max = wi::zero (INT_TYPE_SIZE);
	    }
	  ts->m_vr->quick_push (vr);
	}
    }
}

/* The IPCP driver.  */

static unsigned int
ipcp_driver (void)
{
  struct ipa_topo_info topo;

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
  /* Store results of bits propagation.  */
  ipcp_store_bits_results ();
  /* Store results of value range propagation.  */
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
		      ipcp_write_summary, /* write_summary */
		      ipcp_read_summary, /* read_summary */
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
  virtual bool gate (function *)
    {
      /* FIXME: We should remove the optimize check after we ensure we never run
	 IPA passes when not optimizing.  */
      return (flag_ipa_cp && optimize) || in_lto_p;
    }

  virtual unsigned int execute (function *) { return ipcp_driver (); }

}; // class pass_ipa_cp

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_cp (gcc::context *ctxt)
{
  return new pass_ipa_cp (ctxt);
}

/* Reset all state within ipa-cp.c so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_cp_c_finalize (void)
{
  max_count = profile_count::uninitialized ();
  overall_size = 0;
  max_new_size = 0;
}
