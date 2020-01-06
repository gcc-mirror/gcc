/* IPA function body analysis.
   Copyright (C) 2003-2020 Free Software Foundation, Inc.
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

#ifndef GCC_IPA_SUMMARY_H
#define GCC_IPA_SUMMARY_H

#include "sreal.h"
#include "ipa-predicate.h"


/* Hints are reasons why IPA heuristics should prefer specializing given
   function.  They are represented as bitmap of the following values.  */
enum ipa_hints_vals {
  /* When specialization turns indirect call into a direct call,
     it is good idea to do so.  */
  INLINE_HINT_indirect_call = 1,
  /* Inlining may make loop iterations or loop stride known.  It is good idea
     to do so because it enables loop optimizations.  */
  INLINE_HINT_loop_iterations = 2,
  INLINE_HINT_loop_stride = 4,
  /* Inlining within same strongly connected component of callgraph is often
     a loss due to increased stack frame usage and prologue setup costs.  */
  INLINE_HINT_same_scc = 8,
  /* Inlining functions in strongly connected component is not such a great
     win.  */
  INLINE_HINT_in_scc = 16,
  /* If function is declared inline by user, it may be good idea to inline
     it.  Set by simple_edge_hints in ipa-inline-analysis.c.  */
  INLINE_HINT_declared_inline = 32,
  /* Programs are usually still organized for non-LTO compilation and thus
     if functions are in different modules, inlining may not be so important. 
     Set by simple_edge_hints in ipa-inline-analysis.c.   */
  INLINE_HINT_cross_module = 64,
  /* We know that the callee is hot by profile.  */
  INLINE_HINT_known_hot = 128
};

typedef int ipa_hints;

/* Simple description of whether a memory load or a condition refers to a load
   from an aggregate and if so, how and where from in the aggregate.
   Individual fields have the same meaning like fields with the same name in
   struct condition.  */

struct agg_position_info
{
  HOST_WIDE_INT offset;
  bool agg_contents;
  bool by_ref;
};

/* Representation of function body size and time depending on the call
   context.  We keep simple array of record, every containing of predicate
   and time/size to account.  */
class GTY(()) size_time_entry
{
public:
  /* Predicate for code to be executed.  */
  predicate exec_predicate;
  /* Predicate for value to be constant and optimized out in a specialized copy.
     When deciding on specialization this makes it possible to see how much
     the executed code paths will simplify.  */
  predicate nonconst_predicate;
  int size;
  sreal GTY((skip)) time;
};

/* Summary about function and stack frame sizes.  We keep this info 
   for inline clones and also for WPA streaming. For this reason this is not
   part of ipa_fn_summary which exists only for offline functions.  */
class ipa_size_summary
{
public:
  /* Estimated stack frame consumption by the function.  */
  HOST_WIDE_INT estimated_self_stack_size;
  /* Size of the function body.  */
  int self_size;
  /* Estimated size of the function after inlining.  */
  int size;

  ipa_size_summary ()
  : estimated_self_stack_size (0), self_size (0), size (0)
  {
  }
};

/* Function inlining information.  */
class GTY(()) ipa_fn_summary
{
public:
  /* Keep all field empty so summary dumping works during its computation.
     This is useful for debugging.  */
  ipa_fn_summary ()
    : min_size (0),
      inlinable (false), single_caller (false),
      fp_expressions (false), estimated_stack_size (false),
      time (0), conds (NULL),
      size_time_table (NULL), call_size_time_table (NULL), loop_iterations (NULL),
      loop_stride (NULL), growth (0), scc_no (0)
  {
  }

  /* Copy constructor.  */
  ipa_fn_summary (const ipa_fn_summary &s)
    : min_size (s.min_size),
    inlinable (s.inlinable), single_caller (s.single_caller),
    fp_expressions (s.fp_expressions),
    estimated_stack_size (s.estimated_stack_size),
    time (s.time), conds (s.conds), size_time_table (s.size_time_table),
    call_size_time_table (NULL),
    loop_iterations (s.loop_iterations), loop_stride (s.loop_stride),
    growth (s.growth), scc_no (s.scc_no)
  {}

  /* Default constructor.  */
  ~ipa_fn_summary ();

  /* Information about the function body itself.  */

  /* Minimal size increase after inlining.  */
  int min_size;

  /* False when there something makes inlining impossible (such as va_arg).  */
  unsigned inlinable : 1;
  /* True wen there is only one caller of the function before small function
     inlining.  */
  unsigned int single_caller : 1;
  /* True if function contains any floating point expressions.  */
  unsigned int fp_expressions : 1;

  /* Information about function that will result after applying all the
     inline decisions present in the callgraph.  Generally kept up to
     date only for functions that are not inline clones. */

  /* Estimated stack frame consumption by the function.  */
  HOST_WIDE_INT estimated_stack_size;
  /* Estimated runtime of function after inlining.  */
  sreal GTY((skip)) time;

  /* Conditional size/time information.  The summaries are being
     merged during inlining.  */
  conditions conds;
  /* Normal code is accounted in size_time_table, while calls are
     accounted in call_size_time_table.  This is because calls
     are often adjusted by IPA optimizations and thus this summary
     is generated from call summary information when needed.  */
  vec<size_time_entry, va_gc> *size_time_table;
  vec<size_time_entry, va_gc> *call_size_time_table;

  /* Predicate on when some loop in the function becomes to have known
     bounds.   */
  predicate * GTY((skip)) loop_iterations;
  /* Predicate on when some loop in the function becomes to have known
     stride.   */
  predicate * GTY((skip)) loop_stride;
  /* Estimated growth for inlining all copies of the function before start
     of small functions inlining.
     This value will get out of date as the callers are duplicated, but
     using up-to-date value in the badness metric mean a lot of extra
     expenses.  */
  int growth;
  /* Number of SCC on the beginning of inlining process.  */
  int scc_no;

  /* Record time and size under given predicates.  */
  void account_size_time (int, sreal, const predicate &, const predicate &,
		  	  bool call = false);

  /* We keep values scaled up, so fractional sizes can be accounted.  */
  static const int size_scale = 2;
  /* Maximal size of size_time_table before we start to be conservative.  */
  static const int max_size_time_table_size = 256;
};

class GTY((user)) ipa_fn_summary_t:
  public fast_function_summary <ipa_fn_summary *, va_gc>
{
public:
  ipa_fn_summary_t (symbol_table *symtab):
    fast_function_summary <ipa_fn_summary *, va_gc> (symtab) {}

  static ipa_fn_summary_t *create_ggc (symbol_table *symtab)
  {
    class ipa_fn_summary_t *summary
      = new (ggc_alloc_no_dtor<ipa_fn_summary_t> ()) ipa_fn_summary_t (symtab);
    summary->disable_insertion_hook ();
    return summary;
  }

  /* Remove ipa_fn_summary for all callees of NODE.  */
  void remove_callees (cgraph_node *node);

  virtual void insert (cgraph_node *, ipa_fn_summary *);
  virtual void remove (cgraph_node *node, ipa_fn_summary *)
  {
    remove_callees (node);
  }

  virtual void duplicate (cgraph_node *src, cgraph_node *dst,
			  ipa_fn_summary *src_data, ipa_fn_summary *dst_data);
};

extern GTY(()) fast_function_summary <ipa_fn_summary *, va_gc>
  *ipa_fn_summaries;

class ipa_size_summary_t:
  public fast_function_summary <ipa_size_summary *, va_heap>
{
public:
  ipa_size_summary_t (symbol_table *symtab):
    fast_function_summary <ipa_size_summary *, va_heap> (symtab)
  {
    disable_insertion_hook ();
  }

  virtual void duplicate (cgraph_node *, cgraph_node *,
			  ipa_size_summary *src_data,
			  ipa_size_summary *dst_data)
  {
    *dst_data = *src_data;
  }
};
extern fast_function_summary <ipa_size_summary *, va_heap>
  *ipa_size_summaries;

/* Information kept about callgraph edges.  */
class ipa_call_summary
{
public:
  /* Keep all field empty so summary dumping works during its computation.
     This is useful for debugging.  */
  ipa_call_summary ()
    : predicate (NULL), param (vNULL), call_stmt_size (0), call_stmt_time (0),
      loop_depth (0), is_return_callee_uncaptured (false)
    {
    }

  /* Copy constructor.  */
  ipa_call_summary (const ipa_call_summary &s):
    predicate (s.predicate), param (s.param), call_stmt_size (s.call_stmt_size),
    call_stmt_time (s.call_stmt_time), loop_depth (s.loop_depth),
    is_return_callee_uncaptured (s.is_return_callee_uncaptured)
  {
  }

  /* Default destructor.  */
  ~ipa_call_summary ();

  class predicate *predicate;
  /* Vector indexed by parameters.  */
  vec<inline_param_summary> param;
  /* Estimated size and time of the call statement.  */
  int call_stmt_size;
  int call_stmt_time;
  /* Depth of loop nest, 0 means no nesting.  */
  unsigned int loop_depth;
  /* Indicates whether the caller returns the value of it's callee.  */
  bool is_return_callee_uncaptured;
};

class ipa_call_summary_t: public fast_call_summary <ipa_call_summary *, va_heap>
{
public:
  ipa_call_summary_t (symbol_table *symtab):
    fast_call_summary <ipa_call_summary *, va_heap> (symtab) {}

  /* Hook that is called by summary when an edge is duplicated.  */
  virtual void duplicate (cgraph_edge *src, cgraph_edge *dst,
			  ipa_call_summary *src_data,
			  ipa_call_summary *dst_data);
};

/* This object describe a context of call.  That is a summary of known
   information about its parameters.  Main purpose of this context is
   to give more realistic estimations of function runtime, size and
   inline hints.  */
class ipa_call_context
{
public:
  ipa_call_context (cgraph_node *node,
      		    clause_t possible_truths,
		    clause_t nonspec_possible_truths,
		    vec<tree> known_vals,
		    vec<ipa_polymorphic_call_context> known_contexts,
		    vec<ipa_agg_value_set> known_aggs,
		    vec<inline_param_summary> m_inline_param_summary);
  ipa_call_context ()
  : m_node(NULL)
  {
  }
  void estimate_size_and_time (int *ret_size, int *ret_min_size,
			       sreal *ret_time,
			       sreal *ret_nonspecialized_time,
			       ipa_hints *ret_hints);
  void duplicate_from (const ipa_call_context &ctx);
  void release (bool all = false);
  bool equal_to (const ipa_call_context &);
  bool exists_p ()
  {
    return m_node != NULL;
  }
private:
  /* Called function.  */
  cgraph_node *m_node;
  /* Clause describing what predicate conditionals can be satisfied
     in this context if function is inlined/specialized.  */
  clause_t m_possible_truths;
  /* Clause describing what predicate conditionals can be satisfied
     in this context if function is kept offline.  */
  clause_t m_nonspec_possible_truths;
  /* Inline summary maintains info about change probabilities.  */
  vec<inline_param_summary> m_inline_param_summary;

  /* The following is used only to resolve indirect calls.  */

  /* Vector describing known values of parameters.  */
  vec<tree> m_known_vals;
  /* Vector describing known polymorphic call contexts.  */
  vec<ipa_polymorphic_call_context> m_known_contexts;
  /* Vector describing known aggregate values.  */
  vec<ipa_agg_value_set> m_known_aggs;
};

extern fast_call_summary <ipa_call_summary *, va_heap> *ipa_call_summaries;

/* In ipa-fnsummary.c  */
void ipa_debug_fn_summary (struct cgraph_node *);
void ipa_dump_fn_summaries (FILE *f);
void ipa_dump_fn_summary (FILE *f, struct cgraph_node *node);
void ipa_dump_hints (FILE *f, ipa_hints);
void ipa_free_fn_summary (void);
void ipa_free_size_summary (void);
void inline_analyze_function (struct cgraph_node *node);
void estimate_ipcp_clone_size_and_time (struct cgraph_node *,
					vec<tree>,
					vec<ipa_polymorphic_call_context>,
					vec<ipa_agg_value_set>,
					int *, sreal *, sreal *,
				        ipa_hints *);
void ipa_merge_fn_summary_after_inlining (struct cgraph_edge *edge);
void ipa_update_overall_fn_summary (struct cgraph_node *node, bool reset = true);
void compute_fn_summary (struct cgraph_node *, bool);


void evaluate_properties_for_edge (struct cgraph_edge *e,
	       		           bool inline_p,
				   clause_t *clause_ptr,
				   clause_t *nonspec_clause_ptr,
				   vec<tree> *known_vals_ptr,
				   vec<ipa_polymorphic_call_context>
				   *known_contexts_ptr,
				   vec<ipa_agg_value_set> *);

void ipa_fnsummary_c_finalize (void);
HOST_WIDE_INT ipa_get_stack_frame_offset (struct cgraph_node *node);
void ipa_remove_from_growth_caches (struct cgraph_edge *edge);

/* Return true if EDGE is a cross module call.  */

static inline bool
cross_module_call_p (struct cgraph_edge *edge)
{
  /* Here we do not want to walk to alias target becuase ICF may create
     cross-unit aliases.  */
  if (edge->caller->unit_id == edge->callee->unit_id)
    return false;
  /* If the call is to a (former) comdat function or s symbol with mutiple
     extern inline definitions then treat is as in-module call.  */
  if (edge->callee->merged_extern_inline || edge->callee->merged_comdat
      || DECL_COMDAT (edge->callee->decl))
    return false;
  return true;
}

#endif /* GCC_IPA_FNSUMMARY_H */
