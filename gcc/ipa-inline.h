/* Inlining decision heuristics.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

#ifndef GCC_IPA_INLINE_H
#define GCC_IPA_INLINE_H

#include "sreal.h"
#include "ipa-predicate.h"


/* Inline hints are reasons why inline heuristics should preffer inlining given
   function.  They are represtented as bitmap of the following values.  */
enum inline_hints_vals {
  /* When inlining turns indirect call into a direct call,
     it is good idea to do so.  */
  INLINE_HINT_indirect_call = 1,
  /* Inlining may make loop iterations or loop stride known.  It is good idea
     to do so because it enables loop optimizatoins.  */
  INLINE_HINT_loop_iterations = 2,
  INLINE_HINT_loop_stride = 4,
  /* Inlining within same strongly connected component of callgraph is often
     a loss due to increased stack frame usage and prologue setup costs.  */
  INLINE_HINT_same_scc = 8,
  /* Inlining functions in strongly connected component is not such a great
     win.  */
  INLINE_HINT_in_scc = 16,
  /* If function is declared inline by user, it may be good idea to inline
     it.  */
  INLINE_HINT_declared_inline = 32,
  /* Programs are usually still organized for non-LTO compilation and thus
     if functions are in different modules, inlining may not be so important. 
   */
  INLINE_HINT_cross_module = 64,
  /* If array indexes of loads/stores become known there may be room for
     further optimization.  */
  INLINE_HINT_array_index = 128,
  /* We know that the callee is hot by profile.  */
  INLINE_HINT_known_hot = 256
};

typedef int inline_hints;

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

/* Represnetation of function body size and time depending on the inline
   context.  We keep simple array of record, every containing of predicate
   and time/size to account.

   We keep values scaled up, so fractional sizes can be accounted.  */
#define INLINE_SIZE_SCALE 2
struct GTY(()) size_time_entry
{
  /* Predicate for code to be executed.  */
  predicate exec_predicate;
  /* Predicate for value to be constant and optimized out in a specialized copy.
     When deciding on specialization this makes it possible to see how much
     the executed code paths will simplify.  */
  predicate nonconst_predicate;
  int size;
  sreal GTY((skip)) time;
};

/* Function inlining information.  */
struct GTY(()) inline_summary
{
  /* Information about the function body itself.  */

  /* Estimated stack frame consumption by the function.  */
  HOST_WIDE_INT estimated_self_stack_size;
  /* Size of the function body.  */
  int self_size;
  /* Time of the function body.  */
  sreal GTY((skip)) self_time;
  /* Minimal size increase after inlining.  */
  int min_size;

  /* False when there something makes inlining impossible (such as va_arg).  */
  unsigned inlinable : 1;
  /* True when function contains cilk spawn (and thus we can not inline
     into it).  */
  unsigned contains_cilk_spawn : 1;
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
  /* Expected offset of the stack frame of inlined function.  */
  HOST_WIDE_INT stack_frame_offset;
  /* Estimated size of the function after inlining.  */
  sreal GTY((skip)) time;
  int size;

  /* Conditional size/time information.  The summaries are being
     merged during inlining.  */
  conditions conds;
  vec<size_time_entry, va_gc> *entry;

  /* Predicate on when some loop in the function becomes to have known
     bounds.   */
  predicate * GTY((skip)) loop_iterations;
  /* Predicate on when some loop in the function becomes to have known
     stride.   */
  predicate * GTY((skip)) loop_stride;
  /* Predicate on when some array indexes become constants.  */
  predicate * GTY((skip)) array_index;
  /* Estimated growth for inlining all copies of the function before start
     of small functions inlining.
     This value will get out of date as the callers are duplicated, but
     using up-to-date value in the badness metric mean a lot of extra
     expenses.  */
  int growth;
  /* Number of SCC on the beginning of inlining process.  */
  int scc_no;

  /* Keep all field empty so summary dumping works during its computation.
     This is useful for debugging.  */
  inline_summary ()
    : estimated_self_stack_size (0), self_size (0), self_time (0), min_size (0),
      inlinable (false), contains_cilk_spawn (false), single_caller (false),
      fp_expressions (false), estimated_stack_size (false),
      stack_frame_offset (false), time (0), size (0), conds (NULL),
      entry (NULL), loop_iterations (NULL), loop_stride (NULL),
      array_index (NULL), growth (0), scc_no (0)
    {
    }
};

class GTY((user)) inline_summary_t: public function_summary <inline_summary *>
{
public:
  inline_summary_t (symbol_table *symtab, bool ggc):
    function_summary <inline_summary *> (symtab, ggc) {}

  static inline_summary_t *create_ggc (symbol_table *symtab)
  {
    struct inline_summary_t *summary = new (ggc_alloc <inline_summary_t> ())
      inline_summary_t(symtab, true);
    summary->disable_insertion_hook ();
    return summary;
  }


  virtual void insert (cgraph_node *, inline_summary *);
  virtual void remove (cgraph_node *node, inline_summary *);
  virtual void duplicate (cgraph_node *src, cgraph_node *dst,
			  inline_summary *src_data, inline_summary *dst_data);
};

extern GTY(()) function_summary <inline_summary *> *inline_summaries;

/* Information kept about callgraph edges.  */
struct ipa_call_summary
{
  class predicate *predicate;
  /* Vector indexed by parameters.  */
  vec<inline_param_summary> param;
  /* Estimated size and time of the call statement.  */
  int call_stmt_size;
  int call_stmt_time;
  /* Depth of loop nest, 0 means no nesting.  */
  unsigned int loop_depth;
  
  /* Keep all field empty so summary dumping works during its computation.
     This is useful for debugging.  */
  ipa_call_summary ()
    : predicate (NULL), param (vNULL), call_stmt_size (0), call_stmt_time (0),
      loop_depth (0)
    {
    }
};

class ipa_call_summary_t: public call_summary <ipa_call_summary *>
{
public:
  ipa_call_summary_t (symbol_table *symtab, bool ggc):
    call_summary <ipa_call_summary *> (symtab, ggc) {}

  /* Hook that is called by summary when an edge is duplicated.  */
  virtual void remove (cgraph_edge *cs, ipa_call_summary *);
  /* Hook that is called by summary when an edge is duplicated.  */
  virtual void duplicate (cgraph_edge *src, cgraph_edge *dst,
			  ipa_call_summary *src_data,
			  ipa_call_summary *dst_data);
};

extern call_summary <ipa_call_summary *> *ipa_call_summaries;

/* Data we cache about callgraph edges during inlining to avoid expensive
   re-computations during the greedy algorithm.  */
struct edge_growth_cache_entry
{
  sreal time, nonspec_time;
  int size;
  inline_hints hints;
};

extern vec<edge_growth_cache_entry> edge_growth_cache;

/* In ipa-inline-analysis.c  */
void debug_inline_summary (struct cgraph_node *);
void dump_inline_summaries (FILE *f);
void dump_inline_summary (FILE *f, struct cgraph_node *node);
void dump_inline_hints (FILE *f, inline_hints);
void inline_generate_summary (void);
void inline_read_summary (void);
void inline_write_summary (void);
void inline_free_summary (void);
void inline_analyze_function (struct cgraph_node *node);
void initialize_inline_failed (struct cgraph_edge *);
int estimate_size_after_inlining (struct cgraph_node *, struct cgraph_edge *);
void estimate_ipcp_clone_size_and_time (struct cgraph_node *,
					vec<tree>,
					vec<ipa_polymorphic_call_context>,
					vec<ipa_agg_jump_function_p>,
					int *, sreal *, sreal *,
				        inline_hints *);
int estimate_growth (struct cgraph_node *);
bool growth_likely_positive (struct cgraph_node *, int);
void inline_merge_summary (struct cgraph_edge *edge);
void inline_update_overall_summary (struct cgraph_node *node);
int do_estimate_edge_size (struct cgraph_edge *edge);
sreal do_estimate_edge_time (struct cgraph_edge *edge);
inline_hints do_estimate_edge_hints (struct cgraph_edge *edge);
void initialize_growth_caches (void);
void free_growth_caches (void);
void compute_inline_parameters (struct cgraph_node *, bool);
bool speculation_useful_p (struct cgraph_edge *e, bool anticipate_inlining);
unsigned int early_inliner (function *fun);
bool inline_account_function_p (struct cgraph_node *node);


/* In ipa-inline-transform.c  */
bool inline_call (struct cgraph_edge *, bool, vec<cgraph_edge *> *, int *, bool,
		  bool *callee_removed = NULL);
unsigned int inline_transform (struct cgraph_node *);
void clone_inlined_nodes (struct cgraph_edge *e, bool, bool, int *,
			  int freq_scale);

extern int ncalls_inlined;
extern int nfunctions_inlined;


/* Return estimated size of the inline sequence of EDGE.  */

static inline int
estimate_edge_size (struct cgraph_edge *edge)
{
  int ret;
  if ((int)edge_growth_cache.length () <= edge->uid
      || !(ret = edge_growth_cache[edge->uid].size))
    return do_estimate_edge_size (edge);
  return ret - (ret > 0);
}

/* Return estimated callee growth after inlining EDGE.  */

static inline int
estimate_edge_growth (struct cgraph_edge *edge)
{
  gcc_checking_assert (ipa_call_summaries->get (edge)->call_stmt_size
		       || !edge->callee->analyzed);
  return (estimate_edge_size (edge)
	  - ipa_call_summaries->get (edge)->call_stmt_size);
}

/* Return estimated callee runtime increase after inlining
   EDGE.  */

static inline sreal
estimate_edge_time (struct cgraph_edge *edge, sreal *nonspec_time = NULL)
{
  sreal ret;
  if ((int)edge_growth_cache.length () <= edge->uid
      || !edge_growth_cache[edge->uid].size)
    return do_estimate_edge_time (edge);
  if (nonspec_time)
    *nonspec_time = edge_growth_cache[edge->uid].nonspec_time;
  return edge_growth_cache[edge->uid].time;
}


/* Return estimated callee runtime increase after inlining
   EDGE.  */

static inline inline_hints
estimate_edge_hints (struct cgraph_edge *edge)
{
  inline_hints ret;
  if ((int)edge_growth_cache.length () <= edge->uid
      || !(ret = edge_growth_cache[edge->uid].hints))
    return do_estimate_edge_hints (edge);
  return ret - 1;
}

/* Reset cached value for EDGE.  */

static inline void
reset_edge_growth_cache (struct cgraph_edge *edge)
{
  if ((int)edge_growth_cache.length () > edge->uid)
    {
      struct edge_growth_cache_entry zero = {0, 0, 0, 0};
      edge_growth_cache[edge->uid] = zero;
    }
}

#endif /* GCC_IPA_INLINE_H */
