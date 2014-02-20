/* Inlining decision heuristics.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

#include "ipa-prop.h"

/* Representation of inline parameters that do depend on context function is
   inlined into (i.e. known constant values of function parameters.

   Conditions that are interesting for function body are collected into CONDS
   vector.  They are of simple for  function_param OP VAL, where VAL is
   IPA invariant.  The conditions are then referred by predicates.  */

struct GTY(()) condition
{
  /* If agg_contents is set, this is the offset from which the used data was
     loaded.  */
  HOST_WIDE_INT offset;
  tree val;
  int operand_num;
  ENUM_BITFIELD(tree_code) code : 16;
  /* Set if the used data were loaded from an aggregate parameter or from
     data received by reference.  */
  unsigned agg_contents : 1;
  /* If agg_contents is set, this differentiates between loads from data
     passed by reference and by value.  */
  unsigned by_ref : 1;
};

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
  INLINE_HINT_array_index = 128
};
typedef int inline_hints;


typedef vec<condition, va_gc> *conditions;

/* Representation of predicates i.e. formulas using conditions defined
   above.  Predicates are simple logical formulas in conjunctive-disjunctive
   form.

   Predicate is array of clauses terminated by 0.  Every clause must be true
   in order to make predicate true.
   Clauses are represented as bitmaps of conditions. One of conditions
   must be true in order for clause to be true.  */

#define MAX_CLAUSES 8
typedef unsigned int clause_t;
struct GTY(()) predicate
{
  clause_t clause[MAX_CLAUSES + 1];
};

/* Represnetation of function body size and time depending on the inline
   context.  We keep simple array of record, every containing of predicate
   and time/size to account.

   We keep values scaled up, so fractional sizes and times can be
   accounted.  */
#define INLINE_SIZE_SCALE 2
#define INLINE_TIME_SCALE (CGRAPH_FREQ_BASE * 2)
struct GTY(()) size_time_entry
{
  struct predicate predicate;
  int size;
  int time;
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
  int self_time;

  /* False when there something makes inlining impossible (such as va_arg).  */
  unsigned inlinable : 1;

  /* Information about function that will result after applying all the
     inline decisions present in the callgraph.  Generally kept up to
     date only for functions that are not inline clones. */

  /* Estimated stack frame consumption by the function.  */
  HOST_WIDE_INT estimated_stack_size;
  /* Expected offset of the stack frame of inlined function.  */
  HOST_WIDE_INT stack_frame_offset;
  /* Estimated size of the function after inlining.  */
  int time;
  int size;

  /* Conditional size/time information.  The summaries are being
     merged during inlining.  */
  conditions conds;
  vec<size_time_entry, va_gc> *entry;

  /* Predicate on when some loop in the function becomes to have known
     bounds.   */
  struct predicate * GTY((skip)) loop_iterations;
  /* Predicate on when some loop in the function becomes to have known
     stride.   */
  struct predicate * GTY((skip)) loop_stride;
  /* Predicate on when some array indexes become constants.  */
  struct predicate * GTY((skip)) array_index;
  /* Estimated growth for inlining all copies of the function before start
     of small functions inlining.
     This value will get out of date as the callers are duplicated, but
     using up-to-date value in the badness metric mean a lot of extra
     expenses.  */
  int growth;
  /* Number of SCC on the beginning of inlining process.  */
  int scc_no;
};

/* Need a typedef for inline_summary because of inline function
   'inline_summary' below.  */
typedef struct inline_summary inline_summary_t;
extern GTY(()) vec<inline_summary_t, va_gc> *inline_summary_vec;

/* Information kept about parameter of call site.  */
struct inline_param_summary
{
  /* REG_BR_PROB_BASE based probability that parameter will change in between
     two invocation of the calls.
     I.e. loop invariant parameters
     REG_BR_PROB_BASE/estimated_iterations and regular
     parameters REG_BR_PROB_BASE.

     Value 0 is reserved for compile time invariants. */
  int change_prob;
};

/* Information kept about callgraph edges.  */
struct inline_edge_summary
{
  /* Estimated size and time of the call statement.  */
  int call_stmt_size;
  int call_stmt_time;
  /* Depth of loop nest, 0 means no nesting.  */
  unsigned short int loop_depth;
  struct predicate *predicate;
  /* Array indexed by parameters.
     0 means that parameter change all the time, REG_BR_PROB_BASE means
     that parameter is constant.  */
  vec<inline_param_summary> param;
};

/* Need a typedef for inline_edge_summary because of inline function
   'inline_edge_summary' below.  */
typedef struct inline_edge_summary inline_edge_summary_t;
extern vec<inline_edge_summary_t> inline_edge_summary_vec;

struct edge_growth_cache_entry
{
  int time, size;
  inline_hints hints;
};

extern vec<int> node_growth_cache;
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
void initialize_inline_failed (struct cgraph_edge *);
int estimate_time_after_inlining (struct cgraph_node *, struct cgraph_edge *);
int estimate_size_after_inlining (struct cgraph_node *, struct cgraph_edge *);
void estimate_ipcp_clone_size_and_time (struct cgraph_node *,
					vec<tree>,  vec<tree>,
					vec<ipa_agg_jump_function_p>,
					int *, int *, inline_hints *);
int do_estimate_growth (struct cgraph_node *);
void inline_merge_summary (struct cgraph_edge *edge);
void inline_update_overall_summary (struct cgraph_node *node);
int do_estimate_edge_size (struct cgraph_edge *edge);
int do_estimate_edge_time (struct cgraph_edge *edge);
inline_hints do_estimate_edge_hints (struct cgraph_edge *edge);
void initialize_growth_caches (void);
void free_growth_caches (void);
void compute_inline_parameters (struct cgraph_node *, bool);
bool speculation_useful_p (struct cgraph_edge *e, bool anticipate_inlining);

/* In ipa-inline-transform.c  */
bool inline_call (struct cgraph_edge *, bool, vec<cgraph_edge_p> *, int *, bool);
unsigned int inline_transform (struct cgraph_node *);
void clone_inlined_nodes (struct cgraph_edge *e, bool, bool, int *,
			  int freq_scale);

extern int ncalls_inlined;
extern int nfunctions_inlined;

static inline struct inline_summary *
inline_summary (struct cgraph_node *node)
{
  return &(*inline_summary_vec)[node->uid];
}

static inline struct inline_edge_summary *
inline_edge_summary (struct cgraph_edge *edge)
{
  return &inline_edge_summary_vec[edge->uid];
}

/* Return estimated unit growth after inlning all calls to NODE.
   Quick accesors to the inline growth caches.  
   For convenience we keep zero 0 as unknown.  Because growth
   can be both positive and negative, we simply increase positive
   growths by 1. */
static inline int
estimate_growth (struct cgraph_node *node)
{
  int ret;
  if ((int)node_growth_cache.length () <= node->uid
      || !(ret = node_growth_cache[node->uid]))
    return do_estimate_growth (node);
  return ret - (ret > 0);
}


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
#ifdef ENABLE_CHECKING
  gcc_checking_assert (inline_edge_summary (edge)->call_stmt_size);
#endif
  return (estimate_edge_size (edge)
	  - inline_edge_summary (edge)->call_stmt_size);
}

/* Return estimated callee runtime increase after inlning
   EDGE.  */

static inline int
estimate_edge_time (struct cgraph_edge *edge)
{
  int ret;
  if ((int)edge_growth_cache.length () <= edge->uid
      || !(ret =  edge_growth_cache[edge->uid].time))
    return do_estimate_edge_time (edge);
  return ret - (ret > 0);
}


/* Return estimated callee runtime increase after inlning
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


/* Reset cached value for NODE.  */

static inline void
reset_node_growth_cache (struct cgraph_node *node)
{
  if ((int)node_growth_cache.length () > node->uid)
    node_growth_cache[node->uid] = 0;
}

/* Reset cached value for EDGE.  */

static inline void
reset_edge_growth_cache (struct cgraph_edge *edge)
{
  if ((int)edge_growth_cache.length () > edge->uid)
    {
      struct edge_growth_cache_entry zero = {0, 0, 0};
      edge_growth_cache[edge->uid] = zero;
    }
}
