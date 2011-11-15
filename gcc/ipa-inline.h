/* Inlining decision heuristics.
   Copyright (C) 2003, 2004, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
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

/* Representation of inline parameters that do depend on context function is
   inlined into (i.e. known constant values of function parameters.

   Conditions that are interesting for function body are collected into CONDS
   vector.  They are of simple for  function_param OP VAL, where VAL is
   IPA invariant.  The conditions are then refered by predicates.  */

typedef struct GTY(()) condition
  {
    tree val;
    int operand_num;
    enum tree_code code;
  } condition;

DEF_VEC_O (condition);
DEF_VEC_ALLOC_O (condition, gc);

typedef VEC(condition,gc) *conditions;

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
typedef struct GTY(()) size_time_entry
{
  struct predicate predicate;
  int size;
  int time;
} size_time_entry;
DEF_VEC_O (size_time_entry);
DEF_VEC_ALLOC_O (size_time_entry, gc);

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
  VEC(size_time_entry,gc) *entry;
};


typedef struct inline_summary inline_summary_t;
DEF_VEC_O(inline_summary_t);
DEF_VEC_ALLOC_O(inline_summary_t,gc);
extern GTY(()) VEC(inline_summary_t,gc) *inline_summary_vec;

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
typedef struct inline_param_summary inline_param_summary_t;
DEF_VEC_O(inline_param_summary_t);
DEF_VEC_ALLOC_O(inline_param_summary_t,heap);

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
  VEC (inline_param_summary_t, heap) *param;
};

typedef struct inline_edge_summary inline_edge_summary_t;
DEF_VEC_O(inline_edge_summary_t);
DEF_VEC_ALLOC_O(inline_edge_summary_t,heap);
extern VEC(inline_edge_summary_t,heap) *inline_edge_summary_vec;

typedef struct edge_growth_cache_entry
{
  int time, size;
} edge_growth_cache_entry;
DEF_VEC_O(edge_growth_cache_entry);
DEF_VEC_ALLOC_O(edge_growth_cache_entry,heap);

extern VEC(int,heap) *node_growth_cache;
extern VEC(edge_growth_cache_entry,heap) *edge_growth_cache;

/* In ipa-inline-analysis.c  */
void debug_inline_summary (struct cgraph_node *);
void dump_inline_summaries (FILE *f);
void dump_inline_summary (FILE * f, struct cgraph_node *node);
void inline_generate_summary (void);
void inline_read_summary (void);
void inline_write_summary (cgraph_node_set, varpool_node_set);
void inline_free_summary (void);
void initialize_inline_failed (struct cgraph_edge *);
int estimate_time_after_inlining (struct cgraph_node *, struct cgraph_edge *);
int estimate_size_after_inlining (struct cgraph_node *, struct cgraph_edge *);
void estimate_ipcp_clone_size_and_time (struct cgraph_node *,
					VEC (tree, heap) *known_vals,
					VEC (tree, heap) *known_binfos,
					int *, int *);
int do_estimate_growth (struct cgraph_node *);
void inline_merge_summary (struct cgraph_edge *edge);
int do_estimate_edge_growth (struct cgraph_edge *edge);
int do_estimate_edge_time (struct cgraph_edge *edge);
void initialize_growth_caches (void);
void free_growth_caches (void);
void compute_inline_parameters (struct cgraph_node *, bool);

/* In ipa-inline-transform.c  */
bool inline_call (struct cgraph_edge *, bool, VEC (cgraph_edge_p, heap) **, int *);
unsigned int inline_transform (struct cgraph_node *);
void clone_inlined_nodes (struct cgraph_edge *e, bool, bool, int *);

extern int ncalls_inlined;
extern int nfunctions_inlined;

static inline struct inline_summary *
inline_summary (struct cgraph_node *node)
{
  return VEC_index (inline_summary_t, inline_summary_vec, node->uid);
}

static inline struct inline_edge_summary *
inline_edge_summary (struct cgraph_edge *edge)
{
  return VEC_index (inline_edge_summary_t,
		    inline_edge_summary_vec, edge->uid);
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
  if ((int)VEC_length (int, node_growth_cache) <= node->uid
      || !(ret = VEC_index (int, node_growth_cache, node->uid)))
    return do_estimate_growth (node);
  return ret - (ret > 0);
}


/* Return estimated callee growth after inlining EDGE.  */

static inline int
estimate_edge_growth (struct cgraph_edge *edge)
{
  int ret;
  if ((int)VEC_length (edge_growth_cache_entry, edge_growth_cache) <= edge->uid
      || !(ret = VEC_index (edge_growth_cache_entry,
			    edge_growth_cache,
			    edge->uid)->size))
    return do_estimate_edge_growth (edge);
  return ret - (ret > 0);
}


/* Return estimated callee runtime increase after inlning
   EDGE.  */

static inline int
estimate_edge_time (struct cgraph_edge *edge)
{
  int ret;
  if ((int)VEC_length (edge_growth_cache_entry, edge_growth_cache) <= edge->uid
      || !(ret = VEC_index (edge_growth_cache_entry,
			    edge_growth_cache,
			    edge->uid)->time))
    return do_estimate_edge_time (edge);
  return ret - (ret > 0);
}


/* Reset cached value for NODE.  */

static inline void
reset_node_growth_cache (struct cgraph_node *node)
{
  if ((int)VEC_length (int, node_growth_cache) > node->uid)
    VEC_replace (int, node_growth_cache, node->uid, 0);
}

/* Reset cached value for EDGE.  */

static inline void
reset_edge_growth_cache (struct cgraph_edge *edge)
{
  if ((int)VEC_length (edge_growth_cache_entry, edge_growth_cache) > edge->uid)
    {
      struct edge_growth_cache_entry zero = {0, 0};
      VEC_replace (edge_growth_cache_entry, edge_growth_cache, edge->uid, &zero);
    }
}
