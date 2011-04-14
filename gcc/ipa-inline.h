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

/* Function inlining information.  */

struct inline_summary
{
  /* Estimated stack frame consumption by the function.  */
  HOST_WIDE_INT estimated_self_stack_size;

  /* Size of the function body.  */
  int self_size;
  /* How many instructions are likely going to disappear after inlining.  */
  int size_inlining_benefit;
  /* Estimated time spent executing the function body.  */
  int self_time;
  /* How much time is going to be saved by inlining.  */
  int time_inlining_benefit;
};

typedef struct inline_summary inline_summary_t;
DEF_VEC_O(inline_summary_t);
DEF_VEC_ALLOC_O(inline_summary_t,heap);
extern VEC(inline_summary_t,heap) *inline_summary_vec;

void debug_inline_summary (struct cgraph_node *);
void dump_inline_summaries (FILE *f);
void inline_generate_summary (void);
void inline_read_summary (void);
void inline_write_summary (cgraph_node_set, varpool_node_set);
void inline_free_summary (void);
int estimate_time_after_inlining (struct cgraph_node *, struct cgraph_edge *);
int estimate_size_after_inlining (struct cgraph_node *, struct cgraph_edge *);
int estimate_growth (struct cgraph_node *);

static inline struct inline_summary *
inline_summary (struct cgraph_node *node)
{
  return VEC_index (inline_summary_t, inline_summary_vec, node->uid);
}

/* Estimate the growth of the caller when inlining EDGE.  */

static inline int
estimate_edge_growth (struct cgraph_edge *edge)
{
  int call_stmt_size;
  call_stmt_size = edge->call_stmt_size;
  gcc_checking_assert (call_stmt_size);
  return (edge->callee->global.size
	  - inline_summary (edge->callee)->size_inlining_benefit
	  - call_stmt_size);
}

