/* Trimming an exploded graph to a subset of nodes and edges.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "pretty-print.h"
#include "gcc-rich-location.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "diagnostic-core.h"
#include "diagnostic-event-id.h"
#include "diagnostic-path.h"
#include "bitmap.h"
#include "ordered-hash-map.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/trimmed-graph.h"

#if ENABLE_ANALYZER

namespace ana {

/* class trimmed_node : public dnode<tg_traits>.  */

/* Implementation of dump_dot vfunc, delegating to the inner node.  */

void
trimmed_node::dump_dot (graphviz_out *gv,
			const dump_args_t &args) const
{
  m_inner_node->dump_dot (gv, args.m_inner_args);
}

/* class trimmed_edge : public dedge<tg_traits>.  */

/* trimmed_edge's ctor.  */

trimmed_edge::trimmed_edge (trimmed_node *src, trimmed_node *dest,
			    const exploded_edge *inner_edge)
: dedge<tg_traits> (src, dest), m_inner_edge (inner_edge)
{
}

/* Implementation of dump_dot vfunc, delegating to the inner edge.  */

void
trimmed_edge::dump_dot (graphviz_out *gv, const dump_args_t &args) const
{
  m_inner_edge->dump_dot (gv, args.m_inner_args);
}

/* class trimmed_graph : public digraph <tg_traits>.  */

/* Ctor for trimmed_graph: construct a graph equivalent to trimming
   INNER_GRAPH to all nodes that can reach INNER_DST_NODE.  */

trimmed_graph::trimmed_graph (const exploded_graph &inner_graph,
			      const exploded_node *inner_dst_node)
: m_enodes (), m_eedges ()
{
  /* Determine what subset of nodes and edges to include in the
     trimmed graph.
     Walk backwards from INNER_DST_NODE, finding nodes that reach it,
     iteratively building the set of nodes and edges.  */
  auto_vec <const exploded_node *> worklist;
  worklist.safe_push (inner_dst_node);
  m_enodes.add (inner_dst_node);
  while (worklist.length () > 0)
    {
      const exploded_node *inner_node = worklist.pop ();
      exploded_edge *inner_pred;
      unsigned i;
      FOR_EACH_VEC_ELT (inner_node->m_preds, i, inner_pred)
	{
	  if (!m_enodes.contains (inner_pred->m_src))
	    {
	      worklist.safe_push (inner_pred->m_src);
	      m_enodes.add (inner_pred->m_src);
	    }
	  m_eedges.add (inner_pred);
	}
    }

  /* Create trimmed nodes for all enodes in the set.  */
  {
    /* Iterate over the vec rather than the hash_set
       to ensure deterministic order.  */
    exploded_node *inner_node;
    unsigned i;
    FOR_EACH_VEC_ELT (inner_graph.m_nodes, i, inner_node)
      if (m_enodes.contains (inner_node))
	{
	  trimmed_node *tnode = new trimmed_node (inner_node);
	  add_node (tnode);
	  m_map_from_enode_to_tnode.put (inner_node, tnode);
	}
  }

  /* Create trimmed edges for all edges in the set.  */
  {
    /* Iterate over the vec rather than the hash_set
       to ensure deterministic order.  */
    exploded_edge *inner_edge;
    unsigned i;
    FOR_EACH_VEC_ELT (inner_graph.m_edges, i, inner_edge)
      if (m_eedges.contains (inner_edge))
	{
	  const exploded_node *inner_src = inner_edge->m_src;
	  const exploded_node *inner_dest = inner_edge->m_dest;
	  trimmed_node *tsrc = *m_map_from_enode_to_tnode.get (inner_src);
	  trimmed_node *tdest = *m_map_from_enode_to_tnode.get (inner_dest);
	  trimmed_edge *tedge = new trimmed_edge (tsrc, tdest, inner_edge);
	  add_edge (tedge);
	}
  }
}

/* Dump stats about this graph to LOGGER.  */

void
trimmed_graph::log_stats (logger *logger) const
{
  logger->log ("#nodes: %i", m_nodes.length ());
  logger->log ("#edges: %i", m_edges.length ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
