/* A graph for exploring trees of feasible paths through the egraph.
   Copyright (C) 2021 Free Software Foundation, Inc.
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
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "selftest.h"
#include "ordered-hash-map.h"
#include "json.h"
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
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "cgraph.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/feasible-graph.h"

#if ENABLE_ANALYZER

namespace ana {

/* class base_feasible_node : public dnode<fg_traits>.  */

/* Print an id to PP for this node suitable for use in .dot dumps.  */

void
base_feasible_node::dump_dot_id (pretty_printer *pp) const
{
  pp_printf (pp, "fnode_%i", m_index);
}

/* class feasible_node : public base_feasible_node.  */

/* Implementation of dump_dot vfunc for feasible_node.  */

void
feasible_node::dump_dot (graphviz_out *gv,
			const dump_args_t &) const
{
  pretty_printer *pp = gv->get_pp ();

  dump_dot_id (pp);
  pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	     m_inner_node->get_dot_fillcolor ());
  pp_write_text_to_stream (pp);

  pp_printf (pp, "FN: %i (EN: %i); len=%i", m_index, m_inner_node->m_index,
	     m_path_length);
  pp_newline (pp);

  format f (true);
  m_inner_node->get_point ().print (pp, f);
  pp_newline (pp);

  /* Show the model at this point along expansion of the feasible path,
     rather than the model within the enode.  */
  m_state.get_model ().dump_to_pp (pp, true, true);
  pp_newline (pp);

  m_inner_node->dump_processed_stmts (pp);
  m_inner_node->dump_saved_diagnostics (pp);

  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);

  pp_string (pp, "\"];\n\n");
  pp_flush (pp);
}

/* Implementation of dump_dot vfunc for infeasible_node.
   In particular, show the rejected constraint.  */

void
infeasible_node::dump_dot (graphviz_out *gv,
			   const dump_args_t &) const
{
  pretty_printer *pp = gv->get_pp ();

  dump_dot_id (pp);
  pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	     m_inner_node->get_dot_fillcolor ());
  pp_write_text_to_stream (pp);

  pp_printf (pp, "infeasible edge to EN: %i", m_inner_node->m_index);
  pp_newline (pp);

  pp_string (pp, "rejected constraint:");
  pp_newline (pp);
  m_rc.dump_to_pp (pp);

  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);

  pp_string (pp, "\"];\n\n");
  pp_flush (pp);
}

/* class base_feasible_edge : public dedge<fg_traits>.  */

/* Implementation of dump_dot vfunc for base_easible_edge.  */

void
base_feasible_edge::dump_dot (graphviz_out *gv, const dump_args_t &) const
{
  pretty_printer *pp = gv->get_pp ();

  m_src->dump_dot_id (pp);
  pp_string (pp, " -> ");
  m_dest->dump_dot_id (pp);

  m_inner_edge->dump_dot_label (pp);
}

/* class feasible_graph : public digraph <fg_traits>.  */

/* Ctor for feasible_graph.  */

feasible_graph::feasible_graph ()
: m_num_infeasible (0)
{
}

/* Add a feasible_node to this graph for ENODE, STATE with the
   given PATH_LENGTH. */

feasible_node *
feasible_graph::add_node (const exploded_node *enode,
			  const feasibility_state &state,
			  unsigned path_length)
{
  /* We don't attempt get_or_create here.  */
  feasible_node *fnode = new feasible_node (enode, m_nodes.length (),
					    state, path_length);
  digraph<fg_traits>::add_node (fnode);
  return fnode;
}

/* Add an infeasible_node to this graph and an infeasible_edge connecting
   to it from SRC_FNODE, capturing a failure of RC along EEDGE.  */

void
feasible_graph::add_feasibility_problem (feasible_node *src_fnode,
					 const exploded_edge *eedge,
					 const rejected_constraint &rc)
{
  infeasible_node *dst_fnode
    = new infeasible_node (eedge->m_dest, m_nodes.length (), rc);
  digraph<fg_traits>::add_node (dst_fnode);
  add_edge (new infeasible_edge (src_fnode, dst_fnode, eedge));
  m_num_infeasible++;
}

/* Make an exploded_path from the origin to FNODE's exploded_node,
   following the edges in the feasible_graph.  */

exploded_path *
feasible_graph::make_epath (feasible_node *fnode) const
{
  exploded_path *epath = new exploded_path ();

  /* FG is actually a tree.  Built the path backwards, by walking
     backwards from FNODE until we reach the origin.  */
  while (fnode->get_inner_node ()->m_index != 0)
    {
      gcc_assert (fnode->m_preds.length () == 1);
      feasible_edge *pred_fedge
	= static_cast <feasible_edge *> (fnode->m_preds[0]);
      epath->m_edges.safe_push (pred_fedge->get_inner_edge ());
      fnode = static_cast <feasible_node *> (pred_fedge->m_src);
    }

  /* Now reverse it.  */
  epath->m_edges.reverse ();

  return epath;
}

/* Dump stats about this graph to LOGGER.  */

void
feasible_graph::log_stats (logger *logger) const
{
  logger->log ("#nodes: %i", m_nodes.length ());
  logger->log ("#edges: %i", m_edges.length ());
  logger->log ("#feasible nodes: %i", m_nodes.length () - m_num_infeasible);
  logger->log ("#feasible edges: %i", m_edges.length () - m_num_infeasible);
  logger->log ("#infeasible nodes/edges: %i", m_num_infeasible);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
