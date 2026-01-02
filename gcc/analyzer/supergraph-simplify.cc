/* Simplifying the supergraph.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#define INCLUDE_DEQUE
#define INCLUDE_SET
#include "analyzer/common.h"

#include "cgraph.h"

#include "analyzer/supergraph.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/supergraph-manipulation.h"
#include "analyzer/bar-chart.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* A class for tracking a set of simplifications to a supergraph.
   supergraph.m_nodes and supergraph.m_edges may contain deleted object
   during the lifetime of this class; they are removed at the end.  */

class simplifier
{
public:
  simplifier (supergraph &sg,
	      ana::logger *logger)
  : m_sg (sg),
    m_nodes_to_remove (),
    m_edges_to_remove (),
    m_logger (logger),
    m_stats ()
  {
    for (auto node : sg.m_nodes)
      m_worklist.ensure_node_queued (node, logger);
  }

  ~simplifier ()
  {
    // Remove nodes from m_sg.m_nodes and delete them
    m_sg.delete_nodes (m_nodes_to_remove);

    // Remove edges from m_sg.m_edges and delete them
    {
      unsigned read_index, write_index;
      superedge **elem_ptr;
      VEC_ORDERED_REMOVE_IF (m_sg.m_edges, read_index, write_index, elem_ptr,
			     edge_to_be_removed_p (*elem_ptr));
      for (auto iter : m_edges_to_remove)
	delete iter;
    }

    if (m_logger)
      {
	log_nesting_level sentinel (m_logger, "stats");
	m_stats.log (*m_logger);
      }
  }

  /* Manipulations: logged, and refreshing the worklist.  */

  void
  set_edge_dest (superedge *edge, supernode *new_dest)
  {
    if (edge->m_dest == new_dest)
      return;
    log_nesting_level sentinel
      (m_logger, "updating edge dest: SN: %i -> SN: {%i, %i}",
       edge->m_src->m_id,
       edge->m_dest->m_id,
       new_dest->m_id);
    m_worklist.ensure_node_queued (edge->m_src, m_logger);
    m_worklist.ensure_node_queued (edge->m_dest, m_logger);
    m_worklist.ensure_node_queued (new_dest, m_logger);

    edge->set_dest (new_dest);

    m_stats.m_num_set_edge_dest++;
  }

  void
  remove_node (supernode *node)
  {
    log_nesting_level sentinel
      (m_logger, "removing node: SN: %i", node->m_id);
    for (auto in_edge : node->m_preds)
      remove_edge (in_edge);
    for (auto out_edge : node->m_succs)
      remove_edge (out_edge);
    m_nodes_to_remove.insert (node);
    m_stats.m_num_remove_node++;
  }

  void
  remove_edge (superedge *edge)
  {
    log_nesting_level sentinel
      (m_logger, "removing edge dest: SN: %i -> SN: %i",
       edge->m_src->m_id,
       edge->m_dest->m_id);
    gcc_assert (!edge->preserve_p ());

    m_worklist.ensure_node_queued (edge->m_src, m_logger);
    m_worklist.ensure_node_queued (edge->m_dest, m_logger);

    edge->m_src->remove_out_edge (edge);
    edge->m_dest->remove_in_edge (edge);

    m_edges_to_remove.insert (edge);

    m_stats.m_num_remove_edge++;
  }

  /* High level ops.  */

  supernode *
  pop_next_node_in_queue ()
  {
    return m_worklist.pop ();
  }

  void
  consider_node (supernode *node)
  {
    m_stats.m_num_iterations++;

    log_nesting_level sentinel (m_logger, "considering SN: %i", node->m_id);

    /* Eliminate nodes with no in-edges that aren't function entry nodes.  */
    if (node->m_preds.length () == 0
	&& !node->entry_p ())
      {
	log_nesting_level s2 (m_logger, "no in-edges");
	remove_node (node);
	return;
      }

    /* Handle nodes with a single out-edge.  */
    if (node->m_succs.length () == 1)
      if (consider_single_out_edge (node, node->m_succs[0]))
	return;
  }

  bool
  consider_single_out_edge (supernode *node,
			    superedge *single_out_edge)
  {
    if (node->preserve_p ())
      {
	log_nesting_level s3
	  (m_logger,
	   "node has preserve_p flag; preserving");
	return false;
      }
    if (single_out_edge->preserve_p ())
      {
	log_nesting_level s3
	  (m_logger,
	   "edge has preserve_p flag; preserving");
	return false;
      }

    /* Is the single out-edge a no-op?  */
    if (!single_out_edge->get_op ())
      {
	/* If the node doesn't add useful location information, we can
	   redirect all in-edges to the node to point at the outedge's dst.  */
	log_nesting_level s2 (m_logger,
			      "single outedge is no-op (to SN: %i)",
			      node->m_succs[0]->m_dest->m_id);
	bool redirect = false;
	if (node->m_loc == UNKNOWN_LOCATION)
	  {
	    log_nesting_level s3
	      (m_logger,
	       "node is at UNKNOWN_LOCATION; redirecting in-edges");
	    redirect = true;
	  }
	else if (node->m_loc == single_out_edge->m_dest->m_loc)
	  {
	    log_nesting_level s3
	      (m_logger,
	       "node has same location as successor; redirecting in-edges");
	    redirect = true;
	  }

	if (redirect)
	  {
	    for (auto in_edge : node->m_preds)
	      set_edge_dest (in_edge, single_out_edge->m_dest);
	    return true;
	  }

	return false;
      }

    gcc_assert (single_out_edge->get_op ());

    return false;
  }

private:
  bool edge_to_be_removed_p (superedge *edge)
  {
    return m_edges_to_remove.find (edge) != m_edges_to_remove.end ();
  }

  supergraph &m_sg;
  supergraph_manipulation::worklist m_worklist;
  std::set<supernode *> m_nodes_to_remove;
  std::set<superedge *> m_edges_to_remove;
  ana::logger *m_logger;
  struct stats
  {
    stats () = default;

    void log (ana::logger &logger)
    {
      logger.log ("# iterations taken: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_iterations);
      logger.log ("# nodes removed: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_remove_node);
      logger.log ("# edges removed: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_remove_edge);
      logger.log ("# set_edge_dest: " HOST_SIZE_T_PRINT_UNSIGNED,
		  (fmt_size_t)m_num_set_edge_dest);
    }

    size_t m_num_iterations;
    size_t m_num_remove_node;
    size_t m_num_remove_edge;
    size_t m_num_set_edge_dest;

  } m_stats;
};

} // anonymous namespace

void
supergraph::log_stats (logger *logger) const
{
  if (!logger)
    return;

  logger->log ("# nodes: %u", m_nodes.length ());
  logger->log ("# edges: %u", m_edges.length ());

  /* Show per-function bar charts of supernodes per function.  */
  {
    bar_chart snodes_per_function;
    logger->log ("snodes per function:");

    cgraph_node *cgnode;
    FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (cgnode)
      {
	function *fn = cgnode->get_fun ();

	int snodes_for_this_function = 0;
	for (auto node : m_nodes)
	  if (node->get_function () == fn)
	    ++snodes_for_this_function;

	pretty_printer pp;
	pp_format_decoder (&pp) = default_tree_printer;
	pp_printf (&pp, "%qD", fn->decl);
	snodes_per_function.add_item (pp_formatted_text (&pp),
				      snodes_for_this_function);
      }

    snodes_per_function.print (logger->get_printer ());
  }
}

void
supergraph::simplify (logger *logger)
{
  LOG_SCOPE (logger);

  {
    log_nesting_level sentinel (logger, "before simplification:");
    log_stats (logger);
  }

  {
    simplifier opt (*this, logger);
    while (supernode *node = opt.pop_next_node_in_queue ())
      opt.consider_node (node);
  }

  {
    log_nesting_level sentinel (logger, "after simplification");
    log_stats (logger);
  }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
