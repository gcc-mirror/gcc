/* Converting directed graphs to dot.
   Copyright (C) 2025 Free Software Foundation, Inc.
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

#define INCLUDE_ALGORITHM
#define INCLUDE_MAP
#define INCLUDE_SET
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "graphviz.h"
#include "xml.h"
#include "xml-printer.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/digraphs-to-dot.h"
#include "diagnostics/sarif-sink.h"

#include "selftest.h"

namespace diagnostics {
namespace digraphs {
namespace to_dot {

using digraph = diagnostics::digraphs::digraph;
using digraph_node = diagnostics::digraphs::node;
using digraph_edge = diagnostics::digraphs::edge;

// class conversion_to_dot

std::unique_ptr<dot::graph>
converter::make_dot_graph_from_diagnostic_graph (const digraph &input_graph)
{
  auto output_graph = std::make_unique<dot::graph> ();

  if (const char *description = input_graph.get_description ())
    output_graph->m_stmt_list.add_attr (dot::id ("label"),
					dot::id (description));

  const int num_nodes = input_graph.get_num_nodes ();
  const int num_edges = input_graph.get_num_edges ();

  /* Determine which nodes have in-edges and out-edges.  */
  for (int i = 0; i < num_edges; ++i)
    {
      const digraph_edge &input_edge = input_graph.get_edge (i);
      m_nodes_with_edges.insert (&input_edge.get_src_node ());
      m_nodes_with_edges.insert (&input_edge.get_dst_node ());
    }

  for (int i = 0; i < num_nodes; ++i)
    {
      const digraph_node &input_node = input_graph.get_node (i);
      auto dot_node_stmt = make_dot_node_from_digraph_node (input_node);
      output_graph->m_stmt_list.add_stmt (std::move (dot_node_stmt));
    }

  for (int i = 0; i < num_edges; ++i)
    {
      const digraph_edge &input_edge = input_graph.get_edge (i);
      auto dot_edge_stmt = make_dot_edge_from_digraph_edge (input_edge);
      output_graph->m_stmt_list.add_stmt (std::move (dot_edge_stmt));
    }

  return output_graph;
}

std::unique_ptr<dot::stmt>
converter::
make_dot_node_from_digraph_node (const diagnostics::digraphs::node &input_node)
{
  dot::id dot_id (get_dot_id_for_node (input_node));

  /* For now, we can only do either edges or children, not both
     ...but see https://graphviz.org/docs/attrs/compound/  */

  if (has_edges_p (input_node))
    {
      auto output_node
	= std::make_unique<dot::node_stmt> (std::move (dot_id));
      m_node_map[&input_node] = output_node.get ();
      if (const char *label = input_node.get_label ())
	output_node->set_label (dot::id (label));
      add_any_node_attrs (input_node, *output_node);
      return output_node;
    }
  else
    {
      auto output_node = std::make_unique<dot::subgraph> (std::move (dot_id));
      m_node_map[&input_node] = output_node.get ();
      if (const char *label = input_node.get_label ())
	output_node->add_attr (dot::id ("label"), dot::id (label));
      add_any_subgraph_attrs (input_node, *output_node);
      const int num_children = input_node.get_num_children ();
      for (int i = 0; i < num_children; ++i)
	{
	  const digraph_node &input_child = input_node.get_child (i);
	  auto dot_child_stmt = make_dot_node_from_digraph_node (input_child);
	  output_node->m_stmt_list.add_stmt (std::move (dot_child_stmt));
	}
      return output_node;
    }
}

std::unique_ptr<dot::edge_stmt>
converter::
make_dot_edge_from_digraph_edge (const digraph_edge &input_edge)
{
  const digraph_node &src_dnode = input_edge.get_src_node ();
  const digraph_node &dst_dnode = input_edge.get_dst_node ();
  auto output_edge
    = std::make_unique<dot::edge_stmt> (get_node_id_for_node (src_dnode),
					get_node_id_for_node (dst_dnode));
  if (const char *label = input_edge.get_label ())
    output_edge->set_label (dot::id (label));
  add_any_edge_attrs (input_edge, *output_edge);
  return output_edge;
}

dot::id
converter::get_dot_id_for_node (const digraph_node &input_node)
{
  if (has_edges_p (input_node))
    return input_node.get_id ();
  else
    return std::string ("cluster_") + input_node.get_id ();
}

dot::node_id
converter::get_node_id_for_node (const digraph_node &input_node,
					 const char *compass_point)
{
  dot::id id = get_dot_id_for_node (input_node);
  if (compass_point)
    {
      enum dot::compass_pt pt;
      if (dot::get_compass_pt_from_string (compass_point, pt))
	return dot::node_id (id, pt);
    }
  return dot::node_id (id);
}

bool
converter::has_edges_p (const digraph_node &input_node)
{
  return m_nodes_with_edges.find (&input_node) != m_nodes_with_edges.end ();
}

void
converter::add_any_subgraph_attrs (const digraph_node &/*input_node*/,
				   dot::subgraph &/*output_subgraph*/)
{
  // No-op
}

void
converter::add_any_node_attrs (const digraph_node &/*input_node*/,
			       dot::node_stmt &/*output_node*/)
{
  // No-op
}

void
converter::add_any_edge_attrs (const digraph_edge &/*input_edge*/,
			       dot::edge_stmt &/*output_edge*/)
{
  // No-op
}

std::unique_ptr<converter>
converter::make (const diagnostics::digraphs::digraph &dg)
{
  if (const char *graph_kind = dg.get_graph_kind ())
    {
      // Try to find a suitable converter subclass and use it
      if (strcmp (graph_kind, "cfg") == 0)
	return make_converter_from_cfg ();
    }
  return std::make_unique<converter> ();
}

} // namespace to_dot
} // namespace digraphs
} // namespace diagnostics
