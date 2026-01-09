/* Converting directed graphs to dot.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_DIAGNOSTICS_DIGRAPHS_TO_DOT_H
#define GCC_DIAGNOSTICS_DIGRAPHS_TO_DOT_H

#include "diagnostics/digraphs.h"
#include "graphviz.h"

namespace diagnostics {
namespace digraphs {
namespace to_dot {

using digraph = diagnostics::digraphs::digraph;
using digraph_node = diagnostics::digraphs::node;
using digraph_edge = diagnostics::digraphs::edge;

class converter
{
public:
  static std::unique_ptr<converter>
  make (const digraph &dg);

  virtual std::unique_ptr<dot::graph>
  make_dot_graph_from_diagnostic_graph (const digraph &);

  std::unique_ptr<dot::stmt>
  make_dot_node_from_digraph_node (const digraph_node &);

  std::unique_ptr<dot::edge_stmt>
  make_dot_edge_from_digraph_edge (const digraph_edge &);

  dot::id
  get_dot_id_for_node (const digraph_node &);

  dot::node_id
  get_node_id_for_node (const digraph_node &,
			const char *compass_point = nullptr);

  bool
  has_edges_p (const digraph_node &);

  virtual void
  add_any_subgraph_attrs (const digraph_node &input_node,
			  dot::subgraph &output_subgraph);

  virtual void
  add_any_node_attrs (const digraph_node &input_node,
		      dot::node_stmt &output_node);

  virtual void
  add_any_edge_attrs (const digraph_edge &input_edge,
		      dot::edge_stmt &output_edge);

private:
  std::set<const digraph_node *> m_nodes_with_edges;
  std::map<const digraph_node *, dot::stmt *> m_node_map;
};

extern std::unique_ptr<converter>
make_converter_from_cfg ();

} // namespace to_dot
} // namespace digraphs
} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_DIGRAPHS_TO_DOT_H */
