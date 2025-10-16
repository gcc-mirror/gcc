/* Creating GraphViz .dot files from diagnostic state graphs.
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

#include "custom-sarif-properties/state-graphs.h"
#include "diagnostics/state-graphs.h"
#include "graphviz.h"
#include "xml.h"
#include "xml-printer.h"
#include "intl.h"

using namespace diagnostics;
using namespace diagnostics::state_graphs;

namespace state_node_properties = custom_sarif_properties::state_graphs::node;

static int
get_depth (const digraphs::node &n)
{
  int deepest_child = 0;
  for (size_t i = 0; i < n.get_num_children (); ++i)
      deepest_child = std::max (deepest_child,
				get_depth (n.get_child (i)));
  return deepest_child + 1;
}

static const char *
get_color_for_dynalloc_state (enum state_node_properties::dynalloc_state_t dynalloc_st)
{
  switch (dynalloc_st)
    {
    default:
      gcc_unreachable ();
      break;
    case state_node_properties::dynalloc_state_t::unknown:
    case state_node_properties::dynalloc_state_t::nonnull:
      return nullptr;

    case state_node_properties::dynalloc_state_t::unchecked:
      return "#ec7a08"; // pf-orange-400

    case state_node_properties::dynalloc_state_t::freed:
      return "#cc0000"; // pf-red-100
    }
}

static void
set_color_for_dynalloc_state (dot::attr_list &attrs,
			      enum state_node_properties::dynalloc_state_t state)
{
  if (const char *color = get_color_for_dynalloc_state (state))
    attrs.add (dot::id ("color"), dot::id (color));
}

class state_diagram : public dot::graph
{
public:
  state_diagram (const diagnostics::digraphs::digraph &input_state_graph,
		 const logical_locations::manager &logical_loc_mgr)
  : m_logical_loc_mgr (logical_loc_mgr)
  {
    // "node [shape=plaintext]\n"
    {
      auto attr_stmt
	= std::make_unique<dot::attr_stmt> (dot::attr_stmt::kind::node);
      attr_stmt->m_attrs.add (dot::id ("shape"), dot::id ("plaintext"));
      add_stmt (std::move (attr_stmt));
    }

    /* Determine which nodes are involved in edges.  */
    for (size_t i = 0; i < input_state_graph.get_num_edges (); ++i)
      {
	auto &edge = input_state_graph.get_edge (i);
	m_src_nodes.insert (&edge.get_src_node ());
	m_dst_nodes.insert (&edge.get_dst_node ());
      }

    /* Recurse down the nodes in the state graph, creating subgraphs
       and then eventually creating nodes, and recursively
       creating XML tables, and adding ports for the endpoints of edges
       where needed.  */

    auto root_cluster
      = std::make_unique<dot::subgraph> (dot::id ("cluster_memory_regions"));
    for (size_t i = 0; i < input_state_graph.get_num_nodes (); ++i)
      on_input_state_node (*root_cluster,
			   input_state_graph.get_node (i));
    add_stmt (std::move (root_cluster));

    /* Now create dot edges for edges in input_stage_graph.  */
    for (size_t i = 0; i < input_state_graph.get_num_edges (); ++i)
      {
	auto &edge = input_state_graph.get_edge (i);
	auto &src_node = edge.get_src_node ();
	auto &dst_node = edge.get_dst_node ();

	auto src_port_id = m_src_node_to_port_id.find (&src_node);
	if (src_port_id == m_src_node_to_port_id.end ())
	  continue;
	auto dst_port_id = m_dst_node_to_port_id.find (&dst_node);
	if (dst_port_id == m_dst_node_to_port_id.end ())
	  continue;

	auto e = std::make_unique<dot::edge_stmt> (src_port_id->second,
						   dst_port_id->second);
	set_color_for_dynalloc_state
	  (e->m_attrs,
	   dst_node.get_property (state_node_properties::dynalloc_state_prop));

	add_stmt (std::move (e));
      }
  }

private:
  struct pending_edge
  {
    dot::node_id m_src_node_id;
    std::string m_dst_region_id;
  };

  dot::id
  get_id_for_region (const char *region_id)
  {
    gcc_assert (region_id);
    return std::string ("cluster_region_") + region_id;
  }

  dot::id
  make_id (const diagnostics::digraphs::node &state_node, bool cluster)
  {
    std::string input_node_id = state_node.get_id ();
    if (cluster)
      return std::string ("cluster_") + input_node_id;
    else
      return input_node_id;
  }

  bool
  starts_node_p (const diagnostics::digraphs::node &state_node)
  {
    switch (state_node.get_property (state_node_properties::kind_prop))
      {
      default:
	return false;

      case state_node_properties::kind_t::stack:
	/* We want all frames in the stack in the same table,
	   so they are grouped.  */
      case state_node_properties::kind_t::dynalloc_buffer:
      case state_node_properties::kind_t::variable:
	return true;
      }
  }

  const char *
  get_label_for_node (const diagnostics::digraphs::node &state_node)
  {
    switch (state_node.get_property (state_node_properties::kind_prop))
      {
      default:
	return nullptr;

      case state_node_properties::kind_t::globals:
	return _("Globals");
      case state_node_properties::kind_t::code:
	return _("Code");
      case state_node_properties::kind_t::stack:
	return _("Stack");
      case state_node_properties::kind_t::heap_:
	return _("Heap");
      }
  }

  void
  on_input_state_node (dot::subgraph &parent_subgraph,
		       const diagnostics::digraphs::node &state_node)
  {
    dot::id sg_id = make_id (state_node, true);

    if (starts_node_p (state_node))
      {
	// Create node with table
	xml::element table ("table", false);
	xml::printer xp (table);
	xp.set_attr ("border", "0");
	xp.set_attr ("cellborder", "1");
	xp.set_attr ("cellspacing", "0");

	const int max_depth = get_depth (state_node);
	const int num_columns = max_depth + 2;

	dot::id id_of_dot_node = make_id (state_node, false);
	on_node_in_table (id_of_dot_node, xp, state_node,
			  max_depth, 0, num_columns);

	auto node = std::make_unique<dot::node_stmt> (std::move (id_of_dot_node));
	node->m_attrs.add (dot::id ("shape"),
			   dot::id ("plaintext"));

	// xml must be done by now

	node->m_attrs.add (dot::id ("label"),
			   dot::id (table));

	parent_subgraph.m_stmt_list.add_stmt (std::move (node));
      }
    else
      {
	auto child_subgraph = std::make_unique<dot::subgraph> (std::move (sg_id));

	if (const char *label = get_label_for_node (state_node))
	  child_subgraph->add_attr (dot::id ("label"), dot::id (label));

	// recurse:
	for (size_t i = 0; i < state_node.get_num_children (); ++i)
	  on_input_state_node (*child_subgraph,
			       state_node.get_child (i));
	parent_subgraph.m_stmt_list.add_stmt (std::move (child_subgraph));
      }
  }

  enum class style { h1, h2 };

  void
  add_title_tr (const dot::id &id_of_dot_node,
		xml::printer &xp,
		int num_columns,
		const diagnostics::digraphs::node &state_node,
		std::string heading,
		enum style styl,
		enum state_node_properties::dynalloc_state_t dynalloc_state)
  {
    xp.push_tag ("tr", true);
    xp.push_tag ("td", false);
    xp.set_attr ("colspan", std::to_string (num_columns));
    xp.set_attr ("cellpadding", "5");

    const char *bgcolor;
    const char *color;
    if (const char *c = get_color_for_dynalloc_state (dynalloc_state))
      {
	bgcolor = c;
	color = "white";
      }
    else
      switch (styl)
	{
	default:
	  gcc_unreachable ();
	case style::h1:
	  // from diagnostics/html-sink.cc: HTML_STYLE .linenum
	  bgcolor = "#0088ce";
	  color = "white";
	  break;
	case style::h2:
	  // from diagnostics/html-sink.cc: HTML_STYLE .events-hdr
	  bgcolor = "#393f44"; // pf-black-800
	  color = "white";
	  break;
	}

    xp.set_attr ("bgcolor", bgcolor);
    xp.push_tag ("font", false);
    xp.set_attr ("color", color);
    if (heading == "")
      heading = " ";
    xp.add_text (std::move (heading));
    xp.pop_tag ("font");

    maybe_add_dst_port (id_of_dot_node, xp, state_node);

    xp.pop_tag ("td");
    xp.pop_tag ("tr");
  }

  /* Recursively add <TR> to XP for STATE_NODE and its descendents.  */
  void
  on_node_in_table (const dot::id &id_of_dot_node,
		    xml::printer &xp,
		    const diagnostics::digraphs::node &state_node,
		    int max_depth,
		    int depth,
		    int num_columns)
  {
    bool recurse = true;
    auto input_node_kind
      = state_node.get_property (state_node_properties::kind_prop);

    switch (input_node_kind)
      {
      case state_node_properties::kind_t::padding:
      case state_node_properties::kind_t::other:
	return;

      case state_node_properties::kind_t::stack:
	add_title_tr (id_of_dot_node, xp, num_columns, state_node, "Stack",
		      style::h1,
		      state_node_properties::dynalloc_state_t::unknown);
	break;
      case state_node_properties::kind_t::stack_frame:
	if (auto logical_loc = state_node.get_logical_loc ())
	  if (const char *function
		= m_logical_loc_mgr.get_short_name (logical_loc))
	    add_title_tr (id_of_dot_node, xp, num_columns, state_node,
			  std::string ("Frame: ") + function,
			  style::h2,
			  state_node_properties::dynalloc_state_t::unknown);
	break;
      case state_node_properties::kind_t::dynalloc_buffer:
	{
	  enum state_node_properties::dynalloc_state_t dynalloc_st
	    = state_node.get_property
		(state_node_properties::dynalloc_state_prop);
	  const char *extents
	    = state_node.get_property (state_node_properties::dynamic_extents);
	  const char *type = state_node.get_property (state_node_properties::type);
	  pretty_printer pp;
	  switch (dynalloc_st)
	    {
	    default:
	      gcc_unreachable ();

	    case state_node_properties::dynalloc_state_t::unknown:
	    case state_node_properties::dynalloc_state_t::nonnull:
	      if (type)
		{
		if (extents)
		  pp_printf (&pp, "%s (%s byte allocation)",
			     type, extents);
		else
		  pp_printf (&pp, "%s", type);
		}
	      else
		{
		  if (extents)
		    pp_printf (&pp, "%s byte allocation",
			       extents);
		}
	      break;

	    case state_node_properties::dynalloc_state_t::unchecked:
	      if (type)
		{
		  if (extents)
		    pp_printf (&pp, "%s (unchecked %s byte allocation)",
			       type, extents);
		}
	      else
		{
		  if (extents)
		    pp_printf (&pp, "Unchecked %s byte allocation",
			       extents);
		}
	      break;

	    case state_node_properties::dynalloc_state_t::freed:
	      // TODO: show deallocator
	      // TODO: show deallocation event
	      pp_printf (&pp, "Freed buffer");
	      break;
	    }
	  maybe_add_dst_port (id_of_dot_node, xp, state_node);
	  add_title_tr (id_of_dot_node, xp, num_columns, state_node,
			pp_formatted_text (&pp),
			style::h2,
			dynalloc_st);
	}
	break;

      default:
	{
	  xp.push_tag ("tr", true);

	  maybe_add_dst_port (id_of_dot_node, xp, state_node);

	  if (depth > 0)
	    {
	      /* Indent, by create a <td> spanning "depth" columns.  */
	      xp.push_tag ("td", false);
	      xp.set_attr ("colspan", std::to_string (depth));
	      xp.add_text (" "); // graphviz doesn't like <td/>
	      xp.pop_tag ("td");
	    }

	  switch (input_node_kind)
	    {
	    default:
	      break;
	    case state_node_properties::kind_t::variable:
	      {
		const char *name
		  = state_node.get_property (state_node_properties::name);
		gcc_assert (name);
		xp.push_tag ("td", false);
		maybe_add_dst_port (id_of_dot_node, xp, state_node);
		push_src_text (xp);
		xp.add_text (name);
		pop_src_text (xp);
		xp.pop_tag ("td");
	      }
	      break;
	    case state_node_properties::kind_t::element:
	      {
		const char *index
		  = state_node.get_property (state_node_properties::index);
		gcc_assert (index);
		xp.push_tag ("td", false);
		maybe_add_dst_port (id_of_dot_node, xp, state_node);
		push_src_text (xp);
		xp.add_text ("[");
		xp.add_text (index);
		xp.add_text ("]");
		pop_src_text (xp);
		xp.pop_tag ("td");
	      }
	      break;
	    case state_node_properties::kind_t::field:
	      {
		const char *name
		  = state_node.get_property (state_node_properties::name);
		gcc_assert (name);
		xp.push_tag ("td", false);
		maybe_add_dst_port (id_of_dot_node, xp, state_node);
		push_src_text (xp);
		xp.add_text (".");
		xp.add_text (name);
		pop_src_text (xp);
		xp.pop_tag ("td");
	      }
	      break;
	    }

	  if (const char *type
		= state_node.get_property (state_node_properties::type))
	    {
	      xp.push_tag ("td", false);
	      xp.set_attr ("align", "right");
	      push_src_text (xp);
	      xp.add_text (type);
	      pop_src_text (xp);
	      xp.pop_tag ("td");
	    }

	  if (const char *value
		= state_node.get_property (state_node_properties::value_str))
	    {
	      xp.push_tag ("td", false);
	      xp.set_attr ("align", "left");
	      maybe_add_src_port (id_of_dot_node, xp, state_node);
	      push_src_text (xp);
	      xp.add_text (value);
	      pop_src_text (xp);
	      xp.pop_tag ("td");
	      recurse = false;
	    }

	  xp.pop_tag ("tr");
	}
	break;
      }

    if (recurse)
      for (size_t i = 0; i < state_node.get_num_children (); ++i)
	on_node_in_table (id_of_dot_node, xp,
			  state_node.get_child (i),
			  max_depth, depth + 1, num_columns);
  }

  void
  push_src_text (xml::printer &xp)
  {
    xp.push_tag ("font");
    xp.set_attr ("color", "blue");
  }

  void
  pop_src_text (xml::printer &xp)
  {
    xp.pop_tag ("font");
  }

  /* If STATE_NODE is in m_src_nodes, add a port to XP for possible
     incoming edges to use.  */

  void
  maybe_add_src_port (const dot::id &id_of_dot_node,
		      xml::printer &xp,
		      const diagnostics::digraphs::node &state_node)
  {
    auto iter = m_src_nodes.find (&state_node);
    if (iter == m_src_nodes.end ())
      return;

    dot::id src_id = make_id (state_node, false);
    dot::node_id node_id (id_of_dot_node,
			  dot::port (src_id,
				     dot::compass_pt::e));
    m_src_node_to_port_id.insert ({&state_node, node_id});
    xp.set_attr ("port", src_id.m_str);
  }

  /* If STATE_NODE is in m_dst_nodes, add a port to XP for possible
     incoming edges to use.  */

  void
  maybe_add_dst_port (const dot::id &id_of_dot_node,
		      xml::printer &xp,
		      const diagnostics::digraphs::node &state_node)
  {
    auto iter = m_dst_nodes.find (&state_node);
    if (iter == m_dst_nodes.end ())
      return;

    dot::id dst_id = make_id (state_node, false);
    dot::node_id node_id (id_of_dot_node,
			  dot::port (dst_id/*,
					     dot::compass_pt::w*/));
    m_dst_node_to_port_id.insert ({&state_node, node_id});
    xp.set_attr ("port", dst_id.m_str);
  }

private:
  const logical_locations::manager &m_logical_loc_mgr;

  /* All nodes involved in edges (and thus will need a port).  */
  std::set<const digraphs::node *> m_src_nodes;
  std::set<const digraphs::node *> m_dst_nodes;

  std::map<const digraphs::node *, dot::node_id> m_src_node_to_port_id;
  std::map<const digraphs::node *, dot::node_id> m_dst_node_to_port_id;
};

std::unique_ptr<dot::graph>
state_graphs::
make_dot_graph (const digraphs::digraph &state_graph,
		const logical_locations::manager &logical_loc_mgr)
{
  return std::make_unique<state_diagram> (state_graph, logical_loc_mgr);
}
