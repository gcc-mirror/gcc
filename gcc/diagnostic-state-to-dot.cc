/* Creating GraphViz .dot files from XML state documents.
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

#include "xml.h"
#include "xml-printer.h"
#include "graphviz.h"

static int
get_depth (const xml::element &e)
{
  int deepest_child = 0;
  for (auto &iter : e.m_children)
    if (xml::element *child_element = iter->dyn_cast_element ())
      deepest_child = std::max (deepest_child,
				get_depth (*child_element));
  return deepest_child + 1;
}

enum class dynalloc_state
{
  unknown,
  nonnull,
  unchecked,
  freed
};

static const char *
get_color_for_dynalloc_state (enum dynalloc_state dynalloc_st)
{
  switch (dynalloc_st)
    {
    default:
      gcc_unreachable ();
      break;
    case dynalloc_state::unknown:
    case dynalloc_state::nonnull:
      return nullptr;

    case dynalloc_state::unchecked:
      return "#ec7a08"; // pf-orange-400

    case dynalloc_state::freed:
      return "#cc0000"; // pf-red-100
    }
}

static void
set_color_for_dynalloc_state (dot::attr_list &attrs,
			      enum dynalloc_state dynalloc_state)
{
  if (const char *color = get_color_for_dynalloc_state (dynalloc_state))
    attrs.add (dot::id ("color"), dot::id (color));
}

static enum dynalloc_state
get_dynalloc_state (const xml::element &input_element)
{
  const char *dyn_alloc_state = input_element.get_attr ("dynamic-alloc-state");
  if (!dyn_alloc_state)
    return dynalloc_state::unknown;

  if (dyn_alloc_state == std::string ("unchecked"))
    return dynalloc_state::unchecked;

  if (dyn_alloc_state == std::string ("nonnull"))
    return dynalloc_state::nonnull;

  if (dyn_alloc_state == std::string ("freed"))
    return dynalloc_state::freed;

  return dynalloc_state::unknown;
}

class state_diagram : public dot::graph
{
public:
  state_diagram (const xml::document &input_state_doc)
  : m_next_id (0),
    m_show_tags (false)
  {
    // "node [shape=plaintext]\n"
    {
      auto attr_stmt
	= std::make_unique<dot::attr_stmt> (dot::attr_stmt::kind::node);
      attr_stmt->m_attrs.add (dot::id ("shape"), dot::id ("plaintext"));
      add_stmt (std::move (attr_stmt));
    }

    /* Recurse down the XML state diagram, creating subgraphs
       and then eventually creating nodes, and recursively
       creating XML tables, adding ports for the endpoints of edges,
       and recording edges we'll want to create (into m_pending_edges).  */
    xml::element *input_elmt_state_diagram
      = input_state_doc.find_child_element ("state-diagram");
    gcc_assert (input_elmt_state_diagram);
    xml::element *input_elmt_mem_regions
      = input_elmt_state_diagram->find_child_element ("memory-regions");
    if (!input_elmt_mem_regions)
      return;
    auto root_cluster
      = std::make_unique<dot::subgraph> (dot::id ("cluster_memory_regions"));
    for (auto &iter : input_elmt_mem_regions->m_children)
      on_input_xml_node (*root_cluster, *iter);
    add_stmt (std::move (root_cluster));

    /* We should now have ports for edge endpoints for all region ids.
       Use them now to create edges.  */
    for (auto &pe : m_pending_edges)
      {
	auto search = m_region_id_to_dst_node_id.find (pe.m_dst_region_id);
	if (search != m_region_id_to_dst_node_id.end ())
	  {
	    auto &dst_node_id = search->second;
	    auto e = std::make_unique<dot::edge_stmt> (pe.m_src_node_id,
						       dst_node_id);

	    auto dynalloc_state = m_region_id_to_dynalloc_state.find (pe.m_dst_region_id);
	    if (dynalloc_state != m_region_id_to_dynalloc_state.end ())
	      set_color_for_dynalloc_state (e->m_attrs,
					    dynalloc_state->second);

	    add_stmt (std::move (e));
	  }
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
  make_id (bool cluster = false)
  {
    if (cluster)
      return std::string ("cluster_") + std::to_string (m_next_id++);
    else
      return std::string ("id_") + std::to_string (m_next_id++);
  }

  bool
  starts_node_p (const xml::element &e)
  {
    if (e.m_kind == "stack"
	|| e.m_kind == "heap-buffer"
	|| e.m_kind == "variable") // e.g. within globals
      return true;
    return false;
  }

  void
  on_input_xml_node (dot::subgraph &parent_subgraph,
		     xml::node &input_node)
  {
    xml::element *input_element = input_node.dyn_cast_element ();
    if (!input_element)
      return;

    dot::id sg_id = make_id (true);

    if (starts_node_p (*input_element))
      {
	// Create node with table
	xml::element table ("table", false);
	xml::printer xp (table);
	xp.set_attr ("border", "0");
	xp.set_attr ("cellborder", "1");
	xp.set_attr ("cellspacing", "0");

	const int max_depth = get_depth (*input_element);
	const int num_columns = max_depth + 2;

	dot::id id_of_node = make_id ();
	on_xml_node (id_of_node, xp, *input_element,
		     max_depth, 0, num_columns);

	auto node = std::make_unique<dot::node_stmt> (std::move (id_of_node));
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

	if (const char *label = input_element->get_attr ("label"))
	  child_subgraph->add_attr (dot::id ("label"), dot::id (label));

	// recurse:
	for (auto &iter : input_element->m_children)
	  on_input_xml_node (*child_subgraph, *iter);
	parent_subgraph.m_stmt_list.add_stmt (std::move (child_subgraph));
      }
  }

  enum class style { h1, h2 };

  void
  add_title_tr (const dot::id &id_of_node,
		xml::printer &xp,
		int num_columns,
		const xml::element &input_element,
		std::string heading,
		enum style styl,
		enum dynalloc_state dynalloc_state)
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
	  // from diagnostic-format-html.cc: HTML_STYLE .linenum
	  bgcolor = "#0088ce";
	  color = "white";
	  break;
	case style::h2:
	  // from diagnostic-format-html.cc: HTML_STYLE .events-hdr
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

    maybe_add_dst_port (id_of_node, xp, input_element);

    xp.pop_tag ("td");
    xp.pop_tag ("tr");
  }

  /* Recursively add <TR> to XP for INPUT_NODE and its descendents.  */
  void
  on_xml_node (const dot::id &id_of_node,
	       xml::printer &xp,
	       xml::node &input_node,
	       int max_depth,
	       int depth,
	       int num_columns)
  {
    bool recurse = true;

    xml::element *input_element = input_node.dyn_cast_element ();
    if (!input_element)
      return;

    if (input_element->m_kind == "concrete-bindings")
      return;
    if (input_element->m_kind == "padding")
      return;

    if (input_element->m_kind == "stack")
      {
	add_title_tr (id_of_node, xp, num_columns, *input_element, "Stack",
		      style::h1, dynalloc_state::unknown);
      }
    else if (input_element->m_kind == "stack-frame")
      {
	if (const char *function = input_element->get_attr ("function"))
	  add_title_tr (id_of_node, xp, num_columns, *input_element,
			std::string ("Frame: ") + function,
			style::h2, dynalloc_state::unknown);
      }
    else if (input_element->m_kind == "heap-buffer")
      {
	const char *extents = input_element->get_attr ("dynamic-extents");
	enum dynalloc_state dynalloc_st = get_dynalloc_state (*input_element);
	if (auto region_id = input_element->get_attr ("region_id"))
	    m_region_id_to_dynalloc_state[region_id] = dynalloc_st;
	const char *type = input_element->get_attr ("type");
	pretty_printer pp;
	switch (dynalloc_st)
	  {
	  default:
	    gcc_unreachable ();

	  case dynalloc_state::unknown:
	  case dynalloc_state::nonnull:
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

	  case dynalloc_state::unchecked:
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

	  case dynalloc_state::freed:
	    // TODO: show deallocator
	    // TODO: show deallocation event
	    pp_printf (&pp, "Freed buffer");
	    break;
	  }
	add_title_tr (id_of_node, xp, num_columns, *input_element,
		      pp_formatted_text (&pp),
		      style::h2,
		      dynalloc_st);
      }
    else
      {
	xp.push_tag ("tr", true);
	if (depth > 0)
	  {
	    /* Indent, by create a <td> spanning "depth" columns.  */
	    xp.push_tag ("td", false);
	    xp.set_attr ("colspan", std::to_string (depth));
	    xp.add_text (" "); // graphviz doesn't like <td/>
	    xp.pop_tag ("td");
	  }
	if (m_show_tags)
	  {
	    // Debug: show XML tag
	    xp.push_tag ("td", false);
	    xp.add_text ("<");
	    xp.add_text (input_element->m_kind);
	    xp.add_text (">");
	    xp.pop_tag ("td");
	  }
	if (input_element->m_kind == "variable")
	  {
	    const char *name = input_element->get_attr ("name");
	    gcc_assert (name);
	    xp.push_tag ("td", false);
	    maybe_add_dst_port (id_of_node, xp, *input_element);
	    push_src_text (xp);
	    xp.add_text (name);
	    pop_src_text (xp);
	    xp.pop_tag ("td");
	  }
	else if (input_element->m_kind == "element")
	  {
	    const char *index = input_element->get_attr ("index");
	    gcc_assert (index);
	    xp.push_tag ("td", false);
	    maybe_add_dst_port (id_of_node, xp, *input_element);
	    push_src_text (xp);
	    xp.add_text ("[");
	    xp.add_text (index);
	    xp.add_text ("]");
	    pop_src_text (xp);
	    xp.pop_tag ("td");
	  }
	else if (input_element->m_kind == "field")
	  {
	    const char *name = input_element->get_attr ("name");
	    gcc_assert (name);
	    xp.push_tag ("td", false);
	    maybe_add_dst_port (id_of_node, xp, *input_element);
	    push_src_text (xp);
	    xp.add_text (".");
	    xp.add_text (name);
	    pop_src_text (xp);
	    xp.pop_tag ("td");
	  }
	if (const char *type = input_element->get_attr ("type"))
	  {
	    xp.push_tag ("td", false);
	    if (max_depth > depth)
	      xp.set_attr ("colspan", std::to_string (max_depth - depth));
	    xp.set_attr ("align", "right");
	    push_src_text (xp);
	    xp.add_text (type);
	    pop_src_text (xp);
	    xp.pop_tag ("td");
	  }
	if (auto value = input_element->find_child_element ("value-of-region"))
	  {
	    xp.push_tag ("td", false);
	    for (auto &iter : value->m_children)
	      if (auto child_element = iter->dyn_cast_element ())
		print_value (id_of_node, xp, *child_element);
	    xp.pop_tag ("td");
	    recurse = false;
	  }
	xp.pop_tag ("tr");
      }

    if (recurse)
      for (auto &iter : input_element->m_children)
	on_xml_node (id_of_node, xp, *iter, max_depth, depth + 1, num_columns);
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

  void
  print_value (const dot::id &id_of_node,
	       xml::printer &xp,
	       xml::element &input_element)
  {
    if (input_element.m_kind == "pointer-to-region")
      if (const char *dst_region_id = input_element.get_attr ("region_id"))
	{
	  dot::id src_port_id = make_id ();
	  xp.set_attr ("port", src_port_id.m_str);
	  m_pending_edges.push_back
	    ({dot::node_id (id_of_node,
			    dot::port (src_port_id,
				       dot::compass_pt::e)),
	       dst_region_id});
      }

    if (input_element.m_kind == "uninitialized")
      {
	xp.add_text ("(uninitialized)");
	return;
      }

    if (auto dump_text = input_element.get_attr ("dump-text"))
      xp.add_text (dump_text);
  }

  /* If INPUT_ELEMENT has a "region_id", add a port to XP for possible
     incoming edges to use.  */

  void
  maybe_add_dst_port (const dot::id &id_of_node,
		      xml::printer &xp,
		      const xml::element &input_element)
  {
    if (const char *region_id = input_element.get_attr ("region_id"))
      {
	dot::id dst_id = make_id ();
	dot::node_id node_id (id_of_node,
			      dot::port (dst_id/*,
						 dot::compass_pt::w*/));
	xp.set_attr ("port", dst_id.m_str);
	m_region_id_to_dst_node_id.emplace (std::string (region_id),
					    std::move (node_id));
      }
  }


private:
  int m_next_id;
  std::vector<pending_edge> m_pending_edges;
  std::map<std::string, dot::node_id> m_region_id_to_dst_node_id;
  std::map<std::string, enum dynalloc_state> m_region_id_to_dynalloc_state;
  bool m_show_tags;
};

std::unique_ptr<dot::graph>
make_dot_graph_from_xml_state (const xml::document &xml_state)
{
  return std::make_unique<state_diagram> (xml_state);
}
