/* Directed graphs associated with a diagnostic.
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
#include "diagnostics/digraphs.h"
#include "diagnostic-format-sarif.h"

#include "selftest.h"

using digraph = diagnostics::digraphs::digraph;
using digraph_node = diagnostics::digraphs::node;
using digraph_edge = diagnostics::digraphs::edge;

namespace {

class conversion_to_dot
{
public:
  std::unique_ptr<dot::graph>
  make_dot_graph_from_diagnostic_graph (const digraph &);

  std::unique_ptr<dot::stmt>
  make_dot_node_from_digraph_node (const digraph_node &);

  std::unique_ptr<dot::edge_stmt>
  make_dot_edge_from_digraph_edge (const digraph_edge &);

  dot::id
  get_dot_id_for_node (const digraph_node &);

  bool
  has_edges_p (const digraph_node &);

private:
  std::set<const digraph_node *> m_nodes_with_edges;
  std::map<const digraph_node *, dot::stmt *> m_node_map;
};

} // anonymous namespace

// class conversion_to_dot

std::unique_ptr<dot::graph>
conversion_to_dot::
make_dot_graph_from_diagnostic_graph (const diagnostics::digraphs::digraph &input_graph)
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
conversion_to_dot::
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
      return output_node;
    }
  else
    {
      auto output_node = std::make_unique<dot::subgraph> (std::move (dot_id));
      m_node_map[&input_node] = output_node.get ();
      if (const char *label = input_node.get_label ())
	output_node->add_attr (dot::id ("label"), dot::id (label));
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
conversion_to_dot::
make_dot_edge_from_digraph_edge (const digraph_edge &input_edge)
{
  const digraph_node &src_dnode = input_edge.get_src_node ();
  const digraph_node &dst_dnode = input_edge.get_dst_node ();
  auto output_edge
    = std::make_unique<dot::edge_stmt>
    (get_dot_id_for_node (src_dnode),
     get_dot_id_for_node (dst_dnode));
  if (const char *label = input_edge.get_label ())
    output_edge->set_label (dot::id (label));
  return output_edge;
}

dot::id
conversion_to_dot::get_dot_id_for_node (const digraph_node &input_node)
{
  if (has_edges_p (input_node))
    return input_node.get_id ();
  else
    return std::string ("cluster_") + input_node.get_id ();
}

bool
conversion_to_dot::has_edges_p (const digraph_node &input_node)
{
  return m_nodes_with_edges.find (&input_node) != m_nodes_with_edges.end ();
}

// class object

const char *
diagnostics::digraphs::object::
get_attr (const char *key_prefix, const char *key) const
{
  if (!m_property_bag)
    return nullptr;
  std::string prefixed_key = std::string (key_prefix) + key;
  if (json::value *jv = m_property_bag->get (prefixed_key.c_str ()))
    if (json::string *jstr = jv->dyn_cast_string ())
      return jstr->get_string ();
  return nullptr;
}

void
diagnostics::digraphs::object::
set_attr (const char *key_prefix, const char *key, const char *value)
{
  set_json_attr (key_prefix, key, std::make_unique<json::string> (value));
}

void
diagnostics::digraphs::object::
set_json_attr (const char *key_prefix, const char *key, std::unique_ptr<json::value> value)
{
  std::string prefixed_key = std::string (key_prefix) + key;
  if (!m_property_bag)
    m_property_bag = std::make_unique<json::object> ();
  m_property_bag->set (prefixed_key.c_str (), std::move (value));
}

// class digraph

DEBUG_FUNCTION void
diagnostics::digraphs::digraph::dump () const
{
  make_json_sarif_graph ()->dump ();
}

std::unique_ptr<json::object>
diagnostics::digraphs::digraph::make_json_sarif_graph () const
{
  return make_sarif_graph (*this, nullptr, nullptr);
}

std::unique_ptr<dot::graph>
diagnostics::digraphs::digraph::make_dot_graph () const
{
  conversion_to_dot to_dot;
  return to_dot.make_dot_graph_from_diagnostic_graph (*this);
}

std::unique_ptr<diagnostics::digraphs::digraph>
diagnostics::digraphs::digraph::clone () const
{
  auto result = std::make_unique<diagnostics::digraphs::digraph> ();

  if (get_property_bag ())
    result->set_property_bag (get_property_bag ()->clone_as_object ());

  std::map<diagnostics::digraphs::node *, diagnostics::digraphs::node *> node_mapping;

  for (auto &iter : m_nodes)
    result->add_node (iter->clone (*result, node_mapping));
  for (auto &iter : m_edges)
    result->add_edge (iter->clone (*result, node_mapping));

  return result;
}

void
diagnostics::digraphs::digraph::add_edge (const char *id,
					  node &src_node,
					  node &dst_node,
					  const char *label)
{
  auto e = std::make_unique<digraph_edge> (*this,
					   id,
					   src_node,
					   dst_node);
  if (label)
    e->set_label (label);
  add_edge (std::move (e));
}

/* Utility function for edge ids: either use EDGE_ID, or
   generate a unique one for when we don't care about the name.

   Edges in SARIF "SHALL" have an id that's unique within the graph
   (SARIF 2.1.0 §3.41.2).  This is so that graph traversals can refer
   to edges by id (SARIF 2.1.0's §3.43.2 edgeId property).  */

std::string
diagnostics::digraphs::digraph::make_edge_id (const char *edge_id)
{
  /* If we have an id, use it.  */
  if (edge_id)
    return edge_id;

  /* Otherwise, generate a unique one of the form "edgeN".  */
  while (true)
    {
      auto candidate (std::string ("edge")
		      + std::to_string (m_next_edge_id_index++));
      auto iter = m_id_to_edge_map.find (candidate);
      if (iter != m_id_to_edge_map.end ())
	{
	  // Try again with the next index...
	  continue;
	}
      return candidate;
    }
}

// class node

DEBUG_FUNCTION void
diagnostics::digraphs::node::dump () const
{
  to_json_sarif_node ()->dump ();
}

std::unique_ptr<json::object>
diagnostics::digraphs::node::to_json_sarif_node () const
{
  return make_sarif_node (*this, nullptr, nullptr);
}

std::unique_ptr<diagnostics::digraphs::node>
diagnostics::digraphs::node::clone (digraph &new_graph,
				    std::map<node *, node *> &node_mapping) const
{
  auto result
    = std::make_unique<diagnostics::digraphs::node> (new_graph,
						     get_id ());
  node_mapping.insert ({const_cast <node *> (this), result.get ()});

  result->set_logical_loc (m_logical_loc);

  if (get_property_bag ())
    result->set_property_bag (get_property_bag ()->clone_as_object ());

  for (auto &iter : m_children)
    result->add_child (iter->clone (new_graph, node_mapping));

  return result;
}

// class edge

std::unique_ptr<digraph_edge>
digraph_edge::clone (digraph &new_graph,
		     const std::map<node *, node *> &node_mapping) const
{
  auto iter_new_src = node_mapping.find (&m_src_node);
  gcc_assert (iter_new_src != node_mapping.end ());
  auto iter_new_dst = node_mapping.find (&m_dst_node);
  gcc_assert (iter_new_dst != node_mapping.end ());
  auto result
    = std::make_unique<digraph_edge> (new_graph,
				      m_id.c_str (),
				      *iter_new_src->second,
				      *iter_new_dst->second);
  if (get_property_bag ())
    result->set_property_bag (get_property_bag ()->clone_as_object ());

  return result;
}

DEBUG_FUNCTION void
diagnostics::digraphs::edge::dump () const
{
  to_json_sarif_edge ()->dump ();
}

std::unique_ptr<json::object>
diagnostics::digraphs::edge::to_json_sarif_edge () const
{
  return make_sarif_edge (*this, nullptr);
}

// class lazy_digraph

const diagnostics::digraphs::digraph &
diagnostics::digraphs::lazy_digraph::get_or_create_digraph () const
{
  if (!m_digraph)
    m_digraph = create_digraph ();
  gcc_assert (m_digraph);
  return *m_digraph;
}

// class lazy_digraphs

const std::vector<std::unique_ptr<diagnostics::digraphs::digraph>> &
diagnostics::digraphs::lazy_digraphs::get_or_create_digraphs () const
{
  if (!m_digraphs)
    m_digraphs = create_digraphs ();
  gcc_assert (m_digraphs);
  return *m_digraphs;
}

#if CHECKING_P

namespace selftest {

static void
test_empty_graph ()
{
  digraph g;

  {
    auto sarif = g.make_json_sarif_graph ();

    pretty_printer pp;
    sarif->print (&pp, true);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("{\"nodes\": [],\n"
	" \"edges\": []}"));
  }

  {
    auto dg = g.make_dot_graph ();

    pretty_printer pp;
    dot::writer w (pp);
    dg->print (w);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("digraph {\n"
	"}\n"));
  }
}

static void
test_simple_graph ()
{
#define KEY_PREFIX "/placeholder/"
  auto g = std::make_unique<digraph> ();
  g->set_description ("test graph");
  g->set_attr (KEY_PREFIX, "date", "1066");

  auto a = std::make_unique<digraph_node> (*g, "a");
  auto b = std::make_unique<digraph_node> (*g, "b");
  b->set_attr (KEY_PREFIX, "color", "red");
  auto c = std::make_unique<digraph_node> (*g, "c");
  c->set_label ("I am a node label");

  auto e = std::make_unique<digraph_edge> (*g, nullptr, *a, *c);
  e->set_attr (KEY_PREFIX, "status", "copacetic");
  e->set_label ("I am an edge label");
  g->add_edge (std::move (e));

  g->add_node (std::move (a));

  b->add_child (std::move (c));
  g->add_node (std::move (b));
#undef KEY_PREFIX

  {
    auto sarif = g->make_json_sarif_graph ();

    pretty_printer pp;
    sarif->print (&pp, true);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("{\"properties\": {\"/placeholder/date\": \"1066\"},\n"
	" \"nodes\": [{\"id\": \"a\"},\n"
	"           {\"id\": \"b\",\n"
	"            \"properties\": {\"/placeholder/color\": \"red\"},\n"
	"            \"children\": [{\"id\": \"c\"}]}],\n"
	" \"edges\": [{\"id\": \"edge0\",\n"
	"            \"properties\": {\"/placeholder/status\": \"copacetic\"},\n"
	"            \"sourceNodeId\": \"a\",\n"
	"            \"targetNodeId\": \"c\"}]}"));
  }

  {
    auto dg = g->make_dot_graph ();

    pretty_printer pp;
    dot::writer w (pp);
    dg->print (w);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
       ("digraph {\n"
	"    label=\"test graph\";\n"
	"    a;\n"
	"    \n"
	"    subgraph cluster_b {\n"
	"        c [label=\"I am a node label\"];\n"
	"\n"
	"    };\n"
	"    a -> c [label=\"I am an edge label\"];\n"
	"}\n"));
  }
}

/* Run all of the selftests within this file.  */

void
diagnostics_digraphs_cc_tests ()
{
  test_empty_graph ();
  test_simple_graph ();
}

} // namespace selftest

#endif /* CHECKING_P */
