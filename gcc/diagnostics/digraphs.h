/* Directed graphs associated with a diagnostic.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_DIGRAPHS_H
#define GCC_DIAGNOSTICS_DIGRAPHS_H

#include "json.h"
#include "tristate.h"
#include "diagnostics/logical-locations.h"

class graphviz_out;

class sarif_graph;
class sarif_node;
class sarif_edge;

namespace dot { class graph; }

namespace diagnostics {
namespace digraphs {

/* A family of classes: digraph, node, and edge, closely related to
   SARIF's graph, node, and edge types (SARIF v2.1.0 sections 3.39-3.41).

   Nodes can have child nodes, allowing for arbitrarily deep nesting.
   Edges can be between any pair of nodes (potentially at different
   nesting levels).

   Digraphs, nodes, and edges also optionally have a JSON property bag,
   allowing round-tripping of arbitrary key/value pairs through SARIF.  */

class digraph;
class node;
class edge;

/* A base class for digraph, node, and edge to allow them to have
   an optional JSON property bag.  */

class object
{
public:
  /* String properties.  */
  const char *get_property (const json::string_property &property) const;
  void set_property (const json::string_property &property,
		     const char *utf8_value);

  /* Integer properties.  */
  bool maybe_get_property (const json::integer_property &property, long &out) const;
  void set_property (const json::integer_property &property, long value);

  /* Bool properties.  */
  tristate
  get_property_as_tristate (const json::bool_property &property) const;
  void set_property (const json::bool_property &property, bool value);

  /* Array-of-string properties.  */
  json::array *
  get_property (const json::array_of_string_property &property) const;

  /* enum properties.  */
  template <typename EnumType>
  EnumType
  get_property (const json::enum_property<EnumType> &property) const
  {
    if (m_property_bag)
      {
	EnumType result;
	if (m_property_bag->maybe_get_enum<EnumType> (property, result))
	  return result;
      }
    return json::enum_traits<EnumType>::get_unknown_value ();
  }
  template <typename EnumType>
  void
  set_property (const json::enum_property<EnumType> &property,
		EnumType value)
  {
    auto &bag = ensure_property_bag ();
    bag.set_enum<EnumType> (property, value);
  }

  /* json::value properties.  */
  const json::value *get_property (const json::json_property &property) const;
  void set_property (const json::json_property &property,
		     std::unique_ptr<json::value> value);

  json::object *
  get_property_bag () const { return m_property_bag.get (); }

  json::object &
  ensure_property_bag ();

  void
  set_property_bag (std::unique_ptr<json::object> property_bag)
  {
    m_property_bag = std::move (property_bag);
  }

private:
  std::unique_ptr<json::object> m_property_bag;
};

// A directed graph, corresponding to SARIF v2.1.0 section 3.39.

class digraph : public object
{
 public:
  friend class node;
  friend class edge;

  digraph () : m_next_edge_id_index (0) {}
  virtual ~digraph () {}

  const char *
  get_description () const
  {
    if (!m_description)
      return nullptr;
    return m_description->c_str ();
  }

  void
  set_description (const char *desc)
  {
    if (desc)
      m_description = std::make_unique<std::string> (desc);
    else
      m_description = nullptr;
  }
  void
  set_description (std::string desc)
  {
    m_description = std::make_unique<std::string> (std::move (desc));
  }

  node *
  get_node_by_id (const char *id) const
  {
    auto iter = m_id_to_node_map.find (id);
    if (iter == m_id_to_node_map.end ())
      return nullptr;
    return iter->second;
  }

  edge *
  get_edge_by_id (const char *id) const
  {
    auto iter = m_id_to_edge_map.find (id);
    if (iter == m_id_to_edge_map.end ())
      return nullptr;
    return iter->second;
  }

  size_t
  get_num_nodes () const
  {
    return m_nodes.size ();
  }

  node &
  get_node (size_t idx) const
  {
    return *m_nodes[idx].get ();
  }

  size_t
  get_num_edges () const
  {
    return m_edges.size ();
  }

  edge &
  get_edge (size_t idx) const
  {
    return *m_edges[idx].get ();
  }

  void
  dump () const;

  std::unique_ptr<json::object>
  make_json_sarif_graph () const;

  std::unique_ptr<dot::graph>
  make_dot_graph () const;

  void
  add_node (std::unique_ptr<node> n)
  {
    gcc_assert (n);
    m_nodes.push_back (std::move (n));
  }

  void
  add_edge (std::unique_ptr<edge> e)
  {
    gcc_assert (e);
    m_edges.push_back (std::move (e));
  }

  void
  add_edge (const char *id,
	    node &src_node,
	    node &dst_node,
	    const char *label = nullptr);

  std::unique_ptr<digraph> clone () const;

  const char *get_graph_kind () const;
  void set_graph_kind (const char *);

  const std::map<std::string, node *> &
  get_all_nodes () const
  {
    return m_id_to_node_map;
  }

 private:
  void
  add_node_id (std::string node_id, node &new_node)
  {
    m_id_to_node_map.insert ({std::move (node_id), &new_node});
  }
  void
  add_edge_id (std::string edge_id, edge &new_edge)
  {
    m_id_to_edge_map.insert ({std::move (edge_id), &new_edge});
  }

  std::string
  make_edge_id (const char *edge_id);

  std::unique_ptr<std::string> m_description;
  std::map<std::string, node *> m_id_to_node_map;
  std::map<std::string, edge *> m_id_to_edge_map;
  std::vector<std::unique_ptr<node>> m_nodes;
  std::vector<std::unique_ptr<edge>> m_edges;
  size_t m_next_edge_id_index;
};

// A node in a directed graph, corresponding to SARIF v2.1.0 section 3.40.

class node : public object
{
 public:
  virtual ~node () {}

  node (digraph &g, std::string id)
  : m_id (id),
    m_physical_loc (UNKNOWN_LOCATION)
  {
    g.add_node_id (std::move (id), *this);
  }
  node (const node &) = delete;

  std::string
  get_id () const { return m_id; }

  const char *
  get_label () const
  {
    if (!m_label)
      return nullptr;
    return m_label->c_str ();
  }

  void
  set_label (const char *label)
  {
    if (label)
      m_label = std::make_unique<std::string> (label);
    else
      m_label = nullptr;
  }
  void
  set_label (std::string label)
  {
    m_label = std::make_unique<std::string> (std::move (label));
  }

  size_t
  get_num_children () const { return m_children.size (); }

  node &
  get_child (size_t idx) const { return *m_children[idx].get (); }

  void
  add_child (std::unique_ptr<node> child)
  {
    gcc_assert (child);
    m_children.push_back (std::move (child));
  }

  location_t
  get_physical_loc () const
  {
    return m_physical_loc;
  }

  void
  set_physical_loc (location_t physical_loc)
  {
    m_physical_loc = physical_loc;
  }

  logical_locations::key
  get_logical_loc () const
  {
    return m_logical_loc;
  }

  void
  set_logical_loc (logical_locations::key logical_loc)
  {
    m_logical_loc = logical_loc;
  }

  void print (graphviz_out &gv) const;

  void
  dump () const;

  std::unique_ptr<json::object>
  to_json_sarif_node () const;

  std::unique_ptr<node>
  clone (digraph &new_graph,
	 std::map<node *, node *> &node_mapping) const;

private:
  std::string m_id;
  std::unique_ptr<std::string> m_label;
  std::vector<std::unique_ptr<node>> m_children;
  location_t m_physical_loc;
  logical_locations::key m_logical_loc;
};

// An edge in a directed graph, corresponding to SARIF v2.1.0 section 3.41.

class edge : public object
{
 public:
  virtual ~edge () {}

  /* SARIF requires us to provide unique edge IDs within a graph,
     but otherwise we don't need them.
     Pass in nullptr for the id to get the graph to generate a unique
     edge id for us.  */
  edge (digraph &g,
	const char *id,
	node &src_node,
	node &dst_node)
  : m_id (g.make_edge_id (id)),
    m_src_node (src_node),
    m_dst_node (dst_node)
  {
    g.add_edge_id (m_id, *this);
  }

  std::string
  get_id () const { return m_id; }

  const char *
  get_label () const
  {
    if (!m_label)
      return nullptr;
    return m_label->c_str ();
  }

  void
  set_label (const char *label)
  {
    if (label)
      m_label = std::make_unique<std::string> (label);
    else
      m_label = nullptr;
  }

  node &
  get_src_node () const { return m_src_node; }

  node &
  get_dst_node () const { return m_dst_node; }

  void
  dump () const;

  std::unique_ptr<json::object>
  to_json_sarif_edge () const;

  std::unique_ptr<edge>
  clone (digraph &new_graph,
	 const std::map<diagnostics::digraphs::node *, diagnostics::digraphs::node *> &node_mapping) const;

private:
  std::string m_id;
  std::unique_ptr<std::string> m_label;
  node &m_src_node;
  node &m_dst_node;
};

} // namespace digraphs
} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_DIGRAPHS_H */
