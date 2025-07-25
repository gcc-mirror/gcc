/* Extensions to diagnostics::digraphs to support state graphs.
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

#ifndef GCC_DIAGNOSTICS_STATE_GRAPHS_H
#define GCC_DIAGNOSTICS_STATE_GRAPHS_H

#include "diagnostics/digraphs.h"
#include "logical-location.h"

/* diagnostics::digraphs provides support for directed graphs.

   diagnostics::state_graphs provides a way to extend these graphs
   for representing "state graphs" i.e. a representation of the state
   of memory inside a program, for use e.g. by -fanalyzer.

   Specifically, nodes represent memory regions, and we use property bags
   in these nodes to stash extra properties (e.g. what kind of memory region
   a node is e.g. stack vs heap).  */

class sarif_graph;
namespace dot { class graph; }

namespace diagnostics {
namespace state_graphs {

enum class node_kind
{
  // Memory regions
  globals,
  code,
  function, // code within a particular function
  stack,
  stack_frame,
  heap_,
  thread_local_,

  /* Dynamically-allocated buffer,
     on heap or stack (depending on parent).  */
  dynalloc_buffer,

  variable,

  field, // field within a struct or union
  padding, // padding bits in a struct or union
  element, // element within an array

  other // anything else
};

extern const char *
node_kind_to_str (enum node_kind);

enum class node_dynalloc_state
{
  unknown,
  nonnull,
  unchecked,
  freed
};

/* Prefixes to use in SARIF property bags.  */
#define STATE_GRAPH_PREFIX "gcc/diagnostic_state_graph/"
#define STATE_NODE_PREFIX "gcc/diagnostic_state_node/"
#define STATE_EDGE_PREFIX "gcc/diagnostic_state_edge/"

/* A wrapper around a node that gets/sets attributes, using
   the node's property bag for storage, so that the data roundtrips
   through SARIF.  */

struct state_node_ref
{
  state_node_ref (diagnostics::digraphs::node &node)
  : m_node (node)
  {}

  enum node_kind
  get_node_kind () const;
  void
  set_node_kind (enum node_kind);

  // For node_kind::stack_frame, this will be the function
  logical_location
  get_logical_loc () const
  {
    return m_node.get_logical_loc ();
  }

  // For node_kind::dynalloc_buffer
  enum node_dynalloc_state
  get_dynalloc_state () const;

  void
  set_dynalloc_state (enum node_dynalloc_state) const;

  const char *
  get_dynamic_extents () const;

  const char *
  get_name () const { return get_attr ("name"); }
  void
  set_name (const char *name) const { set_attr ("name", name); }

  const char *
  get_type () const { return get_attr ("type"); }
  void
  set_type (const char *type) const { set_attr ("type", type); }

  const char *
  get_value () const { return get_attr ("value"); }

  const char *
  get_index () const { return get_attr ("index"); }

  const char *
  get_attr (const char *key) const
  {
    return m_node.get_attr (STATE_NODE_PREFIX, key);
  }

  void
  set_attr (const char *key, const char *value) const
  {
    return m_node.set_attr (STATE_NODE_PREFIX, key, value);
  }

  void
  set_json_attr (const char *key, std::unique_ptr<json::value> value) const;

  diagnostics::digraphs::node &m_node;
};

extern std::unique_ptr<dot::graph>
make_dot_graph (const diagnostics::digraphs::digraph &state_graph,
		const logical_location_manager &logical_loc_mgr);

} // namespace state_graphs
} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_STATE_GRAPHS_H */
