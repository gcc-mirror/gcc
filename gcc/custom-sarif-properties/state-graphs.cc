/* Properties for capturing state graphs in SARIF property bags.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "json.h"
#include "custom-sarif-properties/state-graphs.h"

/* graph.  */
namespace graph = custom_sarif_properties::state_graphs::graph;
#define STATE_GRAPH_PREFIX "gcc/diagnostic_state_graph/"
const char *const graph::prefix = STATE_GRAPH_PREFIX;
#undef STATE_GRAPH_PREFIX

/* node.  */
namespace node = custom_sarif_properties::state_graphs::node;
#define STATE_NODE_PREFIX "gcc/diagnostic_state_node/"

const json::enum_property<enum node::kind_t>
  node::kind_prop (STATE_NODE_PREFIX "kind");

const json::string_property node::function (STATE_NODE_PREFIX "function");

const json::string_property node::dynamic_extents
  (STATE_NODE_PREFIX "dynamic-extents");

const json::string_property node::name (STATE_NODE_PREFIX "name");
const json::string_property node::type (STATE_NODE_PREFIX "type");
const json::json_property node::value (STATE_NODE_PREFIX "value");
const json::string_property node::value_str (STATE_NODE_PREFIX "value_str");

const json::string_property node::index (STATE_NODE_PREFIX "index");

const json::string_property node::bits (STATE_NODE_PREFIX "bits");

const json::string_property node::num_bits (STATE_NODE_PREFIX "num_bits");

const json::string_property node::deallocator (STATE_NODE_PREFIX "deallocator");

const json::string_property node::expected_deallocators
  (STATE_NODE_PREFIX "expected-deallocators");

const json::enum_property<enum node::dynalloc_state_t>
  node::dynalloc_state_prop (STATE_NODE_PREFIX "dynalloc-state");

#undef STATE_NODE_PREFIX


/* edge.  */
namespace edge_props = custom_sarif_properties::state_graphs::edge;
#define STATE_EDGE_PREFIX "gcc/diagnostic_state_edge/"
extern const char *const edge_props::prefix = STATE_EDGE_PREFIX;
#undef STATE_EDGE_PREFIX

// Traits for enum node:kind_t

namespace json {

template<>
enum node::kind_t
json::enum_traits<enum node::kind_t>::get_unknown_value ()
{
  return node::kind_t::other;
}

static const char * const node_kind_strs[] = {
  "globals",
  "code",
  "function",
  "stack",
  "stack-frame",
  "heap",
  "thread-local",
  "dynalloc-buffer",
  "variable",
  "field",
  "padding",
  "element",
  "other",
};

template<>
bool
json::enum_traits<enum node::kind_t>::
maybe_get_value_from_string (const char *str,
			     enum_t &out)
{
  for (size_t i = 0; i < ARRAY_SIZE (node_kind_strs); ++i)
    if (!strcmp (node_kind_strs[i], str))
      {
	out = static_cast<enum_t> (i);
	return true;
      }
  return false;
}

template<>
const char *
json::enum_traits<enum node::kind_t>::get_string_for_value (enum_t value)
{
  return node_kind_strs[static_cast<int> (value)];
}

// Traits for enum node:dynalloc_state_t

template<>
enum node::dynalloc_state_t
json::enum_traits<enum node::dynalloc_state_t>::get_unknown_value ()
{
  return node::dynalloc_state_t::unknown;
}

static const char * const dynalloc_state_strs[] = {
  "unknown",
  "nonnull",
  "unchecked",
  "freed"
};

template<>
bool
json::enum_traits<enum node::dynalloc_state_t>::
maybe_get_value_from_string (const char *str,
			     enum_t &out)
{
  for (size_t i = 0; i < ARRAY_SIZE (dynalloc_state_strs); ++i)
    if (!strcmp (dynalloc_state_strs[i], str))
      {
	out = static_cast<enum_t> (i);
	return true;
      }
  return false;
}

template<>
const char *
json::enum_traits<enum node::dynalloc_state_t>::
get_string_for_value (enum_t value)
{
  return dynalloc_state_strs[static_cast <size_t> (value)];
}

} // namespace json
