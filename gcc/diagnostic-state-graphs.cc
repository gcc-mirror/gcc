/* Extensions to diagnostics::digraphs to support state graphs.
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

#include "diagnostic-state-graphs.h"
#include "graphviz.h"
#include "xml.h"
#include "xml-printer.h"
#include "intl.h"
#include "selftest.h"

using namespace diagnostics::state_graphs;

const char * const node_kind_strs[] = {
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

const char *
diagnostics::state_graphs::node_kind_to_str (enum node_kind k)
{
  return node_kind_strs[static_cast<int> (k)];
}

// struct state_node_ref

enum node_kind
state_node_ref::get_node_kind () const
{
  const char *value = get_attr ("kind");
  if (!value)
    return node_kind::other;

  for (size_t i = 0; i < ARRAY_SIZE (node_kind_strs); ++i)
    if (!strcmp (node_kind_strs[i], value))
      return static_cast<enum node_kind> (i);

  return node_kind::other;
}

void
state_node_ref::set_node_kind (enum node_kind k)
{
  set_attr ("kind", node_kind_to_str (k));
}

const char * const dynalloc_state_strs[] = {
  "unknown",
  "nonnull",
  "unchecked",
  "freed"
};

enum node_dynalloc_state
state_node_ref::get_dynalloc_state () const
{
  const char *value = get_attr ("dynalloc-state");
  if (!value)
    return node_dynalloc_state::unknown;

  for (size_t i = 0; i < ARRAY_SIZE (dynalloc_state_strs); ++i)
    if (!strcmp (dynalloc_state_strs[i], value))
      return static_cast<enum node_dynalloc_state> (i);
  
  return node_dynalloc_state::unknown;
}

void
state_node_ref::set_dynalloc_state (enum node_dynalloc_state s) const
{
  set_attr ("dynalloc-state",
	    dynalloc_state_strs[static_cast <size_t> (s)]);
}

const char *
state_node_ref::get_dynamic_extents () const
{
  return m_node.get_attr (STATE_NODE_PREFIX, "dynamic-extents");
}

void
state_node_ref::set_json_attr (const char *key,
			       std::unique_ptr<json::value> value) const
{
  m_node.set_json_attr (STATE_NODE_PREFIX, key, std::move (value));
}

#if CHECKING_P

namespace selftest {

static void
test_node_attrs ()
{
  diagnostics::digraphs::digraph g;
  diagnostics::digraphs::node n (g, "a");
  state_node_ref node_ref (n);

  ASSERT_EQ (node_ref.get_node_kind (), node_kind::other);
  node_ref.set_node_kind (node_kind::stack);
  ASSERT_EQ (node_ref.get_node_kind (), node_kind::stack);

  ASSERT_EQ (node_ref.get_dynalloc_state (), node_dynalloc_state::unknown);
  node_ref.set_dynalloc_state (node_dynalloc_state::freed);
  ASSERT_EQ (node_ref.get_dynalloc_state (), node_dynalloc_state::freed);

  ASSERT_EQ (node_ref.get_type (), nullptr);
  node_ref.set_type ("const char *");
  ASSERT_STREQ (node_ref.get_type (), "const char *");
}

/* Run all of the selftests within this file.  */

void
diagnostic_state_graphs_cc_tests ()
{
  test_node_attrs ();
}

} // namespace selftest

#endif /* CHECKING_P */
