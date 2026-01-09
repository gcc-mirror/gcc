/* Extra properties for digraphs in SARIF property bags.
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
#include "custom-sarif-properties/cfg.h"

namespace cfg = custom_sarif_properties::cfg;

#define GRAPH_PREFIX "gcc/cfg/graph/"
const json::string_property cfg::graph::pass_name
  (GRAPH_PREFIX "pass_name");
const json::integer_property cfg::graph::pass_number
  (GRAPH_PREFIX "pass_number");
#undef GRAPH_PREFIX

#define NODE_PREFIX "gcc/cfg/node/"
const json::string_property cfg::node::kind
  (NODE_PREFIX "kind");
#undef NODE_PREFIX

// For node kind: "loop":
#define LOOP_PREFIX "gcc/cfg/loop/"
const json::integer_property cfg::loop::num (LOOP_PREFIX "num");
const json::integer_property cfg::loop::depth (LOOP_PREFIX "depth");
#undef LOOP_PREFIX

// For node kind: "basic_block":
#define BB_PREFIX "gcc/cfg/basic_block/"

const json::string_property cfg::basic_block::kind (BB_PREFIX "kind");
const json::integer_property cfg::basic_block::index (BB_PREFIX "index");
const json::string_property cfg::basic_block::count (BB_PREFIX "count");

const json::array_of_string_property cfg::basic_block::gimple::phis
  (BB_PREFIX "gimple/phis");
const json::array_of_string_property cfg::basic_block::gimple::stmts
  (BB_PREFIX "gimple/stmts");

const json::array_of_string_property cfg::basic_block::rtl::insns
  (BB_PREFIX "rtl/insns");

#undef BB_PREFIX

#define EDGE_PREFIX "gcc/cfg/edge/"
const json::array_of_string_property cfg::edge::flags
  (EDGE_PREFIX "flags");
const json::integer_property cfg::edge::probability_pc
  (EDGE_PREFIX "probability_pc");
#undef EDGE_PREFIX
