/* Creating diagnostic state graphs from ana::program_state.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_ANA_STATE_TO_DIAGNOSTIC_STATE_H
#define GCC_ANALYZER_ANA_STATE_TO_DIAGNOSTIC_STATE_H

#include "diagnostics/state-graphs.h"
#include "tree-logical-location.h"
#include "custom-sarif-properties/state-graphs.h"

namespace ana {

namespace state_node_properties = custom_sarif_properties::state_graphs::node;

class analyzer_state_graph : public diagnostics::digraphs::digraph
{
public:
  analyzer_state_graph (const program_state &state,
			const extrinsic_state &ext_state);
  diagnostics::digraphs::node &
  get_or_create_state_node (const region &reg);

private:

  struct pending_edge
  {
    diagnostics::digraphs::node & m_src_node;
    const region &m_dst_reg;
  };

  diagnostics::digraphs::node &
  create_and_add_state_node (const region &reg);

  std::unique_ptr<diagnostics::digraphs::node>
  make_state_node (enum state_node_properties::kind_t kind,
		   std::string id);

  std::unique_ptr<diagnostics::digraphs::node>
  make_memspace_state_node (const region &reg,
			    enum state_node_properties::kind_t kind);

  std::unique_ptr<diagnostics::digraphs::node>
  create_state_node (const region &reg);

  /* Spatially sorted concrete bindings.  */
  typedef std::map<bit_range, const svalue *> concrete_bindings_t;

  void
  create_state_nodes_for_binding_cluster (const binding_cluster &cluster,
					  bool create_all);

  std::unique_ptr<diagnostics::digraphs::node>
  create_state_node_for_conc_bindings (const concrete_bindings_t &conc_bindings);

  // Try to get the bit_range of REG within its base region
  bool
  get_bit_range_within_base_region (const region &reg,
				    bit_range &out);

  void
  populate_state_node_for_typed_region (diagnostics::digraphs::node &,
					const region &reg,
					const concrete_bindings_t &conc_bindings,
					bool create_all);

  void
  set_attr_for_dynamic_extents (const region &reg,
				diagnostics::digraphs::node &);

  bool
  show_child_state_node_for_child_region_p (const region &reg,
					    const concrete_bindings_t &conc_bindings,
					    bool create_all);

  std::unique_ptr<diagnostics::digraphs::node>
  create_state_node_for_svalue (const svalue *sval);

  std::string make_node_id (const region &reg);
  std::string make_node_id (const char *prefix);

  tree_logical_location_manager m_logical_loc_mgr;
  const program_state &m_state;
  const extrinsic_state &m_ext_state;
  region_model_manager &m_mgr;
  std::map<const region *,
	   diagnostics::digraphs::node *> m_region_to_state_node_map;
  std::map<const region *, tree> m_types_for_untyped_regions;
  unsigned m_next_id;
  std::vector<pending_edge> m_pending_edges;
};

} // namespace ana

#endif /* GCC_ANALYZER_ANA_STATE_TO_DIAGNOSTIC_STATE_H */
