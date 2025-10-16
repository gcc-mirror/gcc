/* Creating diagnostic state graphs from ana::program_state.
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
#include "analyzer/common.h"

#include "diagnostics/state-graphs.h"
#include "diagnostics/sarif-sink.h"

#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/record-layout.h"
#include "analyzer/ana-state-to-diagnostic-state.h"

#if ENABLE_ANALYZER

#if __GNUC__ >= 10
#pragma GCC diagnostic ignored "-Wformat"
#endif

namespace ana {

namespace node_properties = custom_sarif_properties::state_graphs::node;

static void
set_wi_attr (diagnostics::digraphs::node &state_node,
	     const json::string_property &property,
	     const wide_int_ref &w,
	     signop sgn)
{
  pretty_printer pp;
  pp_wide_int (&pp, w, sgn);
  state_node.set_property (property, pp_formatted_text (&pp));
}

static void
set_type_attr (diagnostics::digraphs::node &state_node,
	       const_tree type)
{
  gcc_assert (type);
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_printf (&pp, "%T", type);
  state_node.set_property (node_properties::type,
			   pp_formatted_text (&pp));
}

static void
set_bits_attr (diagnostics::digraphs::node & state_node,
	       bit_range bits)
{
  pretty_printer pp;
  bits.dump_to_pp (&pp);
  state_node.set_property (node_properties::bits,
			   pp_formatted_text (&pp));
}

static void
set_value_attrs (diagnostics::digraphs::node &state_node,
		 const svalue &sval)
{
  state_node.set_property (node_properties::value,
			   sval.to_json ());
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  sval.dump_to_pp (&pp, true);
  state_node.set_property (node_properties::value_str,
			   pp_formatted_text (&pp));
}


// class analyzer_state_graph : public diagnostics::digraphs::digraph

analyzer_state_graph::analyzer_state_graph (const program_state &state,
					    const extrinsic_state &ext_state)
: m_state (state),
  m_ext_state (ext_state),
  m_mgr (*ext_state.get_engine ()->get_model_manager ()),
  m_next_id (0)
{
  /* Find pointers to heap-allocated regions, and record their types,
     so that we have a user-friendly way of showing the memory
     (by field, rather than by byte offset).  */
  for (auto cluster_iter : *state.m_region_model->get_store ())
    for (auto binding_iter : *cluster_iter.second)
      {
	const svalue *svalue = binding_iter.m_sval;
	if (const region *reg = svalue->maybe_get_region ())
	  if (svalue->get_type () && !reg->get_type ())
	    {
	      tree pointed_to_type = TREE_TYPE (svalue->get_type ());
	      if (!VOID_TYPE_P (pointed_to_type))
		m_types_for_untyped_regions[reg] = pointed_to_type;
	    }
      }

  /* TODO: look for vtable pointers at the top of dynamically-allocated
     regions and use that type as a fallback.  */

  /* Find regions of interest.
     Create elements per region, and build into hierarchy.
     Add edges for pointers.  */

  /* Create stack, from top to bottom.  */
  for (int i = state.m_region_model->get_stack_depth () - 1; i >= 0; --i)
    {
      const frame_region *reg = state.m_region_model->get_frame_at_index (i);
      get_or_create_state_node (*reg);
    }

  /* Create bound memory.  */
  for (auto iter : *state.m_region_model->get_store ())
    {
      const bool create_all = false; // "true" for verbose, for debugging
      create_state_nodes_for_binding_cluster (*iter.second, create_all);
    }

  /* TODO: Constraints.  */

  /* Annotate with information from state machines.  */
  {
    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (state.m_checker_states, i, smap)
      {
	auto &sm = ext_state.get_sm (i);
	for (const auto &iter : *smap)
	  sm.add_state_to_state_graph (*this, *iter.first, iter.second.m_state);
	if (auto s = smap->get_global_state ())
	  sm.add_global_state_to_state_graph (*this, s);
      }
  }

  /* Process pending edges.  */
  while (m_pending_edges.size () > 0)
    {
      pending_edge item = m_pending_edges.back ();
      m_pending_edges.pop_back ();

      /* Ensure we have a node for the dst region.  This
	 could lead to additional pending edges.  */
      auto &dst_node = get_or_create_state_node (item.m_dst_reg);
      add_edge (nullptr, item.m_src_node, dst_node);
    }
}

diagnostics::digraphs::node &
analyzer_state_graph::get_or_create_state_node (const region &reg)
{
  auto existing = m_region_to_state_node_map.find (&reg);
  if (existing != m_region_to_state_node_map.end ())
    return *existing->second;

  auto &state_node = create_and_add_state_node (reg);
  m_region_to_state_node_map[&reg] = &state_node;
  return state_node;
}

diagnostics::digraphs::node &
analyzer_state_graph::create_and_add_state_node (const region &reg)
{
  auto node = create_state_node (reg);

  diagnostics::digraphs::node &result = *node;
  if (auto parent_reg = reg.get_parent_region ())
    if (parent_reg->get_kind () != RK_ROOT)
      {
	auto &parent_state_node = get_or_create_state_node (*parent_reg);
	parent_state_node.add_child (std::move (node));
	return result;
      }
  add_node (std::move (node));
  return result;
}

std::string
analyzer_state_graph::make_node_id (const char *prefix)
{
  return std::string (prefix) + "-" + std::to_string (m_next_id++);
}

std::string
analyzer_state_graph::make_node_id (const region &reg)
{
  const char *prefix = nullptr;
  switch (reg.get_kind ())
    {
    case RK_ROOT:
    default:
      gcc_unreachable ();
      break;

    case RK_GLOBALS:
      return "globals";
    case RK_CODE:
      return "code";
    case RK_STACK:
      return "stack";
    case RK_HEAP:
      return "heap";

    case RK_FRAME:
      prefix = "frame-region";
      break;
    case RK_FUNCTION:
      prefix = "function-region";
      break;
    case RK_LABEL:
      prefix = "label-region";
      break;
    case RK_THREAD_LOCAL:
      prefix = "thread-local-region";
      break;
    case RK_SYMBOLIC:
      prefix = "symbolic-region";
      break;
    case RK_DECL:
      prefix = "decl-region";
      break;
    case RK_FIELD:
      prefix = "field-region";
      break;
    case RK_ELEMENT:
      prefix = "element-region";
      break;
    case RK_OFFSET:
      prefix = "offset-region";
      break;
    case RK_SIZED:
      prefix = "sized-region";
      break;
    case RK_CAST:
      prefix = "cast-region";
      break;
    case RK_HEAP_ALLOCATED:
      prefix = "heap-allocated-region";
      break;
    case RK_ALLOCA:
      prefix = "alloca-region";
      break;
    case RK_STRING:
      prefix = "string-region";
      break;
    case RK_BIT_RANGE:
      prefix = "bit-range-region";
      break;
    case RK_VAR_ARG:
      prefix = "var-arg-region";
      break;
    case RK_ERRNO:
      prefix = "errno-region";
      break;
    case RK_PRIVATE:
      prefix = "private-region";
      break;
    case RK_UNKNOWN:
      prefix = "unknown-region";
      break;
    }
  return std::string (prefix) + "-" + std::to_string (reg.get_id ());
}

std::unique_ptr<diagnostics::digraphs::node>
analyzer_state_graph::
make_state_node (enum node_properties::kind_t kind,
		 std::string id)
{
  auto node = std::make_unique<diagnostics::digraphs::node> (*this, std::move (id));
  node->set_property (node_properties::kind_prop, kind);
  return node;
}

std::unique_ptr<diagnostics::digraphs::node>
analyzer_state_graph::
make_memspace_state_node (const region &reg,
			  enum node_properties::kind_t kind)
{
  return make_state_node (kind, make_node_id (reg));
}

std::unique_ptr<diagnostics::digraphs::node>
analyzer_state_graph::create_state_node (const region &reg)
{
  std::unique_ptr<diagnostics::digraphs::node> node;

  switch (reg.get_kind ())
    {
    default:
      gcc_unreachable ();

    case RK_FRAME:
      {
	const frame_region &frame_reg
	  = static_cast<const frame_region &> (reg);

	node = make_state_node (node_properties::kind_t::stack_frame,
				make_node_id (reg));
	node->set_logical_loc
	  (m_logical_loc_mgr.key_from_tree (frame_reg.get_fndecl ()));
	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  pp_printf (&pp, "%E", frame_reg.get_fndecl ());
	  node->set_property (node_properties::function,
			      pp_formatted_text (&pp));
	}
      }
      break;

    case RK_GLOBALS:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::globals);
      break;
    case RK_CODE:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::code);
      break;
    case RK_FUNCTION:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::function);
      // TODO
      break;

    case RK_STACK:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::stack);
      break;
    case RK_HEAP:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::heap_);
      break;
    case RK_THREAD_LOCAL:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::thread_local_);
      break;
    case RK_ROOT:
      gcc_unreachable ();
      break;
    case RK_SYMBOLIC:
      node = make_memspace_state_node (reg,
				       node_properties::kind_t::other);
      break;

    case RK_DECL:
      {
	node = make_state_node (node_properties::kind_t::variable,
				make_node_id (reg));
	const decl_region &decl_reg
	  = static_cast<const decl_region &> (reg);

	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  pp_printf (&pp, "%E", decl_reg.get_decl ());
	  node->set_property (node_properties::name,
			      pp_formatted_text (&pp));
	}
	set_type_attr (*node, TREE_TYPE (decl_reg.get_decl ()));
      }
      break;

    case RK_FIELD:
    case RK_ELEMENT:
      /* These should be handled in populate_state_node_for_typed_region.  */
      gcc_unreachable ();
      break;

    case RK_LABEL:
    case RK_OFFSET:
    case RK_SIZED:
    case RK_CAST:
    case RK_STRING:
    case RK_BIT_RANGE:
    case RK_VAR_ARG:
    case RK_ERRNO:
    case RK_PRIVATE:
    case RK_UNKNOWN:
      node = make_state_node (node_properties::kind_t::other,
				make_node_id (reg));
      break;

    case RK_HEAP_ALLOCATED:
    case RK_ALLOCA:
      node
	= make_memspace_state_node (reg,
				    node_properties::kind_t::dynalloc_buffer);
      set_attr_for_dynamic_extents (reg, *node);
      break;
    }
  gcc_assert (node);

  if (reg.get_base_region () == &reg)
    if (!reg.get_type ())
      {
	auto search
	  = m_types_for_untyped_regions.find (&reg);
	if (search != m_types_for_untyped_regions.end ())
	  {
	    tree type_to_use = search->second;
	    set_type_attr (*node, type_to_use);
	  }
      }

  return node;
}

void
analyzer_state_graph::
create_state_nodes_for_binding_cluster (const binding_cluster &cluster,
					bool create_all)
{
  /* TODO:
     - symbolic bindings
     - get current svalue, so as to get "zeros" and "uninitialized".  */

  concrete_bindings_t conc_bindings;
  for (auto iter : cluster)
    {
      const binding_key *key = iter.m_key;
      const svalue *svalue = iter.m_sval;
      if (auto conc_key = key->dyn_cast_concrete_binding ())
	conc_bindings[conc_key->get_bit_range ()] = svalue;
      if (const region *reg = svalue->maybe_get_region ())
	get_or_create_state_node (*reg);
    }

  auto &ref = get_or_create_state_node (*cluster.get_base_region ());

  ref.add_child (create_state_node_for_conc_bindings (conc_bindings));

  const region *typed_reg = cluster.get_base_region ();
  if (!typed_reg->get_type ())
    {
      auto search
	= m_types_for_untyped_regions.find (cluster.get_base_region ());
      if (search != m_types_for_untyped_regions.end ())
	{
	  tree type_to_use = search->second;
	  typed_reg = m_mgr.get_cast_region (typed_reg, type_to_use);
	}
    }

  if (typed_reg->get_type ())
    populate_state_node_for_typed_region (ref,
					  *typed_reg,
					  conc_bindings,
					  create_all);
  else
    {
      // TODO
    }
}

std::unique_ptr<diagnostics::digraphs::node>
analyzer_state_graph::create_state_node_for_conc_bindings (const concrete_bindings_t &conc_bindings)
{
  auto node = make_state_node (node_properties::kind_t::other,
			       make_node_id ("concrete-bindings"));
  for (auto iter : conc_bindings)
    {
      const bit_range bits = iter.first;
      const svalue *sval = iter.second;
      auto binding_state_node
	= make_state_node (node_properties::kind_t::other,
			   make_node_id ("binding"));
      set_bits_attr (*binding_state_node, bits);
      gcc_assert (sval);
      set_value_attrs (*binding_state_node, *sval);
      node->add_child (std::move (binding_state_node));
    }
  return node;
}

// Try to get the bit_range of REG within its base region
bool
analyzer_state_graph::get_bit_range_within_base_region (const region &reg,
							bit_range &out)
{
  region_offset start_offset = reg.get_offset (&m_mgr);
  if (!start_offset.concrete_p ())
    return false;
  region_offset next_offset = reg.get_next_offset (&m_mgr);
  if (!next_offset.concrete_p ())
    return false;
  out = bit_range (start_offset.get_bit_offset (),
		   next_offset.get_bit_offset ()
		   - start_offset.get_bit_offset ());
  return true;
}

void
analyzer_state_graph::
populate_state_node_for_typed_region (diagnostics::digraphs::node &state_node,
				      const region &reg,
				      const concrete_bindings_t &conc_bindings,
				      bool create_all)
{
  const_tree reg_type = reg.get_type ();
  gcc_assert (reg_type);
  set_type_attr (state_node, reg_type);

  bit_range bits (0, 0);
  if (get_bit_range_within_base_region (reg, bits))
    {
      set_bits_attr (state_node, bits);

      auto search = conc_bindings.find (bits);
      if (search != conc_bindings.end ())
	{
	  const svalue *bound_sval = search->second;
	  gcc_assert (bound_sval);
	  set_value_attrs (state_node, *bound_sval);
	  if (const region *dst_reg = bound_sval->maybe_get_region ())
	    m_pending_edges.push_back ({state_node, *dst_reg});
	}
    }

  switch (TREE_CODE (reg_type))
    {
    default:
      break;

    case ARRAY_TYPE:
      {
	tree domain = TYPE_DOMAIN (reg_type);
	if (!domain)
	  return;
	const_tree max_idx = TYPE_MAX_VALUE (domain);
	if (!max_idx)
	  return;
	if (TREE_CODE (max_idx) != INTEGER_CST)
	  return;
	const_tree min_idx = TYPE_MIN_VALUE (domain);
	if (TREE_CODE (min_idx) != INTEGER_CST)
	  return;
	for (offset_int idx = wi::to_offset (min_idx);
	     idx <= wi::to_offset (max_idx);
	     ++idx)
	  {
	    const_tree element_type = TREE_TYPE (reg_type);
	    const svalue *sval_index
	      = m_mgr.get_or_create_int_cst (domain, idx);
	    const region *child_reg
	      = m_mgr.get_element_region (&reg,
					  const_cast<tree> (element_type),
					  sval_index);
	    if (show_child_state_node_for_child_region_p (*child_reg,
						       conc_bindings,
						       create_all))
	      {
		auto child_state_node
		  = make_state_node
		      (node_properties::kind_t::element,
		       make_node_id (*child_reg));
		set_wi_attr (*child_state_node,
			     node_properties::index, idx, UNSIGNED);

		// Recurse:
		gcc_assert (element_type);
		populate_state_node_for_typed_region (*child_state_node,
						      *child_reg,
						      conc_bindings,
						      create_all);
		state_node.add_child (std::move (child_state_node));
	      }
	  }
      }
      break;

    case RECORD_TYPE:
      {
	const record_layout layout (reg_type);
	for (auto item : layout)
	  {
	    if (item.m_is_padding)
	      {
		const bit_range bits (0, item.m_bit_range.m_size_in_bits);
		const region *child_reg
		  = m_mgr.get_bit_range (&reg, NULL_TREE, bits);
		if (show_child_state_node_for_child_region_p (*child_reg,
							      conc_bindings,
							      create_all))
		  {
		    auto child_state_node
		      = make_state_node
			  (node_properties::kind_t::padding,
			   make_node_id (*child_reg));
		    set_wi_attr (*child_state_node,
				 node_properties::num_bits,
				 item.m_bit_range.m_size_in_bits, SIGNED);
		    state_node.add_child (std::move (child_state_node));
		  }
	      }
	    else
	      {
		const region *child_reg
		  = m_mgr.get_field_region (&reg,
					    const_cast<tree> (item.m_field));
		if (show_child_state_node_for_child_region_p (*child_reg,
							      conc_bindings,
							      create_all))
		  {
		    auto child_state_node
		      = make_state_node
			  (node_properties::kind_t::field,
			   make_node_id (*child_reg));
		    {
		      pretty_printer pp;
		      pp_format_decoder (&pp) = default_tree_printer;
		      pp_printf (&pp, "%D", item.m_field);
		      child_state_node->set_property (node_properties::name,
						      pp_formatted_text (&pp));
		    }

		    // Recurse:
		    populate_state_node_for_typed_region (*child_state_node,
							  *child_reg,
							  conc_bindings,
							  create_all);
		    state_node.add_child (std::move (child_state_node));
		  }
	      }
	  }
      }
      break;
    }
}

void
analyzer_state_graph::
set_attr_for_dynamic_extents (const region &reg,
			      diagnostics::digraphs::node &state_node)
{
  const svalue *sval = m_state.m_region_model->get_dynamic_extents (&reg);
  if (sval)
    {
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      if (auto cst = sval->maybe_get_constant ())
	pp_wide_int (&pp, wi::to_wide (cst), UNSIGNED);
      else
	sval->dump_to_pp (&pp, true);
      state_node.set_property (state_node_properties::dynamic_extents,
			       pp_formatted_text (&pp));
    }
}

bool
analyzer_state_graph::
show_child_state_node_for_child_region_p (const region &reg,
					  const concrete_bindings_t &conc_bindings,
					  bool create_all)
{
  if (create_all)
    return true;
  bit_range reg_bits (0, 0);
  if (!get_bit_range_within_base_region (reg, reg_bits))
    return true;

  /* Is any of "bits" bound?
     TODO: ideally there would be a more efficient way to do this, using
     spatial relationships.  */
  for (auto iter : conc_bindings)
    {
      const bit_range bound_bits = iter.first;
      if (bound_bits.intersects_p (reg_bits))
	return true;
    }
  return false;
}

std::unique_ptr<diagnostics::digraphs::digraph>
program_state::
make_diagnostic_state_graph (const extrinsic_state &ext_state) const
{
  return std::make_unique<analyzer_state_graph> (*this, ext_state);
}

void
program_state::dump_sarif (const extrinsic_state &ext_state) const
{
  auto g = make_diagnostic_state_graph (ext_state);
  g->dump ();
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
