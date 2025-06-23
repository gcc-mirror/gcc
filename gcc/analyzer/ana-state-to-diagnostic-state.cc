/* Converting ana::program_state to XML state documents.
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

#include "xml.h"
#include "xml-printer.h"

#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/record-layout.h"
#include "analyzer/ana-state-to-diagnostic-state.h"

#if ENABLE_ANALYZER

#if __GNUC__ >= 10
#pragma GCC diagnostic ignored "-Wformat"
#endif

namespace ana {

static void
set_wi_attr (xml::element &e,
	     const char *attr_name,
	     const wide_int_ref &w,
	     signop sgn)
{
  pretty_printer pp;
  pp_wide_int (&pp, w, sgn);
  e.set_attr (attr_name, pp_formatted_text (&pp));
}

static void
set_type_attr (xml::element &e, const_tree type)
{
  gcc_assert (type);
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_printf (&pp, "%T", type);
  e.set_attr ("type", pp_formatted_text (&pp));
}

static void
set_bits_attr (xml::element &e,
	       bit_range bits)
{
  pretty_printer pp;
  bits.dump_to_pp (&pp);
  e.set_attr ("bits", pp_formatted_text (&pp));
}

static void
set_region_id_attr (xml::element &e,
		    const region &reg)
{
  e.set_attr ("region_id", std::to_string (reg.get_id ()));
}

// class xml_state : public xml::document

xml_state::xml_state (const program_state &state,
		      const extrinsic_state &ext_state)
: xml::document (),
  m_state (state),
  m_ext_state (ext_state),
  m_mgr (*ext_state.get_engine ()->get_model_manager ()),
  m_root (nullptr)
{
  auto root = std::make_unique<xml::element> ("state-diagram", false);
  m_root = root.get ();
  add_child (std::move (root));

  /* Find pointers to heap-allocated regions, and record their types,
     so that we have a user-friendly way of showing the memory
     (by field, rather than by byte offset).  */
  for (auto cluster_iter : *state.m_region_model->get_store ())
    for (auto binding_iter : *cluster_iter.second)
      {
	const svalue *svalue = binding_iter.second;
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
      get_or_create_element (*reg);
    }

  /* Create bound memory.  */
  for (auto iter : *state.m_region_model->get_store ())
    {
      const bool create_all = false; // "true" for verbose, for debugging
      create_elements_for_binding_cluster (*iter.second, create_all);
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
	  sm.add_state_to_xml (*this, *iter.first, iter.second.m_state);
	if (auto s = smap->get_global_state ())
	  sm.add_global_state_to_xml (*this, s);
      }
  }
}

xml::element &
xml_state::get_or_create_element (const region &reg)
{
  auto existing = m_region_to_element_map.find (&reg);
  if (existing != m_region_to_element_map.end ())
    return *existing->second;

  auto &e = create_and_add_element (reg);
  m_region_to_element_map[&reg] = &e;
  return e;
}

xml::element&
xml_state::create_and_add_element (const region &reg)
{
  auto e = create_element (reg);
  xml::element &result = *e;
  if (auto parent_reg = reg.get_parent_region ())
    {
      auto parent_element = &get_or_create_element (*parent_reg);
      parent_element->add_child (std::move (e));
    }
    else
      m_root->add_child (std::move (e));
  return result;
}

std::unique_ptr<xml::element>
xml_state::make_memory_space_element (const char *label)
{
  auto e = std::make_unique<xml::element> ("memory-space", false);
  e->set_attr ("label", label);
  return e;
}

std::unique_ptr<xml::element>
xml_state::create_element (const region &reg)
{
  std::unique_ptr<xml::element> e;
  switch (reg.get_kind ())
    {
    default:
      gcc_unreachable ();

    case RK_FRAME:
      {
	e = std::make_unique<xml::element> ("stack-frame", false);
	const frame_region &frame_reg
	  = static_cast<const frame_region &> (reg);
	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  pp_printf (&pp, "%E", frame_reg.get_fndecl ());
	  e->set_attr ("function", pp_formatted_text (&pp));
	}
      }
      break;
    case RK_GLOBALS:
      e = make_memory_space_element ("Globals");
      break;
    case RK_CODE:
      e = make_memory_space_element ("Code");
      break;
    case RK_FUNCTION:
      e = std::make_unique<xml::element> ("function", false);
      // TODO
      break;
    case RK_LABEL:
      e = std::make_unique<xml::element> ("label", false);
      // TODO
      break;
    case RK_STACK:
      e = std::make_unique<xml::element> ("stack", false);
      break;
    case RK_HEAP:
      e = make_memory_space_element ("Heap");
      break;
    case RK_THREAD_LOCAL:
      e = make_memory_space_element ("Thread-local");
      break;
    case RK_ROOT:
      e = std::make_unique<xml::element> ("memory-regions", false);
      break;
    case RK_SYMBOLIC:
      e = std::make_unique<xml::element> ("symbolic-region", false);
      // TODO
      break;
    case RK_DECL:
      {
	e = std::make_unique<xml::element> ("variable", false);
	const decl_region &decl_reg
	  = static_cast<const decl_region &> (reg);
	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  pp_printf (&pp, "%E", decl_reg.get_decl ());
	  e->set_attr ("name", pp_formatted_text (&pp));
	}
	set_type_attr (*e, TREE_TYPE (decl_reg.get_decl ()));
      }
      break;
    case RK_FIELD:
      e = std::make_unique<xml::element> ("field", false);
      break;
    case RK_ELEMENT:
      e = std::make_unique<xml::element> ("element", false);
      break;
    case RK_OFFSET:
      e = std::make_unique<xml::element> ("offset-region", false);
      // TODO
      break;
    case RK_SIZED:
      e = std::make_unique<xml::element> ("sized-region", false);
      // TODO
      break;
    case RK_CAST:
      e = std::make_unique<xml::element> ("cast-region", false);
      // TODO
      break;
    case RK_HEAP_ALLOCATED:
      e = std::make_unique<xml::element> ("heap-buffer", false);
      set_attr_for_dynamic_extents (reg, *e);
      break;
    case RK_ALLOCA:
      e = std::make_unique<xml::element> ("alloca-buffer", false);
      set_attr_for_dynamic_extents (reg, *e);
      break;
    case RK_STRING:
      e = std::make_unique<xml::element> ("string-region", false);
      // TODO
      break;
    case RK_BIT_RANGE:
      e = std::make_unique<xml::element> ("RK_BIT_RANGE", false); // TODO
      break;
    case RK_VAR_ARG:
      e = std::make_unique<xml::element> ("RK_VAR_ARG", false); // TODO
      break;
    case RK_ERRNO:
      e = std::make_unique<xml::element> ("errno", false);
      break;
    case RK_PRIVATE:
      e = std::make_unique<xml::element> ("RK_PRIVATE", false); // TODO
      break;
    case RK_UNKNOWN:
      e = std::make_unique<xml::element> ("RK_UNKNOWN", false); // TODO
      break;
    }
  gcc_assert (e);

  set_region_id_attr (*e, reg);

  if (reg.get_base_region () == &reg)
    if (!reg.get_type ())
      {
	auto search
	  = m_types_for_untyped_regions.find (&reg);
	if (search != m_types_for_untyped_regions.end ())
	  {
	    tree type_to_use = search->second;
	    set_type_attr (*e, type_to_use);
	  }
      }

  return e;
}

void
xml_state::create_elements_for_binding_cluster (const binding_cluster &cluster,
						bool create_all)
{
  /* TODO:
     - symbolic bindings
     - get current svalue, so as to get "zeros" and "uninitialized".  */

  concrete_bindings_t conc_bindings;
  for (auto iter : cluster)
    {
      const binding_key *key = iter.first;
      const svalue *svalue = iter.second;
      if (auto conc_key = key->dyn_cast_concrete_binding ())
	conc_bindings[conc_key->get_bit_range ()] = svalue;
      if (const region *reg = svalue->maybe_get_region ())
	get_or_create_element (*reg);
    }

  auto &e = get_or_create_element (*cluster.get_base_region ());

  e.add_child (create_element_for_conc_bindings (conc_bindings));

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
    populate_element_for_typed_region (e,
				       *typed_reg,
				       conc_bindings,
				       create_all);
  else
    {
      // TODO
    }
}

std::unique_ptr<xml::element>
xml_state::create_element_for_conc_bindings (const concrete_bindings_t &conc_bindings)
{
  auto e = std::make_unique<xml::element> ("concrete-bindings", false);
  for (auto iter : conc_bindings)
    {
      const bit_range bits = iter.first;
      const svalue *sval = iter.second;
      auto binding_element
	= std::make_unique<xml::element> ("binding", false);
      set_bits_attr (*binding_element, bits);
      {
	pretty_printer pp;
	pp_format_decoder (&pp) = default_tree_printer;
	sval->dump_to_pp (&pp, true);
	binding_element->set_attr ("value", pp_formatted_text (&pp));
	if (auto svalue_element = create_element_for_svalue (sval))
	  binding_element->add_child (std::move (svalue_element));
      }
      e->add_child (std::move (binding_element));
    }
  return e;
}

// Try to get the bit_range of REG within its base region
bool
xml_state::get_bit_range_within_base_region (const region &reg,
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
xml_state::populate_element_for_typed_region (xml::element &e,
					      const region &reg,
					      const concrete_bindings_t &conc_bindings,
					      bool create_all)
{
  const_tree reg_type = reg.get_type ();
  gcc_assert (reg_type);
  set_type_attr (e, reg_type);

  bit_range bits (0, 0);
  if (get_bit_range_within_base_region (reg, bits))
    {
      set_bits_attr (e, bits);

      auto search = conc_bindings.find (bits);
      if (search != conc_bindings.end ())
	{
	  const svalue *bound_sval = search->second;
	  if (auto svalue_element = create_element_for_svalue (bound_sval))
	    {
	      xml::printer xp (e);
	      xp.push_tag ("value-of-region");
	      xp.append (std::move (svalue_element));
	    }
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
	    if (show_child_element_for_child_region_p (*child_reg,
						       conc_bindings,
						       create_all))
	      {
		// Here "element" is in the xml sense
		auto child_element
		  = std::make_unique<xml::element> ("element", false);
		set_wi_attr (*child_element, "index", idx, UNSIGNED);
		set_region_id_attr (*child_element, *child_reg);
		// Recurse:
		gcc_assert (element_type);
		populate_element_for_typed_region (*child_element,
						   *child_reg,
						   conc_bindings,
						   create_all);
		e.add_child (std::move (child_element));
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
		if (show_child_element_for_child_region_p (*child_reg,
							   conc_bindings,
							   create_all))
		  {
		    auto child_element
		      = std::make_unique<xml::element> ("padding", false);
		    set_wi_attr (*child_element, "num_bits",
				 item.m_bit_range.m_size_in_bits, SIGNED);
		    e.add_child (std::move (child_element));
		  }
	      }
	    else
	      {
		const region *child_reg
		  = m_mgr.get_field_region (&reg,
					    const_cast<tree> (item.m_field));
		if (show_child_element_for_child_region_p (*child_reg,
							   conc_bindings,
							   create_all))
		  {
		    auto child_element
		      = std::make_unique<xml::element> ("field", false);
		    {
		      pretty_printer pp;
		      pp_format_decoder (&pp) = default_tree_printer;
		      pp_printf (&pp, "%D", item.m_field);
		      child_element->set_attr ("name",
					       pp_formatted_text (&pp));
		    }
		    set_region_id_attr (*child_element, *child_reg);
		    // Recurse:
		    populate_element_for_typed_region (*child_element,
						       *child_reg,
						       conc_bindings,
						       create_all);
		    e.add_child (std::move (child_element));
		  }
	      }
	  }
      }
      break;
    }
}

void
xml_state::set_attr_for_dynamic_extents (const region &reg, xml::element &e)
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
      e.set_attr ("dynamic-extents", pp_formatted_text (&pp));
    }
}

bool
xml_state::
show_child_element_for_child_region_p (const region &reg,
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

std::unique_ptr<xml::element>
xml_state::create_element_for_svalue (const svalue *sval)
{
  if (!sval)
    return nullptr;

  std::unique_ptr<xml::element> result;
  switch (sval->get_kind ())
    {
    default:
      gcc_unreachable ();
    case SK_REGION:
      {
	const region_svalue *region_sval = (const region_svalue *)sval;
	result
	  = std::make_unique<xml::element> ("pointer-to-region", false);
	set_region_id_attr (*result, *region_sval->get_pointee ());
      }
      break;
    case SK_CONSTANT:
      {
	const constant_svalue *constant_sval = (const constant_svalue *)sval;
	result = std::make_unique<xml::element> ("constant", false);
	pretty_printer pp;
	pp_format_decoder (&pp) = default_tree_printer;
	pp_printf (&pp, "%E", constant_sval->get_constant ());
	result->set_attr ("value", pp_formatted_text (&pp));
      }
      break;
    case SK_UNKNOWN:
      result = std::make_unique<xml::element> ("unknown", false);
      break;
    case SK_POISONED:
      {
	const poisoned_svalue *poisoned_sval = (const poisoned_svalue *)sval;
	switch (poisoned_sval->get_poison_kind ())
	  {
	  default:
	    gcc_unreachable ();
	  case poison_kind::uninit:
	    result = std::make_unique<xml::element> ("uninitialized", false);
	    break;
	  case poison_kind::freed:
	    result = std::make_unique<xml::element> ("freed", false);
	    break;
	  case poison_kind::deleted:
	    result = std::make_unique<xml::element> ("deleted", false);
	    break;
	  case poison_kind::popped_stack:
	    result = std::make_unique<xml::element> ("popped-stack", false);
	    break;
	  }
      }
      break;
    case SK_SETJMP:
      {
	//const setjmp_svalue *setjmp_sval = (const setjmp_svalue *)sval;
	result = std::make_unique<xml::element> ("setjmp-buffer", false);
	// TODO
      }
      break;
    case SK_INITIAL:
      {
	const initial_svalue *initial_sval = (const initial_svalue *)sval;
	result = std::make_unique<xml::element> ("initial-value-of", false);
	set_region_id_attr (*result, *initial_sval->get_region ());
      }
      break;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_sval = (const unaryop_svalue *)sval;
	result = std::make_unique<xml::element> ("unary-op", false);
	result->set_attr ("op", get_tree_code_name (unaryop_sval->get_op ()));
	result->add_child
	  (create_element_for_svalue (unaryop_sval->get_arg ()));
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *binop_sval = (const binop_svalue *)sval;
	result = std::make_unique<xml::element> ("binary-op", false);
	result->set_attr ("op", get_tree_code_name (binop_sval->get_op ()));
	result->add_child (create_element_for_svalue (binop_sval->get_arg0 ()));
	result->add_child (create_element_for_svalue (binop_sval->get_arg1 ()));
      }
      break;
    case SK_SUB:
      {
	//const sub_svalue *sub_sval = (const sub_svalue *)sval;
	result = std::make_unique<xml::element> ("subregion-value", false);
	// TODO
      }
      break;
    case SK_REPEATED:
      {
	const repeated_svalue *repeated_sval = (const repeated_svalue *)sval;
	result = std::make_unique<xml::element> ("repeated-value", false);
	result->add_child
	  (create_element_for_svalue (repeated_sval->get_outer_size ()));
	result->add_child
	  (create_element_for_svalue (repeated_sval->get_inner_svalue ()));
      }
      break;
    case SK_BITS_WITHIN:
      {
	const bits_within_svalue *bits_within_sval
	  = (const bits_within_svalue *)sval;
	result = std::make_unique<xml::element> ("bits-within", false);
	set_bits_attr (*result, bits_within_sval->get_bits ());
	result->add_child
	  (create_element_for_svalue (bits_within_sval->get_inner_svalue ()));
      }
      break;
    case SK_UNMERGEABLE:
      {
	const unmergeable_svalue *unmergeable_sval
	  = (const unmergeable_svalue *)sval;
	result = std::make_unique<xml::element> ("unmergeable", false);
	result->add_child
	  (create_element_for_svalue (unmergeable_sval->get_arg ()));
      }
      break;
    case SK_PLACEHOLDER:
      {
	const placeholder_svalue *placeholder_sval
	  = (const placeholder_svalue *)sval;
	result = std::make_unique<xml::element> ("placeholder", false);
	result->set_attr ("name", placeholder_sval->get_name ());
      }
      break;
    case SK_WIDENING:
      {
	//const widening_svalue *widening_sval = (const widening_svalue *)sval;
	result = std::make_unique<xml::element> ("iterating-value", false);
	// TODO
      }
      break;
    case SK_COMPOUND:
      {
	//const compound_svalue *compound_sval = (const compound_svalue *)sval;
	result = std::make_unique<xml::element> ("compound-value", false);
	// TODO
      }
      break;
    case SK_CONJURED:
      {
	//const conjured_svalue *conjured_sval = (const conjured_svalue *)sval;
	result = std::make_unique<xml::element> ("conjured-value", false);
	// TODO
      }
      break;
    case SK_ASM_OUTPUT:
      {
	/* const asm_output_svalue *asm_output_sval
	   = (const asm_output_svalue *)sval; */
	result = std::make_unique<xml::element> ("asm-output", false);
	// TODO
      }
      break;
    case SK_CONST_FN_RESULT:
      {
	/* const const_fn_result_svalue *const_fn_result_sval
	   = (const const_fn_result_svalue *)sval; */
	result = std::make_unique<xml::element> ("const-fn-result", false);
	// TODO
      }
    }

  if (result)
    {
      if (sval->get_type ())
	set_type_attr (*result, sval->get_type ());

      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      sval->dump_to_pp (&pp, true);
      result->set_attr ("dump-text", pp_formatted_text (&pp));
    }

  return result;
}

std::unique_ptr<xml::document>
program_state::make_xml (const extrinsic_state &ext_state) const
{
  return std::make_unique<xml_state> (*this, ext_state);
}

void
program_state::dump_xml_to_pp (const extrinsic_state &ext_state,
			       pretty_printer *pp) const
{
  auto doc = make_xml (ext_state);
  doc->write_as_xml (pp, 0, true);
}

void
program_state::dump_xml_to_file (const extrinsic_state &ext_state,
				 FILE *outf) const
{
  pretty_printer pp;
  pp.set_output_stream (outf);
  dump_xml_to_pp (ext_state, &pp);
  pp_flush (&pp);
}

void
program_state::dump_xml (const extrinsic_state &ext_state) const
{
  dump_xml_to_file (ext_state, stderr);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
