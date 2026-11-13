/* Regions of memory.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "sbitmap.h"
#include "fold-const.h"
#include "tree-ssa.h"

#include "diagnostics/markup.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/supergraph.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region.h"
#include "analyzer/region-model.h"
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "analyzer/symbolic-pointers.h"
#include "text-art/dump.h"

#if ENABLE_ANALYZER

namespace ana {

/* class region and its various subclasses.  */

/* class region.  */

region::~region ()
{
}

void
region::print_dump_widget_label (pretty_printer *pp) const
{
  if (get_type ())
    pp_printf (pp, "%qT: ", get_type ());

  start_any_dump_color (pp);
  print_region_for_dump_widget_label (pp);
  end_any_dump_color (pp);
}

void
region::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  add_dump_widget_children_for_region (w, nds);
  if (m_parent)
    w.add_child (m_parent->make_dump_widget (nds, "parent"));
}

/* Determine the base region for this region: when considering bindings
   for this region, the base region is the ancestor which identifies
   which cluster they should be partitioned into.
   Regions within the same struct/union/array are in the same cluster.
   Different decls are in different clusters.  */

const region *
region::get_base_region () const
{
  const region *iter = this;
  while (iter)
    {
      switch (iter->get_kind ())
	{
	case RK_FIELD:
	case RK_ELEMENT:
	case RK_OFFSET:
	case RK_SIZED:
	case RK_BIT_RANGE:
	case RK_CAST:
	  iter = iter->get_parent_region ();
	  continue;
	default:
	  return iter;
	}
    }
  return iter;
}

/* Return true if get_base_region() == this for this region.  */

bool
region::base_region_p () const
{
  switch (get_kind ())
    {
    /* Region kinds representing a descendent of a base region.  */
    case RK_FIELD:
    case RK_ELEMENT:
    case RK_OFFSET:
    case RK_SIZED:
    case RK_CAST:
    case RK_BIT_RANGE:
      return false;

    default:
      return true;
    }
}

/* Return true if this region is ELDER or one of its descendents.  */

bool
region::descendent_of_p (const region *elder) const
{
  const region *iter = this;
  while (iter)
    {
      if (iter == elder)
	return true;
      iter = iter->get_parent_region ();
    }
  return false;
}

/* If this region is a frame_region, or a descendent of one, return it.
   Otherwise return nullptr.  */

const frame_region *
region::maybe_get_frame_region () const
{
  const region *iter = this;
  while (iter)
    {
      if (const frame_region *frame_reg = iter->dyn_cast_frame_region ())
	return frame_reg;
      iter = iter->get_parent_region ();
    }
  return nullptr;
}

/* Get the memory space of this region.  */

enum memory_space
region::get_memory_space () const
{
  const region *iter = this;
  while (iter)
    {
      switch (iter->get_kind ())
	{
	default:
	  break;
	case RK_GLOBALS:
	  return MEMSPACE_GLOBALS;
	case RK_CODE:
	case RK_FUNCTION:
	case RK_LABEL:
	  return MEMSPACE_CODE;
	case RK_FRAME:
	case RK_STACK:
	case RK_ALLOCA:
	  return MEMSPACE_STACK;
	case RK_HEAP:
	case RK_HEAP_ALLOCATED:
	  return MEMSPACE_HEAP;
	case RK_STRING:
	  return MEMSPACE_READONLY_DATA;
	case RK_PRIVATE:
	  return MEMSPACE_PRIVATE;
	case RK_SYMBOLIC:
	  {
	    auto *sym_reg = as_a<const symbolic_region *> (iter);
	    const svalue *sval = sym_reg->get_pointer ();
	    if (auto *sym_ptr = sval->maybe_get_symbolic_pointer ())
	      return sym_ptr->get_base_region ().get_memory_space ();
	  }
	  break;
	}
      iter = iter->get_parent_region ();
    }
  return MEMSPACE_UNKNOWN;
}

/* Subroutine for use by region_model_manager::get_or_create_initial_value.
   Return true if this region has an initial_svalue.
   Return false if attempting to use INIT_VAL(this_region) should give
   the "UNINITIALIZED" poison value.  */

bool
region::can_have_initial_svalue_p () const
{
  const region *base_reg = get_base_region ();

  /* Check for memory spaces that are uninitialized by default.  */
  enum memory_space mem_space = base_reg->get_memory_space ();
  switch (mem_space)
    {
    default:
      gcc_unreachable ();
    case MEMSPACE_UNKNOWN:
    case MEMSPACE_CODE:
    case MEMSPACE_GLOBALS:
    case MEMSPACE_READONLY_DATA:
    case MEMSPACE_PRIVATE:
      /* Such regions have initial_svalues.  */
      return true;

    case MEMSPACE_HEAP:
      /* Heap allocations are uninitialized by default.  */
      return false;

    case MEMSPACE_STACK:
      if (tree decl = base_reg->maybe_get_decl ())
	{
	  /* See the assertion in frame_region::get_region_for_local for the
	     tree codes we need to handle here.  */
	  switch (TREE_CODE (decl))
	    {
	    default:
	      gcc_unreachable ();

	    case PARM_DECL:
	      /* Parameters have initial values.  */
	      return true;

	    case VAR_DECL:
	    case RESULT_DECL:
	      /* Function locals don't have initial values.  */
	      return false;

	    case SSA_NAME:
	      {
		/* Some SSA names have an implicit default defined value.  */
		tree ssa_name = decl;
		if (SSA_NAME_IS_DEFAULT_DEF (ssa_name))
		  return ssa_defined_default_def_p (ssa_name);
		/* Others don't.  */
		return false;
	      }
	    }
	}

      /* If we have an on-stack region that isn't associated with a decl
	 or SSA name, then we have VLA/alloca, which is uninitialized.  */
      return false;
    }
}

/* For regions within a global decl, get the svalue for the initial
   value of this region when the program starts, caching the result.  */

const svalue *
region::get_initial_value_at_main (region_model_manager *mgr) const
{
  if (!m_cached_init_sval_at_main)
    m_cached_init_sval_at_main = calc_initial_value_at_main (mgr);
  return m_cached_init_sval_at_main;
}

/* Implementation of region::get_initial_value_at_main.  */

const svalue *
region::calc_initial_value_at_main (region_model_manager *mgr) const
{
  if (auto decl_reg = get_base_region ()->dyn_cast_decl_region ())
    {
      /* Attempt to get the initializer value for base_reg.  */
      if (const svalue *base_reg_init
	  = decl_reg->get_svalue_for_initializer (mgr))
	return base_reg_init;
    }
  // TODO: what about string_region?
  
  /* Otherwise, return INIT_VAL(REG).  */
  return mgr->get_or_create_initial_value (this);
}

/* If this region is a decl_region, return the decl.
   Otherwise return NULL_TREE.  */

tree
region::maybe_get_decl () const
{
  if (const decl_region *decl_reg = dyn_cast_decl_region ())
    return decl_reg->get_decl ();
  return NULL_TREE;
}

symbolic::memory_offset
region::get_memory_offset (region_model_manager &mgr) const
{
  if(!m_cached_memory_offset)
    {
      auto &node_mgr = mgr.get_node_manager ();
      auto mem_offset = calc_memory_offset (node_mgr);
      m_cached_memory_offset
	= std::make_unique<symbolic::memory_offset> (mem_offset);
    }
  return *m_cached_memory_offset;
}

/* Get the symbolic::memory_range for this region (calculating it on the
   first call and caching it internally).  */

symbolic::memory_range
region::get_memory_range (region_model_manager &mgr) const
{
  if(!m_cached_memory_range)
    {
      auto &node_mgr = mgr.get_node_manager ();
      auto mem_range = calc_memory_range (node_mgr);
      m_cached_memory_range
	= std::make_unique<symbolic::memory_range> (mem_range);
    }
  return *m_cached_memory_range;
}

symbolic::pointer
region::get_pointer (region_model_manager &mgr) const
{
  if(!m_cached_pointer)
    {
      auto &node_mgr = mgr.get_node_manager ();
      auto pointer = calc_pointer (node_mgr);
      m_cached_pointer = std::make_unique<symbolic::pointer> (pointer);
    }
  return *m_cached_pointer;
}

/* Base class implementation of region::get_byte_size vfunc.
   If the size of this region (in bytes) is known statically, return it.
   Otherwise return nullptr.  */

const symbolic_byte_size *
region::
maybe_get_byte_size (symbolic::scalar_manager<concrete_byte_size> &byte_mgr) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return nullptr;

  HOST_WIDE_INT bytes = int_size_in_bytes (type);
  if (bytes == -1)
    return nullptr;

  return &byte_mgr.get_concrete (concrete_byte_size (bytes));
}

/* Base implementation of region::get_byte_size_sval vfunc.  */

const svalue *
region::get_byte_size_sval (region_model_manager *mgr) const
{
  auto &byte_mgr = mgr->get_node_manager ().get_scalars ().get_bytes ();
  auto *byte_size = maybe_get_byte_size (byte_mgr);
  if (!byte_size)
    return mgr->get_or_create_unknown_svalue (size_type_node);

  return mgr->get_svalue_from_scalar<concrete_byte_size> (size_type_node,
							  *byte_size);
}

/* Attempt to get the size of TYPE in bits.
   If successful, return true and write the size to *OUT.
   Otherwise return false.  */

bool
int_size_in_bits (const_tree type, bit_size_t *out)
{
  if (INTEGRAL_TYPE_P (type))
    {
      *out = TYPE_PRECISION (type);
      return true;
    }

  tree sz = TYPE_SIZE (type);
  if (sz
      && tree_fits_uhwi_p (sz)
      /* If the size is zero, then we may have a zero-sized
	 array; handle such cases by returning false.  */
      && !integer_zerop (sz))
    {
      *out = TREE_INT_CST_LOW (sz);
      return true;
    }
  else
    return false;
}

/* Attempt to get the size of TYPE in bits.  If successful,
   write it to *out and return true.  Otherwise, return false.  */

bool
get_type_size (const_tree type, concrete_bit_size *out)
{
  if (INTEGRAL_TYPE_P (type))
    {
      *out = concrete_bit_size (TYPE_PRECISION (type));
      return true;
    }

  tree sz = TYPE_SIZE (type);
  if (sz
      && tree_fits_uhwi_p (sz)
      /* If the size is zero, then we may have a zero-sized
	 array; handle such cases by returning false.  */
      && !integer_zerop (sz))
    {
      *out = concrete_bit_size (TREE_INT_CST_LOW (sz));
      return true;
    }
  else
    return false;
}

concrete_bit_offset
get_bit_position (const_tree field)
{
  return concrete_bit_offset (int_bit_position (field));
}

/* If the size of this region (in bits) is known statically, return it.
   Otherwise return nullptr.  */

const symbolic_bit_size *
region::
maybe_get_bit_size (symbolic::scalar_manager<concrete_bit_size> &bit_mgr) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return nullptr;

  bit_size_t size;
  if (!int_size_in_bits (type, &size))
    return nullptr;

  concrete_bit_size conc_size = convert_from_bit_size_t (size);
  return &bit_mgr.get_concrete (conc_size);
}

/* Get the field within RECORD_TYPE at BIT_OFFSET.  */

tree
get_field_at_bit_offset (tree record_type, concrete_bit_offset bit_offset)
{
  gcc_assert (TREE_CODE (record_type) == RECORD_TYPE);
  if (bit_offset < 0_bits)
    return nullptr;

  /* Find the first field that has an offset > BIT_OFFSET,
     then return the one preceding it.
     Skip other trees within the chain, such as FUNCTION_DECLs.  */
  tree last_field = NULL_TREE;
  for (tree iter = TYPE_FIELDS (record_type); iter != NULL_TREE;
       iter = DECL_CHAIN (iter))
    {
      if (TREE_CODE (iter) == FIELD_DECL)
	{
	  concrete_bit_offset iter_field_offset
	    = concrete_bit_offset (int_bit_position (iter));
	  if (bit_offset < iter_field_offset)
	    return last_field;
	  last_field = iter;
	}
    }
  return last_field;
}

/* Populate *OUT with descendent regions of type TYPE that match
   RELATIVE_BIT_RANGE within this region.
   If TYPE is null, then don't check for matching types, just bit ranges.  */

void
region::get_subregions_for_bit_range (region_model_manager *mgr,
				      concrete_bit_range relative_bit_range,
				      tree type,
				      auto_vec <const region *> *out) const
{
  if (get_type () == NULL_TREE)
    return;
  if (relative_bit_range.get_start_offset () == 0_bits)
    {
      if (type && types_compatible_p (get_type (), type))
	{
	  out->safe_push (this);
	  return;
	}
      if (type == NULL_TREE)
	{
	  // For the null type case, check extents of this region
	  symbolic::memory_range reg_mem_range = get_memory_range (*mgr);
	  concrete_bit_size reg_conc_size;
	  if (concrete_from_symbolic (reg_mem_range.get_size (), &reg_conc_size))
	    if (reg_conc_size == relative_bit_range.get_size ())
	      {
		out->safe_push (this);
		return;
	      }
	}
    }
  switch (TREE_CODE (get_type ()))
    {
    case ARRAY_TYPE:
      {
	tree element_type = TREE_TYPE (get_type ());
	concrete_byte_size byte_size
	  = concrete_byte_size (int_size_in_bytes (element_type));
	if (byte_size > 0_bytes)
	  {
	    auto bits_per_element = bits_from_bytes (byte_size);
	    auto element_index = (relative_bit_range.get_start_offset ()
				  .floor_div (bits_per_element));
	    tree element_index_cst
	      = make_integer_cst_from_integer (integer_type_node, element_index, false);
	    auto inner_bit_offset = element_index * bits_per_element;
	    const region *subregion = mgr->get_element_region
	      (this, element_type,
	       mgr->get_or_create_constant_svalue (element_index_cst));
	    subregion->get_subregions_for_bit_range
	      (mgr,
	       concrete_bit_range (relative_bit_range.get_start_offset ()
				   - inner_bit_offset,
				   relative_bit_range.get_size ()),
	       type, out);
	  }
      }
      break;
    case RECORD_TYPE:
      {
	/* The bit offset might be *within* one of the fields (such as
	   with nested structs).
	   So we want to find the enclosing field, adjust the offset,
	   and repeat.  */
	if (tree field = get_field_at_bit_offset
			   (get_type (),
			    relative_bit_range.get_start_offset ()))
	  {
	    concrete_bit_offset field_bit_offset
	      = concrete_bit_offset (int_bit_position (field));
	    const region *subregion = mgr->get_field_region (this, field);
	    subregion->get_subregions_for_bit_range
	      (mgr,
	       concrete_bit_range (relative_bit_range.get_start_offset ()
				   - field_bit_offset,
				   relative_bit_range.get_size ()),
	       type, out);
	  }
      }
      break;
    case UNION_TYPE:
      {
	for (tree field = TYPE_FIELDS (get_type ()); field != NULL_TREE;
	     field = DECL_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;
	    const region *subregion = mgr->get_field_region (this, field);
	    subregion->get_subregions_for_bit_range (mgr,
						     relative_bit_range,
						     type,
						     out);
	  }
      }
      break;
    default:
      /* Do nothing.  */
      break;
    }
}

/* Walk from this region up to the base region within its cluster,
   accumulating the symbolic offset in bits relative to the base region.
   Calculate a symbolic size in bits.
   Return the (base region, symbolic offset, symbolic size) as a
   symbolic::memory_offset.  */

symbolic::memory_offset
region::calc_memory_offset (symbolic::node_manager &mgr) const
{
  const region *iter_region = this;
  const region *base_region = nullptr;

  auto &bit_mgr = mgr.get_scalars ().get_bits ();
  const symbolic::scalar<integer_with_unit<units::bit>> *accum_bit_offset
    = &bit_mgr.get_concrete (0_bits);

  while (iter_region)
    {
      switch (iter_region->get_kind ())
	{
	case RK_FIELD:
	case RK_ELEMENT:
	case RK_OFFSET:
	case RK_BIT_RANGE:
	  {
	    auto &rmm = mgr.get_region_model_manager ();
	    auto *bit_offset
	      = iter_region->maybe_get_bit_offset_relative_to_parent (rmm);
	    gcc_assert (bit_offset);
	    accum_bit_offset = &(*accum_bit_offset + *bit_offset);
	    iter_region = iter_region->get_parent_region ();
	  }
	  continue;

	case RK_SIZED:
	case RK_CAST:
	  iter_region = iter_region->get_parent_region ();
	  continue;

	case RK_SYMBOLIC:
	  {
	    const symbolic_region *sym_reg
	      = as_a<const symbolic_region *> (iter_region);
	    if (auto *sym_ptr
		  = sym_reg->get_pointer ()->maybe_get_symbolic_pointer ())
	      {
		base_region = &sym_ptr->get_base_region ();
		iter_region = nullptr;
		accum_bit_offset
		  = &(*accum_bit_offset
		      + bits_from_bytes (sym_ptr->get_byte_offset ()));
		break;
	      }
	  }
	  /* Fall through.  */

	default:
	  base_region = iter_region;
	  iter_region = nullptr;
	  break;
	}
    }

  gcc_assert (base_region);

  return symbolic::memory_offset (*base_region, *accum_bit_offset);
}

/* Get the symbolic::memory_range for this region.  */

symbolic::memory_range
region::calc_memory_range (symbolic::node_manager &mgr) const
{
  symbolic::memory_offset mem_offset
    = get_memory_offset (mgr.get_svalue_manager ());
  auto &bit_mgr = mgr.get_scalars ().get_bits ();
  auto *size = maybe_get_bit_size (bit_mgr);
  if (!size)
    size = &bit_mgr.get_unknown ();

  return symbolic::memory_range
    (mem_offset.get_base_region (),
     symbolic_bit_range (mem_offset.get_bit_offset (), *size));
}

/* Walk from this region up to the base region within its cluster,
   accumulating the symbolic offset in bytes relative to the base region.  */

symbolic::pointer
region::calc_pointer (symbolic::node_manager &mgr) const
{
  const region *iter_region = this;
  const region *base_region = nullptr;

  auto &byte_mgr = mgr.get_scalars ().get_bytes ();
  const symbolic::pointer::symbolic_byte_offset *accum_byte_offset
    = &byte_mgr.get_concrete (0_bytes);

  while (iter_region)
    {
      switch (iter_region->get_kind ())
	{
	case RK_FIELD:
	case RK_ELEMENT:
	case RK_OFFSET:
	case RK_BIT_RANGE:
	  {
	    auto &rmm = mgr.get_region_model_manager ();
	    auto *byte_offset
	      = iter_region->maybe_get_byte_offset_relative_to_parent (rmm);
	    gcc_assert (byte_offset);
	    accum_byte_offset = &(*accum_byte_offset + *byte_offset);
	    iter_region = iter_region->get_parent_region ();
	  }
	  continue;

	case RK_SIZED:
	case RK_CAST:
	  iter_region = iter_region->get_parent_region ();
	  continue;

	case RK_SYMBOLIC:
	  {
	    const symbolic_region *sym_reg
	      = as_a<const symbolic_region *> (iter_region);
	    if (auto *sym_ptr = sym_reg->get_pointer ()->maybe_get_symbolic_pointer ())
	      {
		base_region = &sym_ptr->get_base_region ();
		iter_region = nullptr;
		accum_byte_offset
		  = &(*accum_byte_offset + sym_ptr->get_byte_offset ());
		break;
	      }
	  }
	  /* Fall through.  */

	default:
	  base_region = iter_region;
	  iter_region = nullptr;
	  break;
	}
    }

  gcc_assert (base_region);

  return symbolic::pointer (*base_region,
			    *accum_byte_offset);
}

const region::symbolic_bit_count *
region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &) const
{
  return nullptr;
}

const region::symbolic_byte_count *
region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &) const
{
  return nullptr;
}

DEBUG_FUNCTION void
region::dump () const
{
  symbolic::dump_node_tree_to_stderr<region> (*this);
}

bool
region::maybe_print_for_user (pretty_printer *pp,
			      const region_model &) const
{
  switch (get_kind ())
    {
    default:
      break;
    case RK_DECL:
      {
	const decl_region *reg = (const decl_region *)this;
	tree decl = reg->get_decl ();
	if (TREE_CODE (decl) == SSA_NAME)
	  decl = SSA_NAME_VAR (decl);
	print_expr_for_user (pp, decl);
	return true;
      }
    }

  return false;
}

void
region::add_dump_widget_children_for_region (text_art::tree_widget &,
					     symbolic::node_dump_state &) const
{
  // no-op
}

/* Generate a description of this region.  */

DEBUG_FUNCTION label_text
region::get_desc (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_to_pp (&pp, simple);
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* Base implementation of region::accept vfunc.
   Subclass implementations should chain up to this.  */

void
region::accept (visitor *v) const
{
  v->visit_region (this);
  if (m_parent)
    m_parent->accept (v);
}

std::unique_ptr<json::object>
region::make_json_obj (const char *type_name) const
{
  auto obj = symbolic::node::make_json_obj (type_name);
  if (m_type)
    obj->set ("type", tree_to_json (m_type));
  if (m_parent)
    obj->set ("parent", m_parent->to_json ());
  return obj;
}

/* Return true if this is a symbolic region for dereferencing an
   unknown ptr.
   We shouldn't attempt to bind values for this region (but
   can unbind values for other regions).  */

bool
region::symbolic_for_unknown_ptr_p () const
{
  if (const symbolic_region *sym_reg = dyn_cast_symbolic_region ())
    if (sym_reg->get_pointer ()->get_kind () == SK_UNKNOWN)
      return true;
  return false;
}

/* Return true if this is a symbolic region.  */

bool
region::symbolic_p () const
{
  return get_kind () == RK_SYMBOLIC;
}

/* Return true if this region is known to be zero bits in size.  */

bool
region::empty_p (region_model_manager &rmm) const
{
  bit_size_t num_bits;
  auto &bit_mgr = rmm.get_node_manager ().get_scalars ().get_bits ();
  if (auto *size = maybe_get_bit_size (bit_mgr))
    if (size->is_zero_p ())
      return true;
  return false;
}

/* Return true if this is a region for a decl with name DECL_NAME.
   Intended for use when debugging (for assertions and conditional
   breakpoints).  */

DEBUG_FUNCTION bool
region::is_named_decl_p (const char *decl_name) const
{
  if (tree decl = maybe_get_decl ())
    if (DECL_NAME (decl)
	&& !strcmp (IDENTIFIER_POINTER (DECL_NAME (decl)), decl_name))
      return true;
  return false;
}

/* region's ctor.  */

region::region (complexity c, symbolic::node::id_t id, const region *parent, tree type)
: symbolic::node (c, id),
  m_parent (parent), m_type (type),
  m_cached_memory_range (nullptr),
  m_cached_init_sval_at_main (nullptr)
{
  gcc_assert (type == NULL_TREE || TYPE_P (type));
}

/* Comparator for use by vec<const region *>::qsort,
   using their IDs to order them.  */

int
region::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const region * const *reg1 = (const region * const *)p1;
  const region * const *reg2 = (const region * const *)p2;

  return cmp_ids (*reg1, *reg2);
}

/* Determine if a pointer to this region must be non-NULL.

   Generally, pointers to regions must be non-NULL, but pointers
   to symbolic_regions might, in fact, be NULL.

   This allows us to simulate functions like malloc and calloc with:
   - only one "outcome" from each statement,
   - the idea that the pointer is on the heap if non-NULL
   - the possibility that the pointer could be NULL
   - the idea that successive values returned from malloc are non-equal
   - to be able to zero-fill for calloc.  */

bool
region::non_null_p () const
{
  switch (get_kind ())
    {
    default:
      return true;
    case RK_SYMBOLIC:
      /* Are we within a symbolic_region?  If so, it could be NULL, and we
	 have to fall back on the constraints.  */
      return false;
    case RK_HEAP_ALLOCATED:
      return false;
    }
}

/* Comparator for trees to impose a deterministic ordering on
   T1 and T2.  */

static int
tree_cmp (const_tree t1, const_tree t2)
{
  gcc_assert (t1);
  gcc_assert (t2);

  /* Test tree codes first.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return TREE_CODE (t1) - TREE_CODE (t2);

  /* From this point on, we know T1 and T2 have the same tree code.  */

  if (DECL_P (t1))
    {
      if (DECL_NAME (t1) && DECL_NAME (t2))
	return strcmp (IDENTIFIER_POINTER (DECL_NAME (t1)),
		       IDENTIFIER_POINTER (DECL_NAME (t2)));
      else
	{
	  if (DECL_NAME (t1))
	    return -1;
	  else if (DECL_NAME (t2))
	    return 1;
	  else
	    return DECL_UID (t1) - DECL_UID (t2);
	}
    }

  switch (TREE_CODE (t1))
    {
    case SSA_NAME:
      {
	if (SSA_NAME_VAR (t1) && SSA_NAME_VAR (t2))
	  {
	    int var_cmp = tree_cmp (SSA_NAME_VAR (t1), SSA_NAME_VAR (t2));
	    if (var_cmp)
	      return var_cmp;
	    return SSA_NAME_VERSION (t1) - SSA_NAME_VERSION (t2);
	  }
	else
	  {
	    if (SSA_NAME_VAR (t1))
	      return -1;
	    else if (SSA_NAME_VAR (t2))
	      return 1;
	    else
	      return SSA_NAME_VERSION (t1) - SSA_NAME_VERSION (t2);
	  }
      }
      break;

    case INTEGER_CST:
      return tree_int_cst_compare (t1, t2);

    case REAL_CST:
      {
	const real_value *rv1 = TREE_REAL_CST_PTR (t1);
	const real_value *rv2 = TREE_REAL_CST_PTR (t2);
	if (real_compare (UNORDERED_EXPR, rv1, rv2))
	  {
	    /* Impose an arbitrary order on NaNs relative to other NaNs
	       and to non-NaNs.  */
	    if (int cmp_isnan = real_isnan (rv1) - real_isnan (rv2))
	      return cmp_isnan;
	    if (int cmp_issignaling_nan
		  = real_issignaling_nan (rv1) - real_issignaling_nan (rv2))
	      return cmp_issignaling_nan;
	    return real_isneg (rv1) - real_isneg (rv2);
	  }
	if (real_compare (LT_EXPR, rv1, rv2))
	  return -1;
	if (real_compare (GT_EXPR, rv1, rv2))
	  return 1;
	return 0;
      }

    case STRING_CST:
      return strcmp (TREE_STRING_POINTER (t1),
		     TREE_STRING_POINTER (t2));

    default:
      gcc_unreachable ();
      break;
    }

  gcc_unreachable ();

  return 0;
}

/* qsort comparator for trees to impose a deterministic ordering on
   P1 and P2.  */

int
tree_cmp (const void *p1, const void *p2)
{
  const_tree t1 = *(const_tree const *)p1;
  const_tree t2 = *(const_tree const *)p2;

  return tree_cmp (t1, t2);
}

/* class frame_region : public space_region.  */

frame_region::~frame_region ()
{
  for (map_t::iterator iter = m_locals.begin ();
       iter != m_locals.end ();
       ++iter)
    delete (*iter).second;
}

void
frame_region::accept (visitor *v) const
{
  region::accept (v);
  if (m_calling_frame)
    m_calling_frame->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for frame_region.  */

void
frame_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "frame: %qs@%i", function_name (&m_fun), get_stack_depth ());
  else
    pp_printf (pp, "frame_region(%qs, index: %i, depth: %i)",
	       function_name (&m_fun), m_index, get_stack_depth ());
}

void
frame_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "frame_region(%qs, index: %i, depth: %i)",
	     function_name (&m_fun), m_index, get_stack_depth ());
}

void
frame_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
frame_region::to_json () const
{
  auto obj = region::make_json_obj ("frame_region");
  obj->set_string ("function", function_name (&m_fun));
  obj->set_integer ("index", m_index);
  obj->set_integer ("stack_depth", get_stack_depth ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
frame_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("frame_region");
}

const decl_region *
frame_region::get_region_for_local (region_model_manager *mgr,
				    tree expr,
				    const region_model_context *ctxt) const
{
  if (CHECKING_P)
    {
      /* Verify that EXPR is a local or SSA name, and that it's for the
	 correct function for this stack frame.  */
      gcc_assert (TREE_CODE (expr) == PARM_DECL
		  || TREE_CODE (expr) == VAR_DECL
		  || TREE_CODE (expr) == SSA_NAME
		  || TREE_CODE (expr) == RESULT_DECL);
      switch (TREE_CODE (expr))
	{
	default:
	  gcc_unreachable ();
	case VAR_DECL:
	  gcc_assert (!is_global_var (expr));
	  /* Fall through.  */
	case PARM_DECL:
	case RESULT_DECL:
	  gcc_assert (DECL_CONTEXT (expr) == m_fun.decl);
	  break;
	case SSA_NAME:
	  {
	    if (tree var = SSA_NAME_VAR (expr))
	      {
		if (DECL_P (var))
		  gcc_assert (DECL_CONTEXT (var) == m_fun.decl);
	      }
	    else if (ctxt)
	      if (const extrinsic_state *ext_state = ctxt->get_ext_state ())
		if (const supergraph *sg
		    = ext_state->get_engine ()->get_supergraph ())
		  {
		    const gimple *def_stmt = SSA_NAME_DEF_STMT (expr);
		    if (gimple_code (def_stmt) != GIMPLE_PHI)
		      {
			const supernode *snode
			  = sg->get_supernode_for_stmt (def_stmt);
			gcc_assert (snode->get_function () == &m_fun);
		      }
		  }
	  }
	  break;
	}
    }

  /* Ideally we'd use mutable here.  */
  map_t &mutable_locals = const_cast <map_t &> (m_locals);

  if (decl_region **slot = mutable_locals.get (expr))
    return *slot;
  decl_region *reg
    = new decl_region (mgr->alloc_node_id (), this, expr);
  mutable_locals.put (expr, reg);
  return reg;
}

/* class globals_region : public space_region.  */

/* Implementation of region::dump_to_pp vfunc for globals_region.  */

void
globals_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "::");
  else
    pp_string (pp, "globals");
}

void
globals_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "globals");
}

void
globals_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
globals_region::to_json () const
{
  return region::make_json_obj ("globals_region");
}

std::unique_ptr<diagnostics::markup::node>
globals_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("globals_region");
}

/* class code_region : public map_region.  */

/* Implementation of region::dump_to_pp vfunc for code_region.  */

void
code_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "code region");
  else
    pp_string (pp, "code_region()");
}

void
code_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "code region");
}

void
code_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
code_region::to_json () const
{
  return region::make_json_obj ("code_region");
}

std::unique_ptr<diagnostics::markup::node>
code_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("code_region");
}

/* class function_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for function_region.  */

void
function_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      dump_quoted_tree (pp, m_fndecl);
    }
  else
    {
      pp_string (pp, "function_region(");
      dump_quoted_tree (pp, m_fndecl);
      pp_string (pp, ")");
    }
}

void
function_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "function_region(");
  dump_quoted_tree (pp, m_fndecl);
  pp_string (pp, ")");
}

void
function_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
function_region::to_json () const
{
  auto obj = region::make_json_obj ("function_region");
  obj->set ("fndecl", tree_to_json (m_fndecl));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
function_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("function_region");
}

/* class label_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for label_region.  */

void
label_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      dump_quoted_tree (pp, m_label);
    }
  else
    {
      pp_string (pp, "label_region(");
      dump_quoted_tree (pp, m_label);
      pp_string (pp, ")");
    }
}

void
label_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "label_region(");
  dump_quoted_tree (pp, m_label);
  pp_string (pp, ")");
}

void
label_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
label_region::to_json () const
{
  auto obj = region::make_json_obj ("label_region");
  obj->set ("label", tree_to_json (m_label));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
label_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("label_region");
}

/* class stack_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for stack_region.  */

void
stack_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "stack region");
  else
    pp_string (pp, "stack_region()");
}

void
stack_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "stack region");
}

void
stack_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
stack_region::to_json () const
{
  return region::make_json_obj ("stack_region");
}

std::unique_ptr<diagnostics::markup::node>
stack_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("stack_region");
}

/* class heap_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for heap_region.  */

void
heap_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "heap region");
  else
    pp_string (pp, "heap_region()");
}

void
heap_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "heap_region");
}

void
heap_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
heap_region::to_json () const
{
  return region::make_json_obj ("heap_region");
}

std::unique_ptr<diagnostics::markup::node>
heap_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("heap_region");
}

/* class root_region : public region.  */

/* root_region's ctor.  */

root_region::root_region (symbolic::node::id_t id)
: region (complexity (1, 1), id, nullptr, NULL_TREE)
{
}

/* Implementation of region::dump_to_pp vfunc for root_region.  */

void
root_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "root region");
  else
    pp_string (pp, "root_region()");
}

void
root_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "root region");
}

void
root_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_region (*this);
}

std::unique_ptr<json::object>
root_region::to_json () const
{
  return region::make_json_obj ("root_region");
}

std::unique_ptr<diagnostics::markup::node>
root_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("root_region");
}

/* class thread_local_region : public space_region.  */

void
thread_local_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "thread_local_region");
  else
    pp_string (pp, "thread_local_region()");
}

void
thread_local_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "thread_local_region");
}

void
thread_local_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
thread_local_region::to_json () const
{
  return region::make_json_obj ("thread_local_region");
}

std::unique_ptr<diagnostics::markup::node>
thread_local_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("thread_local_region");
}

/* class symbolic_region : public map_region.  */

/* symbolic_region's ctor.  */

symbolic_region::symbolic_region (symbolic::node::id_t id, region *parent,
				  const svalue *sval_ptr)
: region (complexity::from_pair (parent, sval_ptr), id, parent,
	  (sval_ptr->get_type ()
	   ? TREE_TYPE (sval_ptr->get_type ())
	   : NULL_TREE)),
  m_sval_ptr (sval_ptr)
{
}

/* Implementation of region::accept vfunc for symbolic_region.  */

void
symbolic_region::accept (visitor *v) const
{
  region::accept (v);
  m_sval_ptr->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for symbolic_region.  */

void
symbolic_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "(*");
      m_sval_ptr->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "symbolic_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      if (get_type ())
	{
	  pp_string (pp, ", ");
	  print_quoted_type (pp, get_type ());
	}
      pp_string (pp, ", ");
      m_sval_ptr->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
}

void
symbolic_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "symbolic_region: %<*%>");
}

void
symbolic_region::
add_dump_widget_children_for_region (text_art::tree_widget &w,
				     symbolic::node_dump_state &nds) const
{
  w.add_child (m_sval_ptr->make_dump_widget (nds, "m_sval_ptr"));
}

void
symbolic_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    {
      get_parent_region ()->accept_node_visitor (v);
      m_sval_ptr->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
symbolic_region::to_json () const
{
  auto obj = region::make_json_obj ("symbolic_region");
  obj->set ("sval_ptr", m_sval_ptr->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
symbolic_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("symbolic_region");
}

/* class decl_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for decl_region.  */

void
decl_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "%E", m_decl);
  else
    {
      pp_string (pp, "decl_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ", %qE)", m_decl);
    }
}

void
decl_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "decl_region(%qE)", m_decl);
}

void
decl_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
decl_region::to_json () const
{
  auto obj = region::make_json_obj ("decl_region");
  obj->set ("decl", tree_to_json (m_decl));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
decl_region::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  tree decl = m_decl;
  if (markup_ctxt.m_user_facing)
    if (TREE_CODE (decl) == SSA_NAME)
      decl = SSA_NAME_VAR (decl);

  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  print_expr_for_user (&pp, decl);
  return diagnostics::markup::make_text_node (pp_formatted_text (&pp));
}

/* Get the stack depth for the frame containing this decl, or 0
   for a global.  */

int
decl_region::get_stack_depth () const
{
  if (get_parent_region () == nullptr)
    return 0;
  if (const frame_region *frame_reg
	= get_parent_region ()->dyn_cast_frame_region ())
    return frame_reg->get_stack_depth ();
  return 0;
}

/* If the underlying decl is in the global constant pool,
   return an svalue representing the constant value.
   Otherwise return nullptr.  */

const svalue *
decl_region::maybe_get_constant_value (region_model_manager *mgr) const
{
  if (VAR_P (m_decl)
      && DECL_IN_CONSTANT_POOL (m_decl)
      && DECL_INITIAL (m_decl)
      && TREE_CODE (DECL_INITIAL (m_decl)) == CONSTRUCTOR)
    return get_svalue_for_constructor (DECL_INITIAL (m_decl), mgr);
  return nullptr;
}

/* Get an svalue for CTOR, a CONSTRUCTOR for this region's decl.  */
const svalue *
decl_region::get_svalue_for_constructor (tree ctor,
					 region_model_manager *mgr) const
{
  auto &bit_contents = get_bit_contents_for_constructor (ctor, mgr);
  auto mem_range = get_memory_range (*mgr);
  auto &bv = bit_contents.get_bits (mem_range.get_bit_range ());
  return mgr->get_svalue_from_bit_vector (get_type (), bv);
}

/* Get the contents for CTOR, a CONSTRUCTOR for this region's decl.  */

const symbolic::bit_contents &
decl_region::get_bit_contents_for_constructor (tree ctor,
					       region_model_manager *mgr) const
{
  gcc_assert (!TREE_CLOBBER_P (ctor));
  gcc_assert (ctor == DECL_INITIAL (m_decl));

  if (!m_ctor_bit_contents)
    m_ctor_bit_contents = &calc_bit_contents_for_constructor (ctor, mgr);

  return *m_ctor_bit_contents;
}

/* For use on decl_regions for global variables.

   Get an svalue for the initial value of this region at entry to
   "main" (either based on DECL_INITIAL, or implicit initialization to
   zero.

   Return nullptr if there is a problem.  */

const svalue *
decl_region::get_svalue_for_initializer (region_model_manager *mgr) const
{
  if (!VAR_P (m_decl))
    return nullptr;
  tree init = DECL_INITIAL (m_decl);
  if (!init)
    {
      /* If we have an "extern" decl then there may be an initializer in
	 another TU.  */
      if (DECL_EXTERNAL (m_decl))
	return nullptr;

      if (empty_p (*mgr))
	return nullptr;

      if (!tracked_p ())
	return nullptr;

      /* Implicit initialization to zero.
	 Doing so requires that we have a concrete size for this region,
	 which can fail if we have a region with unknown size
	 (e.g. "extern const char arr[];").  */
      auto &bit_mgr = mgr->get_node_manager ().get_scalars ().get_bits ();
      auto *bit_size = maybe_get_bit_size (bit_mgr);
      if (!bit_size)
	return nullptr;
      auto &bv_mgr = mgr->get_node_manager ().get_bit_vector_manager ();
      auto &bv = bv_mgr.get_all_zeros (*bit_size);
      return mgr->get_svalue_from_bit_vector (get_type (), bv);
    }

  /* LTO can write out error_mark_node as the DECL_INITIAL for simple scalar
     values (to avoid writing out an extra section).  */
  if (init == error_mark_node)
    return nullptr;

  if (TREE_CODE (init) == CONSTRUCTOR)
    return get_svalue_for_constructor (init, mgr);

  /* Reuse the get_rvalue logic from region_model.  */
  region_model m (mgr->get_node_manager ());
  return m.get_rvalue (path_var (init, 0), nullptr);
}

const symbolic::bit_contents *
decl_region::
maybe_get_bit_contents_for_initializer (region_model_manager *mgr) const
{
  if (!VAR_P (m_decl))
    return nullptr;
  tree init = DECL_INITIAL (m_decl);
  if (!init)
    {
      /* If we have an "extern" decl then there may be an initializer in
	 another TU.  */
      if (DECL_EXTERNAL (m_decl))
	return nullptr;

      if (empty_p (*mgr))
	return nullptr;

      if (!tracked_p ())
	return nullptr;

      /* Implicit initialization to zero.  */
      auto &node_mgr = mgr->get_node_manager ();
      auto &bit_contents_mgr = node_mgr.get_bit_contents_manager ();
      return &bit_contents_mgr.get_all_zeros ();
    }

  /* LTO can write out error_mark_node as the DECL_INITIAL for simple scalar
     values (to avoid writing out an extra section).  */
  if (init == error_mark_node)
    return nullptr;

  if (TREE_CODE (init) == CONSTRUCTOR)
    return &get_bit_contents_for_constructor (init, mgr);

  return nullptr;
}

/* Subroutine of symnode_requires_tracking_p; return true if REF
   might imply that we should be tracking the value of its decl.  */

static bool
ipa_ref_requires_tracking (ipa_ref *ref)
{
  /* If we have a load/store/alias of the symbol, then we'll track
     the decl's value.  */
  if (ref->use != IPA_REF_ADDR)
    return true;

  if (ref->stmt == nullptr)
    return true;

  switch (ref->stmt->code)
    {
    default:
      return true;
    case GIMPLE_CALL:
      {
	cgraph_node *caller_cnode = dyn_cast <cgraph_node *> (ref->referring);
	if (caller_cnode == nullptr)
	  return true;
	cgraph_edge *edge = caller_cnode->get_edge (ref->stmt);
	if (!edge)
	  return true;
	if (edge->callee == nullptr)
	  return true; /* e.g. call through function ptr.  */
	if (edge->callee->definition)
	  return true;
	/* If we get here, then this ref is a pointer passed to
	   a function we don't have the definition for.  */
	return false;
      }
      break;
    case GIMPLE_ASM:
      {
	const gasm *asm_stmt = as_a <const gasm *> (ref->stmt);
	if (gimple_asm_noutputs (asm_stmt) > 0)
	  return true;
	if (gimple_asm_nclobbers (asm_stmt) > 0)
	  return true;
	/* If we get here, then this ref is the decl being passed
	   by pointer to asm with no outputs.  */
	return false;
      }
      break;
    }
}

/* Determine if the decl for SYMNODE should have binding_clusters
   in our state objects; return false to optimize away tracking
   certain decls in our state objects, as an optimization.  */

static bool
symnode_requires_tracking_p (symtab_node *symnode)
{
  gcc_assert (symnode);
  if (symnode->externally_visible)
    return true;
  tree context_fndecl = DECL_CONTEXT (symnode->decl);
  if (context_fndecl == nullptr)
    return true;
  if (TREE_CODE (context_fndecl) != FUNCTION_DECL)
    return true;
  for (auto ref : symnode->ref_list.referring)
    if (ipa_ref_requires_tracking (ref))
      return true;

  /* If we get here, then we don't have uses of this decl that require
     tracking; we never read from it or write to it explicitly.  */
  return false;
}

/* Subroutine of decl_region ctor: determine whether this decl_region
   can have binding_clusters; return false to optimize away tracking
   of certain decls in our state objects, as an optimization.  */

bool
decl_region::calc_tracked_p (tree decl)
{
  /* Precondition of symtab_node::get.  */
  if (TREE_CODE (decl) == VAR_DECL
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl) || in_lto_p))
    if (symtab_node *symnode = symtab_node::get (decl))
      return symnode_requires_tracking_p (symnode);
  return true;
}

/* class field_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for field_region.  */

void
field_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ".");
      pp_printf (pp, "%E", m_field);
    }
  else
    {
      pp_string (pp, "field_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ", %qE)", m_field);
    }
}

void
field_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "field_region(%qE)", m_field);
}

void
field_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
field_region::to_json () const
{
  auto obj = region::make_json_obj ("field_region");
  obj->set ("field", tree_to_json (m_field));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
field_region::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  auto parent = get_parent_region ()->make_markup_node (markup_ctxt);

  const char *identifier = IDENTIFIER_POINTER (DECL_NAME (m_field));
  auto field = diagnostics::markup::make_text_node (identifier);
  return std::make_unique<diagnostics::markup::code::field_expr>
    (std::move (parent),
     std::move (field));
}

const region::symbolic_bit_count *
field_region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &node_mgr = mgr.get_node_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  /* Compare with e.g. gimple-fold.cc's
     fold_nonarray_ctor_reference.  */
  tree byte_offset = DECL_FIELD_OFFSET (m_field);
  if (TREE_CODE (byte_offset) != INTEGER_CST)
    return &bit_mgr.get_unknown ();
  tree field_offset = DECL_FIELD_BIT_OFFSET (m_field);
  /* Compute bit offset of the field.  */
  offset_int bitoffset
    = (wi::to_offset (field_offset)
       + (wi::to_offset (byte_offset) << LOG2_BITS_PER_UNIT));
  return &bit_mgr.get_concrete (convert_from_bit_offset_t (bitoffset));
}

const region::symbolic_byte_count *
field_region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &mgr) const
{
  /* Compare with e.g. gimple-fold.cc's
     fold_nonarray_ctor_reference.  */
  tree byte_offset = DECL_FIELD_OFFSET (m_field);
  if (TREE_CODE (byte_offset) != INTEGER_CST)
    return nullptr;
  tree field_offset = DECL_FIELD_BIT_OFFSET (m_field);
  /* Compute bit offset of the field.  */
  offset_int bitoffset
    = (wi::to_offset (field_offset)
       + (wi::to_offset (byte_offset) << LOG2_BITS_PER_UNIT));

  auto scalar_bits = convert_from_bit_offset_t (bitoffset);
  integer_with_unit<units::byte> scalar_bytes;
  if (!maybe_bytes_from_bits (scalar_bits, &scalar_bytes))
    return nullptr;

  auto &node_mgr = mgr.get_node_manager ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  return &byte_mgr.get_concrete (scalar_bytes);
}

/* class element_region : public region.  */

/* Implementation of region::accept vfunc for element_region.  */

void
element_region::accept (visitor *v) const
{
  region::accept (v);
  m_index->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for element_region.  */

void
element_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      //pp_string (pp, "(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, "[");
      m_index->dump_to_pp (pp, simple);
      pp_string (pp, "]");
      //pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "element_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_string (pp, ", ");
      m_index->dump_to_pp (pp, simple);
      pp_printf (pp, ")");
    }
}

void
element_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "element_region: %<[]%>");
}

void
element_region::
add_dump_widget_children_for_region (text_art::tree_widget &w,
				     symbolic::node_dump_state &nds) const
{
  w.add_child (m_index->make_dump_widget (nds, "m_index"));
}

void
element_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    {
      get_parent_region ()->accept_node_visitor (v);
      m_index->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
element_region::to_json () const
{
  auto obj = region::make_json_obj ("element_region");
  obj->set ("index", m_index->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
element_region::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  auto parent = get_parent_region ()->make_markup_node (markup_ctxt);
  auto index = m_index->make_markup_node (markup_ctxt);
  return std::make_unique<diagnostics::markup::code::element_expr>
    (std::move (parent),
     std::move (index));
}

const region::symbolic_bit_count *
element_region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &node_mgr = mgr.get_node_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  tree elem_type = get_type ();

  /* First, use int_size_in_bytes, to reject the case where we
     have an incomplete type, or a non-constant value.  */
  HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (elem_type);
  if (hwi_byte_size > 0)
    {
      auto &elem_size = bit_mgr.get_concrete
	(bits_from_bytes (convert_from_byte_size_t  (hwi_byte_size)));
      auto &index = unitless_mgr.get_scalar_from_svalue (*m_index);
      return &(elem_size * index);
    }

  return &bit_mgr.get_unknown ();
}

const region::symbolic_byte_count *
element_region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &node_mgr = mgr.get_node_manager ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  auto &unitless_mgr = node_mgr.get_scalars ().get_unitless ();

  tree elem_type = get_type ();

  /* First, use int_size_in_bytes, to reject the case where we
     have an incomplete type, or a non-constant value.  */
  HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (elem_type);

  if (hwi_byte_size > 0)
    {
      auto &elem_size
	= byte_mgr.get_concrete (convert_from_byte_size_t (hwi_byte_size));
      auto &index = unitless_mgr.get_scalar_from_svalue (*m_index);
      return &(elem_size * index);
    }

  return &byte_mgr.get_unknown ();
}

/* class offset_region : public region.  */

/* Implementation of region::accept vfunc for offset_region.  */

void
offset_region::accept (visitor *v) const
{
  region::accept (v);
  m_byte_offset->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for offset_region.  */

void
offset_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      //pp_string (pp, "(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, "+");
      m_byte_offset->dump_to_pp (pp, simple);
      //pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "offset_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_string (pp, ", ");
      m_byte_offset->dump_to_pp (pp, simple);
      pp_printf (pp, ")");
    }
}

void
offset_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "offset_region");
}

void
offset_region::
add_dump_widget_children_for_region (text_art::tree_widget &w,
				     symbolic::node_dump_state &nds) const
{
  w.add_child (m_byte_offset->make_dump_widget (nds, "m_byte_offset"));
}

void
offset_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    {
      get_parent_region ()->accept_node_visitor (v);
      m_byte_offset->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
offset_region::to_json () const
{
  auto obj = region::make_json_obj ("offset_region");
  obj->set ("byte_offset", m_byte_offset->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
offset_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("offset_region");
}

const svalue *
offset_region::get_bit_offset (region_model_manager *mgr) const
{
  const svalue *bits_per_byte_sval
    = mgr->get_or_create_int_cst (NULL_TREE, BITS_PER_UNIT);
  return mgr->get_or_create_binop (NULL_TREE, MULT_EXPR,
				   m_byte_offset, bits_per_byte_sval);
}

const region::symbolic_bit_count *
offset_region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &scalar_mgr = mgr.get_node_manager ().get_scalars ();
  auto &eight_bits = scalar_mgr.get_bits ().get_concrete (8_bits);
  auto &unitless_byte_offset
    = scalar_mgr.get_unitless ().get_scalar_from_svalue (*m_byte_offset);
  return &(eight_bits * unitless_byte_offset);
}

const region::symbolic_byte_count *
offset_region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &scalar_mgr = mgr.get_node_manager ().get_scalars ();
  return &scalar_mgr.get_bytes ().get_scalar_from_svalue (*m_byte_offset);
}

/* class sized_region : public region.  */

/* Implementation of region::accept vfunc for sized_region.  */

void
sized_region::accept (visitor *v) const
{
  region::accept (v);
  m_byte_size_sval->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for sized_region.  */

void
sized_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "SIZED_REG(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_byte_size_sval->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "sized_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_byte_size_sval->dump_to_pp (pp, simple);
      pp_printf (pp, ")");
    }
}

void
sized_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "sized_region");
}

void
sized_region::
add_dump_widget_children_for_region (text_art::tree_widget &w,
				     symbolic::node_dump_state &nds) const
{
  w.add_child (m_byte_size_sval->make_dump_widget (nds, "m_byte_size_sval"));
}

void
sized_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    {
      get_parent_region ()->accept_node_visitor (v);
      m_byte_size_sval->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
sized_region::to_json () const
{
  auto obj = region::make_json_obj ("sized_region");
  obj->set ("byte_size_sval", m_byte_size_sval->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
sized_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("sized_region");
}

/* Implementation of region::get_byte_size vfunc for sized_region.  */

const symbolic_byte_size *
sized_region::
maybe_get_byte_size (symbolic::scalar_manager<concrete_byte_size> &byte_mgr) const
{
  return &byte_mgr.get_scalar_from_svalue (*m_byte_size_sval);
}

/* Implementation of region::get_bit_size vfunc for sized_region.  */

const symbolic_bit_size *
sized_region::
maybe_get_bit_size (symbolic::scalar_manager<concrete_bit_size> &bit_mgr) const
{
  auto &byte_mgr = bit_mgr.get_node_manager ().get_scalars ().get_bytes ();
  auto &sym_byte_size = byte_mgr.get_scalar_from_svalue (*m_byte_size_sval);
  return &bits_from_bytes (sym_byte_size);
}

/* class cast_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for cast_region.  */

void
cast_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "CAST_REG(");
      print_quoted_type (pp, get_type ());
      pp_string (pp, ", ");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "cast_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ")");
    }
}

void
cast_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "cast_region");
}

void
cast_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
cast_region::to_json () const
{
  return region::make_json_obj ("cast_region");
}

std::unique_ptr<diagnostics::markup::node>
cast_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("cast_region");
}

const region::symbolic_bit_count *
cast_region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &) const
{
  return nullptr;
}

const region::symbolic_byte_count *
cast_region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &) const
{
  return nullptr;
}

/* class heap_allocated_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for heap_allocated_region.  */

void
heap_allocated_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "HEAP_ALLOCATED_REGION(%i)", get_id ());
  else
    pp_printf (pp, "heap_allocated_region(%i)", get_id ());
}

void
heap_allocated_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "heap_allocated_region");
}

void
heap_allocated_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_region (*this);
}

std::unique_ptr<json::object>
heap_allocated_region::to_json () const
{
  return region::make_json_obj ("heap_allocated_region");
}

std::unique_ptr<diagnostics::markup::node>
heap_allocated_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("heap_allocated_region");
}

/* class alloca_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for alloca_region.  */

void
alloca_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "ALLOCA_REGION(%i)", get_id ());
  else
    pp_printf (pp, "alloca_region(%i)", get_id ());
}

void
alloca_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "alloca_region");
}

void
alloca_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_region (*this);
}

std::unique_ptr<json::object>
alloca_region::to_json () const
{
  return region::make_json_obj ("alloca_region");
}

std::unique_ptr<diagnostics::markup::node>
alloca_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("alloca_region");
}

/* class string_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for string_region.  */

void
string_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    dump_tree (pp, m_string_cst);
  else
    {
      pp_string (pp, "string_region(");
      dump_tree (pp, m_string_cst);
      if (!flag_dump_noaddr)
	{
	  pp_string (pp, " (");
	  pp_pointer (pp, m_string_cst);
	  pp_string (pp, "))");
	}
    }
}

void
string_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "string_region(");
  dump_tree (pp, m_string_cst);
  pp_string (pp, ")");
}

void
string_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
string_region::to_json () const
{
  auto obj = region::make_json_obj ("string_region");
  obj->set ("string", tree_to_json (m_string_cst));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
string_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("string_region");
}

/* class bit_range_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for bit_range_region.  */

void
bit_range_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "BIT_RANGE_REG(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_bits.dump_to_pp (pp);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "bit_range_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_bits.dump_to_pp (pp);
      pp_printf (pp, ")");
    }
}

void
bit_range_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "bit_range_region(m_bits: ");
  m_bits.dump_to_pp (pp);
  pp_string (pp, ")");
}

void
bit_range_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
bit_range_region::to_json () const
{
  auto obj = region::make_json_obj ("bit_range_region");
  obj->set ("bits", m_bits.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
bit_range_region::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  auto result = std::make_unique<diagnostics::markup::compound_node> ();
  result->add_child ("bit_range_region (");
  result->add_child (get_parent_region ()->make_markup_node (markup_ctxt));
  result->add_child (", bits=");
  result->add_child (m_bits.make_markup_node ());
  result->add_child (")");
  return result;
}

const symbolic_byte_size *
bit_range_region::
maybe_get_byte_size (symbolic::scalar_manager<concrete_byte_size> &byte_mgr) const
{
  gcc_unreachable ();
}

/* Implementation of region::get_bit_size vfunc for bit_range_region.  */

const symbolic_bit_size *
bit_range_region::
maybe_get_bit_size (symbolic::scalar_manager<concrete_bit_size> &bit_mgr) const
{
  return &symbolic_from_concrete (m_bits.get_size (), bit_mgr);
}

const region::symbolic_bit_count *
bit_range_region::
maybe_get_bit_offset_relative_to_parent (region_model_manager &mgr) const
{
  auto &node_mgr = mgr.get_node_manager ();
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  return &bit_mgr.get_concrete (m_bits.get_start_offset ());
}

const region::symbolic_byte_count *
bit_range_region::
maybe_get_byte_offset_relative_to_parent (region_model_manager &mgr) const
{
  gcc_unreachable ();
}

/* class var_arg_region : public region.  */

void
var_arg_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "VAR_ARG_REG(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_printf (pp, ", arg_idx: %d)", m_idx);
    }
  else
    {
      pp_string (pp, "var_arg_region(");
      get_parent_region ()->dump_to_pp (pp, simple);
      pp_printf (pp, ", arg_idx: %d)", m_idx);
    }
}

void
var_arg_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "var_arg_region(arg_idx: %i)", m_idx);
}

void
var_arg_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
var_arg_region::to_json () const
{
  auto obj = region::make_json_obj ("var_arg_region");
  obj->set_integer ("index", m_idx);
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
var_arg_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("var_arg_region");
}

/* Get the frame_region for this var_arg_region.  */

const frame_region *
var_arg_region::get_frame_region () const
{
  gcc_assert (get_parent_region ());
  return as_a <const frame_region *> (get_parent_region ());
}

/* class errno_region : public region.  */

void
errno_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "errno_region");
  else
    pp_string (pp, "errno_region()");
}

void
errno_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "errno_region");
}

void
errno_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
errno_region::to_json () const
{
  return region::make_json_obj ("errno");
}

std::unique_ptr<diagnostics::markup::node>
errno_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("errno");
}

/* class private_region : public region.  */

void
private_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "PRIVATE_REG(%qs)", m_desc);
  else
    pp_printf (pp, "private_region(%qs)", m_desc);
}

void
private_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "private_region(%qs)", m_desc);
}

void
private_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
private_region::to_json () const
{
  auto obj = region::make_json_obj ("private_region");
  obj->set_string ("desc", m_desc);
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
private_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("private_region");
}

/* class unknown_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for unknown_region.  */

void
unknown_region::dump_to_pp (pretty_printer *pp, bool /*simple*/) const
{
  pp_string (pp, "UNKNOWN_REGION");
}

void
unknown_region::print_region_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "unknown_region");
}

void
unknown_region::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_region (*this))
    get_parent_region ()->accept_node_visitor (v);
}

std::unique_ptr<json::object>
unknown_region::to_json () const
{
  return region::make_json_obj ("unknown_region");
}

std::unique_ptr<diagnostics::markup::node>
unknown_region::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("unknown_region");
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
