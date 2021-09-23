/* Regions of memory.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "diagnostic-color.h"
#include "diagnostic-metadata.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region.h"
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

/* class region and its various subclasses.  */

/* class region.  */

region::~region ()
{
  delete m_cached_offset;
}

/* Compare REG1 and REG2 by id.  */

int
region::cmp_ids (const region *reg1, const region *reg2)
{
  return (long)reg1->get_id () - (long)reg2->get_id ();
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
	  iter = iter->get_parent_region ();
	  continue;
	case RK_CAST:
	  iter = iter->dyn_cast_cast_region ()->get_original_region ();
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
      if (iter->get_kind () == RK_CAST)
	iter = iter->dyn_cast_cast_region ()->get_original_region ();
      else
	iter = iter->get_parent_region ();
    }
  return false;
}

/* If this region is a frame_region, or a descendent of one, return it.
   Otherwise return NULL.  */

const frame_region *
region::maybe_get_frame_region () const
{
  const region *iter = this;
  while (iter)
    {
      if (const frame_region *frame_reg = iter->dyn_cast_frame_region ())
	return frame_reg;
      if (iter->get_kind () == RK_CAST)
	iter = iter->dyn_cast_cast_region ()->get_original_region ();
      else
	iter = iter->get_parent_region ();
    }
  return NULL;
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
	}
      if (iter->get_kind () == RK_CAST)
	iter = iter->dyn_cast_cast_region ()->get_original_region ();
      else
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
		tree ssa_name = decl;
		/* SSA names that are the default defn of a PARM_DECL
		   have initial_svalues; other SSA names don't.  */
		if (SSA_NAME_IS_DEFAULT_DEF (ssa_name)
		    && SSA_NAME_VAR (ssa_name)
		    && TREE_CODE (SSA_NAME_VAR (ssa_name)) == PARM_DECL)
		  return true;
		else
		  return false;
	      }
	    }
	}

      /* If we have an on-stack region that isn't associated with a decl
	 or SSA name, then we have VLA/alloca, which is uninitialized.  */
      return false;
    }
}

/* If this region is a decl_region, return the decl.
   Otherwise return NULL.  */

tree
region::maybe_get_decl () const
{
  if (const decl_region *decl_reg = dyn_cast_decl_region ())
    return decl_reg->get_decl ();
  return NULL_TREE;
}

/* Get the region_offset for this region (calculating it on the
   first call and caching it internally).  */

region_offset
region::get_offset () const
{
  if(!m_cached_offset)
    m_cached_offset = new region_offset (calc_offset ());
  return *m_cached_offset;
}

/* Base class implementation of region::get_byte_size vfunc.
   If the size of this region (in bytes) is known statically, write it to *OUT
   and return true.
   Otherwise return false.  */

bool
region::get_byte_size (byte_size_t *out) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return false;

  HOST_WIDE_INT bytes = int_size_in_bytes (type);
  if (bytes == -1)
    return false;
  *out = bytes;
  return true;
}

/* Base implementation of region::get_byte_size_sval vfunc.  */

const svalue *
region::get_byte_size_sval (region_model_manager *mgr) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return mgr->get_or_create_unknown_svalue (size_type_node);

  HOST_WIDE_INT bytes = int_size_in_bytes (type);
  if (bytes == -1)
    return mgr->get_or_create_unknown_svalue (size_type_node);

  tree byte_size = size_in_bytes (type);
  if (TREE_TYPE (byte_size) != size_type_node)
    byte_size = fold_build1 (NOP_EXPR, size_type_node, byte_size);
  return mgr->get_or_create_constant_svalue (byte_size);
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
  if (sz && tree_fits_uhwi_p (sz))
    {
      *out = TREE_INT_CST_LOW (sz);
      return true;
    }
  else
    return false;
}

/* If the size of this region (in bits) is known statically, write it to *OUT
   and return true.
   Otherwise return false.  */

bool
region::get_bit_size (bit_size_t *out) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return false;

  return int_size_in_bits (type, out);
}

/* Get the field within RECORD_TYPE at BIT_OFFSET.  */

tree
get_field_at_bit_offset (tree record_type, bit_offset_t bit_offset)
{
  gcc_assert (TREE_CODE (record_type) == RECORD_TYPE);
  if (bit_offset < 0)
    return NULL;

  /* Find the first field that has an offset > BIT_OFFSET,
     then return the one preceding it.
     Skip other trees within the chain, such as FUNCTION_DECLs.  */
  tree last_field = NULL_TREE;
  for (tree iter = TYPE_FIELDS (record_type); iter != NULL_TREE;
       iter = DECL_CHAIN (iter))
    {
      if (TREE_CODE (iter) == FIELD_DECL)
	{
	  int iter_field_offset = int_bit_position (iter);
	  if (bit_offset < iter_field_offset)
	    return last_field;
	  last_field = iter;
	}
    }
  return last_field;
}

/* Populate *OUT with descendent regions of type TYPE that match
   RELATIVE_BIT_OFFSET and SIZE_IN_BITS within this region.  */

void
region::get_subregions_for_binding (region_model_manager *mgr,
				    bit_offset_t relative_bit_offset,
				    bit_size_t size_in_bits,
				    tree type,
				    auto_vec <const region *> *out) const
{
  if (get_type () == NULL_TREE || type == NULL_TREE)
    return;
  if (relative_bit_offset == 0
      && types_compatible_p (get_type (), type))
    {
      out->safe_push (this);
      return;
    }
  switch (TREE_CODE (get_type ()))
    {
    case ARRAY_TYPE:
      {
	tree element_type = TREE_TYPE (get_type ());
	HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (element_type);
	if (hwi_byte_size > 0)
	  {
	    HOST_WIDE_INT bits_per_element
	      = hwi_byte_size << LOG2_BITS_PER_UNIT;
	    HOST_WIDE_INT element_index
	      = (relative_bit_offset.to_shwi () / bits_per_element);
	    tree element_index_cst
	      = build_int_cst (integer_type_node, element_index);
	    HOST_WIDE_INT inner_bit_offset
	      = relative_bit_offset.to_shwi () % bits_per_element;
	    const region *subregion = mgr->get_element_region
	      (this, element_type,
	       mgr->get_or_create_constant_svalue (element_index_cst));
	    subregion->get_subregions_for_binding (mgr, inner_bit_offset,
						   size_in_bits, type, out);
	  }
      }
      break;
    case RECORD_TYPE:
      {
	/* The bit offset might be *within* one of the fields (such as
	   with nested structs).
	   So we want to find the enclosing field, adjust the offset,
	   and repeat.  */
	if (tree field = get_field_at_bit_offset (get_type (),
						  relative_bit_offset))
	  {
	    int field_bit_offset = int_bit_position (field);
	    const region *subregion = mgr->get_field_region (this, field);
	    subregion->get_subregions_for_binding
	      (mgr, relative_bit_offset - field_bit_offset,
	       size_in_bits, type, out);
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
	    subregion->get_subregions_for_binding (mgr,
						   relative_bit_offset,
						   size_in_bits,
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

/* Walk from this region up to the base region within its cluster, calculating
   the offset relative to the base region, either as an offset in bits,
   or a symbolic offset.  */

region_offset
region::calc_offset () const
{
  const region *iter_region = this;
  bit_offset_t accum_bit_offset = 0;

  while (iter_region)
    {
      switch (iter_region->get_kind ())
	{
	case RK_FIELD:
	  {
	    const field_region *field_reg
	      = (const field_region *)iter_region;
	    iter_region = iter_region->get_parent_region ();

	    bit_offset_t rel_bit_offset;
	    if (!field_reg->get_relative_concrete_offset (&rel_bit_offset))
	      return region_offset::make_symbolic (iter_region);
	    accum_bit_offset += rel_bit_offset;
	  }
	  continue;

	case RK_ELEMENT:
	  {
	    const element_region *element_reg
	      = (const element_region *)iter_region;
	    iter_region = iter_region->get_parent_region ();

	    bit_offset_t rel_bit_offset;
	    if (!element_reg->get_relative_concrete_offset (&rel_bit_offset))
	      return region_offset::make_symbolic (iter_region);
	    accum_bit_offset += rel_bit_offset;
	  }
	  continue;

	case RK_OFFSET:
	  {
	    const offset_region *offset_reg
	      = (const offset_region *)iter_region;
	    iter_region = iter_region->get_parent_region ();

	    bit_offset_t rel_bit_offset;
	    if (!offset_reg->get_relative_concrete_offset (&rel_bit_offset))
	      return region_offset::make_symbolic (iter_region);
	    accum_bit_offset += rel_bit_offset;
	  }
	  continue;

	case RK_SIZED:
	  iter_region = iter_region->get_parent_region ();
	  continue;

	case RK_CAST:
	  {
	    const cast_region *cast_reg
	      = as_a <const cast_region *> (iter_region);
	    iter_region = cast_reg->get_original_region ();
	  }
	  continue;

	default:
	  return region_offset::make_concrete (iter_region, accum_bit_offset);
	}
    }
  return region_offset::make_concrete (iter_region, accum_bit_offset);
}

/* Base implementation of region::get_relative_concrete_offset vfunc.  */

bool
region::get_relative_concrete_offset (bit_offset_t *) const
{
  return false;
}

/* Copy from SRC_REG to DST_REG, using CTXT for any issues that occur.  */

void
region_model::copy_region (const region *dst_reg, const region *src_reg,
			   region_model_context *ctxt)
{
  gcc_assert (dst_reg);
  gcc_assert (src_reg);
  if (dst_reg == src_reg)
    return;

  const svalue *sval = get_store_value (src_reg, ctxt);
  set_value (dst_reg, sval, ctxt);
}

/* Dump a description of this region to stderr.  */

DEBUG_FUNCTION void
region::dump (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Return a new json::string describing the region.  */

json::value *
region::to_json () const
{
  label_text desc = get_desc (true);
  json::value *reg_js = new json::string (desc.m_buffer);
  desc.maybe_free ();
  return reg_js;
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

/* Return true if this is a symbolic region for deferencing an
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

/* region's ctor.  */

region::region (complexity c, unsigned id, const region *parent, tree type)
: m_complexity (c), m_id (id), m_parent (parent), m_type (type),
  m_cached_offset (NULL)
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

/* Return true iff this region is defined in terms of SVAL.  */

bool
region::involves_p (const svalue *sval) const
{
  if (const symbolic_region *symbolic_reg = dyn_cast_symbolic_region ())
    {
      if (symbolic_reg->get_pointer ()->involves_p (sval))
	return true;
    }

  return false;
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
    pp_printf (pp, "frame: %qs@%i", function_name (m_fun), get_stack_depth ());
  else
    pp_printf (pp, "frame_region(%qs, index: %i, depth: %i)",
	       function_name (m_fun), m_index, get_stack_depth ());
}

const decl_region *
frame_region::get_region_for_local (region_model_manager *mgr,
				    tree expr) const
{
  // TODO: could also check that VAR_DECLs are locals
  gcc_assert (TREE_CODE (expr) == PARM_DECL
	      || TREE_CODE (expr) == VAR_DECL
	      || TREE_CODE (expr) == SSA_NAME
	      || TREE_CODE (expr) == RESULT_DECL);

  /* Ideally we'd use mutable here.  */
  map_t &mutable_locals = const_cast <map_t &> (m_locals);

  if (decl_region **slot = mutable_locals.get (expr))
    return *slot;
  decl_region *reg
    = new decl_region (mgr->alloc_region_id (), this, expr);
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

/* class root_region : public region.  */

/* root_region's ctor.  */

root_region::root_region (unsigned id)
: region (complexity (1, 1), id, NULL, NULL_TREE)
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

/* class symbolic_region : public map_region.  */

/* symbolic_region's ctor.  */

symbolic_region::symbolic_region (unsigned id, region *parent,
				  const svalue *sval_ptr)
: region (complexity::from_pair (parent, sval_ptr), id, parent,
	  TREE_TYPE (sval_ptr->get_type ())),
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
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_string (pp, ", ");
      m_sval_ptr->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
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

/* Get the stack depth for the frame containing this decl, or 0
   for a global.  */

int
decl_region::get_stack_depth () const
{
  if (get_parent_region () == NULL)
    return 0;
  if (const frame_region *frame_reg
	= get_parent_region ()->dyn_cast_frame_region ())
    return frame_reg->get_stack_depth ();
  return 0;
}

/* If the underlying decl is in the global constant pool,
   return an svalue representing the constant value.
   Otherwise return NULL.  */

const svalue *
decl_region::maybe_get_constant_value (region_model_manager *mgr) const
{
  if (TREE_CODE (m_decl) == VAR_DECL
      && DECL_IN_CONSTANT_POOL (m_decl)
      && DECL_INITIAL (m_decl)
      && TREE_CODE (DECL_INITIAL (m_decl)) == CONSTRUCTOR)
    return get_svalue_for_constructor (DECL_INITIAL (m_decl), mgr);
  return NULL;
}

/* Get an svalue for CTOR, a CONSTRUCTOR for this region's decl.  */

const svalue *
decl_region::get_svalue_for_constructor (tree ctor,
					 region_model_manager *mgr) const
{
  gcc_assert (!TREE_CLOBBER_P (ctor));

  /* Create a binding map, applying ctor to it, using this
     decl_region as the base region when building child regions
     for offset calculations.  */
  binding_map map;
  if (!map.apply_ctor_to_region (this, ctor, mgr))
    return mgr->get_or_create_unknown_svalue (get_type ());

  /* Return a compound svalue for the map we built.  */
  return mgr->get_or_create_compound_svalue (get_type (), map);
}

/* For use on decl_regions for global variables.

   Get an svalue for the initial value of this region at entry to
   "main" (either based on DECL_INITIAL, or implicit initialization to
   zero.

   Return NULL if there is a problem.  */

const svalue *
decl_region::get_svalue_for_initializer (region_model_manager *mgr) const
{
  tree init = DECL_INITIAL (m_decl);
  if (!init)
    {
      /* If we have an "extern" decl then there may be an initializer in
	 another TU.  */
      if (DECL_EXTERNAL (m_decl))
	return NULL;

      /* Implicit initialization to zero; use a compound_svalue for it.
	 Doing so requires that we have a concrete binding for this region,
	 which can fail if we have a region with unknown size
	 (e.g. "extern const char arr[];").  */
      const binding_key *binding
	= binding_key::make (mgr->get_store_manager (), this);
      if (binding->symbolic_p ())
	return NULL;

      binding_cluster c (this);
      c.zero_fill_region (mgr->get_store_manager (), this);
      return mgr->get_or_create_compound_svalue (TREE_TYPE (m_decl),
						 c.get_map ());
    }

  /* LTO can write out error_mark_node as the DECL_INITIAL for simple scalar
     values (to avoid writing out an extra section).  */
  if (init == error_mark_node)
    return NULL;

  if (TREE_CODE (init) == CONSTRUCTOR)
    return get_svalue_for_constructor (init, mgr);

  /* Reuse the get_rvalue logic from region_model.  */
  region_model m (mgr);
  return m.get_rvalue (path_var (init, 0), NULL);
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

/* Implementation of region::get_relative_concrete_offset vfunc
   for field_region.  */

bool
field_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  /* Compare with e.g. gimple-fold.c's
     fold_nonarray_ctor_reference.  */
  tree byte_offset = DECL_FIELD_OFFSET (m_field);
  if (TREE_CODE (byte_offset) != INTEGER_CST)
    return false;
  tree field_offset = DECL_FIELD_BIT_OFFSET (m_field);
  /* Compute bit offset of the field.  */
  offset_int bitoffset
    = (wi::to_offset (field_offset)
       + (wi::to_offset (byte_offset) << LOG2_BITS_PER_UNIT));
  *out = bitoffset;
  return true;
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

/* Implementation of region::get_relative_concrete_offset vfunc
   for element_region.  */

bool
element_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  if (tree idx_cst = m_index->maybe_get_constant ())
    {
      gcc_assert (TREE_CODE (idx_cst) == INTEGER_CST);

      tree elem_type = get_type ();
      offset_int element_idx = wi::to_offset (idx_cst);

      /* First, use int_size_in_bytes, to reject the case where we
	 have an incomplete type, or a non-constant value.  */
      HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (elem_type);
      if (hwi_byte_size > 0)
	{
	  offset_int element_bit_size
	    = hwi_byte_size << LOG2_BITS_PER_UNIT;
	  offset_int element_bit_offset
	    = element_idx * element_bit_size;
	  *out = element_bit_offset;
	  return true;
	}
    }
  return false;
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

/* Implementation of region::get_relative_concrete_offset vfunc
   for offset_region.  */

bool
offset_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  if (tree byte_offset_cst = m_byte_offset->maybe_get_constant ())
    {
      gcc_assert (TREE_CODE (byte_offset_cst) == INTEGER_CST);
      /* Use a signed value for the byte offset, to handle
	 negative offsets.  */
      HOST_WIDE_INT byte_offset
	= wi::to_offset (byte_offset_cst).to_shwi ();
      HOST_WIDE_INT bit_offset = byte_offset * BITS_PER_UNIT;
      *out = bit_offset;
      return true;
    }
  return false;
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

/* Implementation of region::get_byte_size vfunc for sized_region.  */

bool
sized_region::get_byte_size (byte_size_t *out) const
{
  if (tree cst = m_byte_size_sval->maybe_get_constant ())
    {
      gcc_assert (TREE_CODE (cst) == INTEGER_CST);
      *out = tree_to_uhwi (cst);
      return true;
    }
  return false;
}

/* Implementation of region::get_bit_size vfunc for sized_region.  */

bool
sized_region::get_bit_size (bit_size_t *out) const
{
  byte_size_t byte_size;
  if (!get_byte_size (&byte_size))
    return false;
  *out = byte_size * BITS_PER_UNIT;
  return true;
}

/* class cast_region : public region.  */

/* Implementation of region::accept vfunc for cast_region.  */

void
cast_region::accept (visitor *v) const
{
  region::accept (v);
  m_original_region->accept (v);
}

/* Implementation of region::dump_to_pp vfunc for cast_region.  */

void
cast_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "CAST_REG(");
      print_quoted_type (pp, get_type ());
      pp_string (pp, ", ");
      m_original_region->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "cast_region(");
      m_original_region->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ")");
    }
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

/* class alloca_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for alloca_region.  */

void
alloca_region::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_string (pp, "ALLOCA_REGION");
  else
    pp_string (pp, "alloca_region()");
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

/* class unknown_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for unknown_region.  */

void
unknown_region::dump_to_pp (pretty_printer *pp, bool /*simple*/) const
{
  pp_string (pp, "UNKNOWN_REGION");
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
