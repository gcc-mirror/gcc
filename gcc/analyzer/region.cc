/* Regions of memory.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
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
#include "bitmap.h"
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
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "text-art/dump.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

region_offset
region_offset::make_byte_offset (const region *base_region,
				 const svalue *num_bytes_sval)
{
  if (tree num_bytes_cst = num_bytes_sval->maybe_get_constant ())
    {
      gcc_assert (TREE_CODE (num_bytes_cst) == INTEGER_CST);
      bit_offset_t num_bits = wi::to_offset (num_bytes_cst) * BITS_PER_UNIT;
      return make_concrete (base_region, num_bits);
    }
  else
    {
      return make_symbolic (base_region, num_bytes_sval);
    }
}

const svalue &
region_offset::calc_symbolic_bit_offset (region_model_manager *mgr) const
{
  if (symbolic_p ())
    {
      const svalue *bits_per_byte
	= mgr->get_or_create_int_cst (NULL_TREE, BITS_PER_UNIT);
      return *mgr->get_or_create_binop (NULL_TREE, MULT_EXPR,
					m_sym_offset, bits_per_byte);
    }
  else
    return *mgr->get_or_create_int_cst (NULL_TREE, m_offset);
}

const svalue *
region_offset::calc_symbolic_byte_offset (region_model_manager *mgr) const
{
  if (symbolic_p ())
    return m_sym_offset;
  else
    {
      byte_offset_t concrete_byte_offset;
      if (get_concrete_byte_offset (&concrete_byte_offset))
	return mgr->get_or_create_int_cst (size_type_node,
					   concrete_byte_offset);
      else
	/* Can't handle bitfields; return UNKNOWN.  */
	return mgr->get_or_create_unknown_svalue (size_type_node);
    }
}

void
region_offset::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (symbolic_p ())
    {
      /* We don't bother showing the base region.  */
      pp_string (pp, "byte ");
      m_sym_offset->dump_to_pp (pp, simple);
    }
  else
    {
      if (m_offset % BITS_PER_UNIT == 0)
	{
	  pp_string (pp, "byte ");
	  pp_wide_int (pp, m_offset / BITS_PER_UNIT, SIGNED);
	}
      else
	{
	  pp_string (pp, "bit ");
	  pp_wide_int (pp, m_offset, SIGNED);
	}
    }
}

DEBUG_FUNCTION void
region_offset::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

/* An svalue that matches the pattern (BASE * FACTOR) + OFFSET
   where FACTOR or OFFSET could be the identity (represented as NULL).  */

struct linear_op
{
  linear_op (const svalue *base,
	     const svalue *factor,
	     const svalue *offset)
  : m_base (base), m_factor (factor), m_offset (offset)
  {
  }

  bool maybe_get_cst_factor (bit_offset_t *out) const
  {
    if (m_factor == nullptr)
      {
	*out = 1;
	return true;
      }
    if (tree cst_factor = m_factor->maybe_get_constant ())
      {
	*out = wi::to_offset (cst_factor);
	return true;
      }
    return false;
  }

  bool maybe_get_cst_offset (bit_offset_t *out) const
  {
    if (m_offset == nullptr)
      {
	*out = 0;
	return true;
      }
    if (tree cst_offset = m_offset->maybe_get_constant ())
      {
	*out = wi::to_offset (cst_offset);
	return true;
      }
    return false;
  }

  static tristate
  less (const linear_op &a, const linear_op &b)
  {
    /* Same base.  */
    if (a.m_base == b.m_base)
      {
	bit_offset_t a_wi_factor;
	bit_offset_t b_wi_factor;
	if (a.maybe_get_cst_factor (&a_wi_factor)
	    && b.maybe_get_cst_factor (&b_wi_factor))
	  {
	    if (a_wi_factor != b_wi_factor)
	      return tristate (a_wi_factor < b_wi_factor);
	    else
	      {
		bit_offset_t a_wi_offset;
		bit_offset_t b_wi_offset;
		if (a.maybe_get_cst_offset (&a_wi_offset)
		    && b.maybe_get_cst_offset (&b_wi_offset))
		  return tristate (a_wi_offset < b_wi_offset);
	      }
	  }
      }
    return tristate::unknown ();
  }

  static tristate
  le (const linear_op &a, const linear_op &b)
  {
    /* Same base.  */
    if (a.m_base == b.m_base)
      {
	bit_offset_t a_wi_factor;
	bit_offset_t b_wi_factor;
	if (a.maybe_get_cst_factor (&a_wi_factor)
	    && b.maybe_get_cst_factor (&b_wi_factor))
	  {
	    if (a_wi_factor != b_wi_factor)
	      return tristate (a_wi_factor <= b_wi_factor);
	    else
	      {
		bit_offset_t a_wi_offset;
		bit_offset_t b_wi_offset;
		if (a.maybe_get_cst_offset (&a_wi_offset)
		    && b.maybe_get_cst_offset (&b_wi_offset))
		  return tristate (a_wi_offset <= b_wi_offset);
	      }
	  }
      }
    return tristate::unknown ();
  }

  static bool
  from_svalue (const svalue &sval, linear_op *out)
  {
    switch (sval.get_kind ())
      {
      default:
	break;
      case SK_BINOP:
	{
	  const binop_svalue &binop_sval ((const binop_svalue &)sval);
	  if (binop_sval.get_op () == MULT_EXPR)
	    {
	      *out = linear_op (binop_sval.get_arg0 (),
				binop_sval.get_arg1 (),
				NULL);
	      return true;
	    }
	  else if (binop_sval.get_op () == PLUS_EXPR)
	    {
	      if (binop_sval.get_arg0 ()->get_kind () == SK_BINOP)
		{
		  const binop_svalue &inner_binop_sval
		    ((const binop_svalue &)*binop_sval.get_arg0 ());
		  if (inner_binop_sval.get_op () == MULT_EXPR)
		    {
		      *out = linear_op (inner_binop_sval.get_arg0 (),
					inner_binop_sval.get_arg1 (),
					binop_sval.get_arg1 ());
		      return true;
		    }
		}

	      *out = linear_op (binop_sval.get_arg0 (),
				NULL,
				binop_sval.get_arg1 ());
	      return true;
	    }
	}
	break;
      }
    return false;
  }

  const svalue *m_base;
  const svalue *m_factor;
  const svalue *m_offset;
};

bool
operator< (const region_offset &a, const region_offset &b)
{
  if (a.symbolic_p ())
    {
      if (b.symbolic_p ())
	{
	  /* Symbolic vs symbolic.  */
	  const svalue &a_sval = *a.get_symbolic_byte_offset ();
	  const svalue &b_sval = *b.get_symbolic_byte_offset ();

	  linear_op op_a (NULL, NULL, NULL);
	  linear_op op_b (NULL, NULL, NULL);
	  if (linear_op::from_svalue (a_sval, &op_a)
	      && linear_op::from_svalue (b_sval, &op_b))
	    {
	      tristate ts = linear_op::less (op_a, op_b);
	      if (ts.is_true ())
		return true;
	      else if (ts.is_false ())
		return false;
	    }
	  /* Use svalue's deterministic order, for now.  */
	  return (svalue::cmp_ptr (a.get_symbolic_byte_offset (),
				   b.get_symbolic_byte_offset ())
		  < 0);
	}
      else
	/* Symbolic vs concrete: put all symbolic after all concrete.  */
	return false;
    }
  else
    {
      if (b.symbolic_p ())
	/* Concrete vs symbolic: put all concrete before all symbolic.  */
	return true;
      else
	/* Concrete vs concrete.  */
	return a.get_bit_offset () < b.get_bit_offset ();
    }
}

bool
operator<= (const region_offset &a, const region_offset &b)
{
  if (a.symbolic_p ())
    {
      if (b.symbolic_p ())
	{
	  /* Symbolic vs symbolic.  */
	  const svalue &a_sval = *a.get_symbolic_byte_offset ();
	  const svalue &b_sval = *b.get_symbolic_byte_offset ();

	  linear_op op_a (NULL, NULL, NULL);
	  linear_op op_b (NULL, NULL, NULL);
	  if (linear_op::from_svalue (a_sval, &op_a)
	      && linear_op::from_svalue (b_sval, &op_b))
	    {
	      tristate ts = linear_op::le (op_a, op_b);
	      if (ts.is_true ())
		return true;
	      else if (ts.is_false ())
		return false;
	    }
	  /* Use svalue's deterministic order, for now.  */
	  return (svalue::cmp_ptr (a.get_symbolic_byte_offset (),
				   b.get_symbolic_byte_offset ())
		  <= 0);
	}
      else
	/* Symbolic vs concrete: put all symbolic after all concrete.  */
	return false;
    }
  else
    {
      if (b.symbolic_p ())
	/* Concrete vs symbolic: put all concrete before all symbolic.  */
	return true;
      else
	/* Concrete vs concrete.  */
	return a.get_bit_offset () <= b.get_bit_offset ();
    }
}

bool
operator> (const region_offset &a, const region_offset &b)
{
  return b < a;
}

bool
operator>= (const region_offset &a, const region_offset &b)
{
  return b <= a;
}

region_offset
strip_types (const region_offset &offset, region_model_manager &mgr)
{
  if (offset.symbolic_p ())
    return region_offset::make_symbolic
      (offset.get_base_region (),
       strip_types (offset.get_symbolic_byte_offset (),
		    mgr));
  else
    return offset;
}

/* class region and its various subclasses.  */

/* class region.  */

region::~region ()
{
  delete m_cached_offset;
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
   Otherwise return NULL.  */

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
	case RK_PRIVATE:
	  return MEMSPACE_PRIVATE;
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
  const decl_region *base_reg = get_base_region ()->dyn_cast_decl_region ();
  gcc_assert (base_reg);

  /* Attempt to get the initializer value for base_reg.  */
  if (const svalue *base_reg_init
      = base_reg->get_svalue_for_initializer (mgr))
    {
      if (this == base_reg)
	return base_reg_init;
      else
	{
	  /* Get the value for REG within base_reg_init.  */
	  binding_cluster c (base_reg);
	  c.bind (mgr->get_store_manager (), base_reg, base_reg_init);
	  const svalue *sval
	    = c.get_any_binding (mgr->get_store_manager (), this);
	  if (sval)
	    {
	      if (get_type ())
		sval = mgr->get_or_create_cast (get_type (), sval);
	      return sval;
	    }
	}
    }

  /* Otherwise, return INIT_VAL(REG).  */
  return mgr->get_or_create_initial_value (this);
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
region::get_offset (region_model_manager *mgr) const
{
  if(!m_cached_offset)
    m_cached_offset = new region_offset (calc_offset (mgr));
  return *m_cached_offset;
}

/* Get the region_offset for immediately beyond this region.  */

region_offset
region::get_next_offset (region_model_manager *mgr) const
{
  region_offset start = get_offset (mgr);

  bit_size_t bit_size;
  if (get_bit_size (&bit_size))
    {
      if (start.concrete_p ())
	{
	  bit_offset_t next_bit_offset = start.get_bit_offset () + bit_size;
	  return region_offset::make_concrete (start.get_base_region (),
					       next_bit_offset);
	}
    }

  const svalue *start_byte_offset_sval = start.calc_symbolic_byte_offset (mgr);
  const svalue *byte_size_sval = get_byte_size_sval (mgr);
  const svalue *sum_sval
    = mgr->get_or_create_binop (size_type_node,
				PLUS_EXPR,
				start_byte_offset_sval,
				byte_size_sval);
  return region_offset::make_symbolic (start.get_base_region (),
				       sum_sval);
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

/* Base implementation of region::get_bit_size_sval vfunc.  */

const svalue *
region::get_bit_size_sval (region_model_manager *mgr) const
{
  tree type = get_type ();

  /* Bail out e.g. for heap-allocated regions.  */
  if (!type)
    return mgr->get_or_create_unknown_svalue (size_type_node);

  bit_size_t bits;
  if (!int_size_in_bits (type, &bits))
    return mgr->get_or_create_unknown_svalue (size_type_node);

  return mgr->get_or_create_int_cst (size_type_node, bits);
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
region::calc_offset (region_model_manager *mgr) const
{
  const region *iter_region = this;
  bit_offset_t accum_bit_offset = 0;
  const svalue *accum_byte_sval = NULL;

  while (iter_region)
    {
      switch (iter_region->get_kind ())
	{
	case RK_FIELD:
	case RK_ELEMENT:
	case RK_OFFSET:
	case RK_BIT_RANGE:
	  if (accum_byte_sval)
	    {
	      const svalue *sval
		= iter_region->get_relative_symbolic_offset (mgr);
	      accum_byte_sval
		= mgr->get_or_create_binop (ptrdiff_type_node, PLUS_EXPR,
					    accum_byte_sval, sval);
	      iter_region = iter_region->get_parent_region ();
	    }
	  else
	    {
	      bit_offset_t rel_bit_offset;
	      if (iter_region->get_relative_concrete_offset (&rel_bit_offset))
		{
		  accum_bit_offset += rel_bit_offset;
		  iter_region = iter_region->get_parent_region ();
		}
	      else
		{
		  /* If the iter_region is not concrete anymore, convert the
		     accumulated bits to a svalue in bytes and revisit the
		     iter_region collecting the symbolic value.  */
		  byte_offset_t byte_offset = accum_bit_offset / BITS_PER_UNIT;
		  tree offset_tree = wide_int_to_tree (ptrdiff_type_node,
						       byte_offset);
		  accum_byte_sval
		    = mgr->get_or_create_constant_svalue (offset_tree);
		}
	    }
	  continue;
	case RK_SIZED:
	case RK_CAST:
	  iter_region = iter_region->get_parent_region ();
	  continue;

	default:
	  return accum_byte_sval
		  ? region_offset::make_symbolic (iter_region,
						  accum_byte_sval)
		  : region_offset::make_concrete (iter_region,
						  accum_bit_offset);
	}
    }

  return accum_byte_sval ? region_offset::make_symbolic (iter_region,
							 accum_byte_sval)
			 : region_offset::make_concrete (iter_region,
							 accum_bit_offset);
}

/* Base implementation of region::get_relative_concrete_offset vfunc.  */

bool
region::get_relative_concrete_offset (bit_offset_t *) const
{
  return false;
}

/* Base implementation of region::get_relative_symbolic_offset vfunc.  */

const svalue *
region::get_relative_symbolic_offset (region_model_manager *mgr) const
{
  return mgr->get_or_create_unknown_svalue (ptrdiff_type_node);
}

/* Attempt to get the position and size of this region expressed as a
   concrete range of bytes relative to its parent.
   If successful, return true and write to *OUT.
   Otherwise return false.  */

bool
region::get_relative_concrete_byte_range (byte_range *out) const
{
  /* We must have a concrete offset relative to the parent.  */
  bit_offset_t rel_bit_offset;
  if (!get_relative_concrete_offset (&rel_bit_offset))
    return false;
  /* ...which must be a whole number of bytes.  */
  if (rel_bit_offset % BITS_PER_UNIT != 0)
    return false;
  byte_offset_t start_byte_offset = rel_bit_offset / BITS_PER_UNIT;

  /* We must have a concrete size, which must be a whole number
     of bytes.  */
  byte_size_t num_bytes;
  if (!get_byte_size (&num_bytes))
    return false;

  /* Success.  */
  *out = byte_range (start_byte_offset, num_bytes);
  return true;
}

/* Dump a description of this region to stderr.  */

DEBUG_FUNCTION void
region::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

/* Dump a tree-like representation of this region and its constituent symbols
   to stderr, using global_dc's colorization and theming options.

   For example:
   . (gdb) call reg->dump()
   . (26): ‘int’: decl_region(‘x_10(D)’)
   . ╰─ parent: (9): frame_region(‘test_bitmask_2’, index: 0, depth: 1)
   .    ╰─ parent: (1): stack region
   .       ╰─ parent: (0): root region
  */

DEBUG_FUNCTION void
region::dump () const
{
  text_art::dump (*this);
}

/* Return a new json::string describing the region.  */

std::unique_ptr<json::value>
region::to_json () const
{
  label_text desc = get_desc (true);
  auto reg_js = ::make_unique<json::string> (desc.get ());
  return reg_js;
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

/* Use DWI to create a text_art::widget describing this region in
   a tree-like form, using PREFIX as a prefix (e.g. for field names).  */

std::unique_ptr<text_art::tree_widget>
region::make_dump_widget (const text_art::dump_widget_info &dwi,
			  const char *prefix) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = true;

  if (prefix)
    pp_printf (&pp, "%s: ", prefix);

  pp_printf (&pp, "(%i): ", get_id ());
  if (get_type ())
    pp_printf (&pp, "%qT: ", get_type ());

  print_dump_widget_label (&pp);

  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::make (dwi, &pp));

  add_dump_widget_children (*w, dwi);

  if (m_parent)
    w->add_child (m_parent->make_dump_widget (dwi, "parent"));

  return w;
}

void
region::add_dump_widget_children (text_art::tree_widget &,
				  const text_art::dump_widget_info &) const
{
  /* By default, add nothing (parent is added in make_dump_widget).  */
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

/* Return true if this is a symbolic region.  */

bool
region::symbolic_p () const
{
  return get_kind () == RK_SYMBOLIC;
}

/* Return true if this region is known to be zero bits in size.  */

bool
region::empty_p () const
{
  bit_size_t num_bits;
  if (get_bit_size (&num_bits))
    if (num_bits == 0)
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

region::region (complexity c, symbol::id_t id, const region *parent, tree type)
: symbol (c, id),
  m_parent (parent), m_type (type),
  m_cached_offset (NULL), m_cached_init_sval_at_main (NULL)
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
    pp_printf (pp, "frame: %qs@%i", function_name (&m_fun), get_stack_depth ());
  else
    pp_printf (pp, "frame_region(%qs, index: %i, depth: %i)",
	       function_name (&m_fun), m_index, get_stack_depth ());
}

void
frame_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "frame_region(%qs, index: %i, depth: %i)",
	     function_name (&m_fun), m_index, get_stack_depth ());
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
		    const supernode *snode
		      = sg->get_supernode_for_stmt (def_stmt);
		    gcc_assert (snode->get_function () == &m_fun);
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
    = new decl_region (mgr->alloc_symbol_id (), this, expr);
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
globals_region::print_dump_widget_label (pretty_printer *pp) const
{
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

void
code_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "code region");
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
function_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "function_region(");
  dump_quoted_tree (pp, m_fndecl);
  pp_string (pp, ")");
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
label_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "label_region(");
  dump_quoted_tree (pp, m_label);
  pp_string (pp, ")");
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
stack_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "stack region");
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
heap_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "heap_region");
}

/* class root_region : public region.  */

/* root_region's ctor.  */

root_region::root_region (symbol::id_t id)
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

void
root_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "root region");
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
thread_local_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "thread_local_region");
}

/* class symbolic_region : public map_region.  */

/* symbolic_region's ctor.  */

symbolic_region::symbolic_region (symbol::id_t id, region *parent,
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
symbolic_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "symbolic_region: %<*%>");
}

void
symbolic_region::
add_dump_widget_children (text_art::tree_widget &w,
			  const text_art::dump_widget_info &dwi) const
{
  w.add_child (m_sval_ptr->make_dump_widget (dwi, "m_sval_ptr"));
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
decl_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "decl_region(%qE)", m_decl);
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
  if (VAR_P (m_decl)
      && DECL_IN_CONSTANT_POOL (m_decl)
      && DECL_INITIAL (m_decl)
      && TREE_CODE (DECL_INITIAL (m_decl)) == CONSTRUCTOR)
    return get_svalue_for_constructor (DECL_INITIAL (m_decl), mgr);
  return NULL;
}

/* Implementation of decl_region::get_svalue_for_constructor
   for when the cached value hasn't yet been calculated.  */

const svalue *
decl_region::calc_svalue_for_constructor (tree ctor,
					  region_model_manager *mgr) const
{
  /* Create a binding map, applying ctor to it, using this
     decl_region as the base region when building child regions
     for offset calculations.  */
  binding_map map;
  if (!map.apply_ctor_to_region (this, ctor, mgr))
    return mgr->get_or_create_unknown_svalue (get_type ());

  /* Return a compound svalue for the map we built.  */
  return mgr->get_or_create_compound_svalue (get_type (), map);
}

/* Get an svalue for CTOR, a CONSTRUCTOR for this region's decl.  */

const svalue *
decl_region::get_svalue_for_constructor (tree ctor,
					 region_model_manager *mgr) const
{
  gcc_assert (!TREE_CLOBBER_P (ctor));
  gcc_assert (ctor == DECL_INITIAL (m_decl));

  if (!m_ctor_svalue)
    m_ctor_svalue = calc_svalue_for_constructor (ctor, mgr);

  return m_ctor_svalue;
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

      if (empty_p ())
	return NULL;

      /* Implicit initialization to zero; use a compound_svalue for it.
	 Doing so requires that we have a concrete binding for this region,
	 which can fail if we have a region with unknown size
	 (e.g. "extern const char arr[];").  */
      const binding_key *binding
	= binding_key::make (mgr->get_store_manager (), this);
      if (binding->symbolic_p ())
	return NULL;

      /* If we don't care about tracking the content of this region, then
	 it's unused, and the value doesn't matter.  */
      if (!tracked_p ())
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

/* Subroutine of symnode_requires_tracking_p; return true if REF
   might imply that we should be tracking the value of its decl.  */

static bool
ipa_ref_requires_tracking (ipa_ref *ref)
{
  /* If we have a load/store/alias of the symbol, then we'll track
     the decl's value.  */
  if (ref->use != IPA_REF_ADDR)
    return true;

  if (ref->stmt == NULL)
    return true;

  switch (ref->stmt->code)
    {
    default:
      return true;
    case GIMPLE_CALL:
      {
	cgraph_node *caller_cnode = dyn_cast <cgraph_node *> (ref->referring);
	if (caller_cnode == NULL)
	  return true;
	cgraph_edge *edge = caller_cnode->get_edge (ref->stmt);
	if (!edge)
	  return true;
	if (edge->callee == NULL)
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
  if (context_fndecl == NULL)
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
field_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "field_region(%qE)", m_field);
}

/* Implementation of region::get_relative_concrete_offset vfunc
   for field_region.  */

bool
field_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  /* Compare with e.g. gimple-fold.cc's
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


/* Implementation of region::get_relative_symbolic_offset vfunc
   for field_region.
   If known, the returned svalue is equal to the offset converted to bytes and
   rounded off.  */

const svalue *
field_region::get_relative_symbolic_offset (region_model_manager *mgr) const
{
  bit_offset_t out;
  if (get_relative_concrete_offset (&out))
    {
      tree cst_tree
	= wide_int_to_tree (ptrdiff_type_node, out / BITS_PER_UNIT);
      return mgr->get_or_create_constant_svalue (cst_tree);
    }
  return mgr->get_or_create_unknown_svalue (ptrdiff_type_node);
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
element_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "element_region: %<[]%>");
}

void
element_region::
add_dump_widget_children (text_art::tree_widget &w,
			  const text_art::dump_widget_info &dwi) const
{
  w.add_child (m_index->make_dump_widget (dwi, "m_index"));
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

/* Implementation of region::get_relative_symbolic_offset vfunc
   for element_region.  */

const svalue *
element_region::get_relative_symbolic_offset (region_model_manager *mgr) const
{
  tree elem_type = get_type ();

  /* First, use int_size_in_bytes, to reject the case where we
     have an incomplete type, or a non-constant value.  */
  HOST_WIDE_INT hwi_byte_size = int_size_in_bytes (elem_type);
  if (hwi_byte_size > 0)
	  {
      tree byte_size_tree = wide_int_to_tree (ptrdiff_type_node,
					      hwi_byte_size);
      const svalue *byte_size_sval
	= mgr->get_or_create_constant_svalue (byte_size_tree);
      return mgr->get_or_create_binop (NULL_TREE, MULT_EXPR,
				       m_index, byte_size_sval);
    }
  return mgr->get_or_create_unknown_svalue (ptrdiff_type_node);
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
offset_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "offset_region");
}

void
offset_region::
add_dump_widget_children (text_art::tree_widget &w,
			  const text_art::dump_widget_info &dwi) const
{
  w.add_child (m_byte_offset->make_dump_widget (dwi, "m_byte_offset"));
}

const svalue *
offset_region::get_bit_offset (region_model_manager *mgr) const
{
  const svalue *bits_per_byte_sval
    = mgr->get_or_create_int_cst (NULL_TREE, BITS_PER_UNIT);
  return mgr->get_or_create_binop (NULL_TREE, MULT_EXPR,
				   m_byte_offset, bits_per_byte_sval);
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

/* Implementation of region::get_relative_symbolic_offset vfunc
   for offset_region.  */

const svalue *
offset_region::get_relative_symbolic_offset (region_model_manager *mgr
					      ATTRIBUTE_UNUSED) const
{
  return get_byte_offset ();
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
sized_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "sized_region");
}

void
sized_region::
add_dump_widget_children (text_art::tree_widget &w,
			  const text_art::dump_widget_info &dwi) const
{
  w.add_child (m_byte_size_sval->make_dump_widget (dwi, "m_byte_size_sval"));
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

/* Implementation of region::get_bit_size_sval vfunc for sized_region.  */

const svalue *
sized_region::get_bit_size_sval (region_model_manager *mgr) const
{
  const svalue *bits_per_byte_sval
    = mgr->get_or_create_int_cst (NULL_TREE, BITS_PER_UNIT);
  return mgr->get_or_create_binop (NULL_TREE, MULT_EXPR,
				   m_byte_size_sval, bits_per_byte_sval);
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
cast_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "cast_region");
}

/* Implementation of region::get_relative_concrete_offset vfunc
   for cast_region.  */

bool
cast_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  *out = (int) 0;
  return true;
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
heap_allocated_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "heap_allocated_region");
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
alloca_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "alloca_region");
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
string_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "string_region(");
  dump_tree (pp, m_string_cst);
  pp_string (pp, ")");
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
bit_range_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "bit_range_region(m_bits: ");
  m_bits.dump_to_pp (pp);
  pp_string (pp, ")");
}

/* Implementation of region::get_byte_size vfunc for bit_range_region.  */

bool
bit_range_region::get_byte_size (byte_size_t *out) const
{
  if (m_bits.m_size_in_bits % BITS_PER_UNIT == 0)
    {
      *out = m_bits.m_size_in_bits / BITS_PER_UNIT;
      return true;
    }
  return false;
}

/* Implementation of region::get_bit_size vfunc for bit_range_region.  */

bool
bit_range_region::get_bit_size (bit_size_t *out) const
{
  *out = m_bits.m_size_in_bits;
  return true;
}

/* Implementation of region::get_byte_size_sval vfunc for bit_range_region.  */

const svalue *
bit_range_region::get_byte_size_sval (region_model_manager *mgr) const
{
  if (m_bits.m_size_in_bits % BITS_PER_UNIT != 0)
    return mgr->get_or_create_unknown_svalue (size_type_node);

  HOST_WIDE_INT num_bytes = m_bits.m_size_in_bits.to_shwi () / BITS_PER_UNIT;
  return mgr->get_or_create_int_cst (size_type_node, num_bytes);
}

/* Implementation of region::get_bit_size_sval vfunc for bit_range_region.  */

const svalue *
bit_range_region::get_bit_size_sval (region_model_manager *mgr) const
{
  return mgr->get_or_create_int_cst (size_type_node,
				     m_bits.m_size_in_bits);
}

/* Implementation of region::get_relative_concrete_offset vfunc for
   bit_range_region.  */

bool
bit_range_region::get_relative_concrete_offset (bit_offset_t *out) const
{
  *out = m_bits.get_start_bit_offset ();
  return true;
}

/* Implementation of region::get_relative_symbolic_offset vfunc for
   bit_range_region.
   The returned svalue is equal to the offset converted to bytes and
   rounded off.  */

const svalue *
bit_range_region::get_relative_symbolic_offset (region_model_manager *mgr)
  const
{
  byte_offset_t start_byte = m_bits.get_start_bit_offset () / BITS_PER_UNIT;
  tree start_bit_tree = wide_int_to_tree (ptrdiff_type_node, start_byte);
  return mgr->get_or_create_constant_svalue (start_bit_tree);
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
var_arg_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "var_arg_region(arg_idx: %i)", m_idx);
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
errno_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "errno_region");
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
private_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "private_region(%qs)", m_desc);
}

/* class unknown_region : public region.  */

/* Implementation of region::dump_to_pp vfunc for unknown_region.  */

void
unknown_region::dump_to_pp (pretty_printer *pp, bool /*simple*/) const
{
  pp_string (pp, "UNKNOWN_REGION");
}

void
unknown_region::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "unknown_region");
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
