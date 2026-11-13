/* Implementation of decl_region::calc_svalue_for_constructor.
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

#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

/* Handling of tree code CONSTRUCTOR for initialization of a decl_region.  */

static bool
apply_ctor_pair_to_child_region (symbolic::concrete_bit_overlay::key &out,
				 const region *parent_reg,
				 region_model_manager *mgr,
				 tree index, tree val);

static bool
apply_ctor_val_to_range (symbolic::concrete_bit_overlay::key &out,
			 const region *parent_reg,
			 region_model_manager *mgr,
			 tree min_index, tree max_index,
			 tree val);

/* Get the child region of PARENT_REG based upon INDEX within a
   CONSTRUCTOR.   */

static const region *
get_subregion_within_ctor (const region *parent_reg, tree index,
			   region_model_manager *mgr)
{
  switch (TREE_CODE (index))
    {
    default:
      gcc_unreachable ();
    case INTEGER_CST:
      {
	const svalue *index_sval
	  = mgr->get_or_create_constant_svalue (index);
	return mgr->get_element_region (parent_reg,
					TREE_TYPE (parent_reg->get_type ()),
					index_sval);
      }
      break;
    case FIELD_DECL:
      return mgr->get_field_region (parent_reg, index);
    }
}

/* Get the child region of PARENT_REG based upon (INDEX, VALUE) within a
   CONSTRUCTOR.   */

static const region *
get_subregion_within_ctor_for_ctor_pair (const region *parent_reg,
					 tree index,
					 tree value,
					 region_model_manager *mgr)
{
  if (TREE_CODE (index) == INTEGER_CST
      && TREE_CODE (value) == RAW_DATA_CST)
    {
      /* Special-case; see tree.def's description of CONSTRUCTOR.
	 We have RAW_DATA_LENGTH of bytes, starting at INDEX's start.  */
      const region *start_reg
	= get_subregion_within_ctor (parent_reg, index, mgr);
      /* Build a bit range, relative to PARENT_REG.  */

      auto start_offset = start_reg->get_memory_offset (*mgr);
      concrete_bit_offset conc_start_offset;
      if (!concrete_from_symbolic (start_offset.get_bit_offset (),
				   &conc_start_offset))
	return nullptr;
      concrete_byte_size byte_length (RAW_DATA_LENGTH (value));
      concrete_bit_size bit_length = bits_from_bytes (byte_length);
      concrete_bit_range bits (conc_start_offset, bit_length);
      return mgr->get_bit_range (parent_reg, NULL_TREE, bits);
    }

  return get_subregion_within_ctor (parent_reg, index, mgr);
}

/* Get the svalue for VAL, a non-CONSTRUCTOR value within a CONSTRUCTOR.  */

static const svalue *
get_svalue_for_ctor_val (tree val, region_model_manager *mgr)
{
  /* Reuse the get_rvalue logic from region_model.  */
  region_model m (mgr->get_node_manager ());
  return m.get_rvalue (path_var (val, 0), nullptr);
}

/* Bind values from CONSTRUCTOR to OUT, relative to
   PARENT_REG's relationship to its base region.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

static bool
apply_ctor_to_region (symbolic::concrete_bit_overlay::key &out,
		      const region *parent_reg,
		      tree ctor,
		      region_model_manager *mgr)
{
  gcc_assert (parent_reg);
  gcc_assert (TREE_CODE (ctor) == CONSTRUCTOR);

  unsigned ix;
  tree index;
  tree val;
  tree parent_type = parent_reg->get_type ();
  tree field;
  if (TREE_CODE (parent_type) == RECORD_TYPE)
    field = TYPE_FIELDS (parent_type);
  else
    field = NULL_TREE;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (ctor), ix, index, val)
    {
      if (!index)
	{
	  /* If index is NULL, then iterate through the fields for
	     a RECORD_TYPE, or use an INTEGER_CST otherwise.
	     Compare with similar logic in output_constructor.  */
	  if (field)
	    {
	      index = field;
	      field = DECL_CHAIN (field);
	    }
	  else
	    index = build_int_cst (integer_type_node, ix);
	}
      else if (TREE_CODE (index) == RANGE_EXPR)
	{
	  tree min_index = TREE_OPERAND (index, 0);
	  tree max_index = TREE_OPERAND (index, 1);
	  if (min_index == max_index)
	    {
	      if (!apply_ctor_pair_to_child_region (out, parent_reg, mgr,
						    min_index, val))
		return false;
	    }
	  else
	    {
	      if (!apply_ctor_val_to_range (out, parent_reg, mgr,
					    min_index, max_index, val))
		return false;
	    }
	  continue;
	}
      if (!apply_ctor_pair_to_child_region (out, parent_reg, mgr, index, val))
	return false;
    }
  return true;
}

/* Bind the value VAL into OUT at the range of elements within PARENT_REF
   from MIN_INDEX to MAX_INDEX (including endpoints).
   For use in handling RANGE_EXPR within a CONSTRUCTOR.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

static bool
apply_ctor_val_to_range (symbolic::concrete_bit_overlay::key &out,
			 const region *parent_reg,
			 region_model_manager *mgr,
			 tree min_index, tree max_index,
			 tree val)
{
  gcc_assert (TREE_CODE (min_index) == INTEGER_CST);
  gcc_assert (TREE_CODE (max_index) == INTEGER_CST);

  /* Generate a binding key for the range.  */
  const region *min_element
    = get_subregion_within_ctor (parent_reg, min_index, mgr);
  const region *max_element
    = get_subregion_within_ctor (parent_reg, max_index, mgr);

  auto min_element_mem_range = min_element->get_memory_range (*mgr);
  auto max_element_mem_range = max_element->get_memory_range (*mgr);
  gcc_assert (&min_element_mem_range.get_base_region ()
	      == &max_element_mem_range.get_base_region ());
  symbolic_bit_range overall_sym_range
    (min_element_mem_range.get_start_offset (),
     (max_element_mem_range.get_next_offset ()
      - min_element_mem_range.get_next_offset ()));
  concrete_bit_range overall_conc_range;
  if (!concrete_from_symbolic (overall_sym_range, &overall_conc_range))
    return false;

  /* Get the value.  */
  if (TREE_CODE (val) == CONSTRUCTOR)
    return false;
  const svalue *sval = get_svalue_for_ctor_val (val, mgr);

  auto &bv_mgr = mgr->get_node_manager ().get_bit_vector_manager ();
  auto &bv = bv_mgr.get_bit_vector_from_svalue (*sval);

  concrete_bit_size bv_size;
  if (!bv.maybe_get_concrete_size_in_bits (&bv_size))
    return false;

  if (overall_conc_range.get_size ().floor_mod (bv_size) != 0_bits)
    return false;

  auto num_repeats = overall_conc_range.get_size ().floor_div (bv_size);

  auto &repeated_bv = bv_mgr.get_repeated_bits (bv, num_repeats);

  out.m_concrete_range_map.insert ({overall_conc_range, repeated_bv});

  return true;
}

/* Bind the value VAL into INDEX within PARENT_REF.
   For use in handling a pair of entries within a CONSTRUCTOR.
   Return true if successful, false if there was a problem (e.g. due
   to hitting a complexity limit).  */

static bool
apply_ctor_pair_to_child_region (symbolic::concrete_bit_overlay::key &out,
				 const region *parent_reg,
				 region_model_manager *mgr,
				 tree index, tree val)
{
  const region *child_reg
    = get_subregion_within_ctor_for_ctor_pair (parent_reg, index, val, mgr);
  if (!child_reg)
    return false;
  if (TREE_CODE (val) == CONSTRUCTOR)
    return apply_ctor_to_region (out, child_reg, val, mgr);
  else
    {
      const svalue *sval = get_svalue_for_ctor_val (val, mgr);
      if (child_reg->empty_p (*mgr))
	return false;
      auto &bv_mgr = mgr->get_node_manager ().get_bit_vector_manager ();
      auto &bv = bv_mgr.get_bit_vector_from_svalue (*sval);
      /* Use the size of bv rather than that of the region to cope
	 with flexible arrays.  */
      auto mem_offset = child_reg->get_memory_offset (*mgr);
      symbolic_bit_range mem_range (mem_offset.get_bit_offset (),
				    bv.get_size ());
      concrete_bit_range conc_bit_range;
      if (!concrete_from_symbolic (mem_range, &conc_bit_range))
	return false;
      out.m_concrete_range_map.insert ({conc_bit_range, bv});
      return true;
    }
}

/* Implementation of decl_region::get_bit_contents_for_constructor
   for when the cached value hasn't yet been calculated.  */

const symbolic::bit_contents &
decl_region::calc_bit_contents_for_constructor (tree ctor,
						region_model_manager *mgr) const
{
  auto &node_mgr = mgr->get_node_manager ();
  auto &bit_contents_mgr = node_mgr.get_bit_contents_manager ();
  symbolic::concrete_bit_overlay::key bit_overlay_key
    {&bit_contents_mgr.get_all_zeros (), {}};
  if (!apply_ctor_to_region (bit_overlay_key, this, ctor, mgr))
    return bit_contents_mgr.get_untracked ({});
  return bit_contents_mgr.get_concrete_bit_overlay
    (std::move (bit_overlay_key), nullptr);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
