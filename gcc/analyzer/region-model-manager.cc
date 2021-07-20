/* Consolidation of svalues and regions.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

/* class region_model_manager.  */

/* region_model_manager's ctor.  */

region_model_manager::region_model_manager ()
: m_next_region_id (0),
  m_root_region (alloc_region_id ()),
  m_stack_region (alloc_region_id (), &m_root_region),
  m_heap_region (alloc_region_id (), &m_root_region),
  m_unknown_NULL (NULL),
  m_max_complexity (0, 0),
  m_code_region (alloc_region_id (), &m_root_region),
  m_fndecls_map (), m_labels_map (),
  m_globals_region (alloc_region_id (), &m_root_region),
  m_globals_map (),
  m_store_mgr (this)
{
}

/* region_model_manager's dtor.  Delete all of the managed svalues
   and regions.  */

region_model_manager::~region_model_manager ()
{
  /* Delete consolidated svalues.  */
  for (constants_map_t::iterator iter = m_constants_map.begin ();
       iter != m_constants_map.end (); ++iter)
    delete (*iter).second;
  for (unknowns_map_t::iterator iter = m_unknowns_map.begin ();
       iter != m_unknowns_map.end (); ++iter)
    delete (*iter).second;
  delete m_unknown_NULL;
  for (setjmp_values_map_t::iterator iter = m_setjmp_values_map.begin ();
       iter != m_setjmp_values_map.end (); ++iter)
    delete (*iter).second;
  for (poisoned_values_map_t::iterator iter = m_poisoned_values_map.begin ();
       iter != m_poisoned_values_map.end (); ++iter)
    delete (*iter).second;
  for (initial_values_map_t::iterator iter = m_initial_values_map.begin ();
       iter != m_initial_values_map.end (); ++iter)
    delete (*iter).second;
  for (pointer_values_map_t::iterator iter = m_pointer_values_map.begin ();
       iter != m_pointer_values_map.end (); ++iter)
    delete (*iter).second;
  for (unaryop_values_map_t::iterator iter = m_unaryop_values_map.begin ();
       iter != m_unaryop_values_map.end (); ++iter)
    delete (*iter).second;
  for (binop_values_map_t::iterator iter = m_binop_values_map.begin ();
       iter != m_binop_values_map.end (); ++iter)
    delete (*iter).second;
  for (sub_values_map_t::iterator iter = m_sub_values_map.begin ();
       iter != m_sub_values_map.end (); ++iter)
    delete (*iter).second;
  for (unmergeable_values_map_t::iterator iter
	 = m_unmergeable_values_map.begin ();
       iter != m_unmergeable_values_map.end (); ++iter)
    delete (*iter).second;
  for (widening_values_map_t::iterator iter = m_widening_values_map.begin ();
       iter != m_widening_values_map.end (); ++iter)
    delete (*iter).second;
  for (compound_values_map_t::iterator iter = m_compound_values_map.begin ();
       iter != m_compound_values_map.end (); ++iter)
    delete (*iter).second;
  for (conjured_values_map_t::iterator iter = m_conjured_values_map.begin ();
       iter != m_conjured_values_map.end (); ++iter)
    delete (*iter).second;

  /* Delete consolidated regions.  */
  for (fndecls_map_t::iterator iter = m_fndecls_map.begin ();
       iter != m_fndecls_map.end (); ++iter)
    delete (*iter).second;
  for (labels_map_t::iterator iter = m_labels_map.begin ();
       iter != m_labels_map.end (); ++iter)
    delete (*iter).second;
  for (globals_map_t::iterator iter = m_globals_map.begin ();
       iter != m_globals_map.end (); ++iter)
    delete (*iter).second;
  for (string_map_t::iterator iter = m_string_map.begin ();
       iter != m_string_map.end (); ++iter)
    delete (*iter).second;
}

/* Return true if C exceeds the complexity limit for svalues.  */

bool
region_model_manager::too_complex_p (const complexity &c) const
{
  if (c.m_max_depth > (unsigned)param_analyzer_max_svalue_depth)
    return true;
  return false;
}

/* If SVAL exceeds the complexity limit for svalues, delete it
   and return true.
   Otherwise update m_max_complexity and return false.  */

bool
region_model_manager::reject_if_too_complex (svalue *sval)
{
  const complexity &c = sval->get_complexity ();
  if (!too_complex_p (c))
    {
      if (m_max_complexity.m_num_nodes < c.m_num_nodes)
	m_max_complexity.m_num_nodes = c.m_num_nodes;
      if (m_max_complexity.m_max_depth < c.m_max_depth)
	m_max_complexity.m_max_depth = c.m_max_depth;
      return false;
    }

  delete sval;
  return true;
}

/* Macro for imposing a complexity limit on svalues, for use within
   region_model_manager member functions.

   If SVAL exceeds the complexity limit, delete it and return an UNKNOWN
   value of the same type.
   Otherwise update m_max_complexity and carry on.  */

#define RETURN_UNKNOWN_IF_TOO_COMPLEX(SVAL)			\
  do {								\
    svalue *sval_ = (SVAL);					\
    tree type_ = sval_->get_type ();				\
    if (reject_if_too_complex (sval_))				\
      return get_or_create_unknown_svalue (type_);		\
  } while (0)

/* svalue consolidation.  */

/* Return the svalue * for a constant_svalue for CST_EXPR,
   creating it if necessary.
   The constant_svalue instances are reused, based on pointer equality
   of trees  */

const svalue *
region_model_manager::get_or_create_constant_svalue (tree cst_expr)
{
  gcc_assert (cst_expr);

  constant_svalue **slot = m_constants_map.get (cst_expr);
  if (slot)
    return *slot;
  constant_svalue *cst_sval = new constant_svalue (cst_expr);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (cst_sval);
  m_constants_map.put (cst_expr, cst_sval);
  return cst_sval;
}

/* Return the svalue * for a constant_svalue for the INTEGER_CST
   for VAL of type TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_int_cst (tree type, poly_int64 val)
{
  gcc_assert (type);
  tree tree_cst = build_int_cst (type, val);
  return get_or_create_constant_svalue (tree_cst);
}

/* Return the svalue * for a unknown_svalue for TYPE (which can be NULL),
   creating it if necessary.
   The unknown_svalue instances are reused, based on pointer equality
   of the types  */

const svalue *
region_model_manager::get_or_create_unknown_svalue (tree type)
{
  /* Special-case NULL, so that the hash_map can use NULL as the
     "empty" value.  */
  if (type == NULL_TREE)
    {
      if (!m_unknown_NULL)
	m_unknown_NULL = new unknown_svalue (type);
      return m_unknown_NULL;
    }

  unknown_svalue **slot = m_unknowns_map.get (type);
  if (slot)
    return *slot;
  unknown_svalue *sval = new unknown_svalue (type);
  m_unknowns_map.put (type, sval);
  return sval;
}

/* Return the svalue * for the initial value of REG, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_initial_value (const region *reg)
{
  /* The initial value of a cast is a cast of the initial value.  */
  if (const cast_region *cast_reg = reg->dyn_cast_cast_region ())
    {
      const region *original_reg = cast_reg->get_original_region ();
      return get_or_create_cast (cast_reg->get_type (),
				 get_or_create_initial_value (original_reg));
    }

  /* INIT_VAL (*UNKNOWN_PTR) -> UNKNOWN_VAL.  */
  if (reg->symbolic_for_unknown_ptr_p ())
    return get_or_create_unknown_svalue (reg->get_type ());

  if (initial_svalue **slot = m_initial_values_map.get (reg))
    return *slot;
  initial_svalue *initial_sval = new initial_svalue (reg->get_type (), reg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (initial_sval);
  m_initial_values_map.put (reg, initial_sval);
  return initial_sval;
}

/* Return the svalue * for R using type TYPE, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_setjmp_svalue (const setjmp_record &r,
						   tree type)
{
  setjmp_svalue::key_t key (r, type);
  if (setjmp_svalue **slot = m_setjmp_values_map.get (key))
    return *slot;
  setjmp_svalue *setjmp_sval = new setjmp_svalue (r, type);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (setjmp_sval);
  m_setjmp_values_map.put (key, setjmp_sval);
  return setjmp_sval;
}

/* Return the svalue * for a poisoned value of KIND and TYPE, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_poisoned_svalue (enum poison_kind kind,
						     tree type)
{
  poisoned_svalue::key_t key (kind, type);
  if (poisoned_svalue **slot = m_poisoned_values_map.get (key))
    return *slot;
  poisoned_svalue *poisoned_sval = new poisoned_svalue (kind, type);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (poisoned_sval);
  m_poisoned_values_map.put (key, poisoned_sval);
  return poisoned_sval;
}

/* Return the svalue * for a pointer to POINTEE of type PTR_TYPE,
   creating it if necessary.  */

const svalue *
region_model_manager::get_ptr_svalue (tree ptr_type, const region *pointee)
{
  /* If this is a symbolic region from dereferencing a pointer, and the types
     match, then return the original pointer.  */
  if (const symbolic_region *sym_reg = pointee->dyn_cast_symbolic_region ())
    if (ptr_type == sym_reg->get_pointer ()->get_type ())
      return sym_reg->get_pointer ();

  region_svalue::key_t key (ptr_type, pointee);
  if (region_svalue **slot = m_pointer_values_map.get (key))
    return *slot;
  region_svalue *sval = new region_svalue (ptr_type, pointee);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (sval);
  m_pointer_values_map.put (key, sval);
  return sval;
}

/* Subroutine of region_model_manager::get_or_create_unaryop.
   Attempt to fold the inputs and return a simpler svalue *.
   Otherwise, return NULL.  */

const svalue *
region_model_manager::maybe_fold_unaryop (tree type, enum tree_code op,
					  const svalue *arg)
{
  /* Ops on "unknown" are also unknown.  */
  if (arg->get_kind () == SK_UNKNOWN)
    return get_or_create_unknown_svalue (type);

  switch (op)
    {
    default: break;
    case VIEW_CONVERT_EXPR:
    case NOP_EXPR:
      {
	/* Handle redundant casts.  */
	if (arg->get_type ()
	    && useless_type_conversion_p (arg->get_type (), type))
	  return arg;

	/* Fold "cast<TYPE> (cast <INNER_TYPE> (innermost_arg))
	     => "cast<TYPE> (innermost_arg)",
	   unless INNER_TYPE is narrower than TYPE.  */
	if (const svalue *innermost_arg = arg->maybe_undo_cast ())
	  {
	    tree inner_type = arg->get_type ();
	    if (TYPE_SIZE (type)
		&& TYPE_SIZE (inner_type)
		&& (fold_binary (LE_EXPR, boolean_type_node,
				 TYPE_SIZE (type), TYPE_SIZE (inner_type))
		    == boolean_true_node))
	      return maybe_fold_unaryop (type, op, innermost_arg);
	  }
      }
      break;
    case TRUTH_NOT_EXPR:
      {
	/* Invert comparisons e.g. "!(x == y)" => "x != y".  */
	if (const binop_svalue *binop = arg->dyn_cast_binop_svalue ())
	  if (TREE_CODE_CLASS (binop->get_op ()) == tcc_comparison)
	    {
	      enum tree_code inv_op
		= invert_tree_comparison (binop->get_op (),
					  HONOR_NANS (binop->get_type ()));
	      if (inv_op != ERROR_MARK)
		return get_or_create_binop (binop->get_type (), inv_op,
					    binop->get_arg0 (),
					    binop->get_arg1 ());
	    }
      }
      break;
    }

  /* Constants.  */
  if (tree cst = arg->maybe_get_constant ())
    if (tree result = fold_unary (op, type, cst))
      return get_or_create_constant_svalue (result);

  return NULL;
}

/* Return the svalue * for an unary operation OP on ARG with a result of
   type TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_unaryop (tree type, enum tree_code op,
					     const svalue *arg)
{
  if (const svalue *folded = maybe_fold_unaryop  (type, op, arg))
    return folded;
  unaryop_svalue::key_t key (type, op, arg);
  if (unaryop_svalue **slot = m_unaryop_values_map.get (key))
    return *slot;
  unaryop_svalue *unaryop_sval = new unaryop_svalue (type, op, arg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (unaryop_sval);
  m_unaryop_values_map.put (key, unaryop_sval);
  return unaryop_sval;
}

/* Get a tree code for a cast to DST_TYPE from SRC_TYPE.
   Use NOP_EXPR if possible (e.g. to help fold_unary convert casts
   of 0 to (T*) to simple pointer constants), but use FIX_TRUNC_EXPR
   and VIEW_CONVERT_EXPR for cases that fold_unary would otherwise crash
   on.  */

static enum tree_code
get_code_for_cast (tree dst_type, tree src_type)
{
  gcc_assert (dst_type);
  if (!src_type)
    return NOP_EXPR;

  if (TREE_CODE (src_type) == REAL_TYPE)
    {
      if (TREE_CODE (dst_type) == INTEGER_TYPE)
	return FIX_TRUNC_EXPR;
      else
	return VIEW_CONVERT_EXPR;
    }

  return NOP_EXPR;
}

/* Return the svalue * for a cast of ARG to type TYPE, creating it
   if necessary.  */

const svalue *
region_model_manager::get_or_create_cast (tree type, const svalue *arg)
{
  gcc_assert (type);
  enum tree_code op = get_code_for_cast (type, arg->get_type ());
  return get_or_create_unaryop (type, op, arg);
}

/* Subroutine of region_model_manager::maybe_fold_binop for handling
   (TYPE)(COMPOUND_SVAL BIT_AND_EXPR CST) that may have been generated by
   optimize_bit_field_compare, where CST is from ARG1.

   Support masking out bits from a compound_svalue for comparing a bitfield
   against a value, as generated by optimize_bit_field_compare for
   BITFIELD == VALUE.

   If COMPOUND_SVAL has a value for the appropriate bits, return it,
   shifted accordingly.
   Otherwise return NULL.  */

const svalue *
region_model_manager::
maybe_undo_optimize_bit_field_compare (tree type,
				       const compound_svalue *compound_sval,
				       tree cst,
				       const svalue *arg1)
{
  if (type != unsigned_char_type_node)
    return NULL;

  const binding_map &map = compound_sval->get_map ();
  unsigned HOST_WIDE_INT mask = TREE_INT_CST_LOW (cst);
  /* If "mask" is a contiguous range of set bits, see if the
     compound_sval has a value for those bits.  */
  bit_range bits (0, 0);
  if (!bit_range::from_mask (mask, &bits))
    return NULL;

  bit_range bound_bits (bits);
  if (BYTES_BIG_ENDIAN)
    bound_bits = bit_range (BITS_PER_UNIT - bits.get_next_bit_offset (),
			    bits.m_size_in_bits);
  const concrete_binding *conc
    = get_store_manager ()->get_concrete_binding (bound_bits, BK_direct);
  const svalue *sval = map.get (conc);
  if (!sval)
    return NULL;

  /* We have a value;
     shift it by the correct number of bits.  */
  const svalue *lhs = get_or_create_cast (type, sval);
  HOST_WIDE_INT bit_offset = bits.get_start_bit_offset ().to_shwi ();
  const svalue *shift_sval = get_or_create_int_cst (type, bit_offset);
  const svalue *shifted_sval = get_or_create_binop (type, LSHIFT_EXPR,
						    lhs, shift_sval);
  /* Reapply the mask (needed for negative
     signed bitfields).  */
  return get_or_create_binop (type, BIT_AND_EXPR,
			      shifted_sval, arg1);
}

/* Subroutine of region_model_manager::get_or_create_binop.
   Attempt to fold the inputs and return a simpler svalue *.
   Otherwise, return NULL.  */

const svalue *
region_model_manager::maybe_fold_binop (tree type, enum tree_code op,
					const svalue *arg0,
					const svalue *arg1)
{
  tree cst0 = arg0->maybe_get_constant ();
  tree cst1 = arg1->maybe_get_constant ();
  /* (CST OP CST).  */
  if (cst0 && cst1)
    {
      if (tree result = fold_binary (op, type, cst0, cst1))
	if (CONSTANT_CLASS_P (result))
	  return get_or_create_constant_svalue (result);
    }

  if (FLOAT_TYPE_P (type)
      || (arg0->get_type () && FLOAT_TYPE_P (arg0->get_type ()))
      || (arg1->get_type () && FLOAT_TYPE_P (arg1->get_type ())))
    return NULL;

  switch (op)
    {
    default:
      break;
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      /* (VAL + 0) -> VAL.  */
      if (cst1 && zerop (cst1) && type == arg0->get_type ())
	return arg0;
      break;
    case MINUS_EXPR:
      /* (VAL - 0) -> VAL.  */
      if (cst1 && zerop (cst1) && type == arg0->get_type ())
	return arg0;
      break;
    case MULT_EXPR:
      /* (VAL * 0).  */
      if (cst1 && zerop (cst1) && INTEGRAL_TYPE_P (type))
	return get_or_create_constant_svalue (build_int_cst (type, 0));
      /* (VAL * 1) -> VAL.  */
      if (cst1 && integer_onep (cst1))
	return arg0;
      break;
    case BIT_AND_EXPR:
      if (cst1)
	{
	  if (zerop (cst1) && INTEGRAL_TYPE_P (type))
	    /* "(ARG0 & 0)" -> "0".  */
	    return get_or_create_constant_svalue (build_int_cst (type, 0));

	  if (const compound_svalue *compound_sval
		= arg0->dyn_cast_compound_svalue ())
	    if (const svalue *sval
		= maybe_undo_optimize_bit_field_compare (type,
							 compound_sval,
							 cst1, arg1))
	      return sval;
	}
      break;
    case TRUTH_ANDIF_EXPR:
    case TRUTH_AND_EXPR:
      if (cst1)
	{
	  if (zerop (cst1) && INTEGRAL_TYPE_P (type))
	    /* "(ARG0 && 0)" -> "0".  */
	    return get_or_create_constant_svalue (build_int_cst (type, 0));
	  else
	    /* "(ARG0 && nonzero-cst)" -> "ARG0".  */
	    return get_or_create_cast (type, arg0);
	}
      break;
    case TRUTH_ORIF_EXPR:
    case TRUTH_OR_EXPR:
      if (cst1)
	{
	  if (zerop (cst1))
	    /* "(ARG0 || 0)" -> "ARG0".  */
	    return get_or_create_cast (type, arg0);
	  else
	    /* "(ARG0 && nonzero-cst)" -> "nonzero-cst".  */
	    return get_or_create_cast (type, arg1);
	}
      break;
    }

  /* For associative ops, fold "(X op CST_A) op CST_B)" to
     "X op (CST_A op CST_B)".  */
  if (cst1 && associative_tree_code (op))
    if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
      if (binop->get_op () == op
	  && binop->get_arg1 ()->maybe_get_constant ()
	  && type == binop->get_type ()
	  && type == binop->get_arg0 ()->get_type ()
	  && type == binop->get_arg1 ()->get_type ())
	return get_or_create_binop
	  (type, op, binop->get_arg0 (),
	   get_or_create_binop (type, op,
				binop->get_arg1 (), arg1));

  /* associative_tree_code is false for POINTER_PLUS_EXPR, but we
     can fold:
       "(PTR ptr+ CST_A) ptr+ CST_B)" to "PTR ptr+ (CST_A ptr+ CST_B)"
     e.g. in data-model-1.c: test_4c.  */
  if (cst1 && op == POINTER_PLUS_EXPR)
    if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
      if (binop->get_op () == POINTER_PLUS_EXPR)
	if (binop->get_arg1 ()->maybe_get_constant ())
	  return get_or_create_binop
	    (type, op, binop->get_arg0 (),
	     get_or_create_binop (size_type_node, op,
				  binop->get_arg1 (), arg1));

  /* Ops on "unknown" are also unknown (unless we can use one of the
     identities above).  */
  if (arg0->get_kind () == SK_UNKNOWN
      || arg1->get_kind () == SK_UNKNOWN)
    return get_or_create_unknown_svalue (type);

  /* etc.  */

  return NULL;
}

/* Return the svalue * for an binary operation OP on ARG0 and ARG1
   with a result of type TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_binop (tree type, enum tree_code op,
					   const svalue *arg0,
					   const svalue *arg1)
{
  /* For commutative ops, put any constant on the RHS.  */
  if (arg0->maybe_get_constant () && commutative_tree_code (op))
    std::swap (arg0, arg1);

  if (const svalue *folded = maybe_fold_binop (type, op, arg0, arg1))
    return folded;

  binop_svalue::key_t key (type, op, arg0, arg1);
  if (binop_svalue **slot = m_binop_values_map.get (key))
    return *slot;
  binop_svalue *binop_sval = new binop_svalue (type, op, arg0, arg1);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (binop_sval);
  m_binop_values_map.put (key, binop_sval);
  return binop_sval;
}

/* Subroutine of region_model_manager::get_or_create_sub_svalue.
   Return a folded svalue, or NULL.  */

const svalue *
region_model_manager::maybe_fold_sub_svalue (tree type,
					     const svalue *parent_svalue,
					     const region *subregion)
{
  /* Subvalues of "unknown" are unknown.  */
  if (parent_svalue->get_kind () == SK_UNKNOWN)
    return get_or_create_unknown_svalue (type);

  /* If we have a subregion of a zero-fill, it's zero.  */
  if (const unaryop_svalue *unary
      = parent_svalue->dyn_cast_unaryop_svalue ())
    {
      if (unary->get_op () == NOP_EXPR
	  || unary->get_op () == VIEW_CONVERT_EXPR)
	if (tree cst = unary->get_arg ()->maybe_get_constant ())
	  if (zerop (cst))
	    {
	      const svalue *cst_sval
		= get_or_create_constant_svalue (cst);
	      return get_or_create_cast (type, cst_sval);
	    }
    }

  /* Handle getting individual chars from a STRING_CST.  */
  if (tree cst = parent_svalue->maybe_get_constant ())
    if (TREE_CODE (cst) == STRING_CST)
      if (const element_region *element_reg
	    = subregion->dyn_cast_element_region ())
	{
	  const svalue *idx_sval = element_reg->get_index ();
	  if (tree cst_idx = idx_sval->maybe_get_constant ())
	    if (const svalue *char_sval
		= maybe_get_char_from_string_cst (cst, cst_idx))
	      return get_or_create_cast (type, char_sval);
	}

  /* SUB(INIT(r)).FIELD -> INIT(r.FIELD)
     i.e.
     Subvalue(InitialValue(R1), FieldRegion(R2, F))
       -> InitialValue(FieldRegion(R1, F)).  */
  if (const initial_svalue *init_sval
        = parent_svalue->dyn_cast_initial_svalue ())
    {
      if (const field_region *field_reg = subregion->dyn_cast_field_region ())
	{
	  const region *field_reg_new
	    = get_field_region (init_sval->get_region (),
				field_reg->get_field ());
	  return get_or_create_initial_value (field_reg_new);
	}
    }

  return NULL;
}

/* Return the svalue * for extracting a subvalue of type TYPE from
   PARENT_SVALUE based on SUBREGION, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_sub_svalue (tree type,
						const svalue *parent_svalue,
						const region *subregion)
{
  if (const svalue *folded
	= maybe_fold_sub_svalue (type, parent_svalue, subregion))
    return folded;

  sub_svalue::key_t key (type, parent_svalue, subregion);
  if (sub_svalue **slot = m_sub_values_map.get (key))
    return *slot;
  sub_svalue *sub_sval
    = new sub_svalue (type, parent_svalue, subregion);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (sub_sval);
  m_sub_values_map.put (key, sub_sval);
  return sub_sval;
}

/* Return the svalue * that decorates ARG as being unmergeable,
   creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_unmergeable (const svalue *arg)
{
  if (arg->get_kind () == SK_UNMERGEABLE)
    return arg;

  if (unmergeable_svalue **slot = m_unmergeable_values_map.get (arg))
    return *slot;
  unmergeable_svalue *unmergeable_sval = new unmergeable_svalue (arg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (unmergeable_sval);
  m_unmergeable_values_map.put (arg, unmergeable_sval);
  return unmergeable_sval;
}

/* Return the svalue * of type TYPE for the merger of value BASE_SVAL
   and ITER_SVAL at POINT, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_widening_svalue (tree type,
						     const program_point &point,
						     const svalue *base_sval,
						     const svalue *iter_sval)
{
  gcc_assert (base_sval->get_kind () != SK_WIDENING);
  gcc_assert (iter_sval->get_kind () != SK_WIDENING);
  widening_svalue::key_t key (type, point, base_sval, iter_sval);
  if (widening_svalue **slot = m_widening_values_map.get (key))
    return *slot;
  widening_svalue *widening_sval
    = new widening_svalue (type, point, base_sval, iter_sval);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (widening_sval);
  m_widening_values_map.put (key, widening_sval);
  return widening_sval;
}

/* Return the svalue * of type TYPE for the compound values in MAP,
   creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_compound_svalue (tree type,
						     const binding_map &map)
{
  compound_svalue::key_t tmp_key (type, &map);
  if (compound_svalue **slot = m_compound_values_map.get (tmp_key))
    return *slot;
  compound_svalue *compound_sval
    = new compound_svalue (type, map);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (compound_sval);
  /* Use make_key rather than reusing the key, so that we use a
     ptr to compound_sval's binding_map, rather than the MAP param.  */
  m_compound_values_map.put (compound_sval->make_key (), compound_sval);
  return compound_sval;
}

/* Return the svalue * of type TYPE for the value conjured for ID_REG
   at STMT, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_conjured_svalue (tree type,
						     const gimple *stmt,
						     const region *id_reg)
{
  conjured_svalue::key_t key (type, stmt, id_reg);
  if (conjured_svalue **slot = m_conjured_values_map.get (key))
    return *slot;
  conjured_svalue *conjured_sval
    = new conjured_svalue (type, stmt, id_reg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (conjured_sval);
  m_conjured_values_map.put (key, conjured_sval);
  return conjured_sval;
}

/* Given STRING_CST, a STRING_CST and BYTE_OFFSET_CST a constant,
   attempt to get the character at that offset, returning either
   the svalue for the character constant, or NULL if unsuccessful.  */

const svalue *
region_model_manager::maybe_get_char_from_string_cst (tree string_cst,
						      tree byte_offset_cst)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);

  /* Adapted from fold_read_from_constant_string.  */
  scalar_int_mode char_mode;
  if (TREE_CODE (byte_offset_cst) == INTEGER_CST
      && compare_tree_int (byte_offset_cst,
			   TREE_STRING_LENGTH (string_cst)) < 0
      && is_int_mode (TYPE_MODE (TREE_TYPE (TREE_TYPE (string_cst))),
		      &char_mode)
      && GET_MODE_SIZE (char_mode) == 1)
    {
      tree char_cst
	= build_int_cst_type (TREE_TYPE (TREE_TYPE (string_cst)),
			      (TREE_STRING_POINTER (string_cst)
			       [TREE_INT_CST_LOW (byte_offset_cst)]));
      return get_or_create_constant_svalue (char_cst);
    }
  return NULL;
}

/* region consolidation.  */

/* Return the region for FNDECL, creating it if necessary.  */

const function_region *
region_model_manager::get_region_for_fndecl (tree fndecl)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);

  function_region **slot = m_fndecls_map.get (fndecl);
  if (slot)
    return *slot;
  function_region *reg
    = new function_region (alloc_region_id (), &m_code_region, fndecl);
  m_fndecls_map.put (fndecl, reg);
  return reg;
}

/* Return the region for LABEL, creating it if necessary.  */

const label_region *
region_model_manager::get_region_for_label (tree label)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);

  label_region **slot = m_labels_map.get (label);
  if (slot)
    return *slot;

  tree fndecl = DECL_CONTEXT (label);
  gcc_assert (fndecl && TREE_CODE (fndecl) == FUNCTION_DECL);

  const function_region *func_reg = get_region_for_fndecl (fndecl);
  label_region *reg
    = new label_region (alloc_region_id (), func_reg, label);
  m_labels_map.put (label, reg);
  return reg;
}

/* Return the region for EXPR, creating it if necessary.  */

const decl_region *
region_model_manager::get_region_for_global (tree expr)
{
  gcc_assert (TREE_CODE (expr) == VAR_DECL);

  decl_region **slot = m_globals_map.get (expr);
  if (slot)
    return *slot;
  decl_region *reg
    = new decl_region (alloc_region_id (), &m_globals_region, expr);
  m_globals_map.put (expr, reg);
  return reg;
}

/* Return the region that describes accessing field FIELD of PARENT,
   creating it if necessary.  */

const region *
region_model_manager::get_field_region (const region *parent, tree field)
{
  gcc_assert (TREE_CODE (field) == FIELD_DECL);

  /* (*UNKNOWN_PTR).field is (*UNKNOWN_PTR_OF_&FIELD_TYPE).  */
  if (parent->symbolic_for_unknown_ptr_p ())
    {
      tree ptr_to_field_type = build_pointer_type (TREE_TYPE (field));
      const svalue *unknown_ptr_to_field
	= get_or_create_unknown_svalue (ptr_to_field_type);
      return get_symbolic_region (unknown_ptr_to_field);
    }

  field_region::key_t key (parent, field);
  if (field_region *reg = m_field_regions.get (key))
    return reg;

  field_region *field_reg
    = new field_region (alloc_region_id (), parent, field);
  m_field_regions.put (key, field_reg);
  return field_reg;
}

/* Return the region that describes accessing the element of type
   ELEMENT_TYPE at index INDEX of PARENT, creating it if necessary.  */

const region *
region_model_manager::get_element_region (const region *parent,
					  tree element_type,
					  const svalue *index)
{
  element_region::key_t key (parent, element_type, index);
  if (element_region *reg = m_element_regions.get (key))
    return reg;

  element_region *element_reg
    = new element_region (alloc_region_id (), parent, element_type, index);
  m_element_regions.put (key, element_reg);
  return element_reg;
}

/* Return the region that describes accessing the subregion of type
   ELEMENT_TYPE at offset BYTE_OFFSET within PARENT, creating it if
   necessary.  */

const region *
region_model_manager::get_offset_region (const region *parent,
					 tree type,
					 const svalue *byte_offset)
{
  /* If BYTE_OFFSET is zero, return PARENT.  */
  if (tree cst_offset = byte_offset->maybe_get_constant ())
    if (zerop (cst_offset))
      return get_cast_region (parent, type);

  /* Fold OFFSET_REGION(OFFSET_REGION(REG, X), Y)
     to   OFFSET_REGION(REG, (X + Y)).  */
  if (const offset_region *parent_offset_reg
	= parent->dyn_cast_offset_region ())
    {
      const svalue *sval_x = parent_offset_reg->get_byte_offset ();
      const svalue *sval_sum
	= get_or_create_binop (byte_offset->get_type (),
			       PLUS_EXPR, sval_x, byte_offset);
      return get_offset_region (parent->get_parent_region (), type, sval_sum);
    }

  offset_region::key_t key (parent, type, byte_offset);
  if (offset_region *reg = m_offset_regions.get (key))
    return reg;

  offset_region *offset_reg
    = new offset_region (alloc_region_id (), parent, type, byte_offset);
  m_offset_regions.put (key, offset_reg);
  return offset_reg;
}

/* Return the region that describes accessing PARENT_REGION as if
   it were of type TYPE, creating it if necessary.  */

const region *
region_model_manager::get_cast_region (const region *original_region,
				       tree type)
{
  /* If types match, return ORIGINAL_REGION.  */
  if (type == original_region->get_type ())
    return original_region;

  cast_region::key_t key (original_region, type);
  if (cast_region *reg = m_cast_regions.get (key))
    return reg;

  cast_region *cast_reg
    = new cast_region (alloc_region_id (), original_region, type);
  m_cast_regions.put (key, cast_reg);
  return cast_reg;
}

/* Return the frame_region for call to FUN from CALLING_FRAME, creating it
   if necessary.  CALLING_FRAME may be NULL.  */

const frame_region *
region_model_manager::get_frame_region (const frame_region *calling_frame,
					function *fun)
{
  int index = calling_frame ? calling_frame->get_index () + 1 : 0;

  frame_region::key_t key (calling_frame, fun);
  if (frame_region *reg = m_frame_regions.get (key))
    return reg;

  frame_region *frame_reg
    = new frame_region (alloc_region_id (), &m_stack_region, calling_frame,
			 fun, index);
  m_frame_regions.put (key, frame_reg);
  return frame_reg;
}

/* Return the region that describes dereferencing SVAL, creating it
   if necessary.  */

const region *
region_model_manager::get_symbolic_region (const svalue *sval)
{
  symbolic_region::key_t key (&m_root_region, sval);
  if (symbolic_region *reg = m_symbolic_regions.get (key))
    return reg;

  symbolic_region *symbolic_reg
    = new symbolic_region (alloc_region_id (), &m_root_region, sval);
  m_symbolic_regions.put (key, symbolic_reg);
  return symbolic_reg;
}

/* Return the region that describes accessing STRING_CST, creating it
   if necessary.  */

const string_region *
region_model_manager::get_region_for_string (tree string_cst)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);

  string_region **slot = m_string_map.get (string_cst);
  if (slot)
    return *slot;
  string_region *reg
    = new string_region (alloc_region_id (), &m_root_region, string_cst);
  m_string_map.put (string_cst, reg);
  return reg;
}

/* If we see a tree code we don't know how to handle, rather than
   ICE or generate bogus results, create a dummy region, and notify
   CTXT so that it can mark the new state as being not properly
   modelled.  The exploded graph can then stop exploring that path,
   since any diagnostics we might issue will have questionable
   validity.  */

const region *
region_model_manager::
get_region_for_unexpected_tree_code (region_model_context *ctxt,
				     tree t,
				     const dump_location_t &loc)
{
  tree type = TYPE_P (t) ? t : TREE_TYPE (t);
  region *new_reg
    = new unknown_region (alloc_region_id (), &m_root_region, type);
  if (ctxt)
    ctxt->on_unexpected_tree_code (t, loc);
  return new_reg;
}

/* Return a new region describing a heap-allocated block of memory.  */

const region *
region_model_manager::create_region_for_heap_alloc ()
{
  region *reg
    = new heap_allocated_region (alloc_region_id (), &m_heap_region);
  m_managed_dynamic_regions.safe_push (reg);
  return reg;
}

/* Return a new region describing a block of memory allocated within FRAME.  */

const region *
region_model_manager::create_region_for_alloca (const frame_region *frame)
{
  gcc_assert (frame);
  region *reg = new alloca_region (alloc_region_id (), frame);
  m_managed_dynamic_regions.safe_push (reg);
  return reg;
}

/* Log OBJ to LOGGER.  */

template <typename T>
static void
log_managed_object (logger *logger, const T *obj)
{
  logger->start_log_line ();
  pretty_printer *pp = logger->get_printer ();
  pp_string (pp, "    ");
  obj->dump_to_pp (pp, true);
  logger->end_log_line ();
}

/* Specialization for frame_region, which also logs the count of locals
   managed by the frame_region.  */

template <>
void
log_managed_object (logger *logger, const frame_region *obj)
{
  logger->start_log_line ();
  pretty_printer *pp = logger->get_printer ();
  pp_string (pp, "    ");
  obj->dump_to_pp (pp, true);
  pp_printf (pp, " [with %i region(s) for locals]", obj->get_num_locals ());
  logger->end_log_line ();
}

/* Dump the number of objects that were managed by UNIQ_MAP to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

template <typename K, typename T>
static void
log_uniq_map (logger *logger, bool show_objs, const char *title,
	      const hash_map<K, T*> &uniq_map)
{
  logger->log ("  # %s: %li", title, uniq_map.elements ());
  if (!show_objs)
    return;
  auto_vec<const T *> vec_objs (uniq_map.elements ());
  for (typename hash_map<K, T*>::iterator iter = uniq_map.begin ();
       iter != uniq_map.end (); ++iter)
    vec_objs.quick_push ((*iter).second);

  vec_objs.qsort (T::cmp_ptr_ptr);

  unsigned i;
  const T *obj;
  FOR_EACH_VEC_ELT (vec_objs, i, obj)
    log_managed_object<T> (logger, obj);
}

/* Dump the number of objects that were managed by MAP to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

template <typename T>
static void
log_uniq_map (logger *logger, bool show_objs, const char *title,
	      const consolidation_map<T> &map)
{
  logger->log ("  # %s: %li", title, map.elements ());
  if (!show_objs)
    return;

  auto_vec<const T *> vec_objs (map.elements ());
  for (typename consolidation_map<T>::iterator iter = map.begin ();
       iter != map.end (); ++iter)
    vec_objs.quick_push ((*iter).second);

  vec_objs.qsort (T::cmp_ptr_ptr);

  unsigned i;
  const T *obj;
  FOR_EACH_VEC_ELT (vec_objs, i, obj)
    log_managed_object<T> (logger, obj);
}

/* Dump the number of objects of each class that were managed by this
   manager to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.  */

void
region_model_manager::log_stats (logger *logger, bool show_objs) const
{
  LOG_SCOPE (logger);
  logger->log ("svalue consolidation");
  log_uniq_map (logger, show_objs, "constant_svalue", m_constants_map);
  log_uniq_map (logger, show_objs, "unknown_svalue", m_unknowns_map);
  if (m_unknown_NULL)
    log_managed_object (logger, m_unknown_NULL);
  log_uniq_map (logger, show_objs, "poisoned_svalue", m_poisoned_values_map);
  log_uniq_map (logger, show_objs, "setjmp_svalue", m_setjmp_values_map);
  log_uniq_map (logger, show_objs, "initial_svalue", m_initial_values_map);
  log_uniq_map (logger, show_objs, "region_svalue", m_pointer_values_map);
  log_uniq_map (logger, show_objs, "unaryop_svalue", m_unaryop_values_map);
  log_uniq_map (logger, show_objs, "binop_svalue", m_binop_values_map);
  log_uniq_map (logger, show_objs, "sub_svalue", m_sub_values_map);
  log_uniq_map (logger, show_objs, "unmergeable_svalue",
		m_unmergeable_values_map);
  log_uniq_map (logger, show_objs, "widening_svalue", m_widening_values_map);
  log_uniq_map (logger, show_objs, "compound_svalue", m_compound_values_map);
  log_uniq_map (logger, show_objs, "conjured_svalue", m_conjured_values_map);
  logger->log ("max accepted svalue num_nodes: %i",
	       m_max_complexity.m_num_nodes);
  logger->log ("max accepted svalue max_depth: %i",
	       m_max_complexity.m_max_depth);

  logger->log ("region consolidation");
  logger->log ("  next region id: %i", m_next_region_id);
  log_uniq_map (logger, show_objs, "function_region", m_fndecls_map);
  log_uniq_map (logger, show_objs, "label_region", m_labels_map);
  log_uniq_map (logger, show_objs, "decl_region for globals", m_globals_map);
  log_uniq_map (logger, show_objs, "field_region", m_field_regions);
  log_uniq_map (logger, show_objs, "element_region", m_element_regions);
  log_uniq_map (logger, show_objs, "offset_region", m_offset_regions);
  log_uniq_map (logger, show_objs, "cast_region", m_cast_regions);
  log_uniq_map (logger, show_objs, "frame_region", m_frame_regions);
  log_uniq_map (logger, show_objs, "symbolic_region", m_symbolic_regions);
  log_uniq_map (logger, show_objs, "string_region", m_string_map);
  logger->log ("  # managed dynamic regions: %i",
	       m_managed_dynamic_regions.length ());
  m_store_mgr.log_stats (logger, show_objs);
}

/* Dump the number of objects of each class that were managed by this
   manager to LOGGER.
   If SHOW_OBJS is true, also dump the objects themselves.
   This is here so it can use log_uniq_map.  */

void
store_manager::log_stats (logger *logger, bool show_objs) const
{
  LOG_SCOPE (logger);
  log_uniq_map (logger, show_objs, "concrete_binding",
		m_concrete_binding_key_mgr);
  log_uniq_map (logger, show_objs, "symbolic_binding",
		m_symbolic_binding_key_mgr);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
