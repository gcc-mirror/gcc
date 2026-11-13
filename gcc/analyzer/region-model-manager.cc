/* Consolidation of svalues and regions.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#include "fold-const.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "target.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/symbolic-scalars.h"
#include "analyzer/record-layout.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

/* class region_model_manager.  */

/* region_model_manager's ctor.  */

region_model_manager::region_model_manager (symbolic::node_manager &node_mgr,
					    logger *logger)
: m_node_mgr (node_mgr),
  m_logger (logger),
  m_empty_call_string (),
  m_root_region (alloc_node_id ()),
  m_stack_region (alloc_node_id (), &m_root_region),
  m_heap_region (alloc_node_id (), &m_root_region),
  m_unknown_NULL (nullptr),
  m_checking_feasibility (false),
  m_max_complexity (0, 0),
  m_code_region (alloc_node_id (), &m_root_region),
  m_fndecls_map (), m_labels_map (),
  m_globals_region (alloc_node_id (), &m_root_region),
  m_globals_map (),
  m_thread_local_region (alloc_node_id (), &m_root_region),
  m_errno_region (alloc_node_id (), &m_thread_local_region),
  m_range_mgr (new bounded_ranges_manager ()),
  m_known_fn_mgr (logger)
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
  for (poisoned_values_map_t::iterator iter = m_poisoned_values_map.begin ();
       iter != m_poisoned_values_map.end (); ++iter)
    delete (*iter).second;
  for (setjmp_values_map_t::iterator iter = m_setjmp_values_map.begin ();
       iter != m_setjmp_values_map.end (); ++iter)
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
  for (auto iter : m_bit_vector_to_svalue_map)
    delete iter.second;
  for (unmergeable_values_map_t::iterator iter
	 = m_unmergeable_values_map.begin ();
       iter != m_unmergeable_values_map.end (); ++iter)
    delete (*iter).second;
  for (widening_values_map_t::iterator iter = m_widening_values_map.begin ();
       iter != m_widening_values_map.end (); ++iter)
    delete (*iter).second;
  for (auto iter : m_asm_output_values_map)
    delete iter.second;
  for (auto iter : m_const_fn_result_values_map)
    delete iter.second;

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

  delete m_range_mgr;
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
  if (m_checking_feasibility)
    return false;

  const complexity &c = sval->get_complexity ();
  if (!too_complex_p (c))
    {
      if (m_max_complexity.m_num_nodes < c.m_num_nodes)
	m_max_complexity.m_num_nodes = c.m_num_nodes;
      if (m_max_complexity.m_max_depth < c.m_max_depth)
	m_max_complexity.m_max_depth = c.m_max_depth;
      return false;
    }

  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  sval->dump_to_pp (&pp, true);
  if (warning_at (input_location, OPT_Wanalyzer_symbol_too_complex,
		  "symbol too complicated: %qs",
		  pp_formatted_text (&pp)))
    inform (input_location,
	    "max_depth %i exceeds --param=analyzer-max-svalue-depth=%i",
	    c.m_max_depth, param_analyzer_max_svalue_depth);

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
region_model_manager::get_or_create_constant_svalue (tree type, tree cst_expr)
{
  gcc_assert (cst_expr);
  gcc_assert (CONSTANT_CLASS_P (cst_expr));
  gcc_assert (type == TREE_TYPE (cst_expr) || type == NULL_TREE);

  const constant_svalue::key_t key (type, cst_expr);
  auto iter = m_constants_map.find (key);
  if (iter != m_constants_map.end ())
    return iter->second;
  constant_svalue *cst_sval
    = new constant_svalue (alloc_node_id (), type, cst_expr);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (cst_sval);
  m_constants_map.insert ({key, cst_sval});
  return cst_sval;
}

const svalue *
region_model_manager::get_or_create_constant_svalue (tree cst_expr)
{
  tree type = TREE_TYPE (cst_expr);
  if (TREE_CODE (cst_expr) == RAW_DATA_CST)
    /* The type of a RAW_DATA_CST is the type of each element, rather than
       that of the constant as a whole, so use NULL_TREE for simplicity.  */
    type = NULL_TREE;
  return get_or_create_constant_svalue (type, cst_expr);
}

/* Return the svalue * for a constant_svalue for the INTEGER_CST
   for VAL of type TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_int_cst (tree type,
					     const poly_wide_int_ref &cst)
{
  tree effective_type = type;
  if (!type)
    effective_type = ptrdiff_type_node;
  gcc_assert (INTEGRAL_TYPE_P (effective_type)
	      || POINTER_TYPE_P (effective_type));
  tree tree_cst = wide_int_to_tree (effective_type, cst);
  return get_or_create_constant_svalue (type, tree_cst);
}

/* Return the svalue * for the constant_svalue for the NULL pointer
   of POINTER_TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_null_ptr (tree pointer_type)
{
  gcc_assert (pointer_type);
  gcc_assert (POINTER_TYPE_P (pointer_type));
  return get_or_create_int_cst (pointer_type, 0);
}

/* Return the svalue * for a unknown_svalue for TYPE (which can be NULL_TREE),
   creating it if necessary.
   The unknown_svalue instances are reused, based on pointer equality
   of the types  */

const svalue *
region_model_manager::get_or_create_unknown_svalue (tree type)
{
  /* Don't create unknown values when doing feasibility testing;
     instead, create a unique svalue.  */
  if (m_checking_feasibility)
    return create_unique_svalue (type);

  /* Special-case NULL, so that the hash_map can use NULL as the
     "empty" value.  */
  if (type == NULL_TREE)
    {
      if (!m_unknown_NULL)
	m_unknown_NULL = new unknown_svalue (alloc_node_id (), type);
      return m_unknown_NULL;
    }

  auto iter = m_unknowns_map.find (type);
  if (iter != m_unknowns_map.end ())
    return iter->second;
  unknown_svalue *sval = new unknown_svalue (alloc_node_id (), type);
  m_unknowns_map.insert ({type, sval});
  return sval;
}

/* Return a freshly-allocated svalue of TYPE, owned by this manager.  */

const svalue *
region_model_manager::create_unique_svalue (tree type)
{
  svalue *sval = new placeholder_svalue (alloc_node_id (), type, "unique");
  m_managed_dynamic_svalues.safe_push (sval);
  return sval;
}

/* Return the svalue * for the initial value of REG, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_initial_value (const region *reg,
						   bool check_poisoned)
{
  if (!reg->can_have_initial_svalue_p () && check_poisoned)
    return get_or_create_poisoned_svalue (poison_kind::uninit,
					  reg->get_type ());

  /* The initial value of a cast is a cast of the initial value.  */
  if (const cast_region *cast_reg = reg->dyn_cast_cast_region ())
    {
      const region *original_reg = cast_reg->get_parent_region ();
      return get_or_create_cast (cast_reg->get_type (),
				 get_or_create_initial_value (original_reg));
    }

  /* Simplify:
       INIT_VAL(ELEMENT_REG(STRING_REG), CONSTANT_SVAL)
     to:
       CONSTANT_SVAL(STRING[N]).  */
  if (const element_region *element_reg = reg->dyn_cast_element_region ())
    if (tree cst_idx = element_reg->get_index ()->maybe_get_constant ())
      if (const string_region *string_reg
	  = element_reg->get_parent_region ()->dyn_cast_string_region ())
	if (tree_fits_shwi_p (cst_idx))
	  {
	    HOST_WIDE_INT idx = tree_to_shwi (cst_idx);
	    tree string_cst = string_reg->get_string_cst ();
	    if (idx >= 0 && idx <= TREE_STRING_LENGTH (string_cst))
	      {
		int ch = TREE_STRING_POINTER (string_cst)[idx];
		return get_or_create_int_cst (reg->get_type (), ch);
	      }
	  }

  /* INIT_VAL (*UNKNOWN_PTR) -> UNKNOWN_VAL.  */
  if (reg->symbolic_for_unknown_ptr_p ())
    return get_or_create_unknown_svalue (reg->get_type ());

  auto mem_range = reg->get_memory_range (*this);
  if (&mem_range.get_base_region () != reg)
    {
      // Express as a cropping:
      auto &bc_mgr = get_node_manager ().get_bit_contents_manager ();;
      auto &baseline_bc
	= bc_mgr.get_baseline_contents (mem_range.get_base_region (),
					baseline_state::after_main);
      auto &init_bv = baseline_bc.get_bits (mem_range.get_bit_range ());
      return get_svalue_from_bit_vector (reg->get_type (), init_bv);
    }

  auto iter = m_initial_values_map.find (reg);
  if (iter != m_initial_values_map.end ())
    return iter->second;
  initial_svalue *initial_sval
    = new initial_svalue (alloc_node_id (), reg->get_type (), reg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (initial_sval);
  m_initial_values_map.insert ({reg, initial_sval});
  return initial_sval;
}

/* Return the svalue * for R using type TYPE, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_setjmp_svalue (const setjmp_record &r,
						   tree type)
{
  const setjmp_svalue::key_t key (r, type);
  auto iter = m_setjmp_values_map.find (key);
  if (iter != m_setjmp_values_map.end ())
    return iter->second;
  setjmp_svalue *setjmp_sval = new setjmp_svalue (r, alloc_node_id (), type);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (setjmp_sval);
  m_setjmp_values_map.insert ({key, setjmp_sval});
  return setjmp_sval;
}

/* Return the svalue * for a poisoned value of KIND and TYPE, creating it if
   necessary.  */

const svalue *
region_model_manager::get_or_create_poisoned_svalue (enum poison_kind kind,
						     tree type)
{
  const poisoned_svalue::key_t key (kind, type);
  auto iter = m_poisoned_values_map.find (key);
  if (iter != m_poisoned_values_map.end ())
    return iter->second;
  poisoned_svalue *poisoned_sval
    = new poisoned_svalue (kind, alloc_node_id (), type);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (poisoned_sval);
  m_poisoned_values_map.insert ( {key, poisoned_sval} );
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

  auto pointer = pointee->get_pointer (*this);
  return get_ptr_svalue (ptr_type, pointer);
}

const svalue *
region_model_manager::get_ptr_svalue (tree ptr_type, symbolic::pointer ptr)
{
  /* If this is a symbolic region from dereferencing a pointer, then
     return the original pointer.  */
  if (ptr.get_byte_offset ().is_zero_p ())
    if (auto *sym_reg = ptr.get_base_region ().dyn_cast_symbolic_region ())
      return get_or_create_cast (ptr_type, sym_reg->get_pointer ());

  const pointer_svalue::key_t key (ptr_type, ptr);
  auto iter = m_pointer_values_map.find (key);
  if (iter != m_pointer_values_map.end ())
    return iter->second;
  pointer_svalue *sval = new pointer_svalue (alloc_node_id (), key);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (sval);
  m_pointer_values_map.insert ({key, sval});
  return sval;
}

/* Subroutine of region_model_manager::maybe_fold_unaryop
   when the arg is a binop_svalue.
   Invert comparisons e.g. "!(x == y)" => "x != y".
   Otherwise, return nullptr.  */

const svalue *
region_model_manager::
maybe_invert_comparison_in_unaryop (tree result_type,
				    const binop_svalue *binop)
{
  if (TREE_CODE_CLASS (binop->get_op ()) == tcc_comparison)
    {
      enum tree_code inv_op
	= invert_tree_comparison (binop->get_op (),
				  HONOR_NANS (binop->get_type ()));
      if (inv_op != ERROR_MARK)
	return get_or_create_cast
	  (result_type,
	   get_or_create_binop (binop->get_type (), inv_op,
				binop->get_arg0 (),
				binop->get_arg1 ()));
    }
  return nullptr;
}

/* Subroutine of region_model_manager::get_or_create_unaryop.
   Attempt to fold the inputs and return a simpler svalue *.
   Otherwise, return nullptr.  */

const svalue *
region_model_manager::maybe_fold_unaryop (tree type, enum tree_code op,
					  const svalue *arg)
{
  /* Ops on "unknown" are also unknown.  */
  if (arg->get_kind () == SK_UNKNOWN)
    return get_or_create_unknown_svalue (type);
  /* Likewise for "poisoned".  */
  else if (const poisoned_svalue *poisoned_sval
	     = arg->dyn_cast_poisoned_svalue ())
    return get_or_create_poisoned_svalue (poisoned_sval->get_poison_kind (),
					  type);

  gcc_assert (arg->can_have_associated_state_p ());

  switch (op)
    {
    default: break;
    case VIEW_CONVERT_EXPR:
    case NOP_EXPR:
      {
	/* Avoid creating symbolic regions for pointer casts by
	   simplifying (T*)(&REGION) to ((T*)&REGION).  */
	if (auto *sym_ptr = arg->maybe_get_symbolic_pointer ())
	  if (type == NULL_TREE || POINTER_TYPE_P (type))
	    return get_ptr_svalue (type, *sym_ptr);

	if (!type)
	  return nullptr;

	/* Handle redundant casts.  */
	if (arg->get_type ()
	    && useless_type_conversion_p (arg->get_type (), type))
	  return arg;

	if (auto *sval_from_bv = arg->dyn_cast_svalue_from_bit_vector ())
	  {
	    concrete_bit_size type_size;
	    if (get_type_size (type, &type_size))
	      {
		concrete_bit_size bv_size;
		auto &bv = sval_from_bv->get_bit_vector ();
		if (bv.maybe_get_concrete_size_in_bits (&bv_size))
		  if (type_size == bv_size)
		    return get_svalue_from_bit_vector
		      (type, sval_from_bv->get_bit_vector ());
	      }
	  }

	/* Fold "cast<TYPE> (cast <INNER_TYPE> (innermost_arg))
	     => "cast<TYPE> (innermost_arg)",
	   unless INNER_TYPE is narrower than TYPE.  */
	if (const svalue *innermost_arg = arg->maybe_undo_cast ())
	  {
	    if (tree inner_type = arg->get_type ())
	      if (TYPE_SIZE (type)
		  && TYPE_SIZE (inner_type)
		  && (fold_binary (LE_EXPR, boolean_type_node,
				   TYPE_SIZE (type), TYPE_SIZE (inner_type))
		      == boolean_true_node))
		return maybe_fold_unaryop (type, op, innermost_arg);
	  }

	/* Casting all zeroes should give all zeroes.  */
	if (type
	    && arg->all_zeroes_p ()
	    && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
	  return get_or_create_int_cst (type, 0);
      }
      break;
    case TRUTH_NOT_EXPR:
      {
	/* Invert comparisons e.g. "!(x == y)" => "x != y".  */
	if (const binop_svalue *binop = arg->dyn_cast_binop_svalue ())
	  if (const svalue *folded
		= maybe_invert_comparison_in_unaryop (type, binop))
	    return folded;
      }
      break;
    case NEGATE_EXPR:
      {
	/* -(-(VAL)) is VAL, for integer types.  */
	if (const unaryop_svalue *unaryop = arg->dyn_cast_unaryop_svalue ())
	  if (unaryop->get_op () == NEGATE_EXPR
	      && type == unaryop->get_type ()
	      && type
	      && INTEGRAL_TYPE_P (type))
	    return unaryop->get_arg ();
      }
      break;
    case BIT_NOT_EXPR:
      {
	/* Invert comparisons for e.g. "~(x == y)" => "x != y".  */
	if (type
	    && TREE_CODE (type) == BOOLEAN_TYPE
	    && arg->get_type ()
	    && TREE_CODE (arg->get_type ()) == BOOLEAN_TYPE)
	  if (const binop_svalue *binop = arg->dyn_cast_binop_svalue ())
	    if (const svalue *folded
		= maybe_invert_comparison_in_unaryop (type, binop))
	      return folded;
      }
      break;
    }

  /* Constants.  */
  if (type)
    if (tree cst = arg->maybe_get_constant ())
      if (tree result = fold_unary (op, type, cst))
	{
	  if (CONSTANT_CLASS_P (result))
	    return get_or_create_constant_svalue (result);

	  /* fold_unary can return casts of constants; try to handle them.  */
	  if (op != NOP_EXPR
	      && TREE_CODE (result) == NOP_EXPR
	      && CONSTANT_CLASS_P (TREE_OPERAND (result, 0)))
	    {
	      const svalue *inner_cst
		= get_or_create_constant_svalue (TREE_OPERAND (result, 0));
	      return get_or_create_cast (type,
					 get_or_create_cast (TREE_TYPE (result),
							     inner_cst));
	    }
	}

  return nullptr;
}

/* Return the svalue * for an unary operation OP on ARG with a result of
   type TYPE, creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_unaryop (tree type, enum tree_code op,
					     const svalue *arg)
{
  if (const svalue *folded = maybe_fold_unaryop  (type, op, arg))
    return folded;
  const unaryop_svalue::key_t key (type, op, arg);
  auto iter = m_unaryop_values_map.find (key);
  if (iter != m_unaryop_values_map.end ())
    return iter->second;
  unaryop_svalue *unaryop_sval
    = new unaryop_svalue (alloc_node_id (), type, op, arg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (unaryop_sval);
  m_unaryop_values_map.insert ({key, unaryop_sval});
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
  if (!dst_type)
    return NOP_EXPR;
  if (!src_type)
    return NOP_EXPR;

  if (SCALAR_FLOAT_TYPE_P (src_type))
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
  /* No-op if the types are the same.  */
  if (type == arg->get_type ())
    return arg;

  /* Don't attempt to handle casts involving vector types for now.  */
  if (type)
    if (VECTOR_TYPE_P (type)
	|| (arg->get_type ()
	    && VECTOR_TYPE_P (arg->get_type ())))
      return get_or_create_unknown_svalue (type);

  enum tree_code op = get_code_for_cast (type, arg->get_type ());
  return get_or_create_unaryop (type, op, arg);
}

/* Subroutine of region_model_manager::get_or_create_binop.
   Attempt to fold the inputs and return a simpler svalue *.
   Otherwise, return nullptr.  */

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
      if (type)
	{
	  if (tree result = fold_binary (op, type, cst0, cst1))
	    if (CONSTANT_CLASS_P (result))
	      return get_or_create_constant_svalue (result);
	}
      else
	{
	  if (tree result = int_const_binop (op, cst0, cst1, -1))
	    return get_or_create_constant_svalue (NULL_TREE, result);
	}
    }

  if ((type && FLOAT_TYPE_P (type))
      || (arg0->get_type () && FLOAT_TYPE_P (arg0->get_type ()))
      || (arg1->get_type () && FLOAT_TYPE_P (arg1->get_type ())))
    return nullptr;

  switch (op)
    {
    default:
      break;
    case POINTER_PLUS_EXPR:
      if (auto *ptr = arg0->maybe_get_symbolic_pointer ())
	{
	  auto &byte_mgr = get_node_manager ().get_scalars ().get_bytes ();
	  /* Interpret arg1 as signed twos-complement, so that very
	     large values become subtractions.  */
	  auto *signed_arg1
	    = get_or_create_cast (signed_type_for (arg1->get_type ()),
				  arg1);
	  auto &offset = byte_mgr.get_scalar_from_svalue (*signed_arg1);
	  symbolic::pointer ptr_plus_arg1
	    (ptr->get_base_region (),
	     ptr->get_byte_offset () + offset);
	  return get_ptr_svalue (type, ptr_plus_arg1);
	}
      /* Fall through.  */
    case PLUS_EXPR:
      /* (VAL + 0) -> VAL.  */
      if (cst1 && zerop (cst1))
	return get_or_create_cast (type, arg0);
      /* X + (-X) -> 0.  */
      if (const unaryop_svalue *unary_op = arg1->dyn_cast_unaryop_svalue ())
	if (unary_op->get_op () == NEGATE_EXPR
	    && unary_op->get_arg () == arg0
	    && type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
	  return get_or_create_int_cst (type, 0);
      /* X + (Y - X) -> Y.  */
      if (const binop_svalue *bin_op = arg1->dyn_cast_binop_svalue ())
	if (bin_op->get_op () == MINUS_EXPR)
	  if (bin_op->get_arg1 () == arg0)
	    return get_or_create_cast (type, bin_op->get_arg0 ());
      break;
    case MINUS_EXPR:
      /* (VAL - 0) -> VAL.  */
      if (cst1 && zerop (cst1))
	return get_or_create_cast (type, arg0);
      /* (0 - VAL) -> -VAL.  */
      if (cst0 && zerop (cst0))
	return get_or_create_unaryop (type, NEGATE_EXPR, arg1);
      /* (X + Y) - X -> Y.  */
      if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
	if (binop->get_op () == PLUS_EXPR)
	  if (binop->get_arg0 () == arg1)
	    return get_or_create_cast (type, binop->get_arg1 ());
      break;
    case POINTER_DIFF_EXPR:
      /* (X POINTER_PLUS Y) POINTER_DIFF_EXPR X -> Y.  */
      if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
	if (binop->get_op () == POINTER_PLUS_EXPR)
	  if (binop->get_arg0 () == arg1)
	    return get_or_create_cast (type, binop->get_arg1 ());

      /* Fold symptr(base_reg + x) - symptr(base_reg + y) --> x -- y
	 when they share the same base region.  */
      if (auto *sym_ptr_0 = arg0->maybe_get_symbolic_pointer ())
	if (auto *sym_ptr_1 = arg1->maybe_get_symbolic_pointer ())
	  if (&sym_ptr_0->get_base_region () == &sym_ptr_1->get_base_region ())
	    {
	      auto &offset_byte_diff = (sym_ptr_0->get_byte_offset ()
					- sym_ptr_1->get_byte_offset ());
	      return get_svalue_from_scalar<concrete_byte_offset>
		(type,
		 offset_byte_diff);
	    }
      break;
    case MULT_EXPR:
      /* (VAL * 0).  */
      if (cst1
	  && zerop (cst1)
	  && (type == NULL_TREE || INTEGRAL_TYPE_P (type)))
	return get_or_create_int_cst (type, 0);
      /* (VAL * 1) -> VAL.  */
      if (cst1 && integer_onep (cst1))
	return get_or_create_cast (type, arg0);
      break;
    case BIT_AND_EXPR:
      if (cst1)
	{
	  if (zerop (cst1)
	      && (type == NULL_TREE || INTEGRAL_TYPE_P (type)))
	    /* "(ARG0 & 0)" -> "0".  */
	    return get_or_create_int_cst (type, 0);
	}
      if (arg0->get_type () == boolean_type_node
	  && arg1->get_type () == boolean_type_node)
	{
	  /* If the LHS are both _Bool, then... */
	  /* ..."(1 & x) -> x".  */
	  if (cst0 && !zerop (cst0))
	    return get_or_create_cast (type, arg1);
	  /* ..."(x & 1) -> x".  */
	  if (cst1 && !zerop (cst1))
	    return get_or_create_cast (type, arg0);
	  /* ..."(0 & x) -> 0".  */
	  if (cst0 && zerop (cst0))
	    return get_or_create_int_cst (type, 0);
	  /* ..."(x & 0) -> 0".  */
	  if (cst1 && zerop (cst1))
	    return get_or_create_int_cst (type, 0);
	}
      break;
    case BIT_IOR_EXPR:
      if (arg0->get_type () == boolean_type_node
	  && arg1->get_type () == boolean_type_node)
	{
	  /* If the LHS are both _Bool, then... */
	  /* ..."(1 | x) -> 1".  */
	  if (cst0 && !zerop (cst0))
	    return get_or_create_int_cst (type, 1);
	  /* ..."(x | 1) -> 1".  */
	  if (cst1 && !zerop (cst1))
	    return get_or_create_int_cst (type, 1);
	  /* ..."(0 | x) -> x".  */
	  if (cst0 && zerop (cst0))
	    return get_or_create_cast (type, arg1);
	  /* ..."(x | 0) -> x".  */
	  if (cst1 && zerop (cst1))
	    return get_or_create_cast (type, arg0);
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

    case LSHIFT_EXPR:
      if (cst1)
	if (zerop (cst1))
	  return get_or_create_cast (type, arg0);
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
      {
	value_range arg1_vr;
	if (arg1->maybe_get_value_range (arg1_vr))
	  if (arg1_vr.zero_p ())
	    return get_or_create_unknown_svalue (type);
      }
      break;
    }

  /* For associative ops, fold "(X op CST_A) op CST_B)" to
     "X op (CST_A op CST_B)".  */
  if (cst1 && associative_tree_code (op))
    if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
      if (binop->get_op () == op
	  && binop->get_arg1 ()->maybe_get_constant ())
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

  /* Distribute multiplication by a constant through addition/subtraction:
     (X + Y) * CST => (X * CST) + (Y * CST).  */
  if (cst1 && op == MULT_EXPR)
    if (const binop_svalue *binop = arg0->dyn_cast_binop_svalue ())
      if (binop->get_op () == PLUS_EXPR
	  || binop->get_op () == MINUS_EXPR)
	{
	  return get_or_create_binop
	    (type, binop->get_op (),
	     get_or_create_binop (type, op,
				  binop->get_arg0 (), arg1),
	     get_or_create_binop (type, op,
				  binop->get_arg1 (), arg1));
	}


  /* Typeless operations, assumed to be effectively arbitrary sized
     integers following normal arithmetic rules.  */
  if (!type)
    switch (op)
      {
      default:
	break;
      case MINUS_EXPR:
	{
	  /* (X - X) -> 0.  */
	  if (arg0 == arg1)
	    return get_or_create_int_cst (type, 0);

	  /* (X + A) - (A + B) -> (A - B).  */
	  if (const binop_svalue *binop0 = arg0->dyn_cast_binop_svalue ())
	    if (const binop_svalue *binop1 = arg1->dyn_cast_binop_svalue ())
	      if (binop0->get_op () == PLUS_EXPR
		  && binop1->get_op () == PLUS_EXPR
		  && binop0->get_arg0 () == binop1->get_arg0 ())
		return get_or_create_binop (NULL_TREE, op,
					    binop0->get_arg1 (),
					    binop1->get_arg1 ());
	}
	break;

      case EXACT_DIV_EXPR:
	{
	  if (const unaryop_svalue *unaryop0 = arg0->dyn_cast_unaryop_svalue ())
	    {
	      if (unaryop0->get_op () == NOP_EXPR)
		if (const svalue *sval = maybe_fold_binop (NULL_TREE, op,
							   unaryop0->get_arg (),
							   arg1))
		  return sval;
	    }
	  if (const binop_svalue *binop0 = arg0->dyn_cast_binop_svalue ())
	    {
	      switch (binop0->get_op ())
		{
		default:
		  break;

		case PLUS_EXPR:
		case MINUS_EXPR:
		  /* (A op B) / C -> (A / C) op (B / C).  */
		  {
		    if (const svalue *op_on_a
			= maybe_fold_binop (NULL_TREE, op,
					    binop0->get_arg0 (), arg1))
		      if (const svalue *op_on_b
			  = maybe_fold_binop (NULL_TREE, op,
					      binop0->get_arg1 (), arg1))
			return get_or_create_binop (NULL_TREE,
						    binop0->get_op (),
						    op_on_a, op_on_b);
		  }
		  break;

		case MULT_EXPR:
		  /* (A * B) / C -> A * (B / C) if C is a divisor of B.
		     In particular, this should also handle the case
		     (A * B) / B -> A.  */
		  if (const svalue *b_div_c
		      = maybe_fold_binop (NULL_TREE, op,
					  binop0->get_arg1 (), arg1))
		    return get_or_create_binop (NULL_TREE, binop0->get_op (),
						binop0->get_arg0 (), b_div_c);
		}
	    }
	}
	break;
      }

  /* etc.  */

  return nullptr;
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

  /* Ops on "unknown"/"poisoned" are unknown (unless we were able to fold
     it via an identity in maybe_fold_binop).  */
  if (!arg0->can_have_associated_state_p ()
      || !arg1->can_have_associated_state_p ())
    return get_or_create_unknown_svalue (type);

  const binop_svalue::key_t key (type, op, arg0, arg1);
  auto iter = m_binop_values_map.find (key);
  if (iter != m_binop_values_map.end ())
    return iter->second;
  binop_svalue *binop_sval
    = new binop_svalue (alloc_node_id (), type, op, arg0, arg1);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (binop_sval);
  m_binop_values_map.insert ({key, binop_sval});
  return binop_sval;
}

/* Subroutine of get_svalue_from_bit_vector.  */

const svalue *
region_model_manager::
maybe_fold_svalue_from_bit_vector (tree type,
				   const symbolic::bit_vector &bv)
{
  if (auto *svalue_bv = bv.dyn_cast_bit_vector_from_svalue ())
    {
      auto &sval = svalue_bv->get_svalue ();

      if (!type)
	return &sval;

      if (type && sval.get_type ()
	  && types_compatible_p (type, sval.get_type ()))
	return &sval;

      if (auto underlying_ptr
	    = sval.dyn_cast_pointer_svalue ())
	if (type && POINTER_TYPE_P (type))
	  return get_ptr_svalue (type,
				 underlying_ptr->get_symbolic_pointer ());

      if (tree cst = sval.maybe_get_constant ())
	if (type
	    && ANY_INTEGRAL_TYPE_P (type)
	    && ANY_INTEGRAL_TYPE_P (TREE_TYPE (cst)))
	  if (tree cast_expr = fold_build1 (NOP_EXPR, type, cst))
	    if (CONSTANT_CLASS_P (cast_expr))
	      return get_or_create_constant_svalue (cast_expr);

      /* Otherwise, we have SVALUE(OUTER_TYPE, BIT_VECTOR (SVALUE (INNER_TYPE)))
	 Fold it to CAST_SVALUE(OUTER_TYPE (SVALUE (INNER_TYPE))).  */
      return get_or_create_cast (type, &sval);
    }

  /* Fold
       svalue_from_bit_vector (_Bool, crop_to_bit_0 (SVAL (_Bool)))
     to:
       SVAL (_Bool).  */
  if (type == boolean_type_node)
    if (auto *cropped_bv = bv.dyn_cast_cropped_bit_vector ())
      {
	auto uncropped_bit_range = cropped_bv->get_uncropped_bits_to_use ();
	if (uncropped_bit_range.get_start_offset ().is_zero_p ()
	    && uncropped_bit_range.get_size ().is_one_p ())
	  {
	    auto &uncropped_bv = cropped_bv->get_uncropped_bit_vector ();
	    if (auto *from_sval
		  = uncropped_bv.dyn_cast_bit_vector_from_svalue ())
	      {
		const svalue &sval = from_sval->get_svalue ();
		if (sval.get_type () == boolean_type_node)
		  return &sval;
	      }
	  }
      }

  if (!type)
    return nullptr;

  if (auto *tristate_bv = bv.dyn_cast_tristate_bit_vector ())
    switch (tristate_bv->get_bit_value ().get_value ())
      {
      default:
	gcc_unreachable ();
	break;

      case tristate::TS_UNKNOWN:
	return get_or_create_unknown_svalue (type);

      case tristate::TS_FALSE:
	if (INTEGRAL_TYPE_P (type))
	  return get_or_create_int_cst (type, 0);
	break;

      case tristate::TS_TRUE:
	if (INTEGRAL_TYPE_P (type))
	  return get_or_create_int_cst (type,
					wi::minus_one (TYPE_PRECISION (type)));
	break;
      }

  /* Attempt to get a constant_svalue from a concrete bit_vector.  */
  if (can_native_interpret_type_p (type))
    {
      concrete_byte_size byte_size;
      if (bv.maybe_get_concrete_size_in_bytes (&byte_size))
	{
	  // Attempt to extract a constant
	  std::vector<unsigned char> buf;
	  for (concrete_byte_offset byte_offset = 0_bytes;
	       byte_offset < byte_size;
	       byte_offset += 1_byte)
	    {
	      auto &bv_byte = bv.get_byte_at_offset (byte_offset);

	      uint8_t byte_val;
	      if (bv_byte.maybe_as_uint8 (&byte_val))
		buf.push_back (byte_val);
	      else
		return nullptr;
	    }

	  if (tree interpreted = native_interpret_expr (type,
							buf.data (),
							buf.size ()))
	    if (CONSTANT_CLASS_P (interpreted))
	      return get_or_create_constant_svalue (interpreted);
	}
    }

  return nullptr;
}

/* Strip off qualifiers from pointer types, apart from address spaces.  */

static tree
canonicalize_ptr_type_for_bit_vector (tree type)
{
  gcc_assert (type);
  gcc_assert (POINTER_TYPE_P (type));

  tree pointee = TREE_TYPE (type);
  qualifier_set qs (TYPE_UNQUALIFIED, TYPE_ADDR_SPACE (pointee));
  tree unqualified_pointee = build_qualified_type (pointee, qs);
  tree result = build_pointer_type (unqualified_pointee);
  if (CHECKING_P)
    gcc_assert (types_compatible_p (type, result));
  return result;
}

/* Get or create an svalue for BV interpreted as TYPE.  */

const svalue *
region_model_manager::
get_svalue_from_bit_vector (tree type,
			    const symbolic::bit_vector &bv)
{
  // TODO: assert that we have matching sizes

  if (type && POINTER_TYPE_P (type))
    type = canonicalize_ptr_type_for_bit_vector (type);

  if (const svalue *folded = maybe_fold_svalue_from_bit_vector (type, bv))
    return folded;

  const svalue_from_bit_vector::key_t key {type, &bv};
  auto iter = m_bit_vector_to_svalue_map.find (key);
  if (iter != m_bit_vector_to_svalue_map.end ())
    return iter->second;
  svalue_from_bit_vector *sval
    = new svalue_from_bit_vector (alloc_node_id (), type, bv);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (sval);
  m_bit_vector_to_svalue_map.insert ({key, sval});
  return sval;
}

/* Get or create an svalue of TYPE from SCALAR.  */

template <typename ConcreteValueType>
const svalue *
region_model_manager::
get_svalue_from_scalar (tree type,
			const symbolic::scalar<ConcreteValueType> &scalar)
{
  switch (scalar.get_kind ())
    {
    default:
      gcc_unreachable ();

    case symbolic::scalar<ConcreteValueType>::kind::concrete:
      {
	auto &conc
	  = (const symbolic::concrete_scalar<ConcreteValueType> &)scalar;
	tree cst
	  = make_integer_cst_from_integer (type,
					   conc.get_integer (),
					   true);
	return get_or_create_constant_svalue (cst);
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::sum:
      {
	auto &sum = (const symbolic::sum<ConcreteValueType> &)scalar;
	auto *concrete_addend = sum.get_any_concrete_addend ();
	const svalue *result
	  = (concrete_addend
	     ? get_svalue_from_scalar (type, *concrete_addend)
	     : get_or_create_int_cst (type, 0));
	for (size_t i = 0; i < sum.get_num_symbolic_addends (); ++i)
	  {
	    auto &symbolic_addend = sum.get_symbolic_addend (i);
	    result = get_or_create_binop
	      (type,
	       PLUS_EXPR,
	       result,
	       get_svalue_from_scalar (type, symbolic_addend));
	  }
	return result;
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::product:
      {
	auto &product = (const symbolic::product<ConcreteValueType> &)scalar;
	return get_or_create_binop
	      (type,
	       MULT_EXPR,
	       get_svalue_from_scalar (type, product.get_lhs ()),
	       get_svalue_from_scalar (type, product.get_rhs ()));
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::from_svalue:
      {
	auto &scalar_from_sval
	  = (const symbolic::scalar_from_svalue<ConcreteValueType> &)scalar;
	return get_or_create_cast (type, &scalar_from_sval.get_svalue ());
      }
      break;

    case symbolic::scalar<ConcreteValueType>::kind::named_test:
      gcc_unreachable ();
      break;

    case symbolic::scalar<ConcreteValueType>::kind::bits_from_bytes:
      {
	auto &from_bytes = (const symbolic::bits_from_bytes_scalar &)scalar;
	return get_or_create_binop
	      (type,
	       MULT_EXPR,
	       get_svalue_from_scalar (type, from_bytes.get_bytes ()),
	       get_or_create_int_cst (type, BITS_PER_UNIT));
      }
      break;
    }
}

template
const svalue *
region_model_manager::
get_svalue_from_scalar (tree type,
			const symbolic_bit_offset &scalar);

template
const svalue *
region_model_manager::
get_svalue_from_scalar (tree type,
			const symbolic_byte_offset &scalar);

/* Return the svalue * that decorates ARG as being unmergeable,
   creating it if necessary.  */

const svalue *
region_model_manager::get_or_create_unmergeable (const svalue *arg)
{
  if (arg->get_kind () == SK_UNMERGEABLE)
    return arg;

  auto iter = m_unmergeable_values_map.find (arg);
  if (iter != m_unmergeable_values_map.end ())
    return iter->second;
  unmergeable_svalue *unmergeable_sval
    = new unmergeable_svalue (alloc_node_id (), arg);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (unmergeable_sval);
  m_unmergeable_values_map.insert ({arg, unmergeable_sval});
  return unmergeable_sval;
}

/* Return the svalue * of type TYPE for the merger of value BASE_SVAL
   and ITER_SVAL at SNODE, creating it if necessary.  */

const svalue *
region_model_manager::
get_or_create_widening_svalue (tree type,
			       const supernode *snode,
			       const svalue *base_sval,
			       const svalue *iter_sval)
{
  gcc_assert (base_sval->get_kind () != SK_WIDENING);
  gcc_assert (iter_sval->get_kind () != SK_WIDENING);
  const widening_svalue::key_t key (type, snode, base_sval, iter_sval);
  auto iter = m_widening_values_map.find (key);
  if (iter != m_widening_values_map.end ())
    return iter->second;
  widening_svalue *widening_sval
    = new widening_svalue (alloc_node_id (), type, snode, base_sval,
			   iter_sval);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (widening_sval);
  m_widening_values_map.insert ({key, widening_sval});
  return widening_sval;
}

/* Return the svalue * of type TYPE for the value conjured for ID_REG
   at STMT (using IDX for any further disambiguation),
   creating it if necessary.
   Use P to purge existing state from the svalue, for the case where a
   conjured_svalue would be reused along an execution path.  */

const svalue *
region_model_manager::get_or_create_conjured_svalue (tree type,
						     const gimple *stmt,
						     const region *id_reg,
						     const conjured_purge &p,
						     unsigned idx)
{
  auto &bv_mgr = m_node_mgr.get_bit_vector_manager ();
  auto &bv = bv_mgr.get_conjured_bits (stmt, id_reg, p, idx);
  return get_svalue_from_bit_vector (type, bv);
}

/* Subroutine of region_model_manager::get_or_create_asm_output_svalue.
   Return a folded svalue, or nullptr.  */

const svalue *
region_model_manager::
maybe_fold_asm_output_svalue (tree type,
			      const vec<const svalue *> &inputs)
{
  /* Unknown inputs should lead to unknown results.  */
  for (const auto &iter : inputs)
    if (iter->get_kind () == SK_UNKNOWN)
      return get_or_create_unknown_svalue (type);

  return nullptr;
}

/* Return the svalue * of type TYPE for OUTPUT_IDX of the deterministic
   asm stmt ASM_STMT, given INPUTS as inputs.  */

const svalue *
region_model_manager::
get_or_create_asm_output_svalue (tree type,
				 const gasm *asm_stmt,
				 unsigned output_idx,
				 const vec<const svalue *> &inputs)
{
  gcc_assert (inputs.length () <= asm_output_svalue::MAX_INPUTS);

  if (const svalue *folded
	= maybe_fold_asm_output_svalue (type, inputs))
    return folded;

  const char *asm_string = gimple_asm_string (asm_stmt);
  const unsigned noutputs = gimple_asm_noutputs (asm_stmt);

  const asm_output_svalue::key_t key (type, asm_string, output_idx, inputs);
  auto iter = m_asm_output_values_map.find (key);
  if (iter != m_asm_output_values_map.end ())
    return iter->second;
  asm_output_svalue *asm_output_sval
    = new asm_output_svalue (alloc_node_id (), type, asm_string, output_idx,
			     noutputs, inputs);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (asm_output_sval);
  m_asm_output_values_map.insert ({key, asm_output_sval});
  return asm_output_sval;
}

/* Return the svalue * of type TYPE for OUTPUT_IDX of a deterministic
   asm stmt with string ASM_STRING with NUM_OUTPUTS outputs, given
   INPUTS as inputs.  */

const svalue *
region_model_manager::
get_or_create_asm_output_svalue (tree type,
				 const char *asm_string,
				 unsigned output_idx,
				 unsigned num_outputs,
				 const vec<const svalue *> &inputs)
{
  gcc_assert (inputs.length () <= asm_output_svalue::MAX_INPUTS);

  if (const svalue *folded
	= maybe_fold_asm_output_svalue (type, inputs))
    return folded;

  const asm_output_svalue::key_t key (type, asm_string, output_idx, inputs);
  auto iter = m_asm_output_values_map.find (key);
  if (iter != m_asm_output_values_map.end ())
    return iter->second;
  asm_output_svalue *asm_output_sval
    = new asm_output_svalue (alloc_node_id (), type, asm_string, output_idx,
			     num_outputs, inputs);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (asm_output_sval);
  m_asm_output_values_map.insert ({key, asm_output_sval});
  return asm_output_sval;
}

/* Return the svalue * of type TYPE for the result of a call to FNDECL
   with __attribute__((const)), given INPUTS as inputs.  */

const svalue *
region_model_manager::
get_or_create_const_fn_result_svalue (tree type,
				      tree fndecl,
				      const vec<const svalue *> &inputs)
{
  gcc_assert (fndecl);
  gcc_assert (DECL_P (fndecl));
  gcc_assert (TREE_READONLY (fndecl));
  gcc_assert (inputs.length () <= const_fn_result_svalue::MAX_INPUTS);

  const const_fn_result_svalue::key_t key (type, fndecl, inputs);
  auto iter = m_const_fn_result_values_map.find (key);
  if (iter != m_const_fn_result_values_map.end ())
    return iter->second;
  const_fn_result_svalue *const_fn_result_sval
    = new const_fn_result_svalue (alloc_node_id (), type, fndecl, inputs);
  RETURN_UNKNOWN_IF_TOO_COMPLEX (const_fn_result_sval);
  m_const_fn_result_values_map.insert ({key, const_fn_result_sval});
  return const_fn_result_sval;
}

/* Get a tree for the size of STRING_CST, or NULL_TREE.
   Note that this may be larger than TREE_STRING_LENGTH (implying
   a run of trailing zero bytes from TREE_STRING_LENGTH up to this
   higher limit).  */

tree
get_string_cst_size (const_tree string_cst)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);
  gcc_assert (TREE_CODE (TREE_TYPE (string_cst)) == ARRAY_TYPE);

  return TYPE_SIZE_UNIT (TREE_TYPE (string_cst));
}

/* region consolidation.  */

/* Return the region for FNDECL, creating it if necessary.  */

const function_region *
region_model_manager::get_region_for_fndecl (tree fndecl)
{
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);

  auto iter = m_fndecls_map.find (fndecl);
  if (iter != m_fndecls_map.end ())
    return iter->second;
  function_region *reg
    = new function_region (alloc_node_id (), &m_code_region, fndecl);
  m_fndecls_map.insert ({fndecl, reg});
  return reg;
}

/* Return the region for LABEL, creating it if necessary.  */

const label_region *
region_model_manager::get_region_for_label (tree label)
{
  gcc_assert (TREE_CODE (label) == LABEL_DECL);

  auto iter = m_labels_map.find (label);
  if (iter != m_labels_map.end ())
    return iter->second;

  tree fndecl = DECL_CONTEXT (label);
  gcc_assert (fndecl && TREE_CODE (fndecl) == FUNCTION_DECL);

  const function_region *func_reg = get_region_for_fndecl (fndecl);
  label_region *reg
    = new label_region (alloc_node_id (), func_reg, label);
  m_labels_map.insert ({label, reg});
  return reg;
}

/* Return the region for EXPR, creating it if necessary.  */

const decl_region *
region_model_manager::get_region_for_global (tree expr)
{
  gcc_assert (VAR_P (expr));

  auto iter = m_globals_map.find (expr);
  if (iter != m_globals_map.end ())
    return iter->second;

  decl_region *reg
    = new decl_region (alloc_node_id (), &m_globals_region, expr);
  m_globals_map.insert ({expr, reg});
  return reg;
}

/* Return the region for an unknown access of type REGION_TYPE,
   creating it if necessary.
   This is a symbolic_region, where the pointer is an unknown_svalue
   of type &REGION_TYPE.  */

const region *
region_model_manager::get_unknown_symbolic_region (tree region_type)
{
  tree ptr_type = region_type ? build_pointer_type (region_type) : NULL_TREE;
  const svalue *unknown_ptr = get_or_create_unknown_svalue (ptr_type);
  return get_symbolic_region (unknown_ptr);
}

/* Return the region that describes accessing field FIELD of PARENT,
   creating it if necessary.  */

const region *
region_model_manager::get_field_region (const region *parent, tree field)
{
  gcc_assert (parent);
  gcc_assert (field);
  gcc_assert (TREE_CODE (field) == FIELD_DECL);

  /* (*UNKNOWN_PTR).field is (*UNKNOWN_PTR_OF_&FIELD_TYPE).  */
  if (parent->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (TREE_TYPE (field));

  const field_region::key_t key (parent, field);
  if (field_region *reg = m_field_regions.get (key))
    return reg;

  field_region *field_reg
    = new field_region (alloc_node_id (), parent, field);
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
  /* (UNKNOWN_PTR[IDX]) is (UNKNOWN_PTR).  */
  if (parent->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (element_type);

  const element_region::key_t key (parent, element_type, index);
  if (element_region *reg = m_element_regions.get (key))
    return reg;

  element_region *element_reg
    = new element_region (alloc_node_id (), parent, element_type, index);
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
  /* (UNKNOWN_PTR + OFFSET) is (UNKNOWN_PTR).  */
  if (parent->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (type);

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
			       POINTER_PLUS_EXPR, sval_x, byte_offset);
      return get_offset_region (parent->get_parent_region (), type, sval_sum);
    }

  const offset_region::key_t key (parent, type, byte_offset);
  if (offset_region *reg = m_offset_regions.get (key))
    return reg;

  offset_region *offset_reg
    = new offset_region (alloc_node_id (), parent, type, byte_offset);
  m_offset_regions.put (key, offset_reg);
  return offset_reg;
}

/* Fold sized_region[TYPE T1]( region[TYPE T2])
   --> field_region[TYPE T1](region[TYPE T2])
   if T2 is a RECORD_TYPE, the size is that of the record's first field, and
   T1 is the type of that first field.
   Also fold if there's an intermediate cast to NULL_TYPE.  */

const region *
region_model_manager::
maybe_fold_sized_region_to_initial_field (const region *parent,
					  tree type,
					  const svalue *byte_size_sval)
{
  if (!type)
    return nullptr;
  tree cst_byte_size = byte_size_sval->maybe_get_constant ();
  if (!cst_byte_size)
    return nullptr;
  const region *inner = parent;
  if (auto *cast = inner->dyn_cast_cast_region ())
    if (cast->get_type () == NULL_TREE)
      inner = cast->get_parent_region ();
  if (!inner->get_type ())
    return nullptr;
  if (TREE_CODE (inner->get_type ()) != RECORD_TYPE)
    return nullptr;
  record_layout layout (inner->get_type ()); // TODO: amortize this?
  auto *initial_item = layout.get_item_at (0_bits);
  if (!initial_item)
    return nullptr;
  if (!initial_item->m_bit_range.get_start_offset ().is_zero_p ())
    return nullptr;
  concrete_byte_size conc_byte_size_item;
  if (!maybe_bytes_from_bits (initial_item->m_bit_range.get_size (),
			      &conc_byte_size_item))
    return nullptr;
  concrete_byte_size conc_byte_size_from_sval
    (convert_from_integer_cst (cst_byte_size));
  if (conc_byte_size_item != conc_byte_size_from_sval)
    return nullptr;

  return get_cast_region
    (get_field_region (inner,
		       const_cast<tree> (initial_item->m_field)),
     type);
}

/* Return the region that describes accessing the subregion of type
   TYPE of size BYTE_SIZE_SVAL within PARENT, creating it if necessary.  */

const region *
region_model_manager::get_sized_region (const region *parent,
					tree type,
					const svalue *byte_size_sval)
{
  if (parent->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (type);

  if (byte_size_sval->get_type () != size_type_node)
    byte_size_sval = get_or_create_cast (size_type_node, byte_size_sval);

  /* If PARENT is already that size, return it.  */
  const svalue *parent_byte_size_sval = parent->get_byte_size_sval (this);
  if (tree parent_size_cst = parent_byte_size_sval->maybe_get_constant ())
    if (tree size_cst = byte_size_sval->maybe_get_constant ())
      {
	tree comparison
	  = fold_binary (EQ_EXPR, boolean_type_node, parent_size_cst, size_cst);
	if (comparison == boolean_true_node)
	  return parent;
      }

  if (auto *folded = maybe_fold_sized_region_to_initial_field (parent,
							       type,
							       byte_size_sval))
    return folded;

  const sized_region::key_t key (parent, type, byte_size_sval);
  if (sized_region *reg = m_sized_regions.get (key))
    return reg;

  sized_region *sized_reg
    = new sized_region (alloc_node_id (), parent, type, byte_size_sval);
  m_sized_regions.put (key, sized_reg);
  return sized_reg;
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

  if (original_region->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (type);

  /* Fold
       cast_region<T1>(symbolic_region(sval<(T2 *)>()))
       --> symbolic_region(cast_sval<T1 *> (sval<(T2 *)>)).  */
  if (auto *sym_reg = original_region->dyn_cast_symbolic_region ())
    {
      auto *ptr_sval = sym_reg->get_pointer ();
      if (type
	  && ptr_sval->get_type ()
	  && POINTER_TYPE_P (ptr_sval->get_type ()))
	{
	  if (auto *sym_ptr = ptr_sval->maybe_get_symbolic_pointer ())
	    {
	      tree ptr_to_type = build_pointer_type (type);
	      auto *cast_ptr_sval = get_ptr_svalue (ptr_to_type, *sym_ptr);
	      return get_symbolic_region (cast_ptr_sval);
	    }
	}
    }

  const cast_region::key_t key (original_region, type);
  if (cast_region *reg = m_cast_regions.get (key))
    return reg;

  cast_region *cast_reg
    = new cast_region (alloc_node_id (), original_region, type);
  m_cast_regions.put (key, cast_reg);
  return cast_reg;
}

/* Return the frame_region for call to FUN from CALLING_FRAME, creating it
   if necessary.  CALLING_FRAME may be nullptr.  */

const frame_region *
region_model_manager::get_frame_region (const frame_region *calling_frame,
					const function &fun)
{
  int index = calling_frame ? calling_frame->get_index () + 1 : 0;

  const frame_region::key_t key (calling_frame, fun);
  if (frame_region *reg = m_frame_regions.get (key))
    return reg;

  frame_region *frame_reg
    = new frame_region (alloc_node_id (), &m_stack_region, calling_frame,
			 fun, index);
  m_frame_regions.put (key, frame_reg);
  return frame_reg;
}

const region *
region_model_manager::maybe_fold_symbolic_region (const svalue *sval)
{
  auto *sym_ptr = sval->maybe_get_symbolic_pointer ();
  if (!sym_ptr)
    return nullptr;
  auto &base_reg = sym_ptr->get_base_region ();
  if (sym_ptr->zero_offset_p ())
    return &base_reg;
  else
    {
      /* The non-zero offset case.  */
      if (!sval->get_type ())
	return nullptr;

      tree pointed_to_type = TREE_TYPE (sval->get_type ());
      concrete_bit_size type_conc_bit_size;
      if (!get_type_size (pointed_to_type, &type_conc_bit_size))
	return nullptr;
      concrete_byte_size type_conc_byte_size;
      if (!maybe_bytes_from_bits (type_conc_bit_size, &type_conc_byte_size))
	return nullptr;
      auto *byte_offset_sval = get_svalue_from_scalar<concrete_byte_offset>
	(ptrdiff_type_node,
	 sym_ptr->get_byte_offset ());
      auto *offset_reg = get_offset_region (&base_reg,
					    NULL_TREE,
					    byte_offset_sval);
      auto &byte_mgr = get_node_manager ().get_scalars ().get_bytes ();
      auto *byte_size_sval = get_svalue_from_scalar<concrete_byte_offset>
	(size_type_node,
	 byte_mgr.get_concrete (type_conc_byte_size));
      return get_sized_region (offset_reg,
			       pointed_to_type,
			       byte_size_sval);
    }
}

/* Return the region that describes dereferencing SVAL, creating it
   if necessary.  */

const region *
region_model_manager::get_symbolic_region (const svalue *sval)
{
  if (auto *folded = maybe_fold_symbolic_region (sval))
    return folded;

  const symbolic_region::key_t key (&m_root_region, sval);
  if (symbolic_region *reg = m_symbolic_regions.get (key))
    return reg;

  symbolic_region *symbolic_reg
    = new symbolic_region (alloc_node_id (), &m_root_region, sval);
  m_symbolic_regions.put (key, symbolic_reg);
  return symbolic_reg;
}

/* Return the region that describes accessing STRING_CST, creating it
   if necessary.  */

const string_region *
region_model_manager::get_region_for_string (tree string_cst)
{
  gcc_assert (TREE_CODE (string_cst) == STRING_CST);

  auto iter = m_string_map.find (string_cst);
  if (iter != m_string_map.end ())
    return iter->second;

  string_region *reg
    = new string_region (alloc_node_id (), &m_root_region, string_cst);
  m_string_map.insert ({string_cst, reg});
  return reg;
}

/* Return the region that describes accessing BITS within PARENT as TYPE,
   creating it if necessary.  */

const region *
region_model_manager::get_bit_range (const region *parent, tree type,
				     const concrete_bit_range &bits)
{
  gcc_assert (parent);

  if (parent->symbolic_for_unknown_ptr_p ())
    return get_unknown_symbolic_region (type);

  const bit_range_region::key_t key (parent, type, bits);
  if (bit_range_region *reg = m_bit_range_regions.get (key))
    return reg;

  bit_range_region *bit_range_reg
    = new bit_range_region (alloc_node_id (), parent, type, bits);
  m_bit_range_regions.put (key, bit_range_reg);
  return bit_range_reg;
}

/* Return the region that describes accessing the IDX-th variadic argument
   within PARENT_FRAME, creating it if necessary.  */

const var_arg_region *
region_model_manager::get_var_arg_region (const frame_region *parent_frame,
					  unsigned idx)
{
  gcc_assert (parent_frame);

  const var_arg_region::key_t key (parent_frame, idx);
  if (var_arg_region *reg = m_var_arg_regions.get (key))
    return reg;

  var_arg_region *var_arg_reg
    = new var_arg_region (alloc_node_id (), parent_frame, idx);
  m_var_arg_regions.put (key, var_arg_reg);
  return var_arg_reg;
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
    = new unknown_region (alloc_node_id (), &m_root_region, type);
  if (ctxt)
    ctxt->on_unexpected_tree_code (t, loc);
  return new_reg;
}

/* Return a region describing a heap-allocated block of memory.
   Reuse an existing heap_allocated_region is its id is not within
   BASE_REGS_IN_USE.  */

const region *
region_model_manager::
get_or_create_region_for_heap_alloc (const bitmap &base_regs_in_use)
{
  /* Try to reuse an existing region, if it's unreferenced in the
     client state.  */
  for (auto existing_reg : m_managed_dynamic_regions)
    if (!bitmap_bit_p (base_regs_in_use, existing_reg->get_id ()))
      if (existing_reg->get_kind () == RK_HEAP_ALLOCATED)
	return existing_reg;

  /* All existing ones (if any) are in use; create a new one.  */
  region *reg
    = new heap_allocated_region (alloc_node_id (), &m_heap_region);
  m_managed_dynamic_regions.safe_push (reg);
  return reg;
}

/* Return a new region describing a block of memory allocated within FRAME.  */

const region *
region_model_manager::create_region_for_alloca (const frame_region *frame)
{
  gcc_assert (frame);
  region *reg = new alloca_region (alloc_node_id (), frame);
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
  logger->log ("  # %s: %li", title, (long)uniq_map.elements ());
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

template <typename K, typename T>
static void
log_uniq_map (logger *logger, bool show_objs, const char *title,
	      const std::map<K, T*> &map)
{
  logger->log ("  # %s: %li", title, (long)map.size ());
  if (!show_objs)
    return;

  for (auto &iter : map)
    log_managed_object<T> (logger, iter.second);
}

template <typename T>
static void
log_uniq_map (logger *logger, bool show_objs, const char *title,
	      const consolidation_map<T> &map)
{
  logger->log ("  # %s: %li", title, (long)map.elements ());
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
  logger->log ("call string consolidation");
  m_empty_call_string.recursive_log (logger);
  logger->log ("svalue consolidation");
  log_uniq_map (logger, show_objs, "constant_svalue", m_constants_map);
  log_uniq_map (logger, show_objs, "unknown_svalue", m_unknowns_map);
  if (m_unknown_NULL)
    log_managed_object (logger, m_unknown_NULL);
  log_uniq_map (logger, show_objs, "poisoned_svalue", m_poisoned_values_map);
  log_uniq_map (logger, show_objs, "setjmp_svalue", m_setjmp_values_map);
  log_uniq_map (logger, show_objs, "initial_svalue", m_initial_values_map);
  log_uniq_map (logger, show_objs, "pointer_svalue", m_pointer_values_map);
  log_uniq_map (logger, show_objs, "unaryop_svalue", m_unaryop_values_map);
  log_uniq_map (logger, show_objs, "binop_svalue", m_binop_values_map);
  log_uniq_map (logger, show_objs, "svalue_from_bit_vector", m_bit_vector_to_svalue_map);
  log_uniq_map (logger, show_objs, "unmergeable_svalue",
		m_unmergeable_values_map);
  log_uniq_map (logger, show_objs, "widening_svalue", m_widening_values_map);
  log_uniq_map (logger, show_objs, "asm_output_svalue",
		m_asm_output_values_map);
  log_uniq_map (logger, show_objs, "const_fn_result_svalue",
		m_const_fn_result_values_map);

  logger->log ("max accepted svalue num_nodes: %i",
	       m_max_complexity.m_num_nodes);
  logger->log ("max accepted svalue max_depth: %i",
	       m_max_complexity.m_max_depth);

  logger->log ("region consolidation");
  log_uniq_map (logger, show_objs, "function_region", m_fndecls_map);
  log_uniq_map (logger, show_objs, "label_region", m_labels_map);
  log_uniq_map (logger, show_objs, "decl_region for globals", m_globals_map);
  log_uniq_map (logger, show_objs, "field_region", m_field_regions);
  log_uniq_map (logger, show_objs, "element_region", m_element_regions);
  log_uniq_map (logger, show_objs, "offset_region", m_offset_regions);
  log_uniq_map (logger, show_objs, "sized_region", m_sized_regions);
  log_uniq_map (logger, show_objs, "cast_region", m_cast_regions);
  log_uniq_map (logger, show_objs, "frame_region", m_frame_regions);
  log_uniq_map (logger, show_objs, "symbolic_region", m_symbolic_regions);
  log_uniq_map (logger, show_objs, "string_region", m_string_map);
  log_uniq_map (logger, show_objs, "bit_range_region", m_bit_range_regions);
  log_uniq_map (logger, show_objs, "var_arg_region", m_var_arg_regions);
  logger->log ("  # managed dynamic regions: %i",
	       m_managed_dynamic_regions.length ());
  m_range_mgr->log_stats (logger, show_objs);
}

/* Emit a warning showing DECL_REG->tracked_p () for use in DejaGnu tests
   (using -fdump-analyzer-untracked).  */

static void
dump_untracked_region (const decl_region *decl_reg)
{
  tree decl = decl_reg->get_decl ();
  if (TREE_CODE (decl) != VAR_DECL)
    return;
  /* For now, don't emit the status of decls in the constant pool, to avoid
     differences in DejaGnu test results between targets that use these vs
     those that don't.
     (Eventually these decls should probably be untracked and we should test
     for that, but that's not stage 4 material).  */
  if (DECL_IN_CONSTANT_POOL (decl))
    return;
  warning_at (DECL_SOURCE_LOCATION (decl), 0,
	      "track %qD: %s",
	      decl, (decl_reg->tracked_p () ? "yes" : "no"));
}

/* Implementation of -fdump-analyzer-untracked.  */

void
region_model_manager::dump_untracked_regions () const
{
  for (auto iter : m_globals_map)
    {
      const decl_region *decl_reg = iter.second;
      dump_untracked_region (decl_reg);
    }
  for (auto frame_iter : m_frame_regions)
    {
      const frame_region *frame_reg = frame_iter.second;
      frame_reg->dump_untracked_regions ();
    }
}

void
frame_region::dump_untracked_regions () const
{
  for (auto iter : m_locals)
    {
      const decl_region *decl_reg = iter.second;
      dump_untracked_region (decl_reg);
    }
}

#if CHECKING_P

namespace selftest {

static void
test_svalue_from_bit_vector ()
{
  symbolic::node_manager node_mgr;
  auto &bv_mgr = node_mgr.get_bit_vector_manager ();
  auto &rmm = node_mgr.get_region_model_manager ();

  /* Folding the bits of a constant.  */
  ASSERT_SAME_SYMBOLIC_NODE
    (*rmm.get_svalue_from_bit_vector (char_type_node,
				      bv_mgr.get_all_zeros (1_byte)),
     *rmm.get_or_create_int_cst (char_type_node, 0));

  /* Round-tripping an svalue through a bit_vector.  */
  {
    placeholder_svalue x (node_mgr.alloc_node_id (),
			  integer_type_node, "x");
    auto &bv_from_x = bv_mgr.get_bit_vector_from_svalue (x);
    /* With matching types: SVAL_FROM_BITS(typeof(X), BITS_FROM_SVAL(X))
       should fold to just X.  */
    ASSERT_SAME_SYMBOLIC_NODE
      (*rmm.get_svalue_from_bit_vector (integer_type_node, bv_from_x),
       x);

    /* With different types of the same size,
       SVAL_FROM_BITS(OTHER_TYPE, BITS_FROM_SVAL(X))
       should fold to CAST_SVAL (OTHER_TYPE, X).  */
    ASSERT_SAME_SYMBOLIC_NODE
      (*rmm.get_svalue_from_bit_vector (unsigned_type_node, bv_from_x),
       *rmm.get_or_create_cast (unsigned_type_node, &x));
  }

  /* Round-tripping a _Bool cropped to its initial bit
     (as if read from a region with 1-bit precision).  */
  {
    placeholder_svalue b (node_mgr.alloc_node_id (), boolean_type_node, "b");
    auto &bv = bv_mgr.get_bit_vector_from_svalue (b);
    auto &cropped_bv
      = bv.get_cropped_bits (concrete_bit_range (0_bits, 1_bit));
    auto *sval
      = rmm.get_svalue_from_bit_vector (boolean_type_node, cropped_bv);
    ASSERT_SAME_SYMBOLIC_NODE (b, *sval);
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_region_model_manager_cc_tests ()
{
  test_svalue_from_bit_vector ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
