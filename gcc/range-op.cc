/* Code for range operators.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "value-relation.h"
#include "range-op.h"
#include "tree-ssa-ccp.h"
#include "range-op-mixed.h"

// Instantiate the operators which apply to multiple types here.

operator_equal op_equal;
operator_not_equal op_not_equal;
operator_lt op_lt;
operator_le op_le;
operator_gt op_gt;
operator_ge op_ge;
operator_identity op_ident;
operator_cst op_cst;
operator_cast op_cast;
operator_plus op_plus;
operator_abs op_abs;
operator_minus op_minus;
operator_negate op_negate;
operator_mult op_mult;
operator_addr_expr op_addr;
operator_bitwise_not op_bitwise_not;
operator_bitwise_xor op_bitwise_xor;
operator_bitwise_and op_bitwise_and;
operator_bitwise_or op_bitwise_or;
operator_min op_min;
operator_max op_max;

// Instantaite a range operator table.
range_op_table operator_table;

// Invoke the initialization routines for each class of range.

range_op_table::range_op_table ()
{
  initialize_integral_ops ();
  initialize_pointer_ops ();
  initialize_float_ops ();

  set (EQ_EXPR, op_equal);
  set (NE_EXPR, op_not_equal);
  set (LT_EXPR, op_lt);
  set (LE_EXPR, op_le);
  set (GT_EXPR, op_gt);
  set (GE_EXPR, op_ge);
  set (SSA_NAME, op_ident);
  set (PAREN_EXPR, op_ident);
  set (OBJ_TYPE_REF, op_ident);
  set (REAL_CST, op_cst);
  set (INTEGER_CST, op_cst);
  set (NOP_EXPR, op_cast);
  set (CONVERT_EXPR, op_cast);
  set (PLUS_EXPR, op_plus);
  set (ABS_EXPR, op_abs);
  set (MINUS_EXPR, op_minus);
  set (NEGATE_EXPR, op_negate);
  set (MULT_EXPR, op_mult);

  // Occur in both integer and pointer tables, but currently share
  // integral implementation.
  set (ADDR_EXPR, op_addr);
  set (BIT_NOT_EXPR, op_bitwise_not);
  set (BIT_XOR_EXPR, op_bitwise_xor);

  // These are in both integer and pointer tables, but pointer has a different
  // implementation.
  // If commented out, there is a hybrid version in range-op-ptr.cc which
  // is used until there is a pointer range class.  Then we can simply
  // uncomment the operator here and use the unified version.

  // set (BIT_AND_EXPR, op_bitwise_and);
  // set (BIT_IOR_EXPR, op_bitwise_or);
  // set (MIN_EXPR, op_min);
  // set (MAX_EXPR, op_max);
}

// Instantiate a default range operator for opcodes with no entry.

range_operator default_operator;

// Create a default range_op_handler.

range_op_handler::range_op_handler ()
{
  m_operator = &default_operator;
}

// Create a range_op_handler for CODE.  Use a default operatoer if CODE
// does not have an entry.

range_op_handler::range_op_handler (unsigned code)
{
  m_operator = operator_table[code];
  if (!m_operator)
    m_operator = &default_operator;
}

// Return TRUE if this handler has a non-default operator.

range_op_handler::operator bool () const
{
  return m_operator != &default_operator;
}

// Return a pointer to the range operator assocaited with this handler.
// If it is a default operator, return NULL.
// This is the equivalent of indexing the range table.

range_operator *
range_op_handler::range_op () const
{
  if (m_operator != &default_operator)
    return m_operator;
  return NULL;
}

// Create a dispatch pattern for value range discriminators LHS, OP1, and OP2.
// This is used to produce a unique value for each dispatch pattern.  Shift
// values are based on the size of the m_discriminator field in value_range.h.

constexpr unsigned
dispatch_trio (unsigned lhs, unsigned op1, unsigned op2)
{
  return ((lhs << 8) + (op1 << 4) + (op2));
}

// These are the supported dispatch patterns. These map to the parameter list
// of the routines in range_operator.  Note the last 3 characters are
// shorthand for the LHS, OP1, and OP2 range discriminator class.

const unsigned RO_III =	dispatch_trio (VR_IRANGE, VR_IRANGE, VR_IRANGE);
const unsigned RO_IFI = dispatch_trio (VR_IRANGE, VR_FRANGE, VR_IRANGE);
const unsigned RO_IFF = dispatch_trio (VR_IRANGE, VR_FRANGE, VR_FRANGE);
const unsigned RO_FFF = dispatch_trio (VR_FRANGE, VR_FRANGE, VR_FRANGE);
const unsigned RO_FIF = dispatch_trio (VR_FRANGE, VR_IRANGE, VR_FRANGE);
const unsigned RO_FII = dispatch_trio (VR_FRANGE, VR_IRANGE, VR_IRANGE);

// Return a dispatch value for parameter types LHS, OP1 and OP2.

unsigned
range_op_handler::dispatch_kind (const vrange &lhs, const vrange &op1,
				 const vrange& op2) const
{
  return dispatch_trio (lhs.m_discriminator, op1.m_discriminator,
			op2.m_discriminator);
}

// Dispatch a call to fold_range based on the types of R, LH and RH.

bool
range_op_handler::fold_range (vrange &r, tree type,
			      const vrange &lh,
			      const vrange &rh,
			      relation_trio rel) const
{
  gcc_checking_assert (m_operator);
#if CHECKING_P
  if (!lh.undefined_p () && !rh.undefined_p ())
    gcc_assert (m_operator->operand_check_p (type, lh.type (), rh.type ()));
#endif
  switch (dispatch_kind (r, lh, rh))
    {
      case RO_III:
	return m_operator->fold_range (as_a <irange> (r), type,
				       as_a <irange> (lh),
				       as_a <irange> (rh), rel);
      case RO_IFI:
	return m_operator->fold_range (as_a <irange> (r), type,
				       as_a <frange> (lh),
				       as_a <irange> (rh), rel);
      case RO_IFF:
	return m_operator->fold_range (as_a <irange> (r), type,
				       as_a <frange> (lh),
				       as_a <frange> (rh), rel);
      case RO_FFF:
	return m_operator->fold_range (as_a <frange> (r), type,
				       as_a <frange> (lh),
				       as_a <frange> (rh), rel);
      case RO_FII:
	return m_operator->fold_range (as_a <frange> (r), type,
				       as_a <irange> (lh),
				       as_a <irange> (rh), rel);
      default:
	return false;
    }
}

// Dispatch a call to op1_range based on the types of R, LHS and OP2.

bool
range_op_handler::op1_range (vrange &r, tree type,
			     const vrange &lhs,
			     const vrange &op2,
			     relation_trio rel) const
{
  gcc_checking_assert (m_operator);
  if (lhs.undefined_p ())
    return false;
#if CHECKING_P
  if (!op2.undefined_p ())
    gcc_assert (m_operator->operand_check_p (lhs.type (), type, op2.type ()));
#endif
  switch (dispatch_kind (r, lhs, op2))
    {
      case RO_III:
	return m_operator->op1_range (as_a <irange> (r), type,
				      as_a <irange> (lhs),
				      as_a <irange> (op2), rel);
      case RO_FIF:
	return m_operator->op1_range (as_a <frange> (r), type,
				      as_a <irange> (lhs),
				      as_a <frange> (op2), rel);
      case RO_FFF:
	return m_operator->op1_range (as_a <frange> (r), type,
				      as_a <frange> (lhs),
				      as_a <frange> (op2), rel);
      default:
	return false;
    }
}

// Dispatch a call to op2_range based on the types of R, LHS and OP1.

bool
range_op_handler::op2_range (vrange &r, tree type,
			     const vrange &lhs,
			     const vrange &op1,
			     relation_trio rel) const
{
  gcc_checking_assert (m_operator);
  if (lhs.undefined_p ())
    return false;
#if CHECKING_P
  if (!op1.undefined_p ())
    gcc_assert (m_operator->operand_check_p (lhs.type (), op1.type (), type));
#endif
  switch (dispatch_kind (r, lhs, op1))
    {
      case RO_III:
	return m_operator->op2_range (as_a <irange> (r), type,
				      as_a <irange> (lhs),
				      as_a <irange> (op1), rel);
      case RO_FIF:
	return m_operator->op2_range (as_a <frange> (r), type,
				      as_a <irange> (lhs),
				      as_a <frange> (op1), rel);
      case RO_FFF:
	return m_operator->op2_range (as_a <frange> (r), type,
				      as_a <frange> (lhs),
				      as_a <frange> (op1), rel);
      default:
	return false;
    }
}

// Dispatch a call to lhs_op1_relation based on the types of LHS, OP1 and OP2.

relation_kind
range_op_handler::lhs_op1_relation (const vrange &lhs,
				    const vrange &op1,
				    const vrange &op2,
				    relation_kind rel) const
{
  gcc_checking_assert (m_operator);

  switch (dispatch_kind (lhs, op1, op2))
    {
      case RO_III:
	return m_operator->lhs_op1_relation (as_a <irange> (lhs),
					     as_a <irange> (op1),
					     as_a <irange> (op2), rel);
      case RO_IFF:
	return m_operator->lhs_op1_relation (as_a <irange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2), rel);
      case RO_FFF:
	return m_operator->lhs_op1_relation (as_a <frange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2), rel);
      default:
	return VREL_VARYING;
    }
}

// Dispatch a call to lhs_op2_relation based on the types of LHS, OP1 and OP2.

relation_kind
range_op_handler::lhs_op2_relation (const vrange &lhs,
				    const vrange &op1,
				    const vrange &op2,
				    relation_kind rel) const
{
  gcc_checking_assert (m_operator);
  switch (dispatch_kind (lhs, op1, op2))
    {
      case RO_III:
	return m_operator->lhs_op2_relation (as_a <irange> (lhs),
					     as_a <irange> (op1),
					     as_a <irange> (op2), rel);
      case RO_IFF:
	return m_operator->lhs_op2_relation (as_a <irange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2), rel);
      case RO_FFF:
	return m_operator->lhs_op2_relation (as_a <frange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2), rel);
      default:
	return VREL_VARYING;
    }
}

// Dispatch a call to op1_op2_relation based on the type of LHS.

relation_kind
range_op_handler::op1_op2_relation (const vrange &lhs,
				    const vrange &op1,
				    const vrange &op2) const
{
  gcc_checking_assert (m_operator);
  switch (dispatch_kind (lhs, op1, op2))
    {
      case RO_III:
	return m_operator->op1_op2_relation (as_a <irange> (lhs),
					     as_a <irange> (op1),
					     as_a <irange> (op2));

      case RO_IFF:
	return m_operator->op1_op2_relation (as_a <irange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2));

      case RO_FFF:
	return m_operator->op1_op2_relation (as_a <frange> (lhs),
					     as_a <frange> (op1),
					     as_a <frange> (op2));

      default:
	return VREL_VARYING;
    }
}

bool
range_op_handler::overflow_free_p (const vrange &lh,
				   const vrange &rh,
				   relation_trio rel) const
{
  gcc_checking_assert (m_operator);
  switch (dispatch_kind (lh, lh, rh))
    {
      case RO_III:
	return m_operator->overflow_free_p(as_a <irange> (lh),
					   as_a <irange> (rh),
					   rel);
      default:
	return false;
    }
}

bool
range_op_handler::operand_check_p (tree t1, tree t2, tree t3) const
{
  gcc_checking_assert (m_operator);
  return m_operator->operand_check_p (t1, t2, t3);
}

// Update the known bitmasks in R when applying the operation CODE to
// LH and RH.

void
update_known_bitmask (irange &r, tree_code code,
		      const irange &lh, const irange &rh)
{
  if (r.undefined_p () || lh.undefined_p () || rh.undefined_p ()
      || r.singleton_p ())
    return;

  widest_int widest_value, widest_mask;
  tree type = r.type ();
  signop sign = TYPE_SIGN (type);
  int prec = TYPE_PRECISION (type);
  irange_bitmask lh_bits = lh.get_bitmask ();
  irange_bitmask rh_bits = rh.get_bitmask ();

  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_UNARY_RHS:
      bit_value_unop (code, sign, prec, &widest_value, &widest_mask,
		      TYPE_SIGN (lh.type ()),
		      TYPE_PRECISION (lh.type ()),
		      widest_int::from (lh_bits.value (),
					TYPE_SIGN (lh.type ())),
		      widest_int::from (lh_bits.mask (),
					TYPE_SIGN (lh.type ())));
      break;
    case GIMPLE_BINARY_RHS:
      bit_value_binop (code, sign, prec, &widest_value, &widest_mask,
		       TYPE_SIGN (lh.type ()),
		       TYPE_PRECISION (lh.type ()),
		       widest_int::from (lh_bits.value (), sign),
		       widest_int::from (lh_bits.mask (), sign),
		       TYPE_SIGN (rh.type ()),
		       TYPE_PRECISION (rh.type ()),
		       widest_int::from (rh_bits.value (), sign),
		       widest_int::from (rh_bits.mask (), sign));
      break;
    default:
      gcc_unreachable ();
    }

  wide_int mask = wide_int::from (widest_mask, prec, sign);
  wide_int value = wide_int::from (widest_value, prec, sign);
  // Bitmasks must have the unknown value bits cleared.
  value &= ~mask;
  irange_bitmask bm (value, mask);
  r.update_bitmask (bm);
}

// Return the upper limit for a type.

static inline wide_int
max_limit (const_tree type)
{
  return irange_val_max (type);
}

// Return the lower limit for a type.

static inline wide_int
min_limit (const_tree type)
{
  return irange_val_min (type);
}

// Return false if shifting by OP is undefined behavior.  Otherwise, return
// true and the range it is to be shifted by.  This allows trimming out of
// undefined ranges, leaving only valid ranges if there are any.

static inline bool
get_shift_range (irange &r, tree type, const irange &op)
{
  if (op.undefined_p ())
    return false;

  // Build valid range and intersect it with the shift range.
  r = value_range (op.type (),
		   wi::shwi (0, TYPE_PRECISION (op.type ())),
		   wi::shwi (TYPE_PRECISION (type) - 1, TYPE_PRECISION (op.type ())));
  r.intersect (op);

  // If there are no valid ranges in the shift range, returned false.
  if (r.undefined_p ())
    return false;
  return true;
}

// Default wide_int fold operation returns [MIN, MAX].

void
range_operator::wi_fold (irange &r, tree type,
			 const wide_int &lh_lb ATTRIBUTE_UNUSED,
			 const wide_int &lh_ub ATTRIBUTE_UNUSED,
			 const wide_int &rh_lb ATTRIBUTE_UNUSED,
			 const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  gcc_checking_assert (r.supports_type_p (type));
  r.set_varying (type);
}

// Call wi_fold when both op1 and op2 are equivalent. Further split small
// subranges into constants.  This can provide better precision.
// For x + y,  when x == y with a range of [0,4] instead of [0, 8] produce
// [0,0][2, 2][4,4][6, 6][8, 8]
// LIMIT is the maximum number of elements in range allowed before we
// do not process them individually.

void
range_operator::wi_fold_in_parts_equiv (irange &r, tree type,
					const wide_int &lh_lb,
					const wide_int &lh_ub,
					unsigned limit) const
{
  int_range_max tmp;
  widest_int lh_range = wi::sub (widest_int::from (lh_ub, TYPE_SIGN (type)),
				 widest_int::from (lh_lb, TYPE_SIGN (type)));
  // if there are 1 to 8 values in the LH range, split them up.
  r.set_undefined ();
  if (lh_range >= 0 && lh_range < limit)
    {
      for (unsigned x = 0; x <= lh_range; x++)
	{
	  wide_int val = lh_lb + x;
	  wi_fold (tmp, type, val, val, val, val);
	  r.union_ (tmp);
	}
    }
  // Otherwise just call wi_fold.
  else
    wi_fold (r, type, lh_lb, lh_ub, lh_lb, lh_ub);
}

// Call wi_fold, except further split small subranges into constants.
// This can provide better precision. For something   8 >> [0,1]
// Instead of [8, 16], we will produce [8,8][16,16]

void
range_operator::wi_fold_in_parts (irange &r, tree type,
				  const wide_int &lh_lb,
				  const wide_int &lh_ub,
				  const wide_int &rh_lb,
				  const wide_int &rh_ub) const
{
  int_range_max tmp;
  widest_int rh_range = wi::sub (widest_int::from (rh_ub, TYPE_SIGN (type)),
				 widest_int::from (rh_lb, TYPE_SIGN (type)));
  widest_int lh_range = wi::sub (widest_int::from (lh_ub, TYPE_SIGN (type)),
				 widest_int::from (lh_lb, TYPE_SIGN (type)));
  // If there are 2, 3, or 4 values in the RH range, do them separately.
  // Call wi_fold_in_parts to check the RH side.
  if (rh_range > 0 && rh_range < 4)
    {
      wi_fold_in_parts (r, type, lh_lb, lh_ub, rh_lb, rh_lb);
      if (rh_range > 1)
	{
	  wi_fold_in_parts (tmp, type, lh_lb, lh_ub, rh_lb + 1, rh_lb + 1);
	  r.union_ (tmp);
	  if (rh_range == 3)
	    {
	      wi_fold_in_parts (tmp, type, lh_lb, lh_ub, rh_lb + 2, rh_lb + 2);
	      r.union_ (tmp);
	    }
	}
      wi_fold_in_parts (tmp, type, lh_lb, lh_ub, rh_ub, rh_ub);
      r.union_ (tmp);
    }
  // Otherwise check for 2, 3, or 4 values in the LH range and split them up.
  // The RH side has been checked, so no recursion needed.
  else if (lh_range > 0 && lh_range < 4)
    {
      wi_fold (r, type, lh_lb, lh_lb, rh_lb, rh_ub);
      if (lh_range > 1)
	{
	  wi_fold (tmp, type, lh_lb + 1, lh_lb + 1, rh_lb, rh_ub);
	  r.union_ (tmp);
	  if (lh_range == 3)
	    {
	      wi_fold (tmp, type, lh_lb + 2, lh_lb + 2, rh_lb, rh_ub);
	      r.union_ (tmp);
	    }
	}
      wi_fold (tmp, type, lh_ub, lh_ub, rh_lb, rh_ub);
      r.union_ (tmp);
    }
  // Otherwise just call wi_fold.
  else
    wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
}

// The default for fold is to break all ranges into sub-ranges and
// invoke the wi_fold method on each sub-range pair.

bool
range_operator::fold_range (irange &r, tree type,
			    const irange &lh,
			    const irange &rh,
			    relation_trio trio) const
{
  gcc_checking_assert (r.supports_type_p (type));
  if (empty_range_varying (r, type, lh, rh))
    return true;

  relation_kind rel = trio.op1_op2 ();
  unsigned num_lh = lh.num_pairs ();
  unsigned num_rh = rh.num_pairs ();

  // If op1 and op2 are equivalences, then we don't need a complete cross
  // product, just pairs of matching elements.
  if (relation_equiv_p (rel) && lh == rh)
    {
      int_range_max tmp;
      r.set_undefined ();
      for (unsigned x = 0; x < num_lh; ++x)
	{
	  // If the number of subranges is too high, limit subrange creation.
	  unsigned limit = (r.num_pairs () > 32) ? 0 : 8;
	  wide_int lh_lb = lh.lower_bound (x);
	  wide_int lh_ub = lh.upper_bound (x);
	  wi_fold_in_parts_equiv (tmp, type, lh_lb, lh_ub, limit);
	  r.union_ (tmp);
	  if (r.varying_p ())
	    break;
	}
      op1_op2_relation_effect (r, type, lh, rh, rel);
      update_bitmask (r, lh, rh);
      return true;
    }

  // If both ranges are single pairs, fold directly into the result range.
  // If the number of subranges grows too high, produce a summary result as the
  // loop becomes exponential with little benefit.  See PR 103821.
  if ((num_lh == 1 && num_rh == 1) || num_lh * num_rh > 12)
    {
      wi_fold_in_parts (r, type, lh.lower_bound (), lh.upper_bound (),
			rh.lower_bound (), rh.upper_bound ());
      op1_op2_relation_effect (r, type, lh, rh, rel);
      update_bitmask (r, lh, rh);
      return true;
    }

  int_range_max tmp;
  r.set_undefined ();
  for (unsigned x = 0; x < num_lh; ++x)
    for (unsigned y = 0; y < num_rh; ++y)
      {
	wide_int lh_lb = lh.lower_bound (x);
	wide_int lh_ub = lh.upper_bound (x);
	wide_int rh_lb = rh.lower_bound (y);
	wide_int rh_ub = rh.upper_bound (y);
	wi_fold_in_parts (tmp, type, lh_lb, lh_ub, rh_lb, rh_ub);
	r.union_ (tmp);
	if (r.varying_p ())
	  {
	    op1_op2_relation_effect (r, type, lh, rh, rel);
	    update_bitmask (r, lh, rh);
	    return true;
	  }
      }
  op1_op2_relation_effect (r, type, lh, rh, rel);
  update_bitmask (r, lh, rh);
  return true;
}

// The default for op1_range is to return false.

bool
range_operator::op1_range (irange &r ATTRIBUTE_UNUSED,
			   tree type ATTRIBUTE_UNUSED,
			   const irange &lhs ATTRIBUTE_UNUSED,
			   const irange &op2 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

// The default for op2_range is to return false.

bool
range_operator::op2_range (irange &r ATTRIBUTE_UNUSED,
			   tree type ATTRIBUTE_UNUSED,
			   const irange &lhs ATTRIBUTE_UNUSED,
			   const irange &op1 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

// The default relation routines return VREL_VARYING.

relation_kind
range_operator::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
				  const irange &op1 ATTRIBUTE_UNUSED,
				  const irange &op2 ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
				  const irange &op1 ATTRIBUTE_UNUSED,
				  const irange &op2 ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::op1_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
				  const irange &op1 ATTRIBUTE_UNUSED,
				  const irange &op2 ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

// Default is no relation affects the LHS.

bool
range_operator::op1_op2_relation_effect (irange &lhs_range ATTRIBUTE_UNUSED,
				       tree type ATTRIBUTE_UNUSED,
				       const irange &op1_range ATTRIBUTE_UNUSED,
				       const irange &op2_range ATTRIBUTE_UNUSED,
				       relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator::overflow_free_p (const irange &, const irange &,
				 relation_trio) const
{
  return false;
}

// Apply any known bitmask updates based on this operator.

void
range_operator::update_bitmask (irange &, const irange &,
				       const irange &) const
{
}

// Check that operand types are OK.  Default to always OK.

bool
range_operator::operand_check_p (tree, tree, tree) const
{
  return true;
}

// Create and return a range from a pair of wide-ints that are known
// to have overflowed (or underflowed).

static void
value_range_from_overflowed_bounds (irange &r, tree type,
				    const wide_int &wmin,
				    const wide_int &wmax)
{
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);

  wide_int tmin = wide_int::from (wmin, prec, sgn);
  wide_int tmax = wide_int::from (wmax, prec, sgn);

  bool covers = false;
  wide_int tem = tmin;
  tmin = tmax + 1;
  if (wi::cmp (tmin, tmax, sgn) < 0)
    covers = true;
  tmax = tem - 1;
  if (wi::cmp (tmax, tem, sgn) > 0)
    covers = true;

  // If the anti-range would cover nothing, drop to varying.
  // Likewise if the anti-range bounds are outside of the types
  // values.
  if (covers || wi::cmp (tmin, tmax, sgn) > 0)
    r.set_varying (type);
  else
    r.set (type, tmin, tmax, VR_ANTI_RANGE);
}

// Create and return a range from a pair of wide-ints.  MIN_OVF and
// MAX_OVF describe any overflow that might have occurred while
// calculating WMIN and WMAX respectively.

static void
value_range_with_overflow (irange &r, tree type,
			   const wide_int &wmin, const wide_int &wmax,
			   wi::overflow_type min_ovf = wi::OVF_NONE,
			   wi::overflow_type max_ovf = wi::OVF_NONE)
{
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);
  const bool overflow_wraps = TYPE_OVERFLOW_WRAPS (type);

  // For one bit precision if max != min, then the range covers all
  // values.
  if (prec == 1 && wi::ne_p (wmax, wmin))
    {
      r.set_varying (type);
      return;
    }

  if (overflow_wraps)
    {
      // If overflow wraps, truncate the values and adjust the range,
      // kind, and bounds appropriately.
      if ((min_ovf != wi::OVF_NONE) == (max_ovf != wi::OVF_NONE))
	{
	  wide_int tmin = wide_int::from (wmin, prec, sgn);
	  wide_int tmax = wide_int::from (wmax, prec, sgn);
	  // If the limits are swapped, we wrapped around and cover
	  // the entire range.
	  if (wi::gt_p (tmin, tmax, sgn))
	    r.set_varying (type);
	  else
	    // No overflow or both overflow or underflow.  The range
	    // kind stays normal.
	    r.set (type, tmin, tmax);
	  return;
	}

      if ((min_ovf == wi::OVF_UNDERFLOW && max_ovf == wi::OVF_NONE)
	  || (max_ovf == wi::OVF_OVERFLOW && min_ovf == wi::OVF_NONE))
	value_range_from_overflowed_bounds (r, type, wmin, wmax);
      else
	// Other underflow and/or overflow, drop to VR_VARYING.
	r.set_varying (type);
    }
  else
    {
      // If both bounds either underflowed or overflowed, then the result
      // is undefined.
      if ((min_ovf == wi::OVF_OVERFLOW && max_ovf == wi::OVF_OVERFLOW)
	  || (min_ovf == wi::OVF_UNDERFLOW && max_ovf == wi::OVF_UNDERFLOW))
	{
	  r.set_undefined ();
	  return;
	}

      // If overflow does not wrap, saturate to [MIN, MAX].
      wide_int new_lb, new_ub;
      if (min_ovf == wi::OVF_UNDERFLOW)
	new_lb = wi::min_value (prec, sgn);
      else if (min_ovf == wi::OVF_OVERFLOW)
	new_lb = wi::max_value (prec, sgn);
      else
        new_lb = wmin;

      if (max_ovf == wi::OVF_UNDERFLOW)
	new_ub = wi::min_value (prec, sgn);
      else if (max_ovf == wi::OVF_OVERFLOW)
	new_ub = wi::max_value (prec, sgn);
      else
        new_ub = wmax;

      r.set (type, new_lb, new_ub);
    }
}

// Create and return a range from a pair of wide-ints.  Canonicalize
// the case where the bounds are swapped.  In which case, we transform
// [10,5] into [MIN,5][10,MAX].

static inline void
create_possibly_reversed_range (irange &r, tree type,
				const wide_int &new_lb, const wide_int &new_ub)
{
  signop s = TYPE_SIGN (type);
  // If the bounds are swapped, treat the result as if an overflow occurred.
  if (wi::gt_p (new_lb, new_ub, s))
    value_range_from_overflowed_bounds (r, type, new_lb, new_ub);
  else
    // Otherwise it's just a normal range.
    r.set (type, new_lb, new_ub);
}

// Return the summary information about boolean range LHS.  If EMPTY/FULL,
// return the equivalent range for TYPE in R; if FALSE/TRUE, do nothing.

bool_range_state
get_bool_state (vrange &r, const vrange &lhs, tree val_type)
{
  // If there is no result, then this is unexecutable.
  if (lhs.undefined_p ())
    {
      r.set_undefined ();
      return BRS_EMPTY;
    }

  if (lhs.zero_p ())
    return BRS_FALSE;

  // For TRUE, we can't just test for [1,1] because Ada can have
  // multi-bit booleans, and TRUE values can be: [1, MAX], ~[0], etc.
  if (lhs.contains_p (build_zero_cst (lhs.type ())))
    {
      r.set_varying (val_type);
      return BRS_FULL;
    }

  return BRS_TRUE;
}

// ------------------------------------------------------------------------

void
operator_equal::update_bitmask (irange &r, const irange &lh,
				const irange &rh) const
{
  update_known_bitmask (r, EQ_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_equal::op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 == op2 indicates NE_EXPR.
  if (lhs.zero_p ())
    return VREL_NE;

  // TRUE = op1 == op2 indicates EQ_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_EQ;
  return VREL_VARYING;
}

bool
operator_equal::fold_range (irange &r, tree type,
			    const irange &op1,
			    const irange &op2,
			    relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_EQ))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  bool op1_const = wi::eq_p (op1.lower_bound (), op1.upper_bound ());
  bool op2_const = wi::eq_p (op2.lower_bound (), op2.upper_bound ());
  if (op1_const && op2_const)
    {
      if (wi::eq_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true (type);
      else
	r = range_false (type);
    }
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      int_range_max tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_false (type);
      // Check if a constant cannot satisfy the bitmask requirements.
      else if (op2_const && !op1.get_bitmask ().member_p (op2.lower_bound ()))
	 r = range_false (type);
      else if (op1_const && !op2.get_bitmask ().member_p (op1.lower_bound ()))
	 r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_equal::op1_range (irange &r, tree type,
			   const irange &lhs,
			   const irange &op2,
			   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If it's true, the result is the same as OP2.
      r = op2;
      break;

    case BRS_FALSE:
      // If the result is false, the only time we know anything is
      // if OP2 is a constant.
      if (!op2.undefined_p ()
	  && wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_equal::op2_range (irange &r, tree type,
			   const irange &lhs,
			   const irange &op1,
			   relation_trio rel) const
{
  return operator_equal::op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

// -------------------------------------------------------------------------

void
operator_not_equal::update_bitmask (irange &r, const irange &lh,
				    const irange &rh) const
{
  update_known_bitmask (r, NE_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_not_equal::op1_op2_relation (const irange &lhs, const irange &,
				      const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 != op2  indicates EQ_EXPR.
  if (lhs.zero_p ())
    return VREL_EQ;

  // TRUE = op1 != op2  indicates NE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_NE;
  return VREL_VARYING;
}

bool
operator_not_equal::fold_range (irange &r, tree type,
				const irange &op1,
				const irange &op2,
				relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_NE))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  bool op1_const = wi::eq_p (op1.lower_bound (), op1.upper_bound ());
  bool op2_const = wi::eq_p (op2.lower_bound (), op2.upper_bound ());
  if (op1_const && op2_const)
    {
      if (wi::ne_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true (type);
      else
	r = range_false (type);
    }
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      int_range_max tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_true (type);
      // Check if a constant cannot satisfy the bitmask requirements.
      else if (op2_const && !op1.get_bitmask ().member_p (op2.lower_bound ()))
	 r = range_true (type);
      else if (op1_const && !op2.get_bitmask ().member_p (op1.lower_bound ()))
	 r = range_true (type);
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_not_equal::op1_range (irange &r, tree type,
			       const irange &lhs,
			       const irange &op2,
			       relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If the result is true, the only time we know anything is if
      // OP2 is a constant.
      if (!op2.undefined_p ()
	  && wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    case BRS_FALSE:
      // If it's false, the result is the same as OP2.
      r = op2;
      break;

    default:
      break;
    }
  return true;
}


bool
operator_not_equal::op2_range (irange &r, tree type,
			       const irange &lhs,
			       const irange &op1,
			       relation_trio rel) const
{
  return operator_not_equal::op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

// (X < VAL) produces the range of [MIN, VAL - 1].

static void
build_lt (irange &r, tree type, const wide_int &val)
{
  wi::overflow_type ov;
  wide_int lim;
  signop sgn = TYPE_SIGN (type);

  // Signed 1 bit cannot represent 1 for subtraction.
  if (sgn == SIGNED)
    lim = wi::add (val, -1, sgn, &ov);
  else
    lim = wi::sub (val, 1, sgn, &ov);

  // If val - 1 underflows, check if X < MIN, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r = int_range<1> (type, min_limit (type), lim);
}

// (X <= VAL) produces the range of [MIN, VAL].

static void
build_le (irange &r, tree type, const wide_int &val)
{
  r = int_range<1> (type, min_limit (type), val);
}

// (X > VAL) produces the range of [VAL + 1, MAX].

static void
build_gt (irange &r, tree type, const wide_int &val)
{
  wi::overflow_type ov;
  wide_int lim;
  signop sgn = TYPE_SIGN (type);

  // Signed 1 bit cannot represent 1 for addition.
  if (sgn == SIGNED)
    lim = wi::sub (val, -1, sgn, &ov);
  else
    lim = wi::add (val, 1, sgn, &ov);
  // If val + 1 overflows, check is for X > MAX, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r = int_range<1> (type, lim, max_limit (type));
}

// (X >= val) produces the range of [VAL, MAX].

static void
build_ge (irange &r, tree type, const wide_int &val)
{
  r = int_range<1> (type, val, max_limit (type));
}


void
operator_lt::update_bitmask (irange &r, const irange &lh,
			     const irange &rh) const
{
  update_known_bitmask (r, LT_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_lt::op1_op2_relation (const irange &lhs, const irange &,
			       const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 < op2 indicates GE_EXPR.
  if (lhs.zero_p ())
    return VREL_GE;

  // TRUE = op1 < op2 indicates LT_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_LT;
  return VREL_VARYING;
}

bool
operator_lt::fold_range (irange &r, tree type,
			 const irange &op1,
			 const irange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_LT))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::lt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::lt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  // Use nonzero bits to determine if < 0 is false.
  else if (op2.zero_p () && !wi::neg_p (op1.get_nonzero_bits (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_lt::op1_range (irange &r, tree type,
			const irange &lhs,
			const irange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_lt (r, type, op2.upper_bound ());
      break;

    case BRS_FALSE:
      build_ge (r, type, op2.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_lt::op2_range (irange &r, tree type,
			const irange &lhs,
			const irange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_gt (r, type, op1.lower_bound ());
      break;

    case BRS_FALSE:
      build_le (r, type, op1.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}


void
operator_le::update_bitmask (irange &r, const irange &lh,
			     const irange &rh) const
{
  update_known_bitmask (r, LE_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_le::op1_op2_relation (const irange &lhs, const irange &,
			       const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 <= op2 indicates GT_EXPR.
  if (lhs.zero_p ())
    return VREL_GT;

  // TRUE = op1 <= op2 indicates LE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_LE;
  return VREL_VARYING;
}

bool
operator_le::fold_range (irange &r, tree type,
			 const irange &op1,
			 const irange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_LE))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::le_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::le_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_le::op1_range (irange &r, tree type,
			const irange &lhs,
			const irange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_le (r, type, op2.upper_bound ());
      break;

    case BRS_FALSE:
      build_gt (r, type, op2.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_le::op2_range (irange &r, tree type,
			const irange &lhs,
			const irange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_ge (r, type, op1.lower_bound ());
      break;

    case BRS_FALSE:
      build_lt (r, type, op1.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}


void
operator_gt::update_bitmask (irange &r, const irange &lh,
			     const irange &rh) const
{
  update_known_bitmask (r, GT_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_gt::op1_op2_relation (const irange &lhs, const irange &,
			       const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 > op2 indicates LE_EXPR.
  if (lhs.zero_p ())
    return VREL_LE;

  // TRUE = op1 > op2 indicates GT_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_GT;
  return VREL_VARYING;
}

bool
operator_gt::fold_range (irange &r, tree type,
			 const irange &op1, const irange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_GT))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::gt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::gt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_gt::op1_range (irange &r, tree type,
			const irange &lhs, const irange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_gt (r, type, op2.lower_bound ());
      break;

    case BRS_FALSE:
      build_le (r, type, op2.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_gt::op2_range (irange &r, tree type,
			const irange &lhs,
			const irange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_lt (r, type, op1.upper_bound ());
      break;

    case BRS_FALSE:
      build_ge (r, type, op1.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}


void
operator_ge::update_bitmask (irange &r, const irange &lh,
			     const irange &rh) const
{
  update_known_bitmask (r, GE_EXPR, lh, rh);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_ge::op1_op2_relation (const irange &lhs, const irange &,
			       const irange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 >= op2 indicates LT_EXPR.
  if (lhs.zero_p ())
    return VREL_LT;

  // TRUE = op1 >= op2 indicates GE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_GE;
  return VREL_VARYING;
}

bool
operator_ge::fold_range (irange &r, tree type,
			 const irange &op1,
			 const irange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_GE))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::ge_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::ge_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_ge::op1_range (irange &r, tree type,
			const irange &lhs,
			const irange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_ge (r, type, op2.lower_bound ());
      break;

    case BRS_FALSE:
      build_lt (r, type, op2.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_ge::op2_range (irange &r, tree type,
			const irange &lhs,
			const irange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_le (r, type, op1.upper_bound ());
      break;

    case BRS_FALSE:
      build_gt (r, type, op1.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}


void
operator_plus::update_bitmask (irange &r, const irange &lh,
			       const irange &rh) const
{
  update_known_bitmask (r, PLUS_EXPR, lh, rh);
}

// Check to see if the range of OP2 indicates anything about the relation
// between LHS and OP1.

relation_kind
operator_plus::lhs_op1_relation (const irange &lhs,
				 const irange &op1,
				 const irange &op2,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p () || op2.undefined_p ())
    return VREL_VARYING;

  tree type = lhs.type ();
  unsigned prec = TYPE_PRECISION (type);
  wi::overflow_type ovf1, ovf2;
  signop sign = TYPE_SIGN (type);

  // LHS = OP1 + 0  indicates LHS == OP1.
  if (op2.zero_p ())
    return VREL_EQ;

  if (TYPE_OVERFLOW_WRAPS (type))
    {
      wi::add (op1.lower_bound (), op2.lower_bound (), sign, &ovf1);
      wi::add (op1.upper_bound (), op2.upper_bound (), sign, &ovf2);
    }
  else
    ovf1 = ovf2 = wi::OVF_NONE;

  // Never wrapping additions.
  if (!ovf1 && !ovf2)
    {
      // Positive op2 means lhs > op1.
      if (wi::gt_p (op2.lower_bound (), wi::zero (prec), sign))
	return VREL_GT;
      if (wi::ge_p (op2.lower_bound (), wi::zero (prec), sign))
	return VREL_GE;

      // Negative op2 means lhs < op1.
      if (wi::lt_p (op2.upper_bound (), wi::zero (prec), sign))
	return VREL_LT;
      if (wi::le_p (op2.upper_bound (), wi::zero (prec), sign))
	return VREL_LE;
    }
  // Always wrapping additions.
  else if (ovf1 && ovf1 == ovf2)
    {
      // Positive op2 means lhs < op1.
      if (wi::gt_p (op2.lower_bound (), wi::zero (prec), sign))
	return VREL_LT;
      if (wi::ge_p (op2.lower_bound (), wi::zero (prec), sign))
	return VREL_LE;

      // Negative op2 means lhs > op1.
      if (wi::lt_p (op2.upper_bound (), wi::zero (prec), sign))
	return VREL_GT;
      if (wi::le_p (op2.upper_bound (), wi::zero (prec), sign))
	return VREL_GE;
    }

  // If op2 does not contain 0, then LHS and OP1 can never be equal.
  if (!range_includes_zero_p (&op2))
    return VREL_NE;

  return VREL_VARYING;
}

// PLUS is symmetrical, so we can simply call lhs_op1_relation with reversed
// operands.

relation_kind
operator_plus::lhs_op2_relation (const irange &lhs, const irange &op1,
				 const irange &op2, relation_kind rel) const
{
  return lhs_op1_relation (lhs, op2, op1, rel);
}

void
operator_plus::wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi::overflow_type ov_lb, ov_ub;
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::add (lh_lb, rh_lb, s, &ov_lb);
  wide_int new_ub = wi::add (lh_ub, rh_ub, s, &ov_ub);
  value_range_with_overflow (r, type, new_lb, new_ub, ov_lb, ov_ub);
}

// Given addition or subtraction, determine the possible NORMAL ranges and
// OVERFLOW ranges given an OFFSET range.  ADD_P is true for addition.
// Return the relation that exists between the LHS and OP1 in order for the
// NORMAL range to apply.
// a return value of VREL_VARYING means no ranges were applicable.

static relation_kind
plus_minus_ranges (irange &r_ov, irange &r_normal, const irange &offset,
		bool add_p)
{
  relation_kind kind = VREL_VARYING;
  // For now, only deal with constant adds.  This could be extended to ranges
  // when someone is so motivated.
  if (!offset.singleton_p () || offset.zero_p ())
    return kind;

  // Always work with a positive offset.  ie a+ -2 -> a-2  and a- -2 > a+2
  wide_int off = offset.lower_bound ();
  if (wi::neg_p (off, SIGNED))
    {
      add_p = !add_p;
      off = wi::neg (off);
    }

  wi::overflow_type ov;
  tree type = offset.type ();
  unsigned prec = TYPE_PRECISION (type);
  wide_int ub;
  wide_int lb;
  // calculate the normal range and relation for the operation.
  if (add_p)
    {
      //  [ 0 , INF - OFF]
      lb = wi::zero (prec);
      ub = wi::sub (irange_val_max (type), off, UNSIGNED, &ov);
      kind = VREL_GT;
    }
  else
    {
      //  [ OFF, INF ]
      lb = off;
      ub = irange_val_max (type);
      kind = VREL_LT;
    }
  int_range<2> normal_range (type, lb, ub);
  int_range<2> ov_range (type, lb, ub, VR_ANTI_RANGE);

  r_ov = ov_range;
  r_normal = normal_range;
  return kind;
}

// Once op1 has been calculated by operator_plus or operator_minus, check
// to see if the relation passed causes any part of the calculation to
// be not possible.  ie
// a_2 = b_3 + 1  with a_2 < b_3 can refine the range of b_3 to [INF, INF]
// and that further refines a_2 to [0, 0].
// R is the value of op1, OP2 is the offset being added/subtracted, REL is the
// relation between LHS relation OP1  and ADD_P is true for PLUS, false for
// MINUS.    IF any adjustment can be made, R will reflect it.

static void
adjust_op1_for_overflow (irange &r, const irange &op2, relation_kind rel,
			 bool add_p)
{
  if (r.undefined_p ())
    return;
  tree type = r.type ();
  // Check for unsigned overflow and calculate the overflow part.
  signop s = TYPE_SIGN (type);
  if (!TYPE_OVERFLOW_WRAPS (type) || s == SIGNED)
    return;

  // Only work with <, <=, >, >= relations.
  if (!relation_lt_le_gt_ge_p (rel))
    return;

  // Get the ranges for this offset.
  int_range_max normal, overflow;
  relation_kind k = plus_minus_ranges (overflow, normal, op2, add_p);

  // VREL_VARYING means there are no adjustments.
  if (k == VREL_VARYING)
    return;

  // If the relations match use the normal range, otherwise use overflow range.
  if (relation_intersect (k, rel) == k)
    r.intersect (normal);
  else
    r.intersect (overflow);
  return;
}

bool
operator_plus::op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio trio) const
{
  if (lhs.undefined_p ())
    return false;
  // Start with the default operation.
  range_op_handler minus (MINUS_EXPR);
  if (!minus)
    return false;
  bool res = minus.fold_range (r, type, lhs, op2);
  relation_kind rel = trio.lhs_op1 ();
  // Check for a relation refinement.
  if (res)
    adjust_op1_for_overflow (r, op2, rel, true /* PLUS_EXPR */);
  return res;
}

bool
operator_plus::op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio rel) const
{
  return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

class operator_widen_plus_signed : public range_operator
{
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub) const;
} op_widen_plus_signed;

void
operator_widen_plus_signed::wi_fold (irange &r, tree type,
				     const wide_int &lh_lb,
				     const wide_int &lh_ub,
				     const wide_int &rh_lb,
				     const wide_int &rh_ub) const
{
   wi::overflow_type ov_lb, ov_ub;
   signop s = TYPE_SIGN (type);

   wide_int lh_wlb
     = wide_int::from (lh_lb, wi::get_precision (lh_lb) * 2, SIGNED);
   wide_int lh_wub
     = wide_int::from (lh_ub, wi::get_precision (lh_ub) * 2, SIGNED);
   wide_int rh_wlb = wide_int::from (rh_lb, wi::get_precision (rh_lb) * 2, s);
   wide_int rh_wub = wide_int::from (rh_ub, wi::get_precision (rh_ub) * 2, s);

   wide_int new_lb = wi::add (lh_wlb, rh_wlb, s, &ov_lb);
   wide_int new_ub = wi::add (lh_wub, rh_wub, s, &ov_ub);

   r = int_range<2> (type, new_lb, new_ub);
}

class operator_widen_plus_unsigned : public range_operator
{
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub) const;
} op_widen_plus_unsigned;

void
operator_widen_plus_unsigned::wi_fold (irange &r, tree type,
				       const wide_int &lh_lb,
				       const wide_int &lh_ub,
				       const wide_int &rh_lb,
				       const wide_int &rh_ub) const
{
   wi::overflow_type ov_lb, ov_ub;
   signop s = TYPE_SIGN (type);

   wide_int lh_wlb
     = wide_int::from (lh_lb, wi::get_precision (lh_lb) * 2, UNSIGNED);
   wide_int lh_wub
     = wide_int::from (lh_ub, wi::get_precision (lh_ub) * 2, UNSIGNED);
   wide_int rh_wlb = wide_int::from (rh_lb, wi::get_precision (rh_lb) * 2, s);
   wide_int rh_wub = wide_int::from (rh_ub, wi::get_precision (rh_ub) * 2, s);

   wide_int new_lb = wi::add (lh_wlb, rh_wlb, s, &ov_lb);
   wide_int new_ub = wi::add (lh_wub, rh_wub, s, &ov_ub);

   r = int_range<2> (type, new_lb, new_ub);
}

void
operator_minus::update_bitmask (irange &r, const irange &lh,
				const irange &rh) const
{
  update_known_bitmask (r, MINUS_EXPR, lh, rh);
}

void 
operator_minus::wi_fold (irange &r, tree type,
			 const wide_int &lh_lb, const wide_int &lh_ub,
			 const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi::overflow_type ov_lb, ov_ub;
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::sub (lh_lb, rh_ub, s, &ov_lb);
  wide_int new_ub = wi::sub (lh_ub, rh_lb, s, &ov_ub);
  value_range_with_overflow (r, type, new_lb, new_ub, ov_lb, ov_ub);
}


// Return the relation between LHS and OP1 based on the relation between
// OP1 and OP2.

relation_kind
operator_minus::lhs_op1_relation (const irange &, const irange &op1,
				  const irange &, relation_kind rel) const
{
  if (!op1.undefined_p () && TYPE_SIGN (op1.type ()) == UNSIGNED)
    switch (rel)
      {
      case VREL_GT:
      case VREL_GE:
	return VREL_LE;
      default:
	break;
      }
  return VREL_VARYING;
}

// Check to see if the relation REL between OP1 and OP2 has any effect on the
// LHS of the expression.  If so, apply it to LHS_RANGE.  This is a helper
// function for both MINUS_EXPR and POINTER_DIFF_EXPR.

bool
minus_op1_op2_relation_effect (irange &lhs_range, tree type,
			       const irange &op1_range ATTRIBUTE_UNUSED,
			       const irange &op2_range ATTRIBUTE_UNUSED,
			       relation_kind rel)
{
  if (rel == VREL_VARYING)
    return false;

  int_range<2> rel_range;
  unsigned prec = TYPE_PRECISION (type);
  signop sgn = TYPE_SIGN (type);

  // == and != produce [0,0] and ~[0,0] regardless of wrapping.
  if (rel == VREL_EQ)
    rel_range = int_range<2> (type, wi::zero (prec), wi::zero (prec));
  else if (rel == VREL_NE)
    rel_range = int_range<2> (type, wi::zero (prec), wi::zero (prec),
			      VR_ANTI_RANGE);
  else if (TYPE_OVERFLOW_WRAPS (type))
    {
      switch (rel)
	{
	  // For wrapping signed values and unsigned, if op1 > op2 or
	  // op1 < op2, then op1 - op2 can be restricted to ~[0, 0].
	  case VREL_GT:
	  case VREL_LT:
	      rel_range = int_range<2> (type, wi::zero (prec), wi::zero (prec),
					VR_ANTI_RANGE);
	    break;
	  default:
	    return false;
	}
    }
  else
    {
      switch (rel)
	{
	  // op1 > op2, op1 - op2 can be restricted to [1, +INF]
	  case VREL_GT:
	    rel_range = int_range<2> (type, wi::one (prec),
				      wi::max_value (prec, sgn));
	    break;
	  // op1 >= op2, op1 - op2 can be restricted to [0, +INF]
	  case VREL_GE:
	    rel_range = int_range<2> (type, wi::zero (prec),
				      wi::max_value (prec, sgn));
	    break;
	  // op1 < op2, op1 - op2 can be restricted to [-INF, -1]
	  case VREL_LT:
	    rel_range = int_range<2> (type, wi::min_value (prec, sgn),
				      wi::minus_one (prec));
	    break;
	  // op1 <= op2, op1 - op2 can be restricted to [-INF, 0]
	  case VREL_LE:
	    rel_range = int_range<2> (type, wi::min_value (prec, sgn),
				      wi::zero (prec));
	    break;
	  default:
	    return false;
	}
    }
  lhs_range.intersect (rel_range);
  return true;
}

bool
operator_minus::op1_op2_relation_effect (irange &lhs_range, tree type,
					 const irange &op1_range,
					 const irange &op2_range,
					 relation_kind rel) const
{
  return minus_op1_op2_relation_effect (lhs_range, type, op1_range, op2_range,
					rel);
}

bool
operator_minus::op1_range (irange &r, tree type,
			   const irange &lhs,
			   const irange &op2,
			   relation_trio trio) const
{
  if (lhs.undefined_p ())
    return false;
  // Start with the default operation.
  range_op_handler minus (PLUS_EXPR);
  if (!minus)
    return false;
  bool res = minus.fold_range (r, type, lhs, op2);
  relation_kind rel = trio.lhs_op1 ();
  if (res)
    adjust_op1_for_overflow (r, op2, rel, false /* PLUS_EXPR */);
  return res;

}

bool
operator_minus::op2_range (irange &r, tree type,
			   const irange &lhs,
			   const irange &op1,
			   relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  return fold_range (r, type, op1, lhs);
}

void
operator_min::update_bitmask (irange &r, const irange &lh,
			      const irange &rh) const
{
  update_known_bitmask (r, MIN_EXPR, lh, rh);
}

void
operator_min::wi_fold (irange &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::min (lh_lb, rh_lb, s);
  wide_int new_ub = wi::min (lh_ub, rh_ub, s);
  value_range_with_overflow (r, type, new_lb, new_ub);
}


void
operator_max::update_bitmask (irange &r, const irange &lh,
			      const irange &rh) const
{
  update_known_bitmask (r, MAX_EXPR, lh, rh);
}

void
operator_max::wi_fold (irange &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::max (lh_lb, rh_lb, s);
  wide_int new_ub = wi::max (lh_ub, rh_ub, s);
  value_range_with_overflow (r, type, new_lb, new_ub);
}


// Calculate the cross product of two sets of ranges and return it.
//
// Multiplications, divisions and shifts are a bit tricky to handle,
// depending on the mix of signs we have in the two ranges, we need to
// operate on different values to get the minimum and maximum values
// for the new range.  One approach is to figure out all the
// variations of range combinations and do the operations.
//
// However, this involves several calls to compare_values and it is
// pretty convoluted.  It's simpler to do the 4 operations (MIN0 OP
// MIN1, MIN0 OP MAX1, MAX0 OP MIN1 and MAX0 OP MAX0 OP MAX1) and then
// figure the smallest and largest values to form the new range.

void
cross_product_operator::wi_cross_product (irange &r, tree type,
					  const wide_int &lh_lb,
					  const wide_int &lh_ub,
					  const wide_int &rh_lb,
					  const wide_int &rh_ub) const
{
  wide_int cp1, cp2, cp3, cp4;
  // Default to varying.
  r.set_varying (type);

  // Compute the 4 cross operations, bailing if we get an overflow we
  // can't handle.
  if (wi_op_overflows (cp1, type, lh_lb, rh_lb))
    return;
  if (wi::eq_p (lh_lb, lh_ub))
    cp3 = cp1;
  else if (wi_op_overflows (cp3, type, lh_ub, rh_lb))
    return;
  if (wi::eq_p (rh_lb, rh_ub))
    cp2 = cp1;
  else if (wi_op_overflows (cp2, type, lh_lb, rh_ub))
    return;
  if (wi::eq_p (lh_lb, lh_ub))
    cp4 = cp2;
  else if (wi_op_overflows (cp4, type, lh_ub, rh_ub))
    return;

  // Order pairs.
  signop sign = TYPE_SIGN (type);
  if (wi::gt_p (cp1, cp2, sign))
    std::swap (cp1, cp2);
  if (wi::gt_p (cp3, cp4, sign))
    std::swap (cp3, cp4);

  // Choose min and max from the ordered pairs.
  wide_int res_lb = wi::min (cp1, cp3, sign);
  wide_int res_ub = wi::max (cp2, cp4, sign);
  value_range_with_overflow (r, type, res_lb, res_ub);
}


void
operator_mult::update_bitmask (irange &r, const irange &lh,
			       const irange &rh) const
{
  update_known_bitmask (r, MULT_EXPR, lh, rh);
}

bool
operator_mult::op1_range (irange &r, tree type,
			  const irange &lhs, const irange &op2,
			  relation_trio) const
{
  if (lhs.undefined_p ())
    return false;

  // We can't solve 0 = OP1 * N by dividing by N with a wrapping type.
  // For example: For 0 = OP1 * 2, OP1 could be 0, or MAXINT, whereas
  // for 4 = OP1 * 2, OP1 could be 2 or 130 (unsigned 8-bit)
  if (TYPE_OVERFLOW_WRAPS (type))
    return false;

  wide_int offset;
  if (op2.singleton_p (offset) && offset != 0)
    return range_op_handler (TRUNC_DIV_EXPR).fold_range (r, type, lhs, op2);
  return false;
}

bool
operator_mult::op2_range (irange &r, tree type,
			  const irange &lhs, const irange &op1,
			  relation_trio rel) const
{
  return operator_mult::op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

bool
operator_mult::wi_op_overflows (wide_int &res, tree type,
				const wide_int &w0, const wide_int &w1) const
{
  wi::overflow_type overflow = wi::OVF_NONE;
  signop sign = TYPE_SIGN (type);
  res = wi::mul (w0, w1, sign, &overflow);
   if (overflow && TYPE_OVERFLOW_UNDEFINED (type))
     {
       // For multiplication, the sign of the overflow is given
       // by the comparison of the signs of the operands.
       if (sign == UNSIGNED || w0.sign_mask () == w1.sign_mask ())
	 res = wi::max_value (w0.get_precision (), sign);
       else
	 res = wi::min_value (w0.get_precision (), sign);
       return false;
     }
   return overflow;
}

void 
operator_mult::wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const
{
  if (TYPE_OVERFLOW_UNDEFINED (type))
    {
      wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
      return;
    }

  // Multiply the ranges when overflow wraps.  This is basically fancy
  // code so we don't drop to varying with an unsigned
  // [-3,-1]*[-3,-1].
  //
  // This test requires 2*prec bits if both operands are signed and
  // 2*prec + 2 bits if either is not.  Therefore, extend the values
  // using the sign of the result to PREC2.  From here on out,
  // everything is just signed math no matter what the input types
  // were.

  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  widest2_int min0 = widest2_int::from (lh_lb, sign);
  widest2_int max0 = widest2_int::from (lh_ub, sign);
  widest2_int min1 = widest2_int::from (rh_lb, sign);
  widest2_int max1 = widest2_int::from (rh_ub, sign);
  widest2_int sizem1 = wi::mask <widest2_int> (prec, false);
  widest2_int size = sizem1 + 1;

  // Canonicalize the intervals.
  if (sign == UNSIGNED)
    {
      if (wi::ltu_p (size, min0 + max0))
	{
	  min0 -= size;
	  max0 -= size;
	}
      if (wi::ltu_p (size, min1 + max1))
	{
	  min1 -= size;
	  max1 -= size;
	}
    }

  // Sort the 4 products so that min is in prod0 and max is in
  // prod3.
  widest2_int prod0 = min0 * min1;
  widest2_int prod1 = min0 * max1;
  widest2_int prod2 = max0 * min1;
  widest2_int prod3 = max0 * max1;

  // min0min1 > max0max1
  if (prod0 > prod3)
    std::swap (prod0, prod3);

  // min0max1 > max0min1
  if (prod1 > prod2)
    std::swap (prod1, prod2);

  if (prod0 > prod1)
    std::swap (prod0, prod1);

  if (prod2 > prod3)
    std::swap (prod2, prod3);

  // diff = max - min
  prod2 = prod3 - prod0;
  if (wi::geu_p (prod2, sizem1))
    {
      // Multiplying by X, where X is a power of 2 is [0,0][X,+INF].
      if (TYPE_UNSIGNED (type) && rh_lb == rh_ub
	  && wi::exact_log2 (rh_lb) != -1 && prec > 1)
	{
	  r.set (type, rh_lb, wi::max_value (prec, sign));
	  int_range<2> zero;
	  zero.set_zero (type);
	  r.union_ (zero);
	}
      else
	// The range covers all values.
	r.set_varying (type);
    }
  else
    {
      wide_int new_lb = wide_int::from (prod0, prec, sign);
      wide_int new_ub = wide_int::from (prod3, prec, sign);
      create_possibly_reversed_range (r, type, new_lb, new_ub);
    }
}

class operator_widen_mult_signed : public range_operator
{
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub)
    const;
} op_widen_mult_signed;

void
operator_widen_mult_signed::wi_fold (irange &r, tree type,
				     const wide_int &lh_lb,
				     const wide_int &lh_ub,
				     const wide_int &rh_lb,
				     const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);

  wide_int lh_wlb = wide_int::from (lh_lb, wi::get_precision (lh_lb) * 2, SIGNED);
  wide_int lh_wub = wide_int::from (lh_ub, wi::get_precision (lh_ub) * 2, SIGNED);
  wide_int rh_wlb = wide_int::from (rh_lb, wi::get_precision (rh_lb) * 2, s);
  wide_int rh_wub = wide_int::from (rh_ub, wi::get_precision (rh_ub) * 2, s);

  /* We don't expect a widening multiplication to be able to overflow but range
     calculations for multiplications are complicated.  After widening the
     operands lets call the base class.  */
  return op_mult.wi_fold (r, type, lh_wlb, lh_wub, rh_wlb, rh_wub);
}


class operator_widen_mult_unsigned : public range_operator
{
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub)
    const;
} op_widen_mult_unsigned;

void
operator_widen_mult_unsigned::wi_fold (irange &r, tree type,
				       const wide_int &lh_lb,
				       const wide_int &lh_ub,
				       const wide_int &rh_lb,
				       const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);

  wide_int lh_wlb = wide_int::from (lh_lb, wi::get_precision (lh_lb) * 2, UNSIGNED);
  wide_int lh_wub = wide_int::from (lh_ub, wi::get_precision (lh_ub) * 2, UNSIGNED);
  wide_int rh_wlb = wide_int::from (rh_lb, wi::get_precision (rh_lb) * 2, s);
  wide_int rh_wub = wide_int::from (rh_ub, wi::get_precision (rh_ub) * 2, s);

  /* We don't expect a widening multiplication to be able to overflow but range
     calculations for multiplications are complicated.  After widening the
     operands lets call the base class.  */
  return op_mult.wi_fold (r, type, lh_wlb, lh_wub, rh_wlb, rh_wub);
}

class operator_div : public cross_product_operator
{
public:
  operator_div (tree_code div_kind) { m_code = div_kind; }
  virtual void wi_fold (irange &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
			const wide_int &rh_ub) const final override;
  virtual bool wi_op_overflows (wide_int &res, tree type,
				const wide_int &, const wide_int &)
    const final override;
  void update_bitmask (irange &r, const irange &lh, const irange &rh) const
    { update_known_bitmask (r, m_code, lh, rh); }
protected:
  tree_code m_code;
};

static operator_div op_trunc_div (TRUNC_DIV_EXPR);
static operator_div op_floor_div (FLOOR_DIV_EXPR);
static operator_div op_round_div (ROUND_DIV_EXPR);
static operator_div op_ceil_div (CEIL_DIV_EXPR);

bool
operator_div::wi_op_overflows (wide_int &res, tree type,
			       const wide_int &w0, const wide_int &w1) const
{
  if (w1 == 0)
    return true;

  wi::overflow_type overflow = wi::OVF_NONE;
  signop sign = TYPE_SIGN (type);

  switch (m_code)
    {
    case EXACT_DIV_EXPR:
    case TRUNC_DIV_EXPR:
      res = wi::div_trunc (w0, w1, sign, &overflow);
      break;
    case FLOOR_DIV_EXPR:
      res = wi::div_floor (w0, w1, sign, &overflow);
      break;
    case ROUND_DIV_EXPR:
      res = wi::div_round (w0, w1, sign, &overflow);
      break;
    case CEIL_DIV_EXPR:
      res = wi::div_ceil (w0, w1, sign, &overflow);
      break;
    default:
      gcc_unreachable ();
    }

  if (overflow && TYPE_OVERFLOW_UNDEFINED (type))
    {
      // For division, the only case is -INF / -1 = +INF.
      res = wi::max_value (w0.get_precision (), sign);
      return false;
    }
  return overflow;
}

void
operator_div::wi_fold (irange &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  const wide_int dividend_min = lh_lb;
  const wide_int dividend_max = lh_ub;
  const wide_int divisor_min = rh_lb;
  const wide_int divisor_max = rh_ub;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  wide_int extra_min, extra_max;

  // If we know we won't divide by zero, just do the division.
  if (!wi_includes_zero_p (type, divisor_min, divisor_max))
    {
      wi_cross_product (r, type, dividend_min, dividend_max,
		       divisor_min, divisor_max);
      return;
    }

  // If we're definitely dividing by zero, there's nothing to do.
  if (wi_zero_p (type, divisor_min, divisor_max))
    {
      r.set_undefined ();
      return;
    }

  // Perform the division in 2 parts, [LB, -1] and [1, UB], which will
  // skip any division by zero.

  // First divide by the negative numbers, if any.
  if (wi::neg_p (divisor_min, sign))
    wi_cross_product (r, type, dividend_min, dividend_max,
		      divisor_min, wi::minus_one (prec));
  else
    r.set_undefined ();

  // Then divide by the non-zero positive numbers, if any.
  if (wi::gt_p (divisor_max, wi::zero (prec), sign))
    {
      int_range_max tmp;
      wi_cross_product (tmp, type, dividend_min, dividend_max,
			wi::one (prec), divisor_max);
      r.union_ (tmp);
    }
  // We shouldn't still have undefined here.
  gcc_checking_assert (!r.undefined_p ());
}


class operator_exact_divide : public operator_div
{
  using range_operator::op1_range;
public:
  operator_exact_divide () : operator_div (EXACT_DIV_EXPR) { }
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio) const;

} op_exact_div;

bool
operator_exact_divide::op1_range (irange &r, tree type,
				  const irange &lhs,
				  const irange &op2,
				  relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  wide_int offset;
  // [2, 4] = op1 / [3,3]   since its exact divide, no need to worry about
  // remainders in the endpoints, so op1 = [2,4] * [3,3] = [6,12].
  // We wont bother trying to enumerate all the in between stuff :-P
  // TRUE accuracy is [6,6][9,9][12,12].  This is unlikely to matter most of
  // the time however.
  // If op2 is a multiple of 2, we would be able to set some non-zero bits.
  if (op2.singleton_p (offset) && offset != 0)
    return range_op_handler (MULT_EXPR).fold_range (r, type, lhs, op2);
  return false;
}


class operator_lshift : public cross_product_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
public:
  virtual bool op1_range (irange &r, tree type, const irange &lhs,
			  const irange &op2, relation_trio rel = TRIO_VARYING)
    const final override;
  virtual bool fold_range (irange &r, tree type, const irange &op1,
			   const irange &op2, relation_trio rel = TRIO_VARYING)
    const final override;

  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub) const final override;
  virtual bool wi_op_overflows (wide_int &res,
				tree type,
				const wide_int &,
				const wide_int &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    { update_known_bitmask (r, LSHIFT_EXPR, lh, rh); }
  // Check compatibility of LHS and op1.
  bool operand_check_p (tree t1, tree t2, tree) const final override
    { return range_compatible_p (t1, t2); }
} op_lshift;

class operator_rshift : public cross_product_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::lhs_op1_relation;
public:
  virtual bool fold_range (irange &r, tree type, const irange &op1,
			   const irange &op2, relation_trio rel = TRIO_VARYING)
   const final override;
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub) const final override;
  virtual bool wi_op_overflows (wide_int &res,
				tree type,
				const wide_int &w0,
				const wide_int &w1) const final override;
  virtual bool op1_range (irange &, tree type, const irange &lhs,
			  const irange &op2, relation_trio rel = TRIO_VARYING)
    const final override;
  virtual relation_kind lhs_op1_relation (const irange &lhs, const irange &op1,
					  const irange &op2, relation_kind rel)
    const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    { update_known_bitmask (r, RSHIFT_EXPR, lh, rh); }
  // Check compatibility of LHS and op1.
  bool operand_check_p (tree t1, tree t2, tree) const final override
    { return range_compatible_p (t1, t2); }
} op_rshift;


relation_kind
operator_rshift::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
				   const irange &op1,
				   const irange &op2,
				   relation_kind) const
{
  // If both operands range are >= 0, then the LHS <= op1.
  if (!op1.undefined_p () && !op2.undefined_p ()
      && wi::ge_p (op1.lower_bound (), 0, TYPE_SIGN (op1.type ()))
      && wi::ge_p (op2.lower_bound (), 0, TYPE_SIGN (op2.type ())))
    return VREL_LE;
  return VREL_VARYING;
}

bool
operator_lshift::fold_range (irange &r, tree type,
			     const irange &op1,
			     const irange &op2,
			     relation_trio rel) const
{
  int_range_max shift_range;
  if (!get_shift_range (shift_range, type, op2))
    {
      if (op2.undefined_p ())
	r.set_undefined ();
      else
	r.set_zero (type);
      return true;
    }

  // Transform left shifts by constants into multiplies.
  if (shift_range.singleton_p ())
    {
      unsigned shift = shift_range.lower_bound ().to_uhwi ();
      wide_int tmp = wi::set_bit_in_zero (shift, TYPE_PRECISION (type));
      int_range<1> mult (type, tmp, tmp);

      // Force wrapping multiplication.
      bool saved_flag_wrapv = flag_wrapv;
      bool saved_flag_wrapv_pointer = flag_wrapv_pointer;
      flag_wrapv = 1;
      flag_wrapv_pointer = 1;
      bool b = op_mult.fold_range (r, type, op1, mult);
      flag_wrapv = saved_flag_wrapv;
      flag_wrapv_pointer = saved_flag_wrapv_pointer;
      return b;
    }
  else
    // Otherwise, invoke the generic fold routine.
    return range_operator::fold_range (r, type, op1, shift_range, rel);
}

void
operator_lshift::wi_fold (irange &r, tree type,
			  const wide_int &lh_lb, const wide_int &lh_ub,
			  const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  int overflow_pos = sign == SIGNED ? prec - 1 : prec;
  int bound_shift = overflow_pos - rh_ub.to_shwi ();
  // If bound_shift == HOST_BITS_PER_WIDE_INT, the llshift can
  // overflow.  However, for that to happen, rh.max needs to be zero,
  // which means rh is a singleton range of zero, which means we simply return
  // [lh_lb, lh_ub] as the range.
  if (wi::eq_p (rh_ub, rh_lb) && wi::eq_p (rh_ub, 0))
    {
      r = int_range<2> (type, lh_lb, lh_ub);
      return;
    }

  wide_int bound = wi::set_bit_in_zero (bound_shift, prec);
  wide_int complement = ~(bound - 1);
  wide_int low_bound, high_bound;
  bool in_bounds = false;

  if (sign == UNSIGNED)
    {
      low_bound = bound;
      high_bound = complement;
      if (wi::ltu_p (lh_ub, low_bound))
	{
	  // [5, 6] << [1, 2] == [10, 24].
	  // We're shifting out only zeroes, the value increases
	  // monotonically.
	  in_bounds = true;
	}
      else if (wi::ltu_p (high_bound, lh_lb))
	{
	  // [0xffffff00, 0xffffffff] << [1, 2]
	  // == [0xfffffc00, 0xfffffffe].
	  // We're shifting out only ones, the value decreases
	  // monotonically.
	  in_bounds = true;
	}
    }
  else
    {
      // [-1, 1] << [1, 2] == [-4, 4]
      low_bound = complement;
      high_bound = bound;
      if (wi::lts_p (lh_ub, high_bound)
	  && wi::lts_p (low_bound, lh_lb))
	{
	  // For non-negative numbers, we're shifting out only zeroes,
	  // the value increases monotonically.  For negative numbers,
	  // we're shifting out only ones, the value decreases
	  // monotonically.
	  in_bounds = true;
	}
    }

  if (in_bounds)
    wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
  else
   r.set_varying (type);
}

bool
operator_lshift::wi_op_overflows (wide_int &res, tree type,
				  const wide_int &w0, const wide_int &w1) const
{
  signop sign = TYPE_SIGN (type);
  if (wi::neg_p (w1))
    {
      // It's unclear from the C standard whether shifts can overflow.
      // The following code ignores overflow; perhaps a C standard
      // interpretation ruling is needed.
      res = wi::rshift (w0, -w1, sign);
    }
  else
    res = wi::lshift (w0, w1);
  return false;
}

bool
operator_lshift::op1_range (irange &r,
			    tree type,
			    const irange &lhs,
			    const irange &op2,
			    relation_trio) const
{
  if (lhs.undefined_p ())
    return false;

  if (!contains_zero_p (lhs))
    r.set_nonzero (type);
  else
    r.set_varying (type);

  wide_int shift;
  if (op2.singleton_p (shift))
    {
      if (wi::lt_p (shift, 0, SIGNED))
	return false;
      if (wi::ge_p (shift, wi::uhwi (TYPE_PRECISION (type),
				     TYPE_PRECISION (op2.type ())),
		    UNSIGNED))
	return false;
      if (shift == 0)
	{
	  r.intersect (lhs);
	  return true;
	}

      // Work completely in unsigned mode to start.
      tree utype = type;
      int_range_max tmp_range;
      if (TYPE_SIGN (type) == SIGNED)
	{
	  int_range_max tmp = lhs;
	  utype = unsigned_type_for (type);
	  range_cast (tmp, utype);
	  op_rshift.fold_range (tmp_range, utype, tmp, op2);
	}
      else
	op_rshift.fold_range (tmp_range, utype, lhs, op2);

      // Start with ranges which can produce the LHS by right shifting the
      // result by the shift amount.
      // ie   [0x08, 0xF0] = op1 << 2 will start with
      //      [00001000, 11110000] = op1 << 2
      //  [0x02, 0x4C] aka [00000010, 00111100]

      // Then create a range from the LB with the least significant upper bit
      // set, to the upper bound with all the bits set.
      // This would be [0x42, 0xFC] aka [01000010, 11111100].

      // Ideally we do this for each subrange, but just lump them all for now.
      unsigned low_bits = TYPE_PRECISION (utype) - shift.to_uhwi ();
      wide_int up_mask = wi::mask (low_bits, true, TYPE_PRECISION (utype));
      wide_int new_ub = wi::bit_or (up_mask, tmp_range.upper_bound ());
      wide_int new_lb = wi::set_bit (tmp_range.lower_bound (), low_bits);
      int_range<2> fill_range (utype, new_lb, new_ub);
      tmp_range.union_ (fill_range);

      if (utype != type)
	range_cast (tmp_range, type);

      r.intersect (tmp_range);
      return true;
    }

  return !r.varying_p ();
}

bool
operator_rshift::op1_range (irange &r,
			    tree type,
			    const irange &lhs,
			    const irange &op2,
			    relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  wide_int shift;
  if (op2.singleton_p (shift))
    {
      // Ignore nonsensical shifts.
      unsigned prec = TYPE_PRECISION (type);
      if (wi::ge_p (shift,
		    wi::uhwi (prec, TYPE_PRECISION (op2.type ())),
		    UNSIGNED))
	return false;
      if (shift == 0)
	{
	  r = lhs;
	  return true;
	}

      // Folding the original operation may discard some impossible
      // ranges from the LHS.
      int_range_max lhs_refined;
      op_rshift.fold_range (lhs_refined, type, int_range<1> (type), op2);
      lhs_refined.intersect (lhs);
      if (lhs_refined.undefined_p ())
	{
	  r.set_undefined ();
	  return true;
	}
      int_range_max shift_range (op2.type (), shift, shift);
      int_range_max lb, ub;
      op_lshift.fold_range (lb, type, lhs_refined, shift_range);
      //    LHS
      // 0000 0111 = OP1 >> 3
      //
      // OP1 is anything from 0011 1000 to 0011 1111.  That is, a
      // range from LHS<<3 plus a mask of the 3 bits we shifted on the
      // right hand side (0x07).
      wide_int mask = wi::bit_not (wi::lshift (wi::minus_one (prec), shift));
      int_range_max mask_range (type,
				wi::zero (TYPE_PRECISION (type)),
				mask);
      op_plus.fold_range (ub, type, lb, mask_range);
      r = lb;
      r.union_ (ub);
      if (!contains_zero_p (lhs_refined))
	{
	  mask_range.invert ();
	  r.intersect (mask_range);
	}
      return true;
    }
  return false;
}

bool
operator_rshift::wi_op_overflows (wide_int &res,
				  tree type,
				  const wide_int &w0,
				  const wide_int &w1) const
{
  signop sign = TYPE_SIGN (type);
  if (wi::neg_p (w1))
    res = wi::lshift (w0, -w1);
  else
    {
      // It's unclear from the C standard whether shifts can overflow.
      // The following code ignores overflow; perhaps a C standard
      // interpretation ruling is needed.
      res = wi::rshift (w0, w1, sign);
    }
  return false;
}

bool
operator_rshift::fold_range (irange &r, tree type,
			     const irange &op1,
			     const irange &op2,
			     relation_trio rel) const
{
  int_range_max shift;
  if (!get_shift_range (shift, type, op2))
    {
      if (op2.undefined_p ())
	r.set_undefined ();
      else
	r.set_zero (type);
      return true;
    }

  return range_operator::fold_range (r, type, op1, shift, rel);
}

void
operator_rshift::wi_fold (irange &r, tree type,
			  const wide_int &lh_lb, const wide_int &lh_ub,
			  const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
}


// Add a partial equivalence between the LHS and op1 for casts.

relation_kind
operator_cast::lhs_op1_relation (const irange &lhs,
				 const irange &op1,
				 const irange &op2 ATTRIBUTE_UNUSED,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p ())
    return VREL_VARYING;
  unsigned lhs_prec = TYPE_PRECISION (lhs.type ());
  unsigned op1_prec = TYPE_PRECISION (op1.type ());
  // If the result gets sign extended into a larger type check first if this
  // qualifies as a partial equivalence.
  if (TYPE_SIGN (op1.type ()) == SIGNED && lhs_prec > op1_prec)
    {
      // If the result is sign extended, and the LHS is larger than op1,
      // check if op1's range can be negative as the sign extension will
      // cause the upper bits to be 1 instead of 0, invalidating the PE.
      int_range<3> negs = range_negatives (op1.type ());
      negs.intersect (op1);
      if (!negs.undefined_p ())
	return VREL_VARYING;
    }

  unsigned prec = MIN (lhs_prec, op1_prec);
  return bits_to_pe (prec);
}

// Return TRUE if casting from INNER to OUTER is a truncating cast.

inline bool
operator_cast::truncating_cast_p (const irange &inner,
				  const irange &outer) const
{
  return TYPE_PRECISION (outer.type ()) < TYPE_PRECISION (inner.type ());
}

// Return TRUE if [MIN,MAX] is inside the domain of RANGE's type.

bool
operator_cast::inside_domain_p (const wide_int &min,
				const wide_int &max,
				const irange &range) const
{
  wide_int domain_min = irange_val_min (range.type ());
  wide_int domain_max = irange_val_max (range.type ());
  signop domain_sign = TYPE_SIGN (range.type ());
  return (wi::le_p (min, domain_max, domain_sign)
	  && wi::le_p (max, domain_max, domain_sign)
	  && wi::ge_p (min, domain_min, domain_sign)
	  && wi::ge_p (max, domain_min, domain_sign));
}


// Helper for fold_range which work on a pair at a time.

void
operator_cast::fold_pair (irange &r, unsigned index,
			   const irange &inner,
			   const irange &outer) const
{
  tree inner_type = inner.type ();
  tree outer_type = outer.type ();
  signop inner_sign = TYPE_SIGN (inner_type);
  unsigned outer_prec = TYPE_PRECISION (outer_type);

  // check to see if casting from INNER to OUTER is a conversion that
  // fits in the resulting OUTER type.
  wide_int inner_lb = inner.lower_bound (index);
  wide_int inner_ub = inner.upper_bound (index);
  if (truncating_cast_p (inner, outer))
    {
      // We may be able to accommodate a truncating cast if the
      // resulting range can be represented in the target type...
      if (wi::rshift (wi::sub (inner_ub, inner_lb),
		      wi::uhwi (outer_prec, TYPE_PRECISION (inner.type ())),
				inner_sign) != 0)
	{
	  r.set_varying (outer_type);
	  return;
	}
    }
  // ...but we must still verify that the final range fits in the
  // domain.  This catches -fstrict-enum restrictions where the domain
  // range is smaller than what fits in the underlying type.
  wide_int min = wide_int::from (inner_lb, outer_prec, inner_sign);
  wide_int max = wide_int::from (inner_ub, outer_prec, inner_sign);
  if (inside_domain_p (min, max, outer))
    create_possibly_reversed_range (r, outer_type, min, max);
  else
    r.set_varying (outer_type);
}


bool
operator_cast::fold_range (irange &r, tree type ATTRIBUTE_UNUSED,
			   const irange &inner,
			   const irange &outer,
			   relation_trio) const
{
  if (empty_range_varying (r, type, inner, outer))
    return true;

  gcc_checking_assert (outer.varying_p ());
  gcc_checking_assert (inner.num_pairs () > 0);

  // Avoid a temporary by folding the first pair directly into the result.
  fold_pair (r, 0, inner, outer);

  // Then process any additional pairs by unioning with their results.
  for (unsigned x = 1; x < inner.num_pairs (); ++x)
    {
      int_range_max tmp;
      fold_pair (tmp, x, inner, outer);
      r.union_ (tmp);
      if (r.varying_p ())
	return true;
    }

  update_bitmask (r, inner, outer);
  return true;
}

void
operator_cast::update_bitmask (irange &r, const irange &lh,
			       const irange &rh) const
{
  update_known_bitmask (r, CONVERT_EXPR, lh, rh);
}

bool
operator_cast::op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  tree lhs_type = lhs.type ();
  gcc_checking_assert (types_compatible_p (op2.type(), type));

  // If we are calculating a pointer, shortcut to what we really care about.
  if (POINTER_TYPE_P (type))
    {
      // Conversion from other pointers or a constant (including 0/NULL)
      // are straightforward.
      if (POINTER_TYPE_P (lhs.type ())
	  || (lhs.singleton_p ()
	      && TYPE_PRECISION (lhs.type ()) >= TYPE_PRECISION (type)))
	{
	  r = lhs;
	  range_cast (r, type);
	}
      else
	{
	  // If the LHS is not a pointer nor a singleton, then it is
	  // either VARYING or non-zero.
	  if (!lhs.undefined_p () && !contains_zero_p (lhs))
	    r.set_nonzero (type);
	  else
	    r.set_varying (type);
	}
      r.intersect (op2);
      return true;
    }

  if (truncating_cast_p (op2, lhs))
    {
      if (lhs.varying_p ())
	r.set_varying (type);
      else
        {
	  // We want to insert the LHS as an unsigned value since it
	  // would not trigger the signed bit of the larger type.
	  int_range_max converted_lhs = lhs;
	  range_cast (converted_lhs, unsigned_type_for (lhs_type));
	  range_cast (converted_lhs, type);
	  // Start by building the positive signed outer range for the type.
	  wide_int lim = wi::set_bit_in_zero (TYPE_PRECISION (lhs_type),
					      TYPE_PRECISION (type));
	  create_possibly_reversed_range (r, type, lim,
					  wi::max_value (TYPE_PRECISION (type),
							 SIGNED));
	  // For the signed part, we need to simply union the 2 ranges now.
	  r.union_ (converted_lhs);

	  // Create maximal negative number outside of LHS bits.
	  lim = wi::mask (TYPE_PRECISION (lhs_type), true,
			  TYPE_PRECISION (type));
	  // Add this to the unsigned LHS range(s).
	  int_range_max lim_range (type, lim, lim);
	  int_range_max lhs_neg;
	  range_op_handler (PLUS_EXPR).fold_range (lhs_neg, type,
						   converted_lhs, lim_range);
	  // lhs_neg now has all the negative versions of the LHS.
	  // Now union in all the values from SIGNED MIN (0x80000) to
	  // lim-1 in order to fill in all the ranges with the upper
	  // bits set.

	  // PR 97317.  If the lhs has only 1 bit less precision than the rhs,
	  // we don't need to create a range from min to lim-1
	  // calculate neg range traps trying to create [lim, lim - 1].
	  wide_int min_val = wi::min_value (TYPE_PRECISION (type), SIGNED);
	  if (lim != min_val)
	    {
	      int_range_max neg (type,
				 wi::min_value (TYPE_PRECISION (type),
						SIGNED),
				 lim - 1);
	      lhs_neg.union_ (neg);
	    }
	  // And finally, munge the signed and unsigned portions.
	  r.union_ (lhs_neg);
	}
      // And intersect with any known value passed in the extra operand.
      r.intersect (op2);
      return true;
    }

  int_range_max tmp;
  if (TYPE_PRECISION (lhs_type) == TYPE_PRECISION (type))
    tmp = lhs;
  else
    {
      // The cast is not truncating, and the range is restricted to
      // the range of the RHS by this assignment.
      //
      // Cast the range of the RHS to the type of the LHS.
      fold_range (tmp, lhs_type, int_range<1> (type), int_range<1> (lhs_type));
      // Intersect this with the LHS range will produce the range,
      // which will be cast to the RHS type before returning.
      tmp.intersect (lhs);
    }

  // Cast the calculated range to the type of the RHS.
  fold_range (r, type, tmp, int_range<1> (type));
  return true;
}


class operator_logical_and : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  virtual bool fold_range (irange &r, tree type,
			   const irange &lh,
			   const irange &rh,
			   relation_trio rel = TRIO_VARYING) const;
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio rel = TRIO_VARYING) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio rel = TRIO_VARYING) const;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
} op_logical_and;

bool
operator_logical_and::fold_range (irange &r, tree type,
				  const irange &lh,
				  const irange &rh,
				  relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;

  // Precision of LHS and both operands must match.
  if (TYPE_PRECISION (lh.type ()) != TYPE_PRECISION (type)
      || TYPE_PRECISION (type) != TYPE_PRECISION (rh.type ()))
    return false;

  // 0 && anything is 0.
  if ((wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (lh.upper_bound (), 0))
      || (wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (rh.upper_bound (), 0)))
    r = range_false (type);
  else if (contains_zero_p (lh) || contains_zero_p (rh))
    // To reach this point, there must be a logical 1 on each side, and
    // the only remaining question is whether there is a zero or not.
    r = range_true_and_false (type);
  else
    r = range_true (type);
  return true;
}

bool
operator_logical_and::op1_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op2 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
   switch (get_bool_state (r, lhs, type))
     {
     case BRS_TRUE:
       // A true result means both sides of the AND must be true.
       r = range_true (type);
       break;
     default:
       // Any other result means only one side has to be false, the
       // other side can be anything.  So we cannot be sure of any
       // result here.
       r = range_true_and_false (type);
       break;
     }
  return true;
}

bool
operator_logical_and::op2_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op1,
				 relation_trio) const
{
  return operator_logical_and::op1_range (r, type, lhs, op1);
}


void
operator_bitwise_and::update_bitmask (irange &r, const irange &lh,
				      const irange &rh) const
{
  update_known_bitmask (r, BIT_AND_EXPR, lh, rh);
}

// Optimize BIT_AND_EXPR, BIT_IOR_EXPR and BIT_XOR_EXPR of signed types
// by considering the number of leading redundant sign bit copies.
// clrsb (X op Y) = min (clrsb (X), clrsb (Y)), so for example
// [-1, 0] op [-1, 0] is [-1, 0] (where nonzero_bits doesn't help).
static bool
wi_optimize_signed_bitwise_op (irange &r, tree type,
			       const wide_int &lh_lb, const wide_int &lh_ub,
			       const wide_int &rh_lb, const wide_int &rh_ub)
{
  int lh_clrsb = MIN (wi::clrsb (lh_lb), wi::clrsb (lh_ub));
  int rh_clrsb = MIN (wi::clrsb (rh_lb), wi::clrsb (rh_ub));
  int new_clrsb = MIN (lh_clrsb, rh_clrsb);
  if (new_clrsb == 0)
    return false;
  int type_prec = TYPE_PRECISION (type);
  int rprec = (type_prec - new_clrsb) - 1;
  value_range_with_overflow (r, type,
			     wi::mask (rprec, true, type_prec),
			     wi::mask (rprec, false, type_prec));
  return true;
}

// An AND of 8,16, 32 or 64 bits can produce a partial equivalence between
// the LHS and op1.

relation_kind
operator_bitwise_and::lhs_op1_relation (const irange &lhs,
				 const irange &op1,
				 const irange &op2,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p () || op2.undefined_p ())
    return VREL_VARYING;
  if (!op2.singleton_p ())
    return VREL_VARYING;
  // if val == 0xff or 0xFFFF OR 0Xffffffff OR 0Xffffffffffffffff, return TRUE
  int prec1 = TYPE_PRECISION (op1.type ());
  int prec2 = TYPE_PRECISION (op2.type ());
  int mask_prec = 0;
  wide_int mask = op2.lower_bound ();
  if (wi::eq_p (mask, wi::mask (8, false, prec2)))
    mask_prec = 8;
  else if (wi::eq_p (mask, wi::mask (16, false, prec2)))
    mask_prec = 16;
  else if (wi::eq_p (mask, wi::mask (32, false, prec2)))
    mask_prec = 32;
  else if (wi::eq_p (mask, wi::mask (64, false, prec2)))
    mask_prec = 64;
  return bits_to_pe (MIN (prec1, mask_prec));
}

// Optimize BIT_AND_EXPR and BIT_IOR_EXPR in terms of a mask if
// possible.  Basically, see if we can optimize:
//
//	[LB, UB] op Z
//   into:
//	[LB op Z, UB op Z]
//
// If the optimization was successful, accumulate the range in R and
// return TRUE.

static bool
wi_optimize_and_or (irange &r,
		    enum tree_code code,
		    tree type,
		    const wide_int &lh_lb, const wide_int &lh_ub,
		    const wide_int &rh_lb, const wide_int &rh_ub)
{
  // Calculate the singleton mask among the ranges, if any.
  wide_int lower_bound, upper_bound, mask;
  if (wi::eq_p (rh_lb, rh_ub))
    {
      mask = rh_lb;
      lower_bound = lh_lb;
      upper_bound = lh_ub;
    }
  else if (wi::eq_p (lh_lb, lh_ub))
    {
      mask = lh_lb;
      lower_bound = rh_lb;
      upper_bound = rh_ub;
    }
  else
    return false;

  // If Z is a constant which (for op | its bitwise not) has n
  // consecutive least significant bits cleared followed by m 1
  // consecutive bits set immediately above it and either
  // m + n == precision, or (x >> (m + n)) == (y >> (m + n)).
  //
  // The least significant n bits of all the values in the range are
  // cleared or set, the m bits above it are preserved and any bits
  // above these are required to be the same for all values in the
  // range.
  wide_int w = mask;
  int m = 0, n = 0;
  if (code == BIT_IOR_EXPR)
    w = ~w;
  if (wi::eq_p (w, 0))
    n = w.get_precision ();
  else
    {
      n = wi::ctz (w);
      w = ~(w | wi::mask (n, false, w.get_precision ()));
      if (wi::eq_p (w, 0))
	m = w.get_precision () - n;
      else
	m = wi::ctz (w) - n;
    }
  wide_int new_mask = wi::mask (m + n, true, w.get_precision ());
  if ((new_mask & lower_bound) != (new_mask & upper_bound))
    return false;

  wide_int res_lb, res_ub;
  if (code == BIT_AND_EXPR)
    {
      res_lb = wi::bit_and (lower_bound, mask);
      res_ub = wi::bit_and (upper_bound, mask);
    }
  else if (code == BIT_IOR_EXPR)
    {
      res_lb = wi::bit_or (lower_bound, mask);
      res_ub = wi::bit_or (upper_bound, mask);
    }
  else
    gcc_unreachable ();
  value_range_with_overflow (r, type, res_lb, res_ub);

  // Furthermore, if the mask is non-zero, an IOR cannot contain zero.
  if (code == BIT_IOR_EXPR && wi::ne_p (mask, 0))
    {
      int_range<2> tmp;
      tmp.set_nonzero (type);
      r.intersect (tmp);
    }
  return true;
}

// For range [LB, UB] compute two wide_int bit masks.
//
// In the MAYBE_NONZERO bit mask, if some bit is unset, it means that
// for all numbers in the range the bit is 0, otherwise it might be 0
// or 1.
//
// In the MUSTBE_NONZERO bit mask, if some bit is set, it means that
// for all numbers in the range the bit is 1, otherwise it might be 0
// or 1.

void
wi_set_zero_nonzero_bits (tree type,
			  const wide_int &lb, const wide_int &ub,
			  wide_int &maybe_nonzero,
			  wide_int &mustbe_nonzero)
{
  signop sign = TYPE_SIGN (type);

  if (wi::eq_p (lb, ub))
    maybe_nonzero = mustbe_nonzero = lb;
  else if (wi::ge_p (lb, 0, sign) || wi::lt_p (ub, 0, sign))
    {
      wide_int xor_mask = lb ^ ub;
      maybe_nonzero = lb | ub;
      mustbe_nonzero = lb & ub;
      if (xor_mask != 0)
	{
	  wide_int mask = wi::mask (wi::floor_log2 (xor_mask), false,
				    maybe_nonzero.get_precision ());
	  maybe_nonzero = maybe_nonzero | mask;
	  mustbe_nonzero = wi::bit_and_not (mustbe_nonzero, mask);
	}
    }
  else
    {
      maybe_nonzero = wi::minus_one (lb.get_precision ());
      mustbe_nonzero = wi::zero (lb.get_precision ());
    }
}

void
operator_bitwise_and::wi_fold (irange &r, tree type,
			       const wide_int &lh_lb,
			       const wide_int &lh_ub,
			       const wide_int &rh_lb,
			       const wide_int &rh_ub) const
{
  if (wi_optimize_and_or (r, BIT_AND_EXPR, type, lh_lb, lh_ub, rh_lb, rh_ub))
    return;

  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);

  wide_int new_lb = mustbe_nonzero_lh & mustbe_nonzero_rh;
  wide_int new_ub = maybe_nonzero_lh & maybe_nonzero_rh;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  // If both input ranges contain only negative values, we can
  // truncate the result range maximum to the minimum of the
  // input range maxima.
  if (wi::lt_p (lh_ub, 0, sign) && wi::lt_p (rh_ub, 0, sign))
    {
      new_ub = wi::min (new_ub, lh_ub, sign);
      new_ub = wi::min (new_ub, rh_ub, sign);
    }
  // If either input range contains only non-negative values
  // we can truncate the result range maximum to the respective
  // maximum of the input range.
  if (wi::ge_p (lh_lb, 0, sign))
    new_ub = wi::min (new_ub, lh_ub, sign);
  if (wi::ge_p (rh_lb, 0, sign))
    new_ub = wi::min (new_ub, rh_ub, sign);
  // PR68217: In case of signed & sign-bit-CST should
  // result in [-INF, 0] instead of [-INF, INF].
  if (wi::gt_p (new_lb, new_ub, sign))
    {
      wide_int sign_bit = wi::set_bit_in_zero (prec - 1, prec);
      if (sign == SIGNED
	  && ((wi::eq_p (lh_lb, lh_ub)
	       && !wi::cmps (lh_lb, sign_bit))
	      || (wi::eq_p (rh_lb, rh_ub)
		  && !wi::cmps (rh_lb, sign_bit))))
	{
	  new_lb = wi::min_value (prec, sign);
	  new_ub = wi::zero (prec);
	}
    }
  // If the limits got swapped around, return varying.
  if (wi::gt_p (new_lb, new_ub,sign))
    {
      if (sign == SIGNED
	  && wi_optimize_signed_bitwise_op (r, type,
					    lh_lb, lh_ub,
					    rh_lb, rh_ub))
	return;
      r.set_varying (type);
    }
  else
    value_range_with_overflow (r, type, new_lb, new_ub);
}

static void
set_nonzero_range_from_mask (irange &r, tree type, const irange &lhs)
{
  if (lhs.undefined_p () || contains_zero_p (lhs))
    r.set_varying (type);
  else
    r.set_nonzero (type);
}

/* Find out smallest RES where RES > VAL && (RES & MASK) == RES, if any
   (otherwise return VAL).  VAL and MASK must be zero-extended for
   precision PREC.  If SGNBIT is non-zero, first xor VAL with SGNBIT
   (to transform signed values into unsigned) and at the end xor
   SGNBIT back.  */

wide_int
masked_increment (const wide_int &val_in, const wide_int &mask,
		  const wide_int &sgnbit, unsigned int prec)
{
  wide_int bit = wi::one (prec), res;
  unsigned int i;

  wide_int val = val_in ^ sgnbit;
  for (i = 0; i < prec; i++, bit += bit)
    {
      res = mask;
      if ((res & bit) == 0)
	continue;
      res = bit - 1;
      res = wi::bit_and_not (val + bit, res);
      res &= mask;
      if (wi::gtu_p (res, val))
	return res ^ sgnbit;
    }
  return val ^ sgnbit;
}

// This was shamelessly stolen from register_edge_assert_for_2 and
// adjusted to work with iranges.

void
operator_bitwise_and::simple_op1_range_solver (irange &r, tree type,
					       const irange &lhs,
					       const irange &op2) const
{
  if (!op2.singleton_p ())
    {
      set_nonzero_range_from_mask (r, type, lhs);
      return;
    }
  unsigned int nprec = TYPE_PRECISION (type);
  wide_int cst2v = op2.lower_bound ();
  bool cst2n = wi::neg_p (cst2v, TYPE_SIGN (type));
  wide_int sgnbit;
  if (cst2n)
    sgnbit = wi::set_bit_in_zero (nprec - 1, nprec);
  else
    sgnbit = wi::zero (nprec);

  // Solve [lhs.lower_bound (), +INF] = x & MASK.
  //
  // Minimum unsigned value for >= if (VAL & CST2) == VAL is VAL and
  // maximum unsigned value is ~0.  For signed comparison, if CST2
  // doesn't have the most significant bit set, handle it similarly.  If
  // CST2 has MSB set, the minimum is the same, and maximum is ~0U/2.
  wide_int valv = lhs.lower_bound ();
  wide_int minv = valv & cst2v, maxv;
  bool we_know_nothing = false;
  if (minv != valv)
    {
      // If (VAL & CST2) != VAL, X & CST2 can't be equal to VAL.
      minv = masked_increment (valv, cst2v, sgnbit, nprec);
      if (minv == valv)
	{
	  // If we can't determine anything on this bound, fall
	  // through and conservatively solve for the other end point.
	  we_know_nothing = true;
	}
    }
  maxv = wi::mask (nprec - (cst2n ? 1 : 0), false, nprec);
  if (we_know_nothing)
    r.set_varying (type);
  else
    create_possibly_reversed_range (r, type, minv, maxv);

  // Solve [-INF, lhs.upper_bound ()] = x & MASK.
  //
  // Minimum unsigned value for <= is 0 and maximum unsigned value is
  // VAL | ~CST2 if (VAL & CST2) == VAL.  Otherwise, find smallest
  // VAL2 where
  // VAL2 > VAL && (VAL2 & CST2) == VAL2 and use (VAL2 - 1) | ~CST2
  // as maximum.
  // For signed comparison, if CST2 doesn't have most significant bit
  // set, handle it similarly.  If CST2 has MSB set, the maximum is
  // the same and minimum is INT_MIN.
  valv = lhs.upper_bound ();
  minv = valv & cst2v;
  if (minv == valv)
    maxv = valv;
  else
    {
      maxv = masked_increment (valv, cst2v, sgnbit, nprec);
      if (maxv == valv)
	{
	  // If we couldn't determine anything on either bound, return
	  // undefined.
	  if (we_know_nothing)
	    r.set_undefined ();
	  return;
	}
      maxv -= 1;
    }
  maxv |= ~cst2v;
  minv = sgnbit;
  int_range<2> upper_bits;
  create_possibly_reversed_range (upper_bits, type, minv, maxv);
  r.intersect (upper_bits);
}

bool
operator_bitwise_and::op1_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op2,
				 relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_and.op1_range (r, type, lhs, op2);

  r.set_undefined ();
  for (unsigned i = 0; i < lhs.num_pairs (); ++i)
    {
      int_range_max chunk (lhs.type (),
			   lhs.lower_bound (i),
			   lhs.upper_bound (i));
      int_range_max res;
      simple_op1_range_solver (res, type, chunk, op2);
      r.union_ (res);
    }
  if (r.undefined_p ())
    set_nonzero_range_from_mask (r, type, lhs);

  // For MASK == op1 & MASK, all the bits in MASK must be set in op1.
  wide_int mask;
  if (lhs == op2 && lhs.singleton_p (mask))
    {
      r.update_bitmask (irange_bitmask (mask, ~mask));
      return true;
    }

  // For 0 = op1 & MASK, op1 is ~MASK.
  if (lhs.zero_p () && op2.singleton_p ())
    {
      wide_int nz = wi::bit_not (op2.get_nonzero_bits ());
      int_range<2> tmp (type);
      tmp.set_nonzero_bits (nz);
      r.intersect (tmp);
    }
  return true;
}

bool
operator_bitwise_and::op2_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op1,
				 relation_trio) const
{
  return operator_bitwise_and::op1_range (r, type, lhs, op1);
}


class operator_logical_or : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  virtual bool fold_range (irange &r, tree type,
			   const irange &lh,
			   const irange &rh,
			   relation_trio rel = TRIO_VARYING) const;
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio rel = TRIO_VARYING) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio rel = TRIO_VARYING) const;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
} op_logical_or;

bool
operator_logical_or::fold_range (irange &r, tree type ATTRIBUTE_UNUSED,
				 const irange &lh,
				 const irange &rh,
				 relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;

  r = lh;
  r.union_ (rh);
  return true;
}

bool
operator_logical_or::op1_range (irange &r, tree type,
				const irange &lhs,
				const irange &op2 ATTRIBUTE_UNUSED,
				relation_trio) const
{
   switch (get_bool_state (r, lhs, type))
     {
     case BRS_FALSE:
       // A false result means both sides of the OR must be false.
       r = range_false (type);
       break;
     default:
       // Any other result means only one side has to be true, the
       // other side can be anything. so we can't be sure of any result
       // here.
       r = range_true_and_false (type);
       break;
    }
  return true;
}

bool
operator_logical_or::op2_range (irange &r, tree type,
				const irange &lhs,
				const irange &op1,
				relation_trio) const
{
  return operator_logical_or::op1_range (r, type, lhs, op1);
}


void
operator_bitwise_or::update_bitmask (irange &r, const irange &lh,
				     const irange &rh) const
{
  update_known_bitmask (r, BIT_IOR_EXPR, lh, rh);
}

void
operator_bitwise_or::wi_fold (irange &r, tree type,
			      const wide_int &lh_lb,
			      const wide_int &lh_ub,
			      const wide_int &rh_lb,
			      const wide_int &rh_ub) const
{
  if (wi_optimize_and_or (r, BIT_IOR_EXPR, type, lh_lb, lh_ub, rh_lb, rh_ub))
    return;

  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);
  wide_int new_lb = mustbe_nonzero_lh | mustbe_nonzero_rh;
  wide_int new_ub = maybe_nonzero_lh | maybe_nonzero_rh;
  signop sign = TYPE_SIGN (type);
  // If the input ranges contain only positive values we can
  // truncate the minimum of the result range to the maximum
  // of the input range minima.
  if (wi::ge_p (lh_lb, 0, sign)
      && wi::ge_p (rh_lb, 0, sign))
    {
      new_lb = wi::max (new_lb, lh_lb, sign);
      new_lb = wi::max (new_lb, rh_lb, sign);
    }
  // If either input range contains only negative values
  // we can truncate the minimum of the result range to the
  // respective minimum range.
  if (wi::lt_p (lh_ub, 0, sign))
    new_lb = wi::max (new_lb, lh_lb, sign);
  if (wi::lt_p (rh_ub, 0, sign))
    new_lb = wi::max (new_lb, rh_lb, sign);
  // If the limits got swapped around, return a conservative range.
  if (wi::gt_p (new_lb, new_ub, sign))
    {
      // Make sure that nonzero|X is nonzero.
      if (wi::gt_p (lh_lb, 0, sign)
	  || wi::gt_p (rh_lb, 0, sign)
	  || wi::lt_p (lh_ub, 0, sign)
	  || wi::lt_p (rh_ub, 0, sign))
	r.set_nonzero (type);
      else if (sign == SIGNED
	       && wi_optimize_signed_bitwise_op (r, type,
						 lh_lb, lh_ub,
						 rh_lb, rh_ub))
	return;
      else
	r.set_varying (type);
      return;
    }
  value_range_with_overflow (r, type, new_lb, new_ub);
}

bool
operator_bitwise_or::op1_range (irange &r, tree type,
				const irange &lhs,
				const irange &op2,
				relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  // If this is really a logical wi_fold, call that.
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_or.op1_range (r, type, lhs, op2);

  if (lhs.zero_p ())
    {
      r.set_zero (type);
      return true;
    }
  r.set_varying (type);
  return true;
}

bool
operator_bitwise_or::op2_range (irange &r, tree type,
				const irange &lhs,
				const irange &op1,
				relation_trio) const
{
  return operator_bitwise_or::op1_range (r, type, lhs, op1);
}

void
operator_bitwise_xor::update_bitmask (irange &r, const irange &lh,
				      const irange &rh) const
{
  update_known_bitmask (r, BIT_XOR_EXPR, lh, rh);
}

void
operator_bitwise_xor::wi_fold (irange &r, tree type,
			       const wide_int &lh_lb,
			       const wide_int &lh_ub,
			       const wide_int &rh_lb,
			       const wide_int &rh_ub) const
{
  signop sign = TYPE_SIGN (type);
  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);

  wide_int result_zero_bits = ((mustbe_nonzero_lh & mustbe_nonzero_rh)
			       | ~(maybe_nonzero_lh | maybe_nonzero_rh));
  wide_int result_one_bits
    = (wi::bit_and_not (mustbe_nonzero_lh, maybe_nonzero_rh)
       | wi::bit_and_not (mustbe_nonzero_rh, maybe_nonzero_lh));
  wide_int new_ub = ~result_zero_bits;
  wide_int new_lb = result_one_bits;

  // If the range has all positive or all negative values, the result
  // is better than VARYING.
  if (wi::lt_p (new_lb, 0, sign) || wi::ge_p (new_ub, 0, sign))
    value_range_with_overflow (r, type, new_lb, new_ub);
  else if (sign == SIGNED
	   && wi_optimize_signed_bitwise_op (r, type,
					     lh_lb, lh_ub,
					     rh_lb, rh_ub))
    ;  /* Do nothing.  */
  else
    r.set_varying (type);

  /* Furthermore, XOR is non-zero if its arguments can't be equal.  */
  if (wi::lt_p (lh_ub, rh_lb, sign)
      || wi::lt_p (rh_ub, lh_lb, sign)
      || wi::ne_p (result_one_bits, 0))
    {
      int_range<2> tmp;
      tmp.set_nonzero (type);
      r.intersect (tmp);
    }
}

bool
operator_bitwise_xor::op1_op2_relation_effect (irange &lhs_range,
					       tree type,
					       const irange &,
					       const irange &,
					       relation_kind rel) const
{
  if (rel == VREL_VARYING)
    return false;

  int_range<2> rel_range;

  switch (rel)
    {
    case VREL_EQ:
      rel_range.set_zero (type);
      break;
    case VREL_NE:
      rel_range.set_nonzero (type);
      break;
    default:
      return false;
    }

  lhs_range.intersect (rel_range);
  return true;
}

bool
operator_bitwise_xor::op1_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op2,
				 relation_trio) const
{
  if (lhs.undefined_p () || lhs.varying_p ())
    {
      r = lhs;
      return true;
    }
  if (types_compatible_p (type, boolean_type_node))
    {
      switch (get_bool_state (r, lhs, type))
	{
	case BRS_TRUE:
	  if (op2.varying_p ())
	    r.set_varying (type);
	  else if (op2.zero_p ())
	    r = range_true (type);
	  // See get_bool_state for the rationale
	  else if (op2.undefined_p () || contains_zero_p (op2))
	    r = range_true_and_false (type);
	  else
	    r = range_false (type);
	  break;
	case BRS_FALSE:
	  r = op2;
	  break;
	default:
	  break;
	}
      return true;
    }
  r.set_varying (type);
  return true;
}

bool
operator_bitwise_xor::op2_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op1,
				 relation_trio) const
{
  return operator_bitwise_xor::op1_range (r, type, lhs, op1);
}

class operator_trunc_mod : public range_operator
{
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  virtual void wi_fold (irange &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio) const;
  void update_bitmask (irange &r, const irange &lh, const irange &rh) const
    { update_known_bitmask (r, TRUNC_MOD_EXPR, lh, rh); }
} op_trunc_mod;

void
operator_trunc_mod::wi_fold (irange &r, tree type,
			     const wide_int &lh_lb,
			     const wide_int &lh_ub,
			     const wide_int &rh_lb,
			     const wide_int &rh_ub) const
{
  wide_int new_lb, new_ub, tmp;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);

  // Mod 0 is undefined.
  if (wi_zero_p (type, rh_lb, rh_ub))
    {
      r.set_undefined ();
      return;
    }

  // Check for constant and try to fold.
  if (lh_lb == lh_ub && rh_lb == rh_ub)
    {
      wi::overflow_type ov = wi::OVF_NONE;
      tmp = wi::mod_trunc (lh_lb, rh_lb, sign, &ov);
      if (ov == wi::OVF_NONE)
	{
	  r = int_range<2> (type, tmp, tmp);
	  return;
	}
    }

  // ABS (A % B) < ABS (B) and either 0 <= A % B <= A or A <= A % B <= 0.
  new_ub = rh_ub - 1;
  if (sign == SIGNED)
    {
      tmp = -1 - rh_lb;
      new_ub = wi::smax (new_ub, tmp);
    }

  if (sign == UNSIGNED)
    new_lb = wi::zero (prec);
  else
    {
      new_lb = -new_ub;
      tmp = lh_lb;
      if (wi::gts_p (tmp, 0))
	tmp = wi::zero (prec);
      new_lb = wi::smax (new_lb, tmp);
    }
  tmp = lh_ub;
  if (sign == SIGNED && wi::neg_p (tmp))
    tmp = wi::zero (prec);
  new_ub = wi::min (new_ub, tmp, sign);

  value_range_with_overflow (r, type, new_lb, new_ub);
}

bool
operator_trunc_mod::op1_range (irange &r, tree type,
			       const irange &lhs,
			       const irange &,
			       relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  // PR 91029.
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  // (a % b) >= x && x > 0 , then a >= x.
  if (wi::gt_p (lhs.lower_bound (), 0, sign))
    {
      r = value_range (type, lhs.lower_bound (), wi::max_value (prec, sign));
      return true;
    }
  // (a % b) <= x && x < 0 , then a <= x.
  if (wi::lt_p (lhs.upper_bound (), 0, sign))
    {
      r = value_range (type, wi::min_value (prec, sign), lhs.upper_bound ());
      return true;
    }
  return false;
}

bool
operator_trunc_mod::op2_range (irange &r, tree type,
			       const irange &lhs,
			       const irange &,
			       relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  // PR 91029.
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  // (a % b) >= x && x > 0 , then b is in ~[-x, x] for signed
  //			       or b > x for unsigned.
  if (wi::gt_p (lhs.lower_bound (), 0, sign))
    {
      if (sign == SIGNED)
	r = value_range (type, wi::neg (lhs.lower_bound ()),
			 lhs.lower_bound (), VR_ANTI_RANGE);
      else if (wi::lt_p (lhs.lower_bound (), wi::max_value (prec, sign),
			 sign))
	r = value_range (type, lhs.lower_bound () + 1,
			 wi::max_value (prec, sign));
      else
	return false;
      return true;
    }
  // (a % b) <= x && x < 0 , then b is in ~[x, -x].
  if (wi::lt_p (lhs.upper_bound (), 0, sign))
    {
      if (wi::gt_p (lhs.upper_bound (), wi::min_value (prec, sign), sign))
	r = value_range (type, lhs.upper_bound (),
			 wi::neg (lhs.upper_bound ()), VR_ANTI_RANGE);
      else
	return false;
      return true;
    }
  return false;
}


class operator_logical_not : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
public:
  virtual bool fold_range (irange &r, tree type,
			   const irange &lh,
			   const irange &rh,
			   relation_trio rel = TRIO_VARYING) const;
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio rel = TRIO_VARYING) const;
  // Check compatibility of LHS and op1.
  bool operand_check_p (tree t1, tree t2, tree) const final override
    { return range_compatible_p (t1, t2); }
} op_logical_not;

// Folding a logical NOT, oddly enough, involves doing nothing on the
// forward pass through.  During the initial walk backwards, the
// logical NOT reversed the desired outcome on the way back, so on the
// way forward all we do is pass the range forward.
//
// 	b_2 = x_1 < 20
// 	b_3 = !b_2
// 	if (b_3)
//  to determine the TRUE branch, walking  backward
//       if (b_3)		if ([1,1])
//       b_3 = !b_2		[1,1] = ![0,0]
// 	 b_2 = x_1 < 20		[0,0] = x_1 < 20,   false, so x_1 == [20, 255]
//   which is the result we are looking for.. so.. pass it through.

bool
operator_logical_not::fold_range (irange &r, tree type,
				  const irange &lh,
				  const irange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;

  r = lh;
  if (!lh.varying_p () && !lh.undefined_p ())
    r.invert ();

  return true;
}

bool
operator_logical_not::op1_range (irange &r,
				 tree type,
				 const irange &lhs,
				 const irange &op2,
				 relation_trio) const
{
  // Logical NOT is involutary...do it again.
  return fold_range (r, type, lhs, op2);
}

bool
operator_bitwise_not::fold_range (irange &r, tree type,
				  const irange &lh,
				  const irange &rh,
				  relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;

  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.fold_range (r, type, lh, rh);

  // ~X is simply -1 - X.
  int_range<1> minusone (type, wi::minus_one (TYPE_PRECISION (type)),
			 wi::minus_one (TYPE_PRECISION (type)));
  return range_op_handler (MINUS_EXPR).fold_range (r, type, minusone, lh);
}

bool
operator_bitwise_not::op1_range (irange &r, tree type,
				 const irange &lhs,
				 const irange &op2,
				 relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.op1_range (r, type, lhs, op2);

  // ~X is -1 - X and since bitwise NOT is involutary...do it again.
  return fold_range (r, type, lhs, op2);
}

void
operator_bitwise_not::update_bitmask (irange &r, const irange &lh,
				      const irange &rh) const
{
  update_known_bitmask (r, BIT_NOT_EXPR, lh, rh);
}


bool
operator_cst::fold_range (irange &r, tree type ATTRIBUTE_UNUSED,
			  const irange &lh,
			  const irange &rh ATTRIBUTE_UNUSED,
			  relation_trio) const
{
  r = lh;
  return true;
}


// Determine if there is a relationship between LHS and OP1.

relation_kind
operator_identity::lhs_op1_relation (const irange &lhs,
				     const irange &op1 ATTRIBUTE_UNUSED,
				     const irange &op2 ATTRIBUTE_UNUSED,
				     relation_kind) const
{
  if (lhs.undefined_p ())
    return VREL_VARYING;
  // Simply a copy, so they are equivalent.
  return VREL_EQ;
}

bool
operator_identity::fold_range (irange &r, tree type ATTRIBUTE_UNUSED,
			       const irange &lh,
			       const irange &rh ATTRIBUTE_UNUSED,
			       relation_trio) const
{
  r = lh;
  return true;
}

bool
operator_identity::op1_range (irange &r, tree type ATTRIBUTE_UNUSED,
			      const irange &lhs,
			      const irange &op2 ATTRIBUTE_UNUSED,
			      relation_trio) const
{
  r = lhs;
  return true;
}


class operator_unknown : public range_operator
{
  using range_operator::fold_range;
public:
  virtual bool fold_range (irange &r, tree type,
			   const irange &op1,
			   const irange &op2,
			   relation_trio rel = TRIO_VARYING) const;
} op_unknown;

bool
operator_unknown::fold_range (irange &r, tree type,
			      const irange &lh ATTRIBUTE_UNUSED,
			      const irange &rh ATTRIBUTE_UNUSED,
			      relation_trio) const
{
  r.set_varying (type);
  return true;
}


void
operator_abs::wi_fold (irange &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb ATTRIBUTE_UNUSED,
		       const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  wide_int min, max;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);

  // Pass through LH for the easy cases.
  if (sign == UNSIGNED || wi::ge_p (lh_lb, 0, sign))
    {
      r = int_range<1> (type, lh_lb, lh_ub);
      return;
    }

  // -TYPE_MIN_VALUE = TYPE_MIN_VALUE with flag_wrapv so we can't get
  // a useful range.
  wide_int min_value = wi::min_value (prec, sign);
  wide_int max_value = wi::max_value (prec, sign);
  if (!TYPE_OVERFLOW_UNDEFINED (type) && wi::eq_p (lh_lb, min_value))
    {
      r.set_varying (type);
      return;
    }

  // ABS_EXPR may flip the range around, if the original range
  // included negative values.
  if (wi::eq_p (lh_lb, min_value))
    {
      // ABS ([-MIN, -MIN]) isn't representable, but we have traditionally
      // returned [-MIN,-MIN] so this preserves that behavior.  PR37078
      if (wi::eq_p (lh_ub, min_value))
	{
	  r = int_range<1> (type, min_value, min_value);
	  return;
	}
      min = max_value;
    }
  else
    min = wi::abs (lh_lb);

  if (wi::eq_p (lh_ub, min_value))
    max = max_value;
  else
    max = wi::abs (lh_ub);

  // If the range contains zero then we know that the minimum value in the
  // range will be zero.
  if (wi::le_p (lh_lb, 0, sign) && wi::ge_p (lh_ub, 0, sign))
    {
      if (wi::gt_p (min, max, sign))
	max = min;
      min = wi::zero (prec);
    }
  else
    {
      // If the range was reversed, swap MIN and MAX.
      if (wi::gt_p (min, max, sign))
	std::swap (min, max);
    }

  // If the new range has its limits swapped around (MIN > MAX), then
  // the operation caused one of them to wrap around.  The only thing
  // we know is that the result is positive.
  if (wi::gt_p (min, max, sign))
    {
      min = wi::zero (prec);
      max = max_value;
    }
  r = int_range<1> (type, min, max);
}

bool
operator_abs::op1_range (irange &r, tree type,
			 const irange &lhs,
			 const irange &op2,
			 relation_trio) const
{
  if (empty_range_varying (r, type, lhs, op2))
    return true;
  if (TYPE_UNSIGNED (type))
    {
      r = lhs;
      return true;
    }
  // Start with the positives because negatives are an impossible result.
  int_range_max positives = range_positives (type);
  positives.intersect (lhs);
  r = positives;
  // Then add the negative of each pair:
  // ABS(op1) = [5,20] would yield op1 => [-20,-5][5,20].
  for (unsigned i = 0; i < positives.num_pairs (); ++i)
    r.union_ (int_range<1> (type,
			    -positives.upper_bound (i),
			    -positives.lower_bound (i)));
  // With flag_wrapv, -TYPE_MIN_VALUE = TYPE_MIN_VALUE which is
  // unrepresentable.  Add -TYPE_MIN_VALUE in this case.
  wide_int min_value = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  wide_int lb = lhs.lower_bound ();
  if (!TYPE_OVERFLOW_UNDEFINED (type) && wi::eq_p (lb, min_value))
    r.union_ (int_range<2> (type, lb, lb));
  return true;
}

void
operator_abs::update_bitmask (irange &r, const irange &lh,
			      const irange &rh) const
{
  update_known_bitmask (r, ABS_EXPR, lh, rh);
}

class operator_absu : public range_operator
{
 public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
  virtual void update_bitmask (irange &r, const irange &lh,
			       const irange &rh) const final override;
} op_absu;

void
operator_absu::wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb ATTRIBUTE_UNUSED,
			const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  wide_int new_lb, new_ub;

  // Pass through VR0 the easy cases.
  if (wi::ges_p (lh_lb, 0))
    {
      new_lb = lh_lb;
      new_ub = lh_ub;
    }
  else
    {
      new_lb = wi::abs (lh_lb);
      new_ub = wi::abs (lh_ub);

      // If the range contains zero then we know that the minimum
      // value in the range will be zero.
      if (wi::ges_p (lh_ub, 0))
	{
	  if (wi::gtu_p (new_lb, new_ub))
	    new_ub = new_lb;
	  new_lb = wi::zero (TYPE_PRECISION (type));
	}
      else
	std::swap (new_lb, new_ub);
    }

  gcc_checking_assert (TYPE_UNSIGNED (type));
  r = int_range<1> (type, new_lb, new_ub);
}

void
operator_absu::update_bitmask (irange &r, const irange &lh,
			      const irange &rh) const
{
  update_known_bitmask (r, ABSU_EXPR, lh, rh);
}


bool
operator_negate::fold_range (irange &r, tree type,
			     const irange &lh,
			     const irange &rh,
			     relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;
  // -X is simply 0 - X.
  return range_op_handler (MINUS_EXPR).fold_range (r, type,
						   range_zero (type), lh);
}

bool
operator_negate::op1_range (irange &r, tree type,
			    const irange &lhs,
			    const irange &op2,
			    relation_trio) const
{
  // NEGATE is involutory.
  return fold_range (r, type, lhs, op2);
}


bool
operator_addr_expr::fold_range (irange &r, tree type,
				const irange &lh,
				const irange &rh,
				relation_trio) const
{
  if (empty_range_varying (r, type, lh, rh))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2).
  if (lh.zero_p ())
    r = range_zero (type);
  else if (lh.undefined_p () || contains_zero_p (lh))
    r.set_varying (type);
  else
    r.set_nonzero (type);
  return true;
}

bool
operator_addr_expr::op1_range (irange &r, tree type,
			       const irange &lhs,
			       const irange &op2,
			       relation_trio) const
{
  if (empty_range_varying (r, type, lhs, op2))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2), but only
  // if we cant overflow, eitherwise a no-zero offset could wrap to zero.
  // See PR 111009.
  if (!lhs.undefined_p () && !contains_zero_p (lhs) && TYPE_OVERFLOW_UNDEFINED (type))
    r.set_nonzero (type);
  else
    r.set_varying (type);
  return true;
}

// Initialize any integral operators to the primary table

void
range_op_table::initialize_integral_ops ()
{
  set (TRUNC_DIV_EXPR, op_trunc_div);
  set (FLOOR_DIV_EXPR, op_floor_div);
  set (ROUND_DIV_EXPR, op_round_div);
  set (CEIL_DIV_EXPR, op_ceil_div);
  set (EXACT_DIV_EXPR, op_exact_div);
  set (LSHIFT_EXPR, op_lshift);
  set (RSHIFT_EXPR, op_rshift);
  set (TRUTH_AND_EXPR, op_logical_and);
  set (TRUTH_OR_EXPR, op_logical_or);
  set (TRUNC_MOD_EXPR, op_trunc_mod);
  set (TRUTH_NOT_EXPR, op_logical_not);
  set (IMAGPART_EXPR, op_unknown);
  set (REALPART_EXPR, op_unknown);
  set (ABSU_EXPR, op_absu);
  set (OP_WIDEN_MULT_SIGNED, op_widen_mult_signed);
  set (OP_WIDEN_MULT_UNSIGNED, op_widen_mult_unsigned);
  set (OP_WIDEN_PLUS_SIGNED, op_widen_plus_signed);
  set (OP_WIDEN_PLUS_UNSIGNED, op_widen_plus_unsigned);

}

bool
operator_plus::overflow_free_p (const irange &lh, const irange &rh,
				relation_trio) const
{
  if (lh.undefined_p () || rh.undefined_p ())
    return false;

  tree type = lh.type ();
  if (TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  wi::overflow_type ovf;
  signop sgn = TYPE_SIGN (type);
  wide_int wmax0 = lh.upper_bound ();
  wide_int wmax1 = rh.upper_bound ();
  wi::add (wmax0, wmax1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  if (TYPE_UNSIGNED (type))
    return true;

  wide_int wmin0 = lh.lower_bound ();
  wide_int wmin1 = rh.lower_bound ();
  wi::add (wmin0, wmin1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  return true;
}

bool
operator_minus::overflow_free_p (const irange &lh, const irange &rh,
				 relation_trio) const
{
  if (lh.undefined_p () || rh.undefined_p ())
    return false;

  tree type = lh.type ();
  if (TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  wi::overflow_type ovf;
  signop sgn = TYPE_SIGN (type);
  wide_int wmin0 = lh.lower_bound ();
  wide_int wmax1 = rh.upper_bound ();
  wi::sub (wmin0, wmax1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  if (TYPE_UNSIGNED (type))
    return true;

  wide_int wmax0 = lh.upper_bound ();
  wide_int wmin1 = rh.lower_bound ();
  wi::sub (wmax0, wmin1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  return true;
}

bool
operator_mult::overflow_free_p (const irange &lh, const irange &rh,
				relation_trio) const
{
  if (lh.undefined_p () || rh.undefined_p ())
    return false;

  tree type = lh.type ();
  if (TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  wi::overflow_type ovf;
  signop sgn = TYPE_SIGN (type);
  wide_int wmax0 = lh.upper_bound ();
  wide_int wmax1 = rh.upper_bound ();
  wi::mul (wmax0, wmax1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  if (TYPE_UNSIGNED (type))
    return true;

  wide_int wmin0 = lh.lower_bound ();
  wide_int wmin1 = rh.lower_bound ();
  wi::mul (wmin0, wmin1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  wi::mul (wmin0, wmax1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  wi::mul (wmax0, wmin1, sgn, &ovf);
  if (ovf != wi::OVF_NONE)
    return false;

  return true;
}

#if CHECKING_P
#include "selftest.h"

namespace selftest
{
#define INT(x) wi::shwi ((x), TYPE_PRECISION (integer_type_node))
#define UINT(x) wi::uhwi ((x), TYPE_PRECISION (unsigned_type_node))
#define INT16(x) wi::shwi ((x), TYPE_PRECISION (short_integer_type_node))
#define UINT16(x) wi::uhwi ((x), TYPE_PRECISION (short_unsigned_type_node))
#define SCHAR(x) wi::shwi ((x), TYPE_PRECISION (signed_char_type_node))
#define UCHAR(x) wi::uhwi ((x), TYPE_PRECISION (unsigned_char_type_node))

static void
range_op_cast_tests ()
{
  int_range<2> r0, r1, r2, rold;
  r0.set_varying (integer_type_node);
  wide_int maxint = r0.upper_bound ();

  // If a range is in any way outside of the range for the converted
  // to range, default to the range for the new type.
  r0.set_varying (short_integer_type_node);
  wide_int minshort = r0.lower_bound ();
  wide_int maxshort = r0.upper_bound ();
  if (TYPE_PRECISION (integer_type_node)
      > TYPE_PRECISION (short_integer_type_node))
    {
      r1 = int_range<1> (integer_type_node,
			 wi::zero (TYPE_PRECISION (integer_type_node)),
			 maxint);
      range_cast (r1, short_integer_type_node);
      ASSERT_TRUE (r1.lower_bound () == minshort
		   && r1.upper_bound() == maxshort);
    }

  // (unsigned char)[-5,-1] => [251,255].
  r0 = rold = int_range<1> (signed_char_type_node, SCHAR (-5), SCHAR (-1));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == int_range<1> (unsigned_char_type_node,
				   UCHAR (251), UCHAR (255)));
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (signed char)[15, 150] => [-128,-106][15,127].
  r0 = rold = int_range<1> (unsigned_char_type_node, UCHAR (15), UCHAR (150));
  range_cast (r0, signed_char_type_node);
  r1 = int_range<1> (signed_char_type_node, SCHAR (15), SCHAR (127));
  r2 = int_range<1> (signed_char_type_node, SCHAR (-128), SCHAR (-106));
  r1.union_ (r2);
  ASSERT_TRUE (r1 == r0);
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5, 5] => [0,5][251,255].
  r0 = rold = int_range<1> (signed_char_type_node, SCHAR (-5), SCHAR (5));
  range_cast (r0, unsigned_char_type_node);
  r1 = int_range<1> (unsigned_char_type_node, UCHAR (251), UCHAR (255));
  r2 = int_range<1> (unsigned_char_type_node, UCHAR (0), UCHAR (5));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5,5] => [0,5][251,255].
  r0 = int_range<1> (integer_type_node, INT (-5), INT (5));
  range_cast (r0, unsigned_char_type_node);
  r1 = int_range<1> (unsigned_char_type_node, UCHAR (0), UCHAR (5));
  r1.union_ (int_range<1> (unsigned_char_type_node, UCHAR (251), UCHAR (255)));
  ASSERT_TRUE (r0 == r1);

  // (unsigned char)[5U,1974U] => [0,255].
  r0 = int_range<1> (unsigned_type_node, UINT (5), UINT (1974));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == int_range<1> (unsigned_char_type_node, UCHAR (0), UCHAR (255)));
  range_cast (r0, integer_type_node);
  // Going to a wider range should not sign extend.
  ASSERT_TRUE (r0 == int_range<1> (integer_type_node, INT (0), INT (255)));

  // (unsigned char)[-350,15] => [0,255].
  r0 = int_range<1> (integer_type_node, INT (-350), INT (15));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == (int_range<1>
		      (unsigned_char_type_node,
		       min_limit (unsigned_char_type_node),
		       max_limit (unsigned_char_type_node))));

  // Casting [-120,20] from signed char to unsigned short.
  // => [0, 20][0xff88, 0xffff].
  r0 = int_range<1> (signed_char_type_node, SCHAR (-120), SCHAR (20));
  range_cast (r0, short_unsigned_type_node);
  r1 = int_range<1> (short_unsigned_type_node, UINT16 (0), UINT16 (20));
  r2 = int_range<1> (short_unsigned_type_node,
		     UINT16 (0xff88), UINT16 (0xffff));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  // A truncating cast back to signed char will work because [-120, 20]
  // is representable in signed char.
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == int_range<1> (signed_char_type_node,
				   SCHAR (-120), SCHAR (20)));

  // unsigned char -> signed short
  //	(signed short)[(unsigned char)25, (unsigned char)250]
  // => [(signed short)25, (signed short)250]
  r0 = rold = int_range<1> (unsigned_char_type_node, UCHAR (25), UCHAR (250));
  range_cast (r0, short_integer_type_node);
  r1 = int_range<1> (short_integer_type_node, INT16 (25), INT16 (250));
  ASSERT_TRUE (r0 == r1);
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // Test casting a wider signed [-MIN,MAX] to a narrower unsigned.
  r0 = int_range<1> (long_long_integer_type_node,
		     min_limit (long_long_integer_type_node),
		     max_limit (long_long_integer_type_node));
  range_cast (r0, short_unsigned_type_node);
  r1 = int_range<1> (short_unsigned_type_node,
		     min_limit (short_unsigned_type_node),
		     max_limit (short_unsigned_type_node));
  ASSERT_TRUE (r0 == r1);

  // Casting NONZERO to a narrower type will wrap/overflow so
  // it's just the entire range for the narrower type.
  //
  // "NOT 0 at signed 32-bits" ==> [-MIN_32,-1][1, +MAX_32].  This is
  // is outside of the range of a smaller range, return the full
  // smaller range.
  if (TYPE_PRECISION (integer_type_node)
      > TYPE_PRECISION (short_integer_type_node))
    {
      r0 = range_nonzero (integer_type_node);
      range_cast (r0, short_integer_type_node);
      r1 = int_range<1> (short_integer_type_node,
			 min_limit (short_integer_type_node),
			 max_limit (short_integer_type_node));
      ASSERT_TRUE (r0 == r1);
    }

  // Casting NONZERO from a narrower signed to a wider signed.
  //
  // NONZERO signed 16-bits is [-MIN_16,-1][1, +MAX_16].
  // Converting this to 32-bits signed is [-MIN_16,-1][1, +MAX_16].
  r0 = range_nonzero (short_integer_type_node);
  range_cast (r0, integer_type_node);
  r1 = int_range<1> (integer_type_node, INT (-32768), INT (-1));
  r2 = int_range<1> (integer_type_node, INT (1), INT (32767));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
}

static void
range_op_lshift_tests ()
{
  // Test that 0x808.... & 0x8.... still contains 0x8....
  // for a large set of numbers.
  {
    int_range_max res;
    tree big_type = long_long_unsigned_type_node;
    unsigned big_prec = TYPE_PRECISION (big_type);
    // big_num = 0x808,0000,0000,0000
    wide_int big_num = wi::lshift (wi::uhwi (0x808, big_prec),
				   wi::uhwi (48, big_prec));
    op_bitwise_and.fold_range (res, big_type,
			       int_range <1> (big_type),
			       int_range <1> (big_type, big_num, big_num));
    // val = 0x8,0000,0000,0000
    wide_int val = wi::lshift (wi::uhwi (8, big_prec),
			       wi::uhwi (48, big_prec));
    ASSERT_TRUE (res.contains_p (val));
  }

  if (TYPE_PRECISION (unsigned_type_node) > 31)
    {
      // unsigned VARYING = op1 << 1 should be VARYING.
      int_range<2> lhs (unsigned_type_node);
      int_range<2> shift (unsigned_type_node, INT (1), INT (1));
      int_range_max op1;
      op_lshift.op1_range (op1, unsigned_type_node, lhs, shift);
      ASSERT_TRUE (op1.varying_p ());

      // 0 = op1 << 1  should be [0,0], [0x8000000, 0x8000000].
      int_range<2> zero (unsigned_type_node, UINT (0), UINT (0));
      op_lshift.op1_range (op1, unsigned_type_node, zero, shift);
      ASSERT_TRUE (op1.num_pairs () == 2);
      // Remove the [0,0] range.
      op1.intersect (zero);
      ASSERT_TRUE (op1.num_pairs () == 1);
      //  op1 << 1   should be [0x8000,0x8000] << 1,
      //  which should result in [0,0].
      int_range_max result;
      op_lshift.fold_range (result, unsigned_type_node, op1, shift);
      ASSERT_TRUE (result == zero);
    }
  // signed VARYING = op1 << 1 should be VARYING.
  if (TYPE_PRECISION (integer_type_node) > 31)
    {
      // unsigned VARYING = op1 << 1 should be VARYING.
      int_range<2> lhs (integer_type_node);
      int_range<2> shift (integer_type_node, INT (1), INT (1));
      int_range_max op1;
      op_lshift.op1_range (op1, integer_type_node, lhs, shift);
      ASSERT_TRUE (op1.varying_p ());

      //  0 = op1 << 1  should be [0,0], [0x8000000, 0x8000000].
      int_range<2> zero (integer_type_node, INT (0), INT (0));
      op_lshift.op1_range (op1, integer_type_node, zero, shift);
      ASSERT_TRUE (op1.num_pairs () == 2);
      // Remove the [0,0] range.
      op1.intersect (zero);
      ASSERT_TRUE (op1.num_pairs () == 1);
      //  op1 << 1   should be [0x8000,0x8000] << 1,
      //  which should result in [0,0].
      int_range_max result;
      op_lshift.fold_range (result, unsigned_type_node, op1, shift);
      ASSERT_TRUE (result == zero);
    }
}

static void
range_op_rshift_tests ()
{
  // unsigned: [3, MAX] = OP1 >> 1
  {
    int_range_max lhs (unsigned_type_node,
		       UINT (3), max_limit (unsigned_type_node));
    int_range_max one (unsigned_type_node,
		       wi::one (TYPE_PRECISION (unsigned_type_node)),
		       wi::one (TYPE_PRECISION (unsigned_type_node)));
    int_range_max op1;
    op_rshift.op1_range (op1, unsigned_type_node, lhs, one);
    ASSERT_FALSE (op1.contains_p (UINT (3)));
  }

  // signed: [3, MAX] = OP1 >> 1
  {
    int_range_max lhs (integer_type_node,
		       INT (3), max_limit (integer_type_node));
    int_range_max one (integer_type_node, INT (1), INT (1));
    int_range_max op1;
    op_rshift.op1_range (op1, integer_type_node, lhs, one);
    ASSERT_FALSE (op1.contains_p (INT (-2)));
  }

  // This is impossible, so OP1 should be [].
  // signed: [MIN, MIN] = OP1 >> 1
  {
    int_range_max lhs (integer_type_node,
		       min_limit (integer_type_node),
		       min_limit (integer_type_node));
    int_range_max one (integer_type_node, INT (1), INT (1));
    int_range_max op1;
    op_rshift.op1_range (op1, integer_type_node, lhs, one);
    ASSERT_TRUE (op1.undefined_p ());
  }

  // signed: ~[-1] = OP1 >> 31
  if (TYPE_PRECISION (integer_type_node) > 31)
    {
      int_range_max lhs (integer_type_node, INT (-1), INT (-1), VR_ANTI_RANGE);
      int_range_max shift (integer_type_node, INT (31), INT (31));
      int_range_max op1;
      op_rshift.op1_range (op1, integer_type_node, lhs, shift);
      int_range_max negatives = range_negatives (integer_type_node);
      negatives.intersect (op1);
      ASSERT_TRUE (negatives.undefined_p ());
    }
}

static void
range_op_bitwise_and_tests ()
{
  int_range_max res;
  wide_int min = min_limit (integer_type_node);
  wide_int max = max_limit (integer_type_node);
  wide_int tiny = wi::add (min, wi::one (TYPE_PRECISION (integer_type_node)));
  int_range_max i1 (integer_type_node, tiny, max);
  int_range_max i2 (integer_type_node, INT (255), INT (255));

  // [MIN+1, MAX] = OP1 & 255: OP1 is VARYING
  op_bitwise_and.op1_range (res, integer_type_node, i1, i2);
  ASSERT_TRUE (res == int_range<1> (integer_type_node));

  // VARYING = OP1 & 255: OP1 is VARYING
  i1 = int_range<1> (integer_type_node);
  op_bitwise_and.op1_range (res, integer_type_node, i1, i2);
  ASSERT_TRUE (res == int_range<1> (integer_type_node));

  // For 0 = x & MASK, x is ~MASK.
  {
    int_range<2> zero (integer_type_node, INT (0), INT (0));
    int_range<2> mask = int_range<2> (integer_type_node, INT (7), INT (7));
    op_bitwise_and.op1_range (res, integer_type_node, zero, mask);
    wide_int inv = wi::shwi (~7U, TYPE_PRECISION (integer_type_node));
    ASSERT_TRUE (res.get_nonzero_bits () == inv);
  }

  // (NONZERO | X) is nonzero.
  i1.set_nonzero (integer_type_node);
  i2.set_varying (integer_type_node);
  op_bitwise_or.fold_range (res, integer_type_node, i1, i2);
  ASSERT_TRUE (res.nonzero_p ());

  // (NEGATIVE | X) is nonzero.
  i1 = int_range<1> (integer_type_node, INT (-5), INT (-3));
  i2.set_varying (integer_type_node);
  op_bitwise_or.fold_range (res, integer_type_node, i1, i2);
  ASSERT_FALSE (res.contains_p (INT (0)));
}

static void
range_relational_tests ()
{
  int_range<2> lhs (unsigned_char_type_node);
  int_range<2> op1 (unsigned_char_type_node, UCHAR (8), UCHAR (10));
  int_range<2> op2 (unsigned_char_type_node, UCHAR (20), UCHAR (20));

  // Never wrapping additions mean LHS > OP1.
  relation_kind code = op_plus.lhs_op1_relation (lhs, op1, op2, VREL_VARYING);
  ASSERT_TRUE (code == VREL_GT);

  // Most wrapping additions mean nothing...
  op1 = int_range<2> (unsigned_char_type_node, UCHAR (8), UCHAR (10));
  op2 = int_range<2> (unsigned_char_type_node, UCHAR (0), UCHAR (255));
  code = op_plus.lhs_op1_relation (lhs, op1, op2, VREL_VARYING);
  ASSERT_TRUE (code == VREL_VARYING);

  // However, always wrapping additions mean LHS < OP1.
  op1 = int_range<2> (unsigned_char_type_node, UCHAR (1), UCHAR (255));
  op2 = int_range<2> (unsigned_char_type_node, UCHAR (255), UCHAR (255));
  code = op_plus.lhs_op1_relation (lhs, op1, op2, VREL_VARYING);
  ASSERT_TRUE (code == VREL_LT);
}

void
range_op_tests ()
{
  range_op_rshift_tests ();
  range_op_lshift_tests ();
  range_op_bitwise_and_tests ();
  range_op_cast_tests ();
  range_relational_tests ();

  extern void range_op_float_tests ();
  range_op_float_tests ();
}

} // namespace selftest

#endif // CHECKING_P
