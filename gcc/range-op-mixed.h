/* Header file for mixed range operator class.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_RANGE_OP_MIXED_H
#define GCC_RANGE_OP_MIXED_H

void update_known_bitmask (vrange &, tree_code, const vrange &, const vrange &);
bool minus_op1_op2_relation_effect (irange &lhs_range, tree type,
				    const irange &, const irange &,
				    relation_kind rel);


// Return TRUE if 0 is within [WMIN, WMAX].

inline bool
wi_includes_zero_p (tree type, const wide_int &wmin, const wide_int &wmax)
{
  signop sign = TYPE_SIGN (type);
  return wi::le_p (wmin, 0, sign) && wi::ge_p (wmax, 0, sign);
}

// Return TRUE if [WMIN, WMAX] is the singleton 0.

inline bool
wi_zero_p (tree type, const wide_int &wmin, const wide_int &wmax)
{
  unsigned prec = TYPE_PRECISION (type);
  return wmin == wmax && wi::eq_p (wmin, wi::zero (prec));
}


enum bool_range_state { BRS_FALSE, BRS_TRUE, BRS_EMPTY, BRS_FULL };
bool_range_state get_bool_state (vrange &r, const vrange &lhs, tree val_type);

// If the range of either op1 or op2 is undefined, set the result to
// varying and return TRUE.  If the caller truly cares about a result,
// they should pass in a varying if it has an undefined that it wants
// treated as a varying.

inline bool
empty_range_varying (vrange &r, tree type,
		     const vrange &op1, const vrange & op2)
{
  if (op1.undefined_p () || op2.undefined_p ())
    {
      r.set_varying (type);
      return true;
    }
  else
    return false;
}

// For relation opcodes, first try to see if the supplied relation
// forces a true or false result, and return that.
// Then check for undefined operands.  If none of this applies,
// return false.

inline bool
relop_early_resolve (irange &r, tree type, const vrange &op1,
		     const vrange &op2, relation_trio trio,
		     relation_kind my_rel)
{
  relation_kind rel = trio.op1_op2 ();
  // If known relation is a complete subset of this relation, always true.
  if (relation_union (rel, my_rel) == my_rel)
    {
      r = range_true (type);
      return true;
    }

  // If known relation has no subset of this relation, always false.
  if (relation_intersect (rel, my_rel) == VREL_UNDEFINED)
    {
      r = range_false (type);
      return true;
    }

  // If either operand is undefined, return VARYING.
  if (empty_range_varying (r, type, op1, op2))
    return true;

  return false;
}

// ----------------------------------------------------------------------
//                          Mixed Mode Operators.
// ----------------------------------------------------------------------

class operator_equal : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &val,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &val,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &val,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &val,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override;

  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
  { return range_compatible_p (t2, t3) && INTEGRAL_TYPE_P (t1); }
};

class operator_not_equal : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;

  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t0, tree t1, tree t2) const final override
  { return range_compatible_p (t1, t2) && INTEGRAL_TYPE_P (t0); }
};

class operator_lt :  public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
  { return range_compatible_p (t2, t3) && INTEGRAL_TYPE_P (t1); }
};

class operator_le :  public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override;

  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
  { return range_compatible_p (t2, t3) && INTEGRAL_TYPE_P (t1); }
};

class operator_gt :  public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
  { return range_compatible_p (t2, t3) && INTEGRAL_TYPE_P (t1); }
};

class operator_ge :  public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (prange &r, tree type,
		  const irange &lhs, const prange &op1,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;

  relation_kind op1_op2_relation (const irange &lhs, const irange &,
				  const irange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const final override;
  relation_kind op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check op1 and op2 for compatibility.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
  { return range_compatible_p (t2, t3) && INTEGRAL_TYPE_P (t1); }
};

class operator_identity : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::lhs_op1_relation;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (prange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (frange &r, tree type ATTRIBUTE_UNUSED,
		   const frange &op1, const frange &op2 ATTRIBUTE_UNUSED,
		   relation_trio = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const prange &lhs, const prange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type ATTRIBUTE_UNUSED,
		  const frange &lhs, const frange &op2 ATTRIBUTE_UNUSED,
		  relation_trio = TRIO_VARYING) const final override;
  relation_kind lhs_op1_relation (const irange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind rel) const final override;
  relation_kind lhs_op1_relation (const prange &lhs,
				  const prange &op1, const prange &op2,
				  relation_kind rel) const final override;
};

class operator_cst : public range_operator
{
public:
  using range_operator::fold_range;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (prange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (frange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
};


class operator_cast: public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::lhs_op1_relation;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (prange &r, tree type,
		   const prange &op1, const prange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (irange &r, tree type,
		   const prange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (prange &r, tree type,
		   const irange &op1, const prange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const prange &lhs, const prange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const prange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const irange &lhs, const prange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  relation_kind lhs_op1_relation (const irange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind) const final override;
  relation_kind lhs_op1_relation (const prange &lhs,
				  const prange &op1, const prange &op2,
				  relation_kind) const final override;
  relation_kind lhs_op1_relation (const prange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind) const final override;
  relation_kind lhs_op1_relation (const irange &lhs,
				  const prange &op1, const prange &op2,
				  relation_kind) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
private:
  bool truncating_cast_p (const irange &inner, const irange &outer) const;
  bool inside_domain_p (const wide_int &min, const wide_int &max,
			const irange &outer) const;
  void fold_pair (irange &r, unsigned index, const irange &inner,
			   const irange &outer) const;
};

class operator_plus : public range_operator
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::lhs_op1_relation;
  using range_operator::lhs_op2_relation;
  using range_operator::update_bitmask;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio) const final override;
  bool op2_range (frange &r, tree type,
		  const frange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;

  relation_kind lhs_op1_relation (const irange &lhs, const irange &op1,
				  const irange &op2,
				  relation_kind rel) const final override;
  relation_kind lhs_op2_relation (const irange &lhs, const irange &op1,
				  const irange &op2,
				  relation_kind rel) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;

  virtual bool overflow_free_p (const irange &lh, const irange &rh,
				relation_trio = TRIO_VARYING) const;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
private:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override;
  void rv_fold (frange &r, tree type,
		const REAL_VALUE_TYPE &lh_lb, const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb, const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override;
};

class operator_abs : public range_operator
{
 public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::update_bitmask;
  bool fold_range (frange &r, tree type,
		   const frange &op1, const frange &,
		   relation_trio = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type, const irange &lhs,
		  const irange &op2, relation_trio) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check compatibility of LHS and op1.
  bool operand_check_p (tree t1, tree t2, tree) const final override
    { return range_compatible_p (t1, t2); }
private:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override;

};

class operator_minus : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::lhs_op1_relation;
  using range_operator::op1_op2_relation_effect;
  using range_operator::update_bitmask;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio) const final override;
  bool op2_range (frange &r, tree type,
		  const frange &lhs,
		  const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;

  relation_kind lhs_op1_relation (const irange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind rel) const final override;
  bool op1_op2_relation_effect (irange &lhs_range, tree type,
				const irange &op1_range,
				const irange &op2_range,
				relation_kind rel) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;

  virtual bool overflow_free_p (const irange &lh, const irange &rh,
				relation_trio = TRIO_VARYING) const;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
private:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override;
  void rv_fold (frange &r, tree type,
		const REAL_VALUE_TYPE &lh_lb, const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb, const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override;
};

class operator_negate : public range_operator
{
 public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool fold_range (frange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;

  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  // Check compatibility of LHS and op1.
  bool operand_check_p (tree t1, tree t2, tree) const final override
    { return range_compatible_p (t1, t2); }
};


class cross_product_operator : public range_operator
{
public:
  virtual bool wi_op_overflows (wide_int &r,
				tree type,
				const wide_int &,
				const wide_int &) const = 0;
  void wi_cross_product (irange &r, tree type,
			 const wide_int &lh_lb,
			 const wide_int &lh_ub,
			 const wide_int &rh_lb,
			 const wide_int &rh_ub) const;
};

class operator_mult : public cross_product_operator
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::update_bitmask;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;

  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio) const final override;
  bool op2_range (frange &r, tree type,
		  const frange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;

  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;

  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override;
  bool wi_op_overflows (wide_int &res, tree type, const wide_int &w0,
			const wide_int &w1) const final override;

  void rv_fold (frange &r, tree type,
		const REAL_VALUE_TYPE &lh_lb, const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb, const REAL_VALUE_TYPE &rh_ub,
		relation_kind kind) const final override;
  virtual bool overflow_free_p (const irange &lh, const irange &rh,
				relation_trio = TRIO_VARYING) const;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
};

class operator_addr_expr : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  bool fold_range (irange &r, tree type,
		   const irange &op1, const irange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (prange &r, tree type,
		  const prange &lhs, const prange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
};

class operator_bitwise_not : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::update_bitmask;
  bool fold_range (irange &r, tree type,
		   const irange &lh, const irange &rh,
		   relation_trio rel = TRIO_VARYING) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
};

class operator_bitwise_xor : public range_operator
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::op1_op2_relation_effect;
  using range_operator::update_bitmask;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op1_op2_relation_effect (irange &lhs_range,
					tree type,
					const irange &op1_range,
					const irange &op2_range,
					relation_kind rel) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
private:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override;
};

class operator_bitwise_and : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::lhs_op1_relation;
  using range_operator::update_bitmask;
  bool fold_range (prange &r, tree type,
		   const prange &op1,
		   const prange &op2,
		   relation_trio) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const override;
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio rel = TRIO_VARYING) const override;
  relation_kind lhs_op1_relation (const irange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind) const override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
protected:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const override;
  void simple_op1_range_solver (irange &r, tree type,
				const irange &lhs,
				const irange &op2) const;
};

class operator_bitwise_or : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::update_bitmask;

  bool fold_range (prange &r, tree type,
		   const prange &op1,
		   const prange &op2,
		   relation_trio) const final override;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const override;
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio rel = TRIO_VARYING) const override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
protected:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const override;
};

class operator_min : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::update_bitmask;
  bool fold_range (prange &r, tree type,
		   const prange &op1,
		   const prange &op2,
		   relation_trio) const final override;
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
protected:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const override;
};

class operator_max : public range_operator
{
public:
  using range_operator::fold_range;
  using range_operator::update_bitmask;
  bool fold_range (prange &r, tree type,
		   const prange &op1,
		   const prange &op2,
		   relation_trio) const final override;
  void update_bitmask (irange &r, const irange &lh,
      const irange &rh) const override;
  // Check compatibility of all operands.
  bool operand_check_p (tree t1, tree t2, tree t3) const final override
    { return range_compatible_p (t1, t2) && range_compatible_p (t1, t3); }
protected:
  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const override;
};
#endif // GCC_RANGE_OP_MIXED_H
