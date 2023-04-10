/* Header file for range operator class.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

#ifndef GCC_RANGE_OP_H
#define GCC_RANGE_OP_H

// This class is implemented for each kind of operator supported by
// the range generator.  It serves various purposes.
//
// 1 - Generates range information for the specific operation between
//     two ranges.  This provides the ability to fold ranges for an
//     expression.
//
// 2 - Performs range algebra on the expression such that a range can be
//     adjusted in terms of one of the operands:
//
//       def = op1 + op2
//
//     Given a range for def, we can adjust the range so that it is in
//     terms of either operand.
//
//     op1_range (def_range, op2) will adjust the range in place so it
//     is in terms of op1.  Since op1 = def - op2, it will subtract
//     op2 from each element of the range.
//
// 3 - Creates a range for an operand based on whether the result is 0 or
//     non-zero.  This is mostly for logical true false, but can serve other
//     purposes.
//       ie   0 = op1 - op2 implies op2 has the same range as op1.

class range_operator
{
  friend class range_op_table;
public:
  range_operator () : m_code (ERROR_MARK) { }
  // Perform an operation between 2 ranges and return it.
  virtual bool fold_range (irange &r, tree type,
			   const irange &lh,
			   const irange &rh,
			   relation_trio = TRIO_VARYING) const;

  // Return the range for op[12] in the general case.  LHS is the range for
  // the LHS of the expression, OP[12]is the range for the other
  //
  // The operand and the result is returned in R.
  //
  // TYPE is the expected type of the range.
  //
  // Return TRUE if the operation is performed and a valid range is available.
  //
  // i.e.  [LHS] = ??? + OP2
  // is re-formed as R = [LHS] - OP2.
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio = TRIO_VARYING) const;

  // The following routines are used to represent relations between the
  // various operations.  If the caller knows where the symbolics are,
  // it can query for relationships between them given known ranges.
  // the optional relation passed in is the relation between op1 and op2.
  virtual relation_kind lhs_op1_relation (const irange &lhs,
					  const irange &op1,
					  const irange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op2_relation (const irange &lhs,
					  const irange &op1,
					  const irange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind op1_op2_relation (const irange &lhs) const;
protected:
  // Perform an integral operation between 2 sub-ranges and return it.
  virtual void wi_fold (irange &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  // Effect of relation for generic fold_range clients.
  virtual bool op1_op2_relation_effect (irange &lhs_range, tree type,
					const irange &op1_range,
					const irange &op2_range,
					relation_kind rel) const;
  // Called by fold range to split small subranges into parts.
  void wi_fold_in_parts (irange &r, tree type,
			 const wide_int &lh_lb,
			 const wide_int &lh_ub,
			 const wide_int &rh_lb,
			 const wide_int &rh_ub) const;

  // Called by fold range to split small subranges into parts when op1 == op2
  void wi_fold_in_parts_equiv (irange &r, tree type,
			       const wide_int &lb,
			       const wide_int &ub,
			       unsigned limit) const;

  // Tree code of the range operator or ERROR_MARK if unknown.
  tree_code m_code;
};

// Like range_operator above, but for floating point operators.

class range_operator_float
{
public:
  virtual bool fold_range (frange &r, tree type,
			   const frange &lh,
			   const frange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual void rv_fold (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub,
			bool &maybe_nan,
			tree type,
			const REAL_VALUE_TYPE &lh_lb,
			const REAL_VALUE_TYPE &lh_ub,
			const REAL_VALUE_TYPE &rh_lb,
			const REAL_VALUE_TYPE &rh_ub,
			relation_kind) const;
  // Unary operations have the range of the LHS as op2.
  virtual bool fold_range (irange &r, tree type,
			   const frange &lh,
			   const irange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool fold_range (irange &r, tree type,
			   const frange &lh,
			   const frange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op1_range (frange &r, tree type,
			  const irange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op2_range (frange &r, tree type,
			  const irange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const;

  virtual relation_kind lhs_op1_relation (const frange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op1_relation (const irange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op2_relation (const frange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op2_relation (const irange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind op1_op2_relation (const irange &lhs) const;
  virtual relation_kind op1_op2_relation (const frange &lhs) const;
};

class range_op_handler
{
public:
  range_op_handler ();
  range_op_handler (enum tree_code code, tree type);
  inline operator bool () const { return m_valid; }

  bool fold_range (vrange &r, tree type,
		   const vrange &lh,
		   const vrange &rh,
		   relation_trio = TRIO_VARYING) const;
  bool op1_range (vrange &r, tree type,
		  const vrange &lhs,
		  const vrange &op2,
		  relation_trio = TRIO_VARYING) const;
  bool op2_range (vrange &r, tree type,
		  const vrange &lhs,
		  const vrange &op1,
		  relation_trio = TRIO_VARYING) const;
  relation_kind lhs_op1_relation (const vrange &lhs,
				  const vrange &op1,
				  const vrange &op2,
				  relation_kind = VREL_VARYING) const;
  relation_kind lhs_op2_relation (const vrange &lhs,
				  const vrange &op1,
				  const vrange &op2,
				  relation_kind = VREL_VARYING) const;
  relation_kind op1_op2_relation (const vrange &lhs) const;
protected:
  void set_op_handler (enum tree_code code, tree type);
  bool m_valid;
  range_operator *m_int;
  range_operator_float *m_float;
};

extern bool range_cast (vrange &, tree type);
extern void wi_set_zero_nonzero_bits (tree type,
				      const wide_int &, const wide_int &,
				      wide_int &maybe_nonzero,
				      wide_int &mustbe_nonzero);

// op1_op2_relation methods that are the same across irange and frange.
relation_kind equal_op1_op2_relation (const irange &lhs);
relation_kind not_equal_op1_op2_relation (const irange &lhs);
relation_kind lt_op1_op2_relation (const irange &lhs);
relation_kind le_op1_op2_relation (const irange &lhs);
relation_kind gt_op1_op2_relation (const irange &lhs);
relation_kind ge_op1_op2_relation (const irange &lhs);

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

// This implements the range operator tables as local objects.

class range_op_table
{
public:
  range_operator *operator[] (enum tree_code code);
protected:
  void set (enum tree_code code, range_operator &op);
private:
  range_operator *m_range_tree[MAX_TREE_CODES];
};

// Like above, but for floating point operators.

class floating_op_table
{
public:
  floating_op_table ();
  range_operator_float *operator[] (enum tree_code code);
private:
  void set (enum tree_code code, range_operator_float &op);
  range_operator_float *m_range_tree[MAX_TREE_CODES];
};

// This holds the range op table for floating point operations.
extern floating_op_table *floating_tree_table;

extern range_operator *ptr_op_widen_mult_signed;
extern range_operator *ptr_op_widen_mult_unsigned;
extern range_operator *ptr_op_widen_plus_signed;
extern range_operator *ptr_op_widen_plus_unsigned;
#endif // GCC_RANGE_OP_H
