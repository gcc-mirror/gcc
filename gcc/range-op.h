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
//
// 4 - All supported range combinations are explicitly specified.
//     Any desired combinations should be implemented for each operator.
//     When new range classes are added, new matching prototypes should be
//     added.

class range_operator
{
  friend class range_op_table;
public:
  // Perform an operation between 2 ranges and return it.
  virtual bool fold_range (irange &r, tree type,
			   const irange &lh,
			   const irange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool fold_range (frange &r, tree type,
			   const frange &lh,
			   const frange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool fold_range (irange &r, tree type,
			   const frange &lh,
			   const irange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool fold_range (irange &r, tree type,
			   const frange &lh,
			   const frange &rh,
			   relation_trio = TRIO_VARYING) const;
  virtual bool fold_range (frange &r, tree type,
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
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op1_range (frange &r, tree type,
			  const irange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const;


  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const;
  virtual bool op2_range (frange &r, tree type,
			  const irange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const;

  // The following routines are used to represent relations between the
  // various operations.  If the caller knows where the symbolics are,
  // it can query for relationships between them given known ranges.
  // the optional relation passed in is the relation between op1 and op2.
  virtual relation_kind lhs_op1_relation (const irange &lhs,
					  const irange &op1,
					  const irange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op1_relation (const frange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op1_relation (const irange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;

  virtual relation_kind lhs_op2_relation (const irange &lhs,
					  const irange &op1,
					  const irange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op2_relation (const frange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;
  virtual relation_kind lhs_op2_relation (const irange &lhs,
					  const frange &op1,
					  const frange &op2,
					  relation_kind = VREL_VARYING) const;

  virtual relation_kind op1_op2_relation (const irange &lhs,
					  const irange &op1,
					  const irange &op2) const;
  virtual relation_kind op1_op2_relation (const irange &lhs,
					  const frange &op1,
					  const frange &op2) const;
  virtual relation_kind op1_op2_relation (const frange &lhs,
					  const frange &op1,
					  const frange &op2) const;

  virtual bool overflow_free_p (const irange &lh, const irange &rh,
				relation_trio = TRIO_VARYING) const;
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
  // Apply any bitmasks implied by these ranges.
  virtual void update_bitmask (irange &, const irange &, const irange &) const;

  // Perform an float operation between 2 ranges and return it.
  virtual void rv_fold (frange &r, tree type,
			const REAL_VALUE_TYPE &lh_lb,
			const REAL_VALUE_TYPE &lh_ub,
			const REAL_VALUE_TYPE &rh_lb,
			const REAL_VALUE_TYPE &rh_ub,
			relation_kind) const;
};

class range_op_handler
{
public:
  range_op_handler ();
  range_op_handler (unsigned);
  operator bool () const;
  range_operator *range_op () const;

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
  relation_kind op1_op2_relation (const vrange &lhs,
				  const vrange &op1,
				  const vrange &op2) const;
  bool overflow_free_p (const vrange &lh, const vrange &rh,
			relation_trio = TRIO_VARYING) const;
protected:
  unsigned dispatch_kind (const vrange &lhs, const vrange &op1,
			  const vrange& op2) const;
  range_operator *m_operator;
};

// Cast the range in R to TYPE if R supports TYPE.

inline bool
range_cast (vrange &r, tree type)
{
  gcc_checking_assert (r.supports_type_p (type));
  Value_Range tmp (r);
  Value_Range varying (type);
  varying.set_varying (type);
  // Call op_convert, if it fails, the result is varying.
  if (!range_op_handler (CONVERT_EXPR).fold_range (r, type, tmp, varying))
    {
      r.set_varying (type);
      return false;
    }
  return true;
}

// Range cast which is capable of switching range kinds.
// ie for float to int.

inline bool
range_cast (Value_Range &r, tree type)
{
  Value_Range tmp (r);
  Value_Range varying (type);
  varying.set_varying (type);

  // Ensure we are in the correct mode for the call to fold.
  r.set_type (type);

  // Call op_convert, if it fails, the result is varying.
  if (!range_op_handler (CONVERT_EXPR).fold_range (r, type, tmp, varying))
    {
      r.set_varying (type);
      return false;
    }
  return true;
}


extern void wi_set_zero_nonzero_bits (tree type,
				      const wide_int &, const wide_int &,
				      wide_int &maybe_nonzero,
				      wide_int &mustbe_nonzero);

// These are extra operators that do not fit in the normal scheme of things.
// Add them to the end of the tree-code vector, and provide a name for
// each allowing for easy access when required.

#define OP_WIDEN_MULT_SIGNED	((unsigned) MAX_TREE_CODES)
#define OP_WIDEN_MULT_UNSIGNED	((unsigned) MAX_TREE_CODES + 1)
#define OP_WIDEN_PLUS_SIGNED	((unsigned) MAX_TREE_CODES + 2)
#define OP_WIDEN_PLUS_UNSIGNED	((unsigned) MAX_TREE_CODES + 3)
#define RANGE_OP_TABLE_SIZE	((unsigned) MAX_TREE_CODES + 4)

// This implements the range operator tables as local objects.

class range_op_table
{
public:
  range_op_table ();
  inline range_operator *operator[] (unsigned code)
    {
      gcc_checking_assert (code < RANGE_OP_TABLE_SIZE);
      return m_range_tree[code];
    }
protected:
  inline void set (unsigned code, range_operator &op)
    {
      gcc_checking_assert (code < RANGE_OP_TABLE_SIZE);
      gcc_checking_assert (m_range_tree[code] == NULL);
      m_range_tree[code] = &op;
    }
  range_operator *m_range_tree[RANGE_OP_TABLE_SIZE];
  void initialize_integral_ops ();
  void initialize_pointer_ops ();
  void initialize_float_ops ();
};
#endif // GCC_RANGE_OP_H
