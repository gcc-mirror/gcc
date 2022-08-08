/* Floating point range operators.
   Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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

// Default definitions for floating point operators.

bool
range_operator_float::fold_range (frange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const frange &rh ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator_float::fold_range (irange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const frange &rh ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator_float::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator_float::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator_float::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator_float::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_kind rel ATTRIBUTE_UNUSED) const
{
  return false;
}

relation_kind
range_operator_float::lhs_op1_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op2_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::op1_op2_relation (const irange &lhs ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

// Return TRUE if OP1 and OP2 are known to be free of NANs.

static inline bool
finite_operands_p (const frange &op1, const frange &op2)
{
  return (flag_finite_math_only
	  || (op1.get_nan ().no_p ()
	      && op2.get_nan ().no_p ()));
}

// Floating version of relop_early_resolve that takes into account NAN
// and -ffinite-math-only.

inline bool
frelop_early_resolve (irange &r, tree type,
		      const frange &op1, const frange &op2,
		      relation_kind rel, relation_kind my_rel)
{
  // If either operand is undefined, return VARYING.
  if (empty_range_varying (r, type, op1, op2))
    return true;

  // We can fold relations from the oracle when we know both operands
  // are free of NANs, or when -ffinite-math-only.
  return (finite_operands_p (op1, op2)
	  && relop_early_resolve (r, type, op1, op2, rel, my_rel));
}

// Default implementation of fold_range for relational operators.
// This amounts to passing on any known relations from the oracle, iff
// we know the operands are not NAN or -ffinite-math-only holds.

static inline bool
default_frelop_fold_range (irange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_kind rel, relation_kind my_rel)
{
  if (frelop_early_resolve (r, type, op1, op2, rel, my_rel))
    return true;

  r.set_varying (type);
  return true;
}

class foperator_identity : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;

  bool fold_range (frange &r, tree type ATTRIBUTE_UNUSED,
		   const frange &op1, const frange &op2 ATTRIBUTE_UNUSED,
		   relation_kind) const final override
  {
    r = op1;
    return true;
  }
  bool op1_range (frange &r, tree type ATTRIBUTE_UNUSED,
		  const frange &lhs, const frange &op2 ATTRIBUTE_UNUSED,
		  relation_kind) const final override
  {
    r = lhs;
    return true;
  }
public:
} fop_identity;

class foperator_equal : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_EQ);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return equal_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override
  {
    return op1_range (r, type, lhs, op1, rel);
  }
} fop_equal;

bool
foperator_equal::op1_range (frange &r, tree type,
			    const irange &lhs,
			    const frange &op2 ATTRIBUTE_UNUSED,
			    relation_kind rel) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 == op2 implies op1 is !NAN.
      r.set_nan (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      // The FALSE side of op1 == op1 implies op1 is a NAN.
      if (rel == VREL_EQ)
	r.set_nan (fp_prop::YES);
      break;

    default:
      break;
    }
  return true;
}

class foperator_not_equal : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_NE);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return not_equal_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
} fop_not_equal;

bool
foperator_not_equal::op1_range (frange &r, tree type,
				const irange &lhs,
				const frange &op2 ATTRIBUTE_UNUSED,
				relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      // The FALSE side of op1 != op2 implies op1 is !NAN.
      r.set_nan (fp_prop::NO);
      break;

    default:
      break;
    }
  return true;
}

class foperator_lt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_LT);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return lt_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override;
} fop_lt;

bool
foperator_lt::op1_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op2 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 < op2 implies op1 is !NAN and !INF.
      r.set_nan (fp_prop::NO);
      r.set_inf (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_lt::op2_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op1 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 < op2 implies op2 is !NAN and !NINF.
      r.set_nan (fp_prop::NO);
      r.set_ninf (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

class foperator_le : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_LE);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return le_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override
  {
    return op1_range (r, type, lhs, op1, rel);
  }
} fop_le;

bool
foperator_le::op1_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op2 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 <= op2 implies op1 is !NAN.
      r.set_nan (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

class foperator_gt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_GT);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return gt_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override;
} fop_gt;

bool
foperator_gt::op1_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op2 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 > op2 implies op1 is !NAN and !NINF.
      r.set_nan (fp_prop::NO);
      r.set_ninf (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_gt::op2_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op1 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 > op2 implies op2 is !NAN and !INF.
      r.set_nan (fp_prop::NO);
      r.set_inf (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

class foperator_ge : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override
  {
    return default_frelop_fold_range (r, type, op1, op2, rel, VREL_GE);
  }
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return ge_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override
  {
    return op1_range (r, type, lhs, op1, rel);
  }
} fop_ge;

bool
foperator_ge::op1_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op2 ATTRIBUTE_UNUSED,
			 relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 >= op2 implies op1 is !NAN.
      r.set_nan (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

// UNORDERED_EXPR comparison.

class foperator_unordered : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override
  {
    return op1_range (r, type, lhs, op1, rel);
  }
} fop_unordered;

bool
foperator_unordered::fold_range (irange &r, tree type,
				 const frange &op1, const frange &op2,
				 relation_kind) const
{
  // UNORDERED is TRUE if either operand is a NAN.
  if (op1.get_nan ().yes_p () || op2.get_nan ().yes_p ())
    r = range_true (type);
  // UNORDERED is FALSE if neither operand is a NAN.
  else if (op1.get_nan ().no_p () && op2.get_nan ().no_p ())
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_unordered::op1_range (frange &r, tree type,
				const irange &lhs,
				const frange &op2 ATTRIBUTE_UNUSED,
				relation_kind) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // Since at least one operand must be NAN, if one of them is
      // not, the other must be.
      if (op2.get_nan ().no_p ())
	r.set_nan (fp_prop::YES);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      // A false UNORDERED means both operands are !NAN.
      r.set_nan (fp_prop::NO);
      break;

    default:
      break;
    }
  return true;
}

// ORDERED_EXPR comparison.

class foperator_ordered : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;

public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_kind rel) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_kind rel) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_kind rel) const final override
  {
    return op1_range (r, type, lhs, op1, rel);
  }
} fop_ordered;

bool
foperator_ordered::fold_range (irange &r, tree type,
			       const frange &op1, const frange &op2,
			       relation_kind) const
{
  // ORDERED is TRUE if neither operand is a NAN.
  if (op1.get_nan ().no_p () && op2.get_nan ().no_p ())
    r = range_true (type);
  // ORDERED is FALSE if either operand is a NAN.
  else if (op1.get_nan ().yes_p () || op2.get_nan ().yes_p ())
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_ordered::op1_range (frange &r, tree type,
			      const irange &lhs,
			      const frange &op2 ATTRIBUTE_UNUSED,
			      relation_kind rel) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      r.set_varying (type);
      // The TRUE side of op1 ORDERED op2 implies op1 is !NAN.
      r.set_nan (fp_prop::NO);
      break;

    case BRS_FALSE:
      r.set_varying (type);
      // The FALSE side of op1 ORDERED op1 implies op1 is !NAN.
      if (rel == VREL_EQ)
	r.set_nan (fp_prop::NO);
      break;

    default:
      break;
    }
  return true;
}

// Placeholder for unimplemented relational operators.

class foperator_relop_unknown : public range_operator_float
{
  using range_operator_float::fold_range;

public:
  bool fold_range (irange &r, tree type,
		   const frange &, const frange &,
		   relation_kind) const final override
  {
    r.set_varying (type);
    return true;
  }
} fop_relop_unknown;


// Instantiate a range_op_table for floating point operations.
static floating_op_table global_floating_table;

// Pointer to the float table so the dispatch code can access it.
floating_op_table *floating_tree_table = &global_floating_table;

floating_op_table::floating_op_table ()
{
  set (SSA_NAME, fop_identity);
  set (PAREN_EXPR, fop_identity);
  set (OBJ_TYPE_REF, fop_identity);
  set (REAL_CST, fop_identity);

  // All the relational operators are expected to work, because the
  // calculation of ranges on outgoing edges expect the handlers to be
  // present.
  set (EQ_EXPR, fop_equal);
  set (NE_EXPR, fop_not_equal);
  set (LT_EXPR, fop_lt);
  set (LE_EXPR, fop_le);
  set (GT_EXPR, fop_gt);
  set (GE_EXPR, fop_ge);
  set (UNLE_EXPR, fop_relop_unknown);
  set (UNLT_EXPR, fop_relop_unknown);
  set (UNGE_EXPR, fop_relop_unknown);
  set (UNGT_EXPR, fop_relop_unknown);
  set (UNEQ_EXPR, fop_relop_unknown);
  set (ORDERED_EXPR, fop_ordered);
  set (UNORDERED_EXPR, fop_unordered);
}

// Return a pointer to the range_operator_float instance, if there is
// one associated with tree_code CODE.

range_operator_float *
floating_op_table::operator[] (enum tree_code code)
{
  return m_range_tree[code];
}

// Add OP to the handler table for CODE.

void
floating_op_table::set (enum tree_code code, range_operator_float &op)
{
  gcc_checking_assert (m_range_tree[code] == NULL);
  m_range_tree[code] = &op;
}
