/* Code for range related grange gimple statement.
   Copyright (C) 2019 Free Software Foundation, Inc.
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
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "range.h"
#include "grange.h"

// This file implements the gimple range statement and associated data.
// the grange_op statement kind provides access to the range-op fold machinery
// as well as to a set of range adjustemnts which are gimple IL aware.
//
// This allows a statement to make adjustments to operands
// or results based on IL contextual information such as specific feeding
// statements or operand position.
//
// A table is indexed by tree-code which provides any neccessary code
// for implementing the IL specific work. It is modelled after the range op
// table where a class is implemented for a given tree code, and
// auto-registered into the table for use when it is encountered.

// This function returns a range for tree node EXPR in R.  Return
// false if ranges are not supported.

irange
get_tree_range (tree expr)
{
  tree type;
  if (TYPE_P (expr))
    type = expr;
  else
    type = TREE_TYPE (expr);

  gcc_checking_assert (irange::supports_type_p (type));

  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
	// If we encounter an overflow, simply punt and drop to varying
	// since we have no idea how it will be used.
        if (!TREE_OVERFLOW_P (expr))
	  return irange (expr, expr);
	break;

      case SSA_NAME:
        if (irange::supports_ssa_p (expr) && SSA_NAME_RANGE_INFO (expr)
	    && !POINTER_TYPE_P (type))
	  {
	    // Return a range from an SSA_NAME's available range.  
	    wide_int min, max;
	    enum value_range_kind kind = get_range_info (expr, &min, &max);
	    return irange (kind, type, min, max);
	  }
	break;

      case ADDR_EXPR:
        {
	  // handle &var which can show up in phi arguments
	  bool ov;
	  if (tree_single_nonzero_warnv_p (expr, &ov))
	     return range_nonzero (type);
	  break;
	}

      default:
        break;
    }
  return irange (type);
}

static gimple *
calc_single_range (irange &r, gswitch *sw, edge e)
{
  unsigned x, lim;
  lim = gimple_switch_num_labels (sw);
  tree type = TREE_TYPE (gimple_switch_index (sw));

  // ADA currently has cases where the index is 64 bits and the case
  // arguments are  32 bit, causing a trap when we create a case_range.
  // Until this is resolved (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87798)
  // punt on these switches.  
  // cfamily fails during a bootstrap due to a signed index and unsigned cases.
  // so changing to types_compatible_p for now.
  tree case_type = TREE_TYPE (CASE_LOW (gimple_switch_label (sw, 1)));
  if (lim > 1 && !types_compatible_p (type, case_type))
    return NULL;


  if (e != gimple_switch_default_edge (cfun, sw))
    {
      r.set_undefined ();
      // Loop through all the switches edges, ignoring the default edge.
      // unioning the ranges together.
      for (x = 1; x < lim; x++)
	{
	  if (gimple_switch_edge (cfun, sw, x) != e)
	    continue;
	  tree low = CASE_LOW (gimple_switch_label (sw, x));
	  tree high = CASE_HIGH (gimple_switch_label (sw, x));
	  if (!high)
	    high = low;
	  irange case_range (low, high);
	  r.union_ (case_range);
	}
    }
  else
    {
      r.set_varying (type);
      // Loop through all the switches edges, ignoring the default edge.
      // intersecting the ranges not covered by the case.
      for (x = 1; x < lim; x++)
	{
	  tree low = CASE_LOW (gimple_switch_label (sw, x));
	  tree high = CASE_HIGH (gimple_switch_label (sw, x));
	  if (!high)
	    high = low;
	  irange case_range (VR_ANTI_RANGE, low, high);
	  r.intersect (case_range);
	}
    }
  return sw;
}


// If there is a range control statment at the end of block BB, return it.

gimple_stmt_iterator
gsi_outgoing_range_stmt (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  if (!gsi_end_p (gsi))
    {
      gimple *s = gsi_stmt (gsi);
      if (is_a<gcond *> (s) || is_a<gswitch *> (s))
	return gsi;
    }
  return gsi_none ();
}

gimple *
gimple_outgoing_range_stmt_p (basic_block bb)
{
  // This will return NULL if there is not a branch statement.
  return gsi_stmt (gsi_outgoing_range_stmt (bb));
}

// Calculate the range forced on on edge E by control flow, if any,  and
// return it in R.  Return the statment which defines the range, otherwise
// return NULL;

gimple *
gimple_outgoing_edge_range_p (irange &r, edge e)
{
  // Determine if there is an outgoing edge.
  gimple *s = gimple_outgoing_range_stmt_p (e->src);
  if (!s)
    return NULL;
  if (is_a<gcond *> (s))
    {
      if (e->flags & EDGE_TRUE_VALUE)
	r = irange (boolean_true_node, boolean_true_node);
      else if (e->flags & EDGE_FALSE_VALUE)
	r = irange (boolean_false_node, boolean_false_node);
      else
	gcc_unreachable ();
      return s;
    }

  gcc_checking_assert (is_a<gswitch *> (s));
  gswitch *sw = as_a<gswitch *> (s);
  tree type = TREE_TYPE (gimple_switch_index (sw));

  if (!irange::supports_type_p (type))
    return NULL;

  return calc_single_range (r, sw, e);
}


// ------------------------------------------------------------------------

// This is the class which is used to implement range adjustments in GIMPLE.
// It follows the same model as range_ops, where you can adjust the LHS, OP1,
// or OP2, and self registers into the lookup table.

class grange_adjust
{
public:
  grange_adjust (enum tree_code c);
  virtual bool lhs_adjust (irange &r, const gimple *s) const;
  virtual bool op1_adjust (irange &r, const gimple *s) const;
  virtual bool op2_adjust (irange &r, const gimple *s) const;
protected:
  enum tree_code code;
};


// This class implements a table of range adjustments that can be done for 
// each tree code.  

class gimple_range_table
{
public:
  inline grange_adjust* operator[] (enum tree_code code);
  void register_operator (enum tree_code code, grange_adjust *);
private:
  grange_adjust *m_range_tree[MAX_TREE_CODES];
} grange_table;


// Return the adjustment class pointer for tree_code CODE, or NULL if none.

grange_adjust *
gimple_range_table::operator[] (enum tree_code code)
{
  gcc_assert (code > 0 && code < MAX_TREE_CODES);
  return m_range_tree[code];
}

// Called by the adjustment class constructor to register the operator
// in the table.

void
gimple_range_table::register_operator (enum tree_code code, grange_adjust *op)
{
  gcc_checking_assert (m_range_tree[code] == NULL && op != NULL);
  m_range_tree[code] = op;
}

// External entry point to return the handler for a given tree_code hiding 
// the existance of the table.    This is required for the is_a helper
// templates.

grange_adjust *
gimple_range_adjust_handler (enum tree_code c)
{
  return grange_table[c];
}

// --------------------------------------------------------------------------

// Construct class grange_adjust, and register it in the table.

grange_adjust::grange_adjust (enum tree_code c)
{
  code = c;
  grange_table.register_operator (c, this);
}

// Return any adjustments to the range of the lhs

bool
grange_adjust::lhs_adjust (irange &r ATTRIBUTE_UNUSED,
			   const gimple *s ATTRIBUTE_UNUSED) const
{
  return false;
}

// Return any adjustments to the range of op1 when querying it.

bool
grange_adjust::op1_adjust (irange &r ATTRIBUTE_UNUSED,
			   const gimple *s ATTRIBUTE_UNUSED) const
{
  return false;
}

// Return any adjustments to the range of op2 when querying it.

bool
grange_adjust::op2_adjust (irange &r ATTRIBUTE_UNUSED,
			   const gimple *s ATTRIBUTE_UNUSED) const
{
  return false;
}


// -------------------------------------------------------------------------

// WHEN a certain class of builtin functions are called which return a 
// COMPLEX_INT, the IMAGPART_EXPR is known to be either true or false.
// ie
//    a_5 = .ADD_OVERFLOW (blah)
//    c_6 = IMAGPART_EXPR (a_5)
// c_6 is actually a boolean indicating if an overflow happened.

class imagpart : public grange_adjust
{
public:
  imagpart () : grange_adjust (IMAGPART_EXPR) { }
  virtual bool lhs_adjust (irange &r, const gimple *s) const;
} imagpart_adjust;


// If the operand of the IMAGPART_EXPR is a builtin function which is known
// to return a boolean result, adjust the LHS ot be a 0 or 1.

bool
imagpart::lhs_adjust (irange &r, const gimple *s) const
{
  tree name;
  tree type = TREE_TYPE (gimple_assign_lhs (s));

  name = TREE_OPERAND (gimple_assign_rhs1 (s), 0);
  if (TREE_CODE (name) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      if (def_stmt && is_gimple_call (def_stmt)
	  && gimple_call_internal_p (def_stmt))
	{
	  switch (gimple_call_internal_fn (def_stmt))
	    {
	    case IFN_ADD_OVERFLOW:
	    case IFN_SUB_OVERFLOW:
	    case IFN_MUL_OVERFLOW:
	    case IFN_ATOMIC_COMPARE_EXCHANGE:
	      r.set_varying (boolean_type_node);
	      range_cast (r, type);
	      return true;
	    default:
	      r.set_varying (type);
	      return true;
	    }
	}
    }
  return false;
}


// ------------------------------------------------------------------------
// grange_op statement kind implementation. 

// Return the grange_adjust pointer for this statement, if there is one..

inline grange_adjust *
grange_op::grange_adjust_handler () const
{
  return gimple_range_adjust_handler (gimple_expr_code (this));
}

// Return the range_operator pointer for this statement.

inline range_operator *
grange_op::handler () const
{
  return range_op_handler (gimple_expr_code (this), gimple_expr_type (this));
}

// Return the first operand of this statement if it is a valid operand 
// supported by ranges, otherwise return NULL_TREE. 

tree
grange_op::operand1 () const
{
  switch (gimple_code (this))
    {
      case GIMPLE_COND:
        return gimple_cond_lhs (this);
      case GIMPLE_ASSIGN:
        {
	  tree expr = gimple_assign_rhs1 (this);
	  if (gimple_assign_rhs_code (this) == ADDR_EXPR)
	    {
	      // If the base address is an SSA_NAME, we return it here.
	      // This allows processing of the range of that name, while the
	      // rest of the expression is simply ignored.  The code in
	      // range_ops will see the ADDR_EXPR and do the right thing.
	      tree base = get_base_address (TREE_OPERAND (expr, 0));
	      if (base != NULL_TREE && TREE_CODE (base) == MEM_REF)
	        {
		  // If the base address is an SSA_NAME, return it. 
		  tree b = TREE_OPERAND (base, 0);
		  if (TREE_CODE (b) == SSA_NAME)
		    return b;
		}
	    }
	  return expr;
	}
      default:
        break;
    }
  return NULL;
}


// Fold this unary statement uusing R1 as operand1's range, returning the
// result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange &orig_r1) const
{
  irange r1, r2;
  r1 = orig_r1;
  // Single ssa operations require the LHS type as the second range.
  if (lhs ())
    r2.set_varying (TREE_TYPE (lhs ()));
  else
    r2.set_undefined ();

  return fold (res, r1, r2);
}


// Fold this binary statement using R1 and R2 as the operands ranges,
// returning the result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange &r1, const irange &r2) const
{
  irange adj_range;
  bool adj = false;
  bool hand = false;

  if (grange_adjust_handler ())
    adj = grange_adjust_handler()->lhs_adjust (adj_range, this);
  if (handler ())
    {
      hand = true;
      res = handler()->fold_range (gimple_expr_type (this), r1, r2);
    }

  // Handle common case first where res was set by handler
  // This handles whatever handler() would ahve returned.
  if (!adj)
    return hand;

  // Now we know there was an adjustment, so make it.
  if (!hand)
    res =  adj_range;
  else
    res.intersect (adj_range);

  return true;
}


// Calculate what we can determine of the range of this unary statement's
// operand if the lhs of the expression has the range LHS_RANGE.  Return false
// if nothing can be determined.

bool
grange_op::calc_op1_irange (irange &r, const irange &lhs_range) const
{  
  irange type_range;
  gcc_checking_assert (gimple_num_ops (this) < 3);
  // An empty range is viral, so return an empty range.
  
  tree type = TREE_TYPE (operand1 ());
  if (lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  // Unary operations require the type of the first operand in the second range
  // position.
  type_range.set_varying (type);
  return handler ()->op1_range (r, type, lhs_range, type_range);
}


// Calculate what we can determine of the range of this statement's first 
// operand if the lhs of the expression has the range LHS_RANGE and the second
// operand has the range OP2_RANGE.  Return false if nothing can be determined.

bool
grange_op::calc_op1_irange (irange &r, const irange &lhs_range,
			    const irange &op2_range) const
{  
  // Unary operation are allowed to pass a range in for second operand
  // as there are often additional restrictions beyond the type which can
  // be imposed.  See operator_cast::op1_irange.()
  
  tree type = TREE_TYPE (operand1 ());
  // An empty range is viral, so return an empty range.
  if (op2_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  return handler ()->op1_range (r, type, lhs_range, op2_range);
}


// Calculate what we can determine of the range of this statement's second
// operand if the lhs of the expression has the range LHS_RANGE and the first
// operand has the range OP1_RANGE.  Return false if nothing can be determined.

bool
grange_op::calc_op2_irange (irange &r, const irange &lhs_range,
			    const irange &op1_range) const
{  
  tree type = TREE_TYPE (operand2 ());
  // An empty range is viral, so return an empty range.
  if (op1_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  return handler ()->op2_range (r, type, lhs_range, op1_range);
}
