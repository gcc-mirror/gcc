/* SSA range support functions.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "ssa-range.h"
#include "ssa-range-gori.h"
#include "fold-const.h"


// If there is a range control statment at the end of block BB, return it.

gimple *
gimple_outgoing_range_stmt_p (basic_block bb)
{
  gimple *s = last_stmt (bb);
  if (s && (is_a<gcond *> (s) || is_a<gswitch *> (s)))
    return s;
  return NULL;
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
	r = irange (boolean_type_node, boolean_true_node, boolean_true_node);
      else if (e->flags & EDGE_FALSE_VALUE)
	r = irange (boolean_type_node, boolean_false_node, boolean_false_node);
      else
	gcc_unreachable ();
      return s;
    }

  gcc_checking_assert (is_a<gswitch *> (s));
  gswitch *sw = as_a<gswitch *> (s);
  tree type = TREE_TYPE (gimple_switch_index (sw));

  if (!ssa_ranger::supports_type_p (type))
    return NULL;

  unsigned x, lim;
  lim = gimple_switch_num_labels (sw);

  // ADA currently has cases where the index is 64 bits and the case
  // arguments are  32 bit, causing a trap when we create a case_range.
  // Until this is resolved (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87798)
  // punt on these switches.
  if (lim > 1 && 
      TYPE_PRECISION (TREE_TYPE (CASE_LOW (gimple_switch_label (sw, 1)))) != 
      TYPE_PRECISION (type))
    return NULL;

  if (e != gimple_switch_default_edge (cfun, sw))
    {
      r.set_undefined (type);
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
	  irange case_range (type, low, high);
	  r.union_ (case_range);
	}
      return s;
    }
  r.set_varying (type);
  // Loop through all the switches edges, ignoring the default edge.
  // intersecting the ranges not covered by the case.
  for (x = 1; x < lim; x++)
    {
      tree low = CASE_LOW (gimple_switch_label (sw, x));
      tree high = CASE_HIGH (gimple_switch_label (sw, x));
      if (!high)
	high = low;
      irange case_range (type, low, high, irange::INVERSE);
      r.intersect (case_range);
    }

  return s;
}

// This function returns a range for tree node EXPR in R.  
// Return false if ranges are not supported.

bool
get_tree_range (irange &r, tree expr)
{
  tree type;
  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
	r = irange (TREE_TYPE (expr), expr, expr);
	return true;

      case SSA_NAME:
        if (ssa_ranger::supports_ssa_p (expr))
	  {
	    r = range_from_ssa (expr);
	    return true;
	  }
	break;

      case ADDR_EXPR:
        {
	  // handle &var which can show up in phi arguments
	  bool ov;
	  type = TREE_TYPE (expr);
	  if (ssa_ranger::supports_type_p (type))
	    {
	      if (tree_single_nonzero_warnv_p (expr, &ov))
		range_non_zero (&r, type);
	      else
		r.set_varying (type);
	      return true;
	    }
	  break;
	}

      default:
	if (TYPE_P (expr))
	  type = expr;
	else
	  type = TREE_TYPE (expr);
	if (ssa_ranger::supports_type_p (type))
	  {
	    // Set to range for this type.
	    r.set_varying (type);
	    return true;
	  }
	break;
    }

  return false;
}


// Initialize a ranger.

ssa_ranger::ssa_ranger ()
{
}

// Destruct a ranger.

ssa_ranger::~ssa_ranger ()
{
}

// This function returns a range for a tree node.  If optional statement S
// is present, then the range would be if it were to appear as a use on S.
// Return false if ranges are not supported.

bool
ssa_ranger::range_of_expr (irange &r, tree expr, gimple *s ATTRIBUTE_UNUSED)
{
  return get_tree_range (r, expr);
}

// Determine a range for OP on edge E, returning the result in R.

bool
ssa_ranger::range_of_expr (irange&r, tree op, edge e)
{
  if (!supports_p (op))
     return false;
  if (valid_ssa_p (op))
    {
      gcc_assert (range_on_edge (r, e, op));
      return true;
    }
  // If it is not an SSA_NAME, just get the basic range.
  return get_tree_range (r, op);
}

// Calculate a range for NAME on edge E and return it in R.  
// Return false if no range can be determined.

bool
ssa_ranger::range_on_edge (irange &r, edge e, tree name)
{
  irange edge_range;

  if (!supports_ssa_p (name))
    return false;

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (r, e, name))
    return true;
  return range_of_expr (r, name);
}

// Calculate a range on edge E and return it in R.  Try to evaluate a range
// for NAME on this edge.  Return FALSE if this is either not a control edge
// or NAME is not defined by this edge.

bool
ssa_ranger::outgoing_edge_range_p (irange &r, edge e, tree name,
				    irange *name_range)
{
  irange lhs;

  gcc_checking_assert (valid_ssa_p (name));
  // Determine if there is an outgoing edge.
  gimple *s = gimple_outgoing_edge_range_p (lhs, e);

  // If there is no outgoing stmt, there is no range produced.
  if (!s)
    return false;

  // Otherwise use the outgoing edge as a LHS and try to calculate a range.
  return compute_operand_range_on_stmt (r, s, lhs, name, name_range);
}

// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.

bool
ssa_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  // If name is specified, make sure it a LHS of S.
  gcc_checking_assert (name ? SSA_NAME_DEF_STMT (name) == s : true);

  if (is_a<grange_op *> (s))
    return range_of_range_op (r, as_a<grange_op *> (s));
  if (is_a<gphi *>(s))
    return range_of_phi (r, as_a<gphi *> (s));
  if (is_a<gcall *>(s))
    return range_of_call (r, as_a<gcall *> (s));
  if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    return range_of_cond_expr (r, as_a<gassign *> (s));

  // If no name is specified, try the expression kind.
  if (!name)
    {
      tree t = gimple_expr_type (s);
      if (!supports_type_p (t))
	return false;
      r.set_varying (t);
      return true;
    }
  // We don't understand the stmt, so return the global range.
  r = range_from_ssa (name);
  return true;
}

// Calculate a range for statement S and return it in R.  If NAME is found
// in any of the operands, replace it with NAME_RANGE rather than looking it
// up. If a range cannot be calculated on this statement, return false.

bool
ssa_ranger::range_of_stmt_with_range (irange &r, gimple *s, tree name,
				       const irange &name_range)
{
  if (is_a<grange_op *> (s))
    return range_of_range_op (r, as_a<grange_op *> (s), name, name_range);
  if (is_a<gphi *>(s))
    return range_of_phi (r, as_a<gphi *> (s), name, &name_range);
  if (is_a<gcall *>(s))
    return range_of_call (r, as_a<gcall *> (s), name, &name_range);
  if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    return range_of_cond_expr (r, as_a<gassign *> (s), name, &name_range);

  return false;
}

// Calculate range-on-entry for NAME to BB and return in R.
// unimplemented at the statement level.

bool
ssa_ranger::range_on_entry (irange &r ATTRIBUTE_UNUSED,
			    basic_block bb ATTRIBUTE_UNUSED,
			    tree name ATTRIBUTE_UNUSED)
{
  return false;
}

// Calculate range-on-exit for NAME from BB and return in R.
// unimplemented at the statement level.

bool
ssa_ranger::range_on_exit (irange &r ATTRIBUTE_UNUSED,
			   basic_block bb ATTRIBUTE_UNUSED,
			   tree name ATTRIBUTE_UNUSED)
{
  return false;
}



// Calculate a range for range_op statement S given RANGE1 and RANGE2 and 
// return it in R.  If a range cannot be calculated, return false.  
// If valid is false, then one or more of the ranges are invalid, and
// return false.

inline bool
ssa_ranger::range_of_range_op_core (irange &r, grange_op *s, bool valid,
				     irange &range1, irange &range2)
{
  if (valid)
    {
      if (s->operand2 ())
	valid = s->fold (r, range1, range2);
      else
	valid = s->fold (r, range1);
    }

  // If range_of_expr or fold() fails, return varying.
  if (!valid)
    r.set_varying (gimple_expr_type (s));
  return true;
}

// Calculate a range for range_op statement S and return it in R.  If a range
// cannot be calculated, return false.  

bool
ssa_ranger::range_of_range_op (irange &r, grange_op *s)
{
  irange range1, range2;
  bool res = true;
  gcc_checking_assert (supports_type_p (gimple_expr_type (s)));

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Calculate a range for operand 1.
  res = range_of_expr (range1, op1, s);

  // Calculate a result for op2 if it is needed.
  if (res && op2)
    res = range_of_expr (range2, op2, s);

  return range_of_range_op_core (r, s, res, range1, range2);
}

// Calculate a range for range_op statement S and return it in R.  If any
// operand matches NAME, replace it with NAME_RANGE.  If a range
// cannot be calculated, return false.  

bool
ssa_ranger::range_of_range_op (irange &r, grange_op *s, tree name,
			      const irange &name_range)
{
  irange range1, range2;
  bool res = true;
  gcc_checking_assert (supports_type_p (gimple_expr_type (s)));

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Calculate a range for operand 1.
  if (op1 == name)
    range1 = name_range;
  else
    res = range_of_expr (range1, op1, s);

  // Calculate a result for op2 if it is needed.
  if (res && op2)
    {
      if (op2 == name)
	range2 = name_range;
      else
	res = range_of_expr (range2, op2, s);
    }

  return range_of_range_op_core (r, s, res, range1, range2);
}

// Calculate a range for range_op statement S and return it in R.  Evaluate
// the statement as if it immediately preceeded stmt EVAL_FROM.  If a range
// cannot be calculated, return false.  

bool
ssa_ranger::range_of_range_op (irange &r, grange_op *s, gimple *eval_from)
{
  irange range1, range2;
  bool res = true;
  gcc_checking_assert (supports_type_p (gimple_expr_type (s)));

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Calculate a range for operand 1.
  res = range_of_expr (range1, op1, eval_from);

  // Calculate a result for op2 if it is needed.
  if (res && op2)
    res = range_of_expr (range2, op2, eval_from);

  return range_of_range_op_core (r, s, res, range1, range2);
}


// Calculate a range for range_op statement S and return it in R.  Evaluate
// the statement as if it were on edge EVAL_ON.  If a range cannot be
// calculated, return false.  

bool
ssa_ranger::range_of_range_op (irange &r, grange_op *s, edge eval_on)
{
  irange range1, range2;
  bool res = true;
  gcc_checking_assert (supports_type_p (gimple_expr_type (s)));

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Calculate a range for operand 1.
  res = range_of_expr (range1, op1, eval_on);

  // Calculate a result for op2 if it is needed.
  if (res && op2)
    res = range_of_expr (range2, op2, eval_on);

  return range_of_range_op_core (r, s, res, range1, range2);
}

// Calculate a range for phi statement S and return it in R. 
// If NAME is non-null, replace any occurences of NAME in arguments with
// NAME_RANGE.
// If EVAL_FOM is non-null, evaluate the PHI as if it occured right before
// EVAL_FROM.
// if ON_EDGE is non-null, only evaluate the argument on edge ON_EDGE.
// If a range cannot be calculated, return false.  

bool
ssa_ranger::range_of_phi (irange &r, gphi *phi, tree name,
			   const irange *name_range, gimple *eval_from,
			   edge on_edge)
{
  tree phi_def = gimple_phi_result (phi);
  tree type = TREE_TYPE (phi_def);
  irange phi_range;
  unsigned x;

  if (!supports_type_p (type))
    return false;

  // And start with an empty range, unioning in each argument's range.
  r.set_undefined (type);
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      irange arg_range;
      tree arg = gimple_phi_arg_def (phi, x);
      edge e = gimple_phi_arg_edge (phi, x);
      if (on_edge && e != on_edge)
        continue;
      if (name == arg)
        arg_range = *name_range;
      else if (valid_ssa_p (arg) && !eval_from)
      // Try to find a range from the edge.  If that fails, return varying.
	gcc_assert (range_on_edge (arg_range, e, arg));
      else
	gcc_assert (range_of_expr (arg_range, arg, eval_from));

      r.union_ (arg_range);
      // Once the value reaches varying, stop looking.
      if (r.varying_p ())
	break;
    }

  return true;
}

// Calculate a range for call statement S and return it in R.  
// If NAME is non-null, replace any occurences of NAME in arguments with
// NAME_RANGE.
// If EVAL_FOM is non-null, evaluate the PHI as if it occured right before
// EVAL_FROM.
// if ON_EDGE is non-null, only evaluate the argument on edge ON_EDGE.
// If a range cannot be calculated, return false.  

bool
ssa_ranger::range_of_call (irange &r, gcall *call, tree name ATTRIBUTE_UNUSED,
			    const irange *name_range ATTRIBUTE_UNUSED,
			    gimple *eval_from ATTRIBUTE_UNUSED,
			    edge on_edge ATTRIBUTE_UNUSED)
{
  tree type = gimple_call_return_type (call);
  if (!supports_type_p (type))
    return false;

  if (gimple_call_nonnull_result_p (call))
    {
      range_non_zero (&r, type);
      return true;
    }
  r.set_varying (type);
  return true;
}

// Calculate a range for COND_EXPR statement S and return it in R.  Evaluate 
// the stateemnt as if it occured on edge ON_EDGE.
// If a range cannot be calculated, return false. 

bool
ssa_ranger::range_of_cond_expr  (irange &r, gassign *s, edge on_edge)
{
  irange cond_range, range1, range2;
  tree cond = gimple_assign_rhs1 (s);
  tree op1 = gimple_assign_rhs2 (s);
  tree op2 = gimple_assign_rhs3 (s);

  gcc_checking_assert (gimple_assign_rhs_code (s) == COND_EXPR);
  gcc_checking_assert (useless_type_conversion_p  (TREE_TYPE (op1),
						   TREE_TYPE (op2)));
  if (!supports_type_p (TREE_TYPE (op1)))
    return false;

  gcc_assert (range_of_expr (cond_range, cond, on_edge));
  gcc_assert (range_of_expr (range1, op1, on_edge));
  gcc_assert (range_of_expr (range2, op2, on_edge));

  if (cond_range.singleton_p ())
    {
      // False, pick second operand
      if (cond_range.zero_p ())
        r = range2;
      else
        r = range1;
    }
  else
    r = range_union (range1, range2);
  return true;
}

// Calculate a range for COND_EXPR statement S and return it in R.  
// If NAME is non-null, replace any occurences of NAME in arguments with
// NAME_RANGE.
// If EVAL_FOM is non-null, evaluate the COND_EXPR as if it occured right 
// before EVAL_FROM.
// If a range cannot be calculated, return false.  

bool
ssa_ranger::range_of_cond_expr  (irange &r, gassign *s, tree name,
				  const irange *name_range, gimple *eval_from)
{
  irange cond_range, range1, range2;
  tree cond = gimple_assign_rhs1 (s);
  tree op1 = gimple_assign_rhs2 (s);
  tree op2 = gimple_assign_rhs3 (s);

  gcc_checking_assert (gimple_assign_rhs_code (s) == COND_EXPR);
  gcc_checking_assert (useless_type_conversion_p  (TREE_TYPE (op1),
						   TREE_TYPE (op2)));
  if (!supports_type_p (TREE_TYPE (op1)))
    return false;

  if (!eval_from)
    eval_from = s;

  if (name == cond)
    cond_range = *name_range;
  else
    gcc_assert (range_of_expr (cond_range, cond, eval_from));

  if (name == op1)
    range1 = *name_range;
  else
    gcc_assert (range_of_expr (range1, op1, eval_from));

  if (name == op2)
    range2 = *name_range;
  else
    gcc_assert (range_of_expr (range2, op2, eval_from));

  if (cond_range.singleton_p ())
    {
      // False, pick second operand
      if (cond_range.zero_p ())
        r = range2;
      else
        r = range1;
    }
  else
    r = range_union (range1, range2);
  return true;
}

// Initialize a ranger.

gori_ranger::gori_ranger ()
{
}

// Destruct a ranger.

gori_ranger::~gori_ranger ()
{
}

// Calculate a range for NAME as if its defining statement was actually
// at location S and return it in R.  This involves evaluating its arguments
// at location S and recalculating the result.

bool
gori_ranger::reevaluate_range_of_name (irange &r, tree name, gimple *s)
{
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return false;

  gimple *def = SSA_NAME_DEF_STMT (name);
  if (is_a<grange_op *> (def))
    return range_of_range_op (r, as_a<grange_op *> (def), s);
  if (is_a<gphi *>(def))
    return range_of_phi (r, as_a<gphi *> (def), NULL_TREE, NULL, s);
  if (is_a<gcall *>(def))
    return range_of_call (r, as_a<gcall *> (def), NULL_TREE, NULL, s);
  if (is_a<gassign *> (def) && gimple_assign_rhs_code (def) == COND_EXPR)
    return range_of_cond_expr (r, as_a<gassign *> (def), NULL_TREE, NULL, s);

  return false;
}

// Calculate a range for NAME as if its defining statement was actually
// on edge ON_EDGE and return it in R.  This involves evaluating its arguments
// on edge ON_EDGE and recalculating the result.

bool
gori_ranger::reevaluate_range_of_name (irange &r, tree name, edge on_edge)
{
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return false;

  gimple *def = SSA_NAME_DEF_STMT (name);
  if (is_a<grange_op *> (def))
    return range_of_range_op (r, as_a<grange_op *> (def), on_edge);
  if (is_a<gphi *>(def))
    return range_of_phi (r, as_a<gphi *> (def), NULL_TREE, NULL, NULL,
			 on_edge);
  if (is_a<gcall *>(def))
    return range_of_call (r, as_a<gcall *> (def), NULL_TREE, NULL, NULL,
			  on_edge);
  if (is_a<gassign *> (def) && gimple_assign_rhs_code (def) == COND_EXPR)
    return range_of_cond_expr (r, as_a<gassign *> (def), on_edge);

  return false;
}


// Calculate a range for NAME on edge E and return it in R.  
// Return false if no range can be determined.

bool
gori_ranger::range_on_edge (irange &r, edge e, tree name)
{
  irange edge_range;

  if (!supports_ssa_p (name))
    return false;

  gcc_assert (range_on_exit (r, e->src, name));

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (edge_range, e, name, &r))
    r = edge_range;

  return true;
}

// Calculate a range on edge E and return it in R.  Try to evaluate a range
// for NAME on this edge.  Return FALSE if this is either not a control edge
// or NAME is not defined by this edge.

bool
gori_ranger::outgoing_edge_range_p (irange &r, edge e, tree name,
				    irange *name_range)
{
  irange lhs;

  gcc_checking_assert (valid_ssa_p (name));
  // Determine if there is an outgoing edge.
  gimple *s = gimple_outgoing_edge_range_p (lhs, e);
  if (!s)
    return false;

  // If NAME can be calculated on the edge, use that.
  if (m_gori.is_export_p (name, e->src))
    return m_gori.compute_operand_range (r, s, lhs, name, name_range);

  // Otherwise see if NAME is derived from something that can be calculated.
  // This performs no dynamic lookups whatsover, so it is low cost.
  return reevaluate_definition (r, name, e, name_range);
}

// Return the range for NAME on entry to basic block BB in R.  
// Return false if no range can be calculated.
// Calculation is performed by unioning all the ranges on incoming edges.

bool
gori_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  edge_iterator ei;
  edge e;
  tree type = TREE_TYPE (name);
  irange pred_range;

  if (!supports_type_p (type))
    return false;

  // Start with an empty range.
  r.set_undefined (type);

  gcc_checking_assert (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun));

  // Visit each predecessor to resolve them.
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      gcc_assert (range_on_edge (pred_range, e, name));
      r.union_ (pred_range);
      // If varying is reach, stop processing.
      if (r.varying_p ())
        break;
    }

  return true;
}

// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

bool
gori_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  // on-exit from the exit block?
  gcc_checking_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  gimple *s = last_stmt (bb);
  // If there is no statement in the block and this isnt the entry block,
  // go get the range_on_entry for this block.
  // For the entry block, a NULL stmt will return the global value for NAME.
  if (!s && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun))
    return range_on_entry (r, bb, name);
  return range_of_expr (r, name, s);
}

// If the src block of edge E defines an outgoing range for a name that is
// is in the def_chain for NAME, get the outgoing range for that ssa_name
// and re-evaluate NAME using this value. If NAME_RANGE is supplied (normally
// the current value of NAME), intersect this with the edge range to produce 
// and accurate outgoing range on edge E for NAME. Return the results in R.
// Return false if nothing can be re-evaluated, or if NAME is actually exported
// and doesnt need redefining.

bool
gori_ranger::reevaluate_definition (irange &r, tree name, edge e,
				    irange *name_range)
{
  basic_block bb = e->src;
  gimple *def_stmt;
  ssa_op_iter iter;
  use_operand_p use_p;

  // Ensure there is no outgoing range for NAME.
  gcc_checking_assert (!m_gori.is_export_p (name, bb));

  // If nothing in the def chain is exported, there is no evaluation.
  if (!m_gori.def_chain_in_export_p (name, bb))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (name);
  gcc_checking_assert (def_stmt);

  // We know its possible to evaluate NAME from SOMETHING in its defintion.
  FOR_EACH_SSA_USE_OPERAND (use_p, def_stmt, iter, SSA_OP_USE)
    {
      tree use = valid_ssa_p (USE_FROM_PTR (use_p));
      if (use)
        {
	  irange use_range;
	  if (m_gori.is_export_p (use, bb))
	    {
	      if (!outgoing_edge_range_p (use_range, e, use))
	        continue;
	    }
	  // Try reevaluating USE. we have no range on entry for USE.
	  else if (!reevaluate_definition (use_range, use, e, NULL))
	    continue;
	  // Invoke a no-overhead range calculator for the statement so we dont
	  // get any dynamic calls to range_of_expr which could cause another
	  // iterative evaluation to fill a cache.
	  ssa_ranger eval;
	  gcc_assert (eval.range_of_stmt_with_range (r, def_stmt, use,
						     use_range));
	  // If this is the root def and it has a range, combine them.
	  if (name_range)
	    r.intersect (*name_range);
	  return true;
        }
    }
  return false;
}


// Dump the block rangers data structures.

void
gori_ranger::dump (FILE *f)
{
  if (!f)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  m_gori.dump (f);
  fprintf (f, "\n");
}

// If NAME's derived chain has a termnial name, return it otherwise NULL_TREE.

tree
gori_ranger::terminal_name (tree name)
{
  return m_gori.terminal_name (name);
}

// Class used to track non-null references of an ssa-name
// A vector of bitmaps indexed by ssa-name is maintained. When indexed by 
// Basic Block, an on-bit indicates there is a non-null dereference for
// that ssa_name in that basic block.
//
// There is a single API: no_null_deref_p(name, bb) which takes care of making
// sure its a pointer type and allocated the bitmap and populating it if needed.
//
// In order to populate the bitmap, a quick run of all the immediate uses
// are made and the statement checked to see if a non-null dereference is made
// on that statement.

class non_null_ref
{
public:
  non_null_ref ();
  ~non_null_ref ();
  bool non_null_deref_p (tree name, basic_block bb);
private:
  vec <bitmap> m_nn;
  void process_name (tree name);
};

// During contructor, Allocate the vector of ssa_names.

non_null_ref::non_null_ref ()
{
  m_nn.create (0);
  m_nn.safe_grow_cleared (num_ssa_names);
}

// Free any bitmaps which were allocated,a swell as the vector itself.

non_null_ref::~non_null_ref ()
{
  unsigned x;
  for (x = 0; x< m_nn.length (); x++)
    if (m_nn[x])
      BITMAP_FREE (m_nn[x]);
}

// Allocate an populate the bitmap for NAME.  An ON bit for a block index
// indicates there is a non-null reference in that block.

void
non_null_ref::process_name (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  use_operand_p use_p;
  imm_use_iterator iter;
  bitmap b;

  // Only tracked for pointers.
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return;

  // Already processed if a bitmap has been allocated.
  if (m_nn[v])
    return;

  b = BITMAP_ALLOC (NULL);

  // Loop over each immediate use and see if it implies a non-null value.
  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *s = USE_STMT (use_p);
      unsigned index = gimple_bb (s)->index;
      tree value;
      enum tree_code comp_code;

      // If bit is already set for this block, dont bother looking again.
      if (bitmap_bit_p (b, index))
        continue;

      // If we can infer a != 0 range, then set the bit for this BB
      if (infer_value_range (s, name, &comp_code, &value))
        {
	  if (comp_code == NE_EXPR && integer_zerop (value))
	    bitmap_set_bit (b, index);
	}
    }

  m_nn[v] = b;
}

// Return true if NAME has a non-null dereference in block bb.  If this is the
// first query for NAME, calculate the summary first.

bool
non_null_ref::non_null_deref_p (tree name, basic_block bb)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return false;

  if (!m_nn[v])
    process_name (name);

  return bitmap_bit_p (m_nn[v], bb->index);
}


// This class implements a cache of ranges indexed by basic block.
// It represents all that is known about an SSA_NAME on entry to each block
// It caches a range-for-type varying range so it doesnt need to be reformed all
// the time.  If a range is ever always associated with a type, we can use that
// instead,.
// Whenever varying is being set for a block, the cache simply points
// to this cached one rather than create a new one each time.

class ssa_block_ranges
{
public:
  ssa_block_ranges (tree t);
  ~ssa_block_ranges ();

  void set_bb_range (const basic_block bb, const irange &r);
  void set_bb_varying (const basic_block bb);
  bool get_bb_range (irange &r, const basic_block bb);
  bool bb_range_p (const basic_block bb);

  void dump(FILE *f);
private:
  vec<irange_storage *> m_tab;
  irange_storage *m_type_range;
  tree m_type;
};

// Initialize a block cache for an ssa_name of type T

ssa_block_ranges::ssa_block_ranges (tree t)
{
  irange tr;
  gcc_assert (TYPE_P (t));
  m_type = t;

  m_tab.create (0);
  m_tab.safe_grow_cleared (last_basic_block_for_fn (cfun));

  // Create the cached type range.
  tr.set_varying (t);
  m_type_range = irange_storage::ggc_alloc_init (tr);

  m_tab[ENTRY_BLOCK_PTR_FOR_FN (cfun)->index] = m_type_range;
}

// Destruct block range.

ssa_block_ranges::~ssa_block_ranges ()
{
  m_tab.release ();
}

// Set the range for block BB to be R.

void
ssa_block_ranges::set_bb_range (const basic_block bb, const irange &r)
{
  irange_storage *m = m_tab[bb->index];

  // If there is already range memory for this block, reuse it.
  if (m && m != m_type_range)
    m->set_irange (r);
  else
    m = irange_storage::ggc_alloc_init (r);

  m_tab[bb->index] = m;
}

// Set the range for block BB to the range for the type.

void
ssa_block_ranges::set_bb_varying (const basic_block bb)
{
  m_tab[bb->index] = m_type_range;
}

// Return the range associated with block BB in R. Return false if there is no
// range,

bool
ssa_block_ranges::get_bb_range (irange &r, const basic_block bb)
{
  irange_storage *m = m_tab[bb->index];
  if (m)
    {
      r = irange (m_type, m);
      return true;
    }
  return false;
}

// Returns true if a range is present

bool
ssa_block_ranges::bb_range_p (const basic_block bb)
{
  return m_tab[bb->index] != NULL;
}

// Print the list of known ranges for file F in a nice format.

void
ssa_block_ranges::dump (FILE *f)
{
  basic_block bb;
  irange r;

  FOR_EACH_BB_FN (bb, cfun)
    if (get_bb_range (r, bb))
      {
	fprintf (f, "BB%d  -> ", bb->index);
	r.dump (f);
      }
}

// -------------------------------------------------------------------------

// This class manages a vector of pointers to ssa_block ranges.
// THis provides the basis for the "range on entry" cache for
// all ssa-names.

class block_range_cache
{
public:
  block_range_cache ();
  ~block_range_cache ();

  // Hide the details of the block cache with these wrappers
  void set_bb_range (tree name, const basic_block bb, const irange &r);
  void set_bb_varying (tree name, const basic_block bb);
  bool get_bb_range (irange &r, tree name, const basic_block bb);
  bool bb_range_p (tree name, const basic_block bb);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb, bool print_varying = true);
private:
  vec<ssa_block_ranges *> m_ssa_ranges;
  ssa_block_ranges& get_block_ranges (tree name);
};

// Initialize the block cache.

block_range_cache::block_range_cache ()
{
  m_ssa_ranges.create (0);
  m_ssa_ranges.safe_grow_cleared (num_ssa_names);
}

// Remove any m_block_caches which have been created.

block_range_cache::~block_range_cache ()
{
  unsigned x;
  for (x = 0; x < m_ssa_ranges.length (); ++x)
    {
      if (m_ssa_ranges[x])
	delete m_ssa_ranges[x];
    }
  // Release the vector itself.
  m_ssa_ranges.release ();
}

// Return a reference to the m_block_cache for NAME. If it has not been
// accessed yet, allocate it.

ssa_block_ranges&
block_range_cache::get_block_ranges (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!m_ssa_ranges[v])
    m_ssa_ranges[v] = new ssa_block_ranges (TREE_TYPE (name));

  return *(m_ssa_ranges[v]);
}

// Set the range for NAME on entry to block BB to R.

void
block_range_cache::set_bb_range (tree name, const basic_block bb,
				 const irange &r)
{
  return get_block_ranges (name).set_bb_range (bb, r);
}

// Set the range for NAME on entry to block BB to varying..

void
block_range_cache::set_bb_varying (tree name, const basic_block bb)
{
  return get_block_ranges (name).set_bb_varying (bb);
}

// Return the range for NAME on entry to BB in R.  return true if here is one.

bool 
block_range_cache::get_bb_range (irange &r, tree name, const basic_block bb)
{
  return get_block_ranges (name).get_bb_range (r, bb);
}

// Return true if NAME has a range set in block BB.

bool 
block_range_cache::bb_range_p (tree name, const basic_block bb)
{
  return get_block_ranges (name).bb_range_p (bb);
}

// Print all known block caches to file F.
void
block_range_cache::dump (FILE *f)
{
  unsigned x;
  for (x = 0; x < num_ssa_names; ++x)
    {
      if (m_ssa_ranges[x])
        {
	  fprintf (f, " Ranges for ");
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, ":\n");
	  m_ssa_ranges[x]->dump (f);
	}
    }
  
}

// Print all known ranges on entry to blobk BB to file F.
void
block_range_cache::dump (FILE *f, basic_block bb, bool print_varying)
{
  unsigned x;
  irange r;
  bool summarize_varying = false;
  for (x = 1; x < num_ssa_names; ++x)
    {
      if (!gori_ranger::valid_ssa_p (ssa_name (x)))
        continue;
      if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
        {
	  if (!print_varying && r.varying_p ())
	    {
	      summarize_varying = true;
	      continue;
	    }
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, "\t");
	  r.dump(f);
	}
    }
  // If there were any varying entries, lump them all together.
  if (summarize_varying)
    {
      fprintf (f, "VARYING_P on entry : ");
      for (x = 1; x < num_ssa_names; ++x)
	{
	  if (!gori_ranger::valid_ssa_p (ssa_name (x)))
	    continue;
	  if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
	    {
	      if (r.varying_p ())
		{
		  print_generic_expr (f, ssa_name (x), TDF_NONE);
		  fprintf (f, "  ");
	        }
	    }
	}
      fprintf (f, "\n");
    }
}
// -------------------------------------------------------------------------

// This global cache is used with the range engine as markers for what
// has been visited during this incarnation.  Once the ranger evaluates
// a name, it is not re-evaluated again.   
//
// An iterative ranger could detect that things have changed, and re-evaluate 
// a specific range
//
// When retreiving a global name, a check is first made to see if the 
// global irange cache has a range associated with it, and that is returned
// if it does.  If it does not, then any range assocaited with the
// existing SSA_NAME_RANGE_INFO field is extracted and that is returned,
// albeit with a false flagindicating there is not a global cache entry.

class ssa_global_cache
{
public:
  ssa_global_cache ();
  ~ssa_global_cache ();
  bool get_global_range (irange& r, tree name)  const;
  void set_global_range (tree name, const irange&r);
  void clear_global_range (tree name);
  void clear ();
  void dump (FILE *f = stderr);
private:
  vec<irange_storage *> m_tab;
};

// Initialize a global cache.

ssa_global_cache::ssa_global_cache ()
{
  m_tab.create (0);
  m_tab.safe_grow_cleared (num_ssa_names);
}

// Deconstruct a global cache.

ssa_global_cache::~ssa_global_cache ()
{
  m_tab.release ();
}

// Retrieve the global range of NAME from cache memory if it exists. 
// Return the value in R.

bool
ssa_global_cache::get_global_range (irange &r, tree name) const
{
  irange_storage *stow = m_tab[SSA_NAME_VERSION (name)];
  if (stow)
    {
      r = irange (TREE_TYPE (name), stow);
      return true;
    }
  r = range_from_ssa (name);
  return false;
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::set_global_range (tree name, const irange& r)
{
  irange_storage *m = m_tab[SSA_NAME_VERSION (name)];

  if (m)
    m->set_irange (r);
  else
    {
      m = irange_storage::ggc_alloc_init (r);
      m_tab[SSA_NAME_VERSION (name)] = m;
    }
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::clear_global_range (tree name)
{
  m_tab[SSA_NAME_VERSION (name)] = NULL;
}

// Clear the global cache.

void
ssa_global_cache::clear ()
{
  memset (m_tab.address(), 0, m_tab.length () * sizeof (irange_storage *));
}

// Dump the contents of the global cache to F. 

void
ssa_global_cache::dump (FILE *f)
{
  unsigned x;
  irange r;
  fprintf (f, "Non-varying global ranges:\n");
  fprintf (f, "=========================:\n");
  for ( x = 1; x < num_ssa_names; x++)
    if (gori_ranger::valid_ssa_p (ssa_name (x)) &&
	get_global_range (r, ssa_name (x))  && !r.varying_p ())
      {
        print_generic_expr (f, ssa_name (x), TDF_NONE);
	fprintf (f, "  : ");
        r.dump (f);
      }
  fputc ('\n', dump_file);
}



// -------------------------------------------------------------------------

// Initialize a global_ranger.

global_ranger::global_ranger ()
{
  m_block_cache = new block_range_cache ();
  m_globals = new ssa_global_cache ();
  m_non_null = new non_null_ref ();
  m_workback.create (0);
  m_workback.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.truncate (0);
}

// Deallocate global_ranger members.

global_ranger::~global_ranger () 
{
  delete m_block_cache;
  delete m_globals;
  delete m_non_null;
  m_workback.release ();
  m_update_list.release ();
}

// Print everything known about the global cache to file F.

inline void
global_ranger::dump_global_ssa_range (FILE *f)
{
  m_globals->dump (f);
}

// Return the global range for NAME in R, if it has one. Otherwise return false.

bool
global_ranger::has_global_ssa_range (irange &r, tree name)
{
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);
  if (!supports_ssa_p (name))
    return false;

  if (m_globals->get_global_range (r, name))
    return true;
  return false;
}


// Return the global range for NAME in R. IF it does not have a range set,
// invoke the ranger and attempt to find one.  If none can be found, simply 
// set the global range to the default range.

bool
global_ranger::get_global_ssa_range (irange &r, tree name)
{
  gimple *s;
  gcc_checking_assert (valid_ssa_p (name));

  if (m_globals->get_global_range (r, name))
    return true;

  // No range set so try to evaluate the definition.
  s = SSA_NAME_DEF_STMT (name);
  // Range of stmt will set the global value.
  gcc_assert (range_of_stmt (r, s, name));
  return true;
}

// Set global range of NAME to R.

inline void
global_ranger::set_global_ssa_range (tree name, const irange&r)
{
  m_globals->set_global_range (name, r);
#if 0
  if (r.varying_p ())
    {
      // we need a proper set_to_varying and undefined.
      SSA_NAME_RANGE_INFO (name) = NULL;
    }
  else if (!r.undefined_p () && !POINTER_TYPE_P (TREE_TYPE (name)))
    {
      value_range vr;
      irange_to_value_range (vr, r);
      set_range_info (name, vr.kind (), wi::to_wide (vr.min ()),
		      wi::to_wide (vr.max ()));
    }
#endif
}

// clear any global range for NAME.

void
global_ranger::clear_global_ssa_range (tree name) 
{
  m_globals->clear_global_range (name);
}

// Return true if there is a non-null dereference of name in BB somewhere and
// return the non-null range in R.

inline bool
global_ranger::non_null_deref_in_block (irange &r, tree name, basic_block bb)
{
  tree type = TREE_TYPE (name);
  if (!POINTER_TYPE_P (type))
    return false;
  if (m_non_null->non_null_deref_p (name, bb))
    {
      range_non_zero (&r, type);
      return true;
    }
  return false;
}

// Check if the range for NAME on outgoing edge E may change the incoming 
// range for NAME at the destination.  IF it does, update that range and
// push the destination block onto the update list.

bool
global_ranger::maybe_propagate_on_edge (tree name, edge e)
{
  basic_block pred = e->src;
  basic_block succ = e->dest;
  irange block_range;
  irange edge_range;
  irange succ_range_on_entry;

  // Look for entry block, DEF block or a block will a filled cache.
  if (pred == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    get_tree_range (block_range, name);
  else if (pred == gimple_bb (SSA_NAME_DEF_STMT (name)))
    get_tree_range (block_range, name);
  else if (!m_block_cache->get_bb_range (block_range, name, pred))
    return false;

  // Otherwise pick up the edge range and intersect it with block.
  if (outgoing_edge_range_p (edge_range, e, name, &block_range))
    block_range = edge_range;

  // Check if pointers have any non-null dereferences.
  // Non-call exceptions mean we could throw in the middle of he block,
  // so just punt for now on those.
  if (block_range.varying_p () && POINTER_TYPE_P (TREE_TYPE (name)) &&
      !cfun->can_throw_non_call_exceptions)
    non_null_deref_in_block (block_range, name, pred);

  // Now pick up the current range on entry to the successor and see if
  // union with the edge range causes a change.
  gcc_assert (m_block_cache->get_bb_range (succ_range_on_entry, name, succ));
  irange new_range = range_union (succ_range_on_entry, block_range);

  if (new_range != succ_range_on_entry)
    {
      // If ranges are different, Set new range and mark succ to be visited.
      m_block_cache->set_bb_range (name, succ, new_range);
      if (!m_update_list.contains (succ))
	m_update_list.quick_push (succ);
    }
  return true;
}

// See if the cache for NAME may need updating on any outgoing edges of BB.

inline void
global_ranger::maybe_propagate_block (tree name, basic_block bb)
{
  edge_iterator ei;
  edge e;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      basic_block dest = e->dest;

      // Only look at edges where the succ has been visited.
      if (!m_block_cache->bb_range_p (name, dest))
	continue;

      // Anything in this list ought to have range_on_entry set and
      // thus return a TRUE.
      gcc_assert (maybe_propagate_on_edge (name, e));
    }
}

// If there is anything in the iterative update_list, continue processing NAME
// until the list of blocks is empty.  Note calls to maybe_propagate_block may
// add to the current list.

void
global_ranger::iterative_cache_update (tree name)
{
  // Now the update_list queue has all the blocks that have had their entry
  // ranges "updated" from undefined. Start propagating until nothing
  // changes.

  while (m_update_list.length () > 0)
    {
      basic_block bb = m_update_list.pop ();
// fprintf (stderr, "FWD visiting block %d\n", bb->index);
      maybe_propagate_block (name, bb);
    }
// fprintf (stderr, "DONE visiting blocks \n\n");
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back thourgh the CFG to DEF_BB ensuring the range is calculated 
// on the block/edges leading back to that point.

void
global_ranger::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  tree type = TREE_TYPE (name);
  edge_iterator ei;
  edge e;
  irange block_result;
  irange undefined;
  undefined.set_undefined (type);

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == def_bb)
    return;

  // If the block cache is set, then we've already visited this block.
  if (m_block_cache->bb_range_p (name, bb))
    return;

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.truncate (0);
  m_workback.quick_push (bb);
  m_block_cache->set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update_list.length () == 0);

// fprintf (stderr, "\n");
// print_generic_expr (stderr, name, TDF_SLIM);
// fprintf (stderr, " : ");

  while (m_workback.length () > 0)
    {
      basic_block node = m_workback.pop ();
// fprintf (stderr, "BACK visiting block %d\n", node->index);

      FOR_EACH_EDGE (e, ei, node->preds)
        {
	  // If this is a propagation edge, dont go to the pred.
	  if (maybe_propagate_on_edge (name, e))
	    continue;

	  // If the pred hasn't been visited, add it to the list.
	  if (!m_block_cache->bb_range_p (name, e->src))
	    {
	      m_block_cache->set_bb_range (name, e->src, undefined);
	      m_workback.quick_push (e->src);
	    }
	  }
      }

  iterative_cache_update (name);
}

// Return the range of NAME on entry to block BB in R.  Return false if there
// is no range other than varying.

bool
global_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  gimple *def_stmt;
  basic_block def_bb = NULL;

  if (!supports_ssa_p (name))
    return false;

  // Determine an origination block for the defining statement.
  def_stmt = SSA_NAME_DEF_STMT (name);
  if (def_stmt)
    def_bb = gimple_bb (def_stmt);;

  if (!def_bb)
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  // Start with any known range.  This also ensures any prerequisites are
  // calculated before populating the on-entry cache.
  get_global_ssa_range (r, name);

  // If its defined in this basic block, then there is no range on entry,
  // Otherwise, go figure out what is known in predecessor blocks. 
  if (def_bb != bb)
    {
      bool res;
      irange entry_range;
      // Ensure the on entry cache is filled and get it's value.
      fill_block_cache (name, bb, def_bb);
      res = m_block_cache->get_bb_range (entry_range, name, bb);
      gcc_checking_assert (res);
      r.intersect (entry_range);
    }

  return true;
}

// Calculate a range for statement S and return it in R.  If NAME is provided
// it represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.
// Check the global cache for NAME first to see if the evaluation can be
// avoided.  If a range cannot be calculated, return false.

bool
global_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  // If no name, simply call the base routine.
  if (!name)
    return ssa_ranger::range_of_stmt (r, s, name);

  gcc_checking_assert (supports_ssa_p (name));

  // If this STMT has already been processed, return that value. 
  if (has_global_ssa_range (r, name))
    return true;
 
  // Avoid infinite recursion by initializing global cache
  irange tmp;
  tmp = range_from_ssa (name);
  set_global_ssa_range (name, tmp);

  gcc_assert (ssa_ranger::range_of_stmt (r, s, name));

  if (is_a<gphi *> (s))
    r.intersect (tmp);
  set_global_ssa_range (name, r);
  return true;
}

bool
global_ranger::range_of_expr (irange&r, tree op, edge e)
{
  return ssa_ranger::range_of_expr (r, op, e);
}

// Determine a range for OP on stmt S, returning the result in R.
// If OP is not defined in BB, find the range on entry to this block.

bool
global_ranger::range_of_expr (irange&r, tree op, gimple *s)
{
  if (!supports_p (op))
     return false;

  // If there is a statement, and a valid ssa_name, try to find a range.
  if (s && valid_ssa_p (op))
    {
      basic_block bb = gimple_bb (s);
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
        
      // if name is defined in this block, try to get an range from S.
      if (def_stmt && gimple_bb (def_stmt) == bb)
	gcc_assert (range_of_stmt (r, def_stmt, op));
      else
	// Otherwise OP comes from outside this block, use range on entry.
	gcc_assert (range_on_entry (r, bb, op));

      // No range yet, see if there is a dereference in the block.
      // We don't care if it's between the def and a use within a block
      // because the entire block must be executed anyway.
      // FIXME:?? For non-call exceptions we could have a statement throw
      // which causes an early block exit. 
      // in which case we may need to walk from S back to the def/top of block
      // to make sure the deref happens between S and there before claiming 
      // there is a deref.   Punt for now.
      if (!cfun->can_throw_non_call_exceptions && r.varying_p ())
	non_null_deref_in_block (r, op, bb);
      return true;
    }

  // Fall back to the default.
  return ssa_ranger::range_of_expr (r, op);
}

// Print the known table values to file F.

void
global_ranger::dump (FILE *f)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      unsigned x;
      edge_iterator ei;
      edge e;
      irange range;
      fprintf (f, "\n=========== BB %d ============\n", bb->index);
      m_block_cache->dump (f, bb);

      dump_bb (f, bb, 4, TDF_NONE);

      // Now find any globals defined in this block 
      for (x = 1; x < num_ssa_names; x++)
	{
	  tree name = ssa_name (x);
	  if (valid_ssa_p (name) && SSA_NAME_DEF_STMT (name) &&
	      gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
	      has_global_ssa_range (range, name))
	    {
	      gcc_assert (get_global_ssa_range (range, name));
	      if (!range.varying_p ())
	       {
		 print_generic_expr (f, name, TDF_SLIM);
		 fprintf (f, " : ");
		 range.dump(f);
	       }

	    }
	}

      // And now outgoing edges, if they define anything.
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = valid_ssa_p (ssa_name (x));
	      if (name && outgoing_edge_range_p (range, e, name))
		{
		  if (range_on_edge (range, e, name))
		    {
		      if (!range.varying_p ())
			{
			  fprintf (f, "%d->%d ", e->src->index,
				   e->dest->index);
			  if (e->flags & EDGE_TRUE_VALUE)
			    fprintf (f, " (T) ");
			  else if (e->flags & EDGE_FALSE_VALUE)
			    fprintf (f, " (F) ");
			  else
			    fprintf (f, "     ");
			  print_generic_expr (f, name, TDF_SLIM);
			  fprintf(f, " : \t");
			  range.dump(f);
//	Uncomment this when ranges no longer dump a newline.
//			  if (!m_gori.is_export_p (name, bb))
//			    fprintf (f, "(re-eval) ");
			}
		    }
		}
	    }
	}
    }

  dump_global_ssa_range (dump_file);

  if (dump_flags & TDF_DETAILS)
    gori_ranger::dump (dump_file);
}


// Calculate all ranges by visiting every block and asking for the range of 
// each ssa_name on each statement, and then dump those ranges to OUTPUT.

void
global_ranger::calculate_and_dump (FILE *output)
{
  basic_block bb;
  irange r;
  
  //  Walk every statement asking for a range.
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator gpi = gsi_start_phis (bb); !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  tree phi_def = gimple_phi_result (phi);
	  if (valid_ssa_p (phi_def))
	    gcc_assert (range_of_stmt (r, phi));
	}

      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  ssa_op_iter iter;
	  use_operand_p use_p;

	  range_of_stmt (r, stmt);
	  // and make sure to query every operand.
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	    {
	      tree use = valid_ssa_p (USE_FROM_PTR (use_p));
	      if (use)
	        range_of_expr (r, use, stmt);
	    }
	}
    }
  // The dump it.
  dump (output);
}

// Constructor for trace_ranger.

trace_ranger::trace_ranger ()
{
  indent = 0;
  counter = 0;
}

// If dumping, return true and print the prefix for the next output line.

inline bool
trace_ranger::dumping ()
{
  counter++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      // Print counter index as well as INDENT spaces.
      fprintf (dump_file, "  %-7u", counter);
      unsigned x;
      for (x = 0; x< indent; x++)
        fputc (' ', dump_file);
      return true;
    }
  return false;
}

// After calling a routine, if dumping, print the CALLER, NAME, and RESULT,
// returning RESULT.

bool
trace_ranger::trailer (const char *caller, bool result, tree name,
		       const irange &r)
{
  indent -= bump;
  if (dumping ())
    {
      fputs(result ? "TRUE : " : "FALSE : ", dump_file);
      fputs (caller, dump_file);
      fputs (" (",dump_file);
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") ",dump_file);
      if (result)
	r.dump (dump_file);
      else
	fputc('\n', dump_file);
    }
  // Marks the end of a request.
  if (indent == 0)
    fputc('\n', dump_file);
  return result;
}

// Tracing version of range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree expr, gimple *s)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_of_expr (");
      print_generic_expr (dump_file, expr, TDF_SLIM);
      fprintf (dump_file, ") at stmt ");
      if (s)
	print_gimple_stmt (dump_file, s , 0, TDF_SLIM);
      else
        fprintf (dump_file, " NULL\n");
      indent += bump;
    }

  res =  super::range_of_expr (r, expr, s);

  return trailer ("range_of_expr", res, expr, r);
}

// Tracing version of edge range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree expr, edge e)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_of_expr (");
      print_generic_expr (dump_file, expr, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  res =  super::range_of_expr (r, expr, e);

  return trailer ("range_of_expr", res, expr, r);
}

// Tracing version of range_on_edge.  Call it with printing wrappers.

bool
trace_ranger::range_on_edge (irange &r, edge e, tree name)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_on_edge (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  res =  super::range_on_edge (r, e, name);

  return trailer ("range_on_edge", res, name, r);
}

// Tracing version of range_on_entry.  Call it with printing wrappers.

bool
trace_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_on_entry (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") to BB %d\n", bb->index);
      indent += bump;
    }

  res =  super::range_on_entry (r, bb, name);

  return trailer ("range_on_entry", res, name, r);
}

// Tracing version of range_on_exit.  Call it with printing wrappers.

bool
trace_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_on_exit (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") from BB %d\n", bb->index);
      indent += bump;
    }

  res =  super::range_on_exit (r, bb, name);

  return trailer ("range_on_exit", res, name, r);
}

// Tracing version of range_of_stmt.  Call it with printing wrappers.

bool
trace_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "range_of_stmt (");
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") at stmt ", dump_file);
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      indent += bump;
    }

  res =  super::range_of_stmt (r, s, name);

  return trailer ("range_of_stmt", res, name, r);
}

// Tracing version of outgoing_edge_range_p.  Call it with printing wrappers.

bool
trace_ranger::outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range)
{
  bool res;
  if (dumping ())
    {
      fprintf (dump_file, "outgoing_edge_range_p (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d, with range ", e->src->index,
	       e->dest->index);
      if (name_range)
        name_range->dump (dump_file);
      else
        fputs ("NULL\n", dump_file);
      indent += bump;
    }

  res =  super::outgoing_edge_range_p (r, e, name, name_range);

  return trailer ("outgoing_edge_range_p", res, name, r);
}
