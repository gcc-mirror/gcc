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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "dbgcnt.h"

// #define RANGER_SWITCH_NONE
// #define RANGER_SWITCH_CALC


class switch_edge_manager
{
public:
  switch_edge_manager ();
  ~switch_edge_manager ();
  gimple *get_range (irange &r, gimple *s, edge e, bool must_exist = false);
  void clear_ranges ();
private:
  void calc_switch_ranges (gswitch *sw);
  void calc_single_range (irange &r, gswitch *sw, edge e);
  void init_ranges ();
  irange *new_range ();
  hash_map<edge, irange *> *m_edge_table;
  obstack m_rstack;
} switch_edge_range;

switch_edge_manager::switch_edge_manager ()
{
  m_edge_table = NULL;
}

switch_edge_manager::~switch_edge_manager ()
{
  clear_ranges ();
}

void
switch_edge_manager::init_ranges ()
{
  gcc_assert (!m_edge_table);
  m_edge_table = new hash_map<edge, irange *> (n_edges_for_fn (cfun));
  gcc_obstack_init (&m_rstack);
}

void
switch_edge_manager::clear_ranges ()
{
  if (m_edge_table)
    {
      delete m_edge_table;
      obstack_free (&m_rstack, NULL);
    }
  m_edge_table = NULL;
}

inline irange *
switch_edge_manager::new_range ()
{
  return XOBNEW (&m_rstack, irange);
}


gimple *
switch_edge_manager::get_range (irange &r, gimple *s, edge e, bool must_exist)
{
  gcc_checking_assert (is_a<gswitch *> (s));
  gswitch *sw = as_a<gswitch *> (s);

  // ADA currently has cases where the index is 64 bits and the case
  // arguments are  32 bit, causing a trap when we create a case_range.
  // Until this is resolved (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87798)
  // punt on these switches.
  if (gimple_switch_num_labels (sw) > 1 && 
      TYPE_PRECISION (TREE_TYPE (CASE_LOW (gimple_switch_label (sw, 1)))) != 
      TYPE_PRECISION (TREE_TYPE (gimple_switch_index (sw))))
    return NULL;

  if (!m_edge_table)
    init_ranges ();

#ifdef RANGER_SWITCH_NONE
  // Turn off switch support in ranger
  return NULL;
#endif

#ifdef RANGER_SWITCH_CALC
  // Calculate the switch edge each time.
  calc_single_range (r, sw, e);
  return s;
#endif

  irange **val = m_edge_table->get (e);
  if (!val)
    {
      // Avoid infinite recursion in case of a bug.
      gcc_assert (!must_exist);
      // calculate ranges for the entire switch.
      calc_switch_ranges (sw);
      return get_range (r, s, e, true);
    }

  r = **val;
#ifdef VERIFY_SWITCH
  // Verify the range is as expected.
  irange verify;
  calc_single_range (verify, sw, e);
  gcc_assert (verify == r);
#endif
  return s;
}

void
switch_edge_manager::calc_switch_ranges (gswitch *sw)
{
  bool existed;
  unsigned x, lim;
  lim = gimple_switch_num_labels (sw);
  tree type = TREE_TYPE (gimple_switch_index (sw));
  
  edge default_edge = gimple_switch_default_edge (cfun, sw);
  irange *&default_slot = m_edge_table->get_or_insert (default_edge, &existed);
  // This should be the first call into this switch.
  // For the default range case, start with varying and intersect each other 
  // case from it. 
  gcc_assert (!existed);
  default_slot = new_range ();
  default_slot->set_varying (type);

  for (x = 1; x < lim; x++)
    {
      edge e = gimple_switch_edge (cfun, sw, x);

      // If this edge is the same as the default edge, do nothing else.
      if (e == default_edge)
        continue;

      tree low = CASE_LOW (gimple_switch_label (sw, x));
      tree high = CASE_HIGH (gimple_switch_label (sw, x));
      if (!high)
	high = low;

      irange def_case_range (irange::INVERSE, low, high);
      def_case_range.cast (type);
      default_slot->intersect (def_case_range);

      irange case_range (low, high);
      case_range.cast (type);
      irange *&slot = m_edge_table->get_or_insert (e, &existed);
      if (!existed)
        {
	  slot = new_range ();
	  *slot = case_range;
	}
      else
        // Add this case range to the existing ranges on the edge.
	slot->union_ (case_range);
    }

#ifdef VERIFY_SWITCH
  for (x = 1; x < lim; x++)
    {
      edge e = gimple_switch_edge (cfun, sw, x);
      irange r1;
      // get range verifies it as is it should be.
      gcc_assert (get_range (r1, sw, e, true));
    }
#endif

}

void
switch_edge_manager::calc_single_range (irange &r, gswitch *sw, edge e)
{
  tree type = TREE_TYPE (gimple_switch_index (sw));

  unsigned x, lim;
  lim = gimple_switch_num_labels (sw);

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
	  irange case_range (irange::INVERSE, low, high);
	  r.intersect (case_range);
	}
    }
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

  return switch_edge_range.get_range (r, s, e);
}

// Initialize a ranger.

ssa_ranger::ssa_ranger ()
{
  switch_edge_range.clear_ranges ();
}

// Destruct a ranger.

ssa_ranger::~ssa_ranger ()
{
  switch_edge_range.clear_ranges ();
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
  if (!irange::supports_p (op))
     return false;
  if (valid_ssa_p (op))
    {
      range_on_edge (r, e, op);
      return true;
    }
  // If it is not an SSA_NAME, just get the basic range.
  return get_tree_range (r, op);
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

// Calculate a range for NAME on edge E and return it in R.  
// if NAME is a PHI node at the dest of E, get the argument result.
// Return false if no range can be determined.

void
ssa_ranger::range_on_edge (irange &r, edge e, tree name)
{
  irange edge_range;
  gcc_checking_assert (valid_ssa_p (name));

  range_on_exit (r, e->src, name);

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (edge_range, e, name, &r))
    r = edge_range;
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
      if (!irange::supports_type_p (t))
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

// Return the range for NAME on entry to basic block BB in R.  
// Return false if no range can be calculated.
// Calculation is performed by unioning all the ranges on incoming edges.

void
ssa_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  edge_iterator ei;
  edge e;
  tree type = TREE_TYPE (name);
  irange pred_range;

  gcc_checking_assert (irange::supports_type_p (type));

  // Start with an empty range.
  r.set_undefined (type);

  gcc_checking_assert (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun));

  // Visit each predecessor to resolve them.
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      range_on_edge (pred_range, e, name);
      r.union_ (pred_range);
      // If varying is reach, stop processing.
      if (r.varying_p ())
        break;
    }
}


// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

void
ssa_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  // on-exit from the exit block?
  gcc_checking_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  gimple *s = last_stmt (bb);
  // If there is no statement in the block and this isnt the entry block,
  // go get the range_on_entry for this block.
  // For the entry block, a NULL stmt will return the global value for NAME.
  if (!s && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun))
    range_on_entry (r, bb, name);
  else
    gcc_assert (range_of_expr (r, name, s));
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
  gcc_checking_assert (irange::supports_type_p (gimple_expr_type (s)));

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
  gcc_checking_assert (irange::supports_type_p (gimple_expr_type (s)));

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
  gcc_checking_assert (irange::supports_type_p (gimple_expr_type (s)));

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
  gcc_checking_assert (irange::supports_type_p (gimple_expr_type (s)));

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

  if (!irange::supports_type_p (type))
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
	range_on_edge (arg_range, e, arg);
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
  if (!irange::supports_type_p (type))
    return false;

  if (gimple_call_nonnull_result_p (call))
    {
      r = range_nonzero (type);
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
  if (!irange::supports_type_p (TREE_TYPE (op1)))
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
  if (!irange::supports_type_p (TREE_TYPE (op1)))
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

// -------------------------------------------------------------------------

// Initialize a global_ranger.

global_ranger::global_ranger ()
{
}

// Deallocate global_ranger members.

global_ranger::~global_ranger () 
{
}

// Return the range of NAME on entry to block BB in R.

void
global_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  irange entry_range;
  gcc_checking_assert (valid_ssa_p (name));

  // Start with any known range
  gcc_assert (range_of_stmt (r, SSA_NAME_DEF_STMT (name), name));

  // NOw see if there is any on_entry value which may refine it .
  if (m_gori.block_range (entry_range, bb, name))
    r.intersect (entry_range);
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
    {
      // first check to see if the stmt has a name.
      name = gimple_get_lhs (s);
      if (!name)
	return ssa_ranger::range_of_stmt (r, s, name);
    }

  gcc_checking_assert (irange::supports_ssa_p (name));

  // If this STMT has already been processed, return that value. 
  if (m_gori.m_globals.get_global_range (r, name))
    return true;
 
  // Avoid infinite recursion by initializing global cache
  irange tmp;
  tmp = range_from_ssa (name);
  m_gori.m_globals.set_global_range (name, tmp);

  gcc_assert (ssa_ranger::range_of_stmt (r, s, name));

  if (is_a<gphi *> (s))
    r.intersect (tmp);
  m_gori.m_globals.set_global_range (name, r);
  return true;
}

// Return the range of expr OP on edge E in R.  

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
  if (!irange::supports_p (op))
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
	range_on_entry (r, bb, op);

      // No range yet, see if there is a dereference in the block.
      // We don't care if it's between the def and a use within a block
      // because the entire block must be executed anyway.
      // FIXME:?? For non-call exceptions we could have a statement throw
      // which causes an early block exit. 
      // in which case we may need to walk from S back to the def/top of block
      // to make sure the deref happens between S and there before claiming 
      // there is a deref.   Punt for now.
      if (!cfun->can_throw_non_call_exceptions && r.varying_p () &&
	  m_gori.non_null_deref_p (op, bb))
	r = range_nonzero (TREE_TYPE (op));
      return true;
    }

  // Fall back to the default.
  return ssa_ranger::range_of_expr (r, op);
}

// Calculate a range on edge E and return it in R.  Try to evaluate a range
// for NAME on this edge.  Return FALSE if this is either not a control edge
// or NAME is not defined by this edge.

bool
global_ranger::outgoing_edge_range_p (irange &r, edge e, tree name,
				    irange *name_range)
{
  return m_gori.outgoing_edge_range_p (r, e, name, name_range);
}

// If NAME's derived chain has a termnial name, return it otherwise NULL_TREE.

tree
global_ranger::terminal_name (tree name)
{
  return m_gori.terminal_name (name);
}

// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO fields.

void
global_ranger::export_global_ranges ()
{
  unsigned x;
  irange r;
  if (dump_file)
    {
      fprintf (dump_file, "Exported global range table\n");
      fprintf (dump_file, "===========================\n");
    }

  for ( x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (name && !SSA_NAME_IN_FREE_LIST (name) &&
	  valid_ssa_p (name) && m_gori.m_globals.get_global_range (r, name) &&
	  !r.varying_p())
	{
	  // Make sure that the new range is a subet of the old range.
	  irange old_range = range_from_ssa (name);
	  old_range.intersect (r);
	  gcc_checking_assert (old_range == r);

	  // WTF? Can't write non-null pointer ranges?? stupid set_range_info!
	  if (!POINTER_TYPE_P (TREE_TYPE (name)) && !r.undefined_p ())
	    {
	      if (!dbg_cnt (ranger_export_count))
		return;

	      value_range_base vr = irange_to_value_range (r);
	      set_range_info (name, vr);
	      if (dump_file)
		{
		  print_generic_expr (dump_file, name , TDF_SLIM);
		  fprintf (dump_file, " --> ");
		  vr.dump (dump_file);
		  fprintf (dump_file, "         irange : ");
		  r.dump (dump_file);
		}
	    }
	}
    }
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
      m_gori.dump_block (f, bb);

      dump_bb (f, bb, 4, TDF_NONE);

      // Now find any globals defined in this block 
      for (x = 1; x < num_ssa_names; x++)
	{
	  tree name = ssa_name (x);
	  if (valid_ssa_p (name) && SSA_NAME_DEF_STMT (name) &&
	      gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
	      m_gori.m_globals.get_global_range (range, name))
	    {
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
		  gimple *s = SSA_NAME_DEF_STMT (name);
		  // Only print the range if this is the def block,
		  // or the on entry cache for either end of the edge is set.
		  if ((s && bb == gimple_bb (s)) ||
		      m_gori.block_range (range, bb, name, false) ||
		      m_gori.block_range (range, e->dest, name, false))
		    {
		      range_on_edge (range, e, name);
		      if (!range.varying_p ())
			{
			  fprintf (f, "%d->%d ", e->src->index,
				   e->dest->index);
			  char c = (m_gori.is_export_p (name, bb) ? ' ' : '*');
			  if (e->flags & EDGE_TRUE_VALUE)
			    fprintf (f, " (T)%c", c);
			  else if (e->flags & EDGE_FALSE_VALUE)
			    fprintf (f, " (F)%c", c);
			  else
			    fprintf (f, "     ");
			  print_generic_expr (f, name, TDF_SLIM);
			  fprintf(f, " : \t");
			  range.dump(f);
			}
		    }
		}
	    }
	}
    }

  m_gori.m_globals.dump (dump_file);

  if (dump_flags & TDF_DETAILS)
    {
      fprintf (f, "\nDUMPING GORI MAP\n");
      m_gori.dump (f);
      fprintf (f, "\n");
    }
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

	  // Calculate a range for the LHS if there is one.
	  if (valid_ssa_p (gimple_get_lhs (stmt)))
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

loop_ranger::loop_ranger ()
{
  m_vr_values = new vr_values;
}

loop_ranger::~loop_ranger ()
{
  delete m_vr_values;
}

// Adjust a PHI result with loop information.

void
loop_ranger::adjust_phi_with_loop_info (irange &r, gphi *phi)
{
  struct loop *l = loop_containing_stmt (phi);
  if (l && l->header == gimple_bb (phi))
    {
      tree phi_result = PHI_RESULT (phi);
      value_range vr = irange_to_value_range (r);
      m_vr_values->adjust_range_with_scev (&vr, l, phi, phi_result);
      if (vr.constant_p ())
	{
	  irange old = r;
	  r = value_range_to_irange (TREE_TYPE (phi_result), vr);
	  if (old != r
	      && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "LOOP_RANGER refined this PHI result: ");
	      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	      fprintf (dump_file, "  from this range: ");
	      old.dump (dump_file);
	      fprintf (dump_file, "  into this range: ");
	      r.dump (dump_file);
	    }
	}
    }
}

// Virtual override of global_ranger::range_of_phi() that adjusts the
// result with loop information if available.

bool
loop_ranger::range_of_phi (irange &r, gphi *phi, tree name,
			   const irange *name_range, gimple *eval_from,
			   edge on_edge)
{
  bool result = global_ranger::range_of_phi (r, phi, name, name_range,
					     eval_from, on_edge);
  if (result && scev_initialized_p ())
    adjust_phi_with_loop_info (r, phi);
  return result;
}

// Constructor for trace_ranger.

trace_ranger::trace_ranger ()
{
  indent = 0;
  trace_count = 0;
}

// If dumping, return true and print the prefix for the next output line.

inline bool
trace_ranger::dumping (unsigned counter, bool trailing)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      // Print counter index as well as INDENT spaces.
      if (!trailing)
	fprintf (dump_file, " %-7u ", counter);
      else
	fprintf (dump_file, "         ");
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
trace_ranger::trailer (unsigned counter, const char *caller, bool result,
		       tree name, const irange &r)
{
  indent -= bump;
  if (dumping (counter, true))
    {
      fputs(result ? "TRUE : " : "FALSE : ", dump_file);
      fprintf (dump_file, "(%u) ", counter);
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
  unsigned idx = ++trace_count;
  if (dumping (idx))
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

  res = super::range_of_expr (r, expr, s);

  return trailer (idx, "range_of_expr", res, expr, r);
}

// Tracing version of edge range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree expr, edge e)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_expr (");
      print_generic_expr (dump_file, expr, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  res =  super::range_of_expr (r, expr, e);

  return trailer (idx, "range_of_expr", res, expr, r);
}

// Tracing version of range_on_edge.  Call it with printing wrappers.

void
trace_ranger::range_on_edge (irange &r, edge e, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_edge (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  super::range_on_edge (r, e, name);

  trailer (idx, "range_on_edge", true, name, r);
}

// Tracing version of range_on_entry.  Call it with printing wrappers.

void
trace_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_entry (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") to BB %d\n", bb->index);
      indent += bump;
    }

  super::range_on_entry (r, bb, name);

  trailer (idx, "range_on_entry", true, name, r);
}

// Tracing version of range_on_exit.  Call it with printing wrappers.

void
trace_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_exit (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") from BB %d\n", bb->index);
      indent += bump;
    }

  super::range_on_exit (r, bb, name);

  trailer (idx, "range_on_exit", true, name, r);
}

// Tracing version of range_of_stmt.  Call it with printing wrappers.

bool
trace_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_stmt (");
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") at stmt ", dump_file);
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      indent += bump;
    }

  res = super::range_of_stmt (r, s, name);

  return trailer (idx, "range_of_stmt", res, name, r);
}

// Tracing version of outgoing_edge_range_p.  Call it with printing wrappers.

bool
trace_ranger::outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
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

  res = super::outgoing_edge_range_p (r, e, name, name_range);

  return trailer (idx, "outgoing_edge_range_p", res, name, r);
}
