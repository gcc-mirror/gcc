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
#include "fold-const.h"


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

  if (!ssa_range::supports_type_p (type))
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

// This function returns a range for a tree node.  If optional statement S
// is present, then the range would be if it were to appear as a use on S.
// Return false if ranges are not supported.

bool
ssa_range::range_of_expr (irange &r, tree expr, gimple *s ATTRIBUTE_UNUSED)
{
  tree type;
  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
	r = irange (TREE_TYPE (expr), expr, expr);
	return true;

      case SSA_NAME:
        if (supports_ssa_p (expr))
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
	  if (supports_type_p (type))
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
	if (supports_type_p (type))
	  {
	    // Set to range for this type.
	    r.set_varying (type);
	    return true;
	  }
	break;
    }

  return false;
}


// Calculate a range for range_op statement S and return it in R.  If a range
// cannot be calculated, return false.  

bool
ssa_range::range_of_range_op (irange &r, grange_op *s)
{
  irange range1, range2;
  bool res;
  gcc_checking_assert (supports_type_p (gimple_expr_type (s)));

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // The statement can be a supported type, but the operands may not be
  // ie,   int_a_2 = (int) float_f_2.
  
  // Calculate a range for operand 1.
  res = range_of_expr (range1, op1, s);

  // Calculate a result for op2 if it is needed.
  if (res && op2)
      res = range_of_expr (range2, op2, s);

  // If we have all the operands we need, call fold.
  if (res)
    {
      if (op2)
	res = s->fold (r, range1, range2);
      else
	res = s->fold (r, range1);
    }

  // If range_of_expr or fold() fails, return varying.
  if (!res)
    r.set_varying (gimple_expr_type (s));
  return true;
}

// Calculate a range for phi statement S and return it in R.  If a range
// cannot be calculated, return false.  

bool
ssa_range::range_of_phi (irange &r, gphi *phi)
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
      // Try to find a range from the edge.  If that fails, return varying.
      if (valid_ssa_p (arg))
	gcc_assert (range_on_edge (arg_range, e, arg));
      else
	gcc_assert (range_of_expr (arg_range, arg));

      r.union_ (arg_range);
      // Once the value reaches varying, stop looking.
      if (r.varying_p ())
	break;
    }

  return true;
}

// Calculate a range for call statement S and return it in R.  If a range
// cannot be calculated, return false. 

bool
ssa_range::range_of_call (irange &r, gcall *call)
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

// Calculate a range for COND_EXPR statement S and return it in R.  If a range
// cannot be calculated, return false. 

bool
ssa_range::range_of_cond_expr  (irange &r, gassign *s)
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

  gcc_assert (range_of_expr (cond_range, cond, s));
  gcc_assert (range_of_expr (range1, op1, s));
  gcc_assert (range_of_expr (range2, op2, s));

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


// Calculate a range for statement S and return it in R.  If a range cannot
// be calculated, return false.

bool
ssa_range::range_of_stmt (irange &r, gimple *s, tree name)
{
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

// Calculate a range for NAME on edge E and return it in R.  
// Return false if no range can be determined.

bool
ssa_range::range_on_edge (irange &r, edge e, tree name)
{
  irange edge_range;

  if (!supports_ssa_p (name))
    return false;

  gcc_assert (range_on_exit (r, e->src, name));

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (edge_range, e, name))
    r.intersect (edge_range);

  return true;
}

// Calculate a range on edge E and return it in R.  Try to evaluate a range
// for NAME on this edge.  Return FALSE if this is either not a control edge
// or NAME is not defined by this edge.

bool
ssa_range::outgoing_edge_range_p (irange &r, edge e, tree name)
{
  irange lhs;

  gcc_checking_assert (valid_ssa_p (name));
  // Determine if there is an outgoing edge.
  gimple *s = gimple_outgoing_edge_range_p (lhs, e);

  // If there are no gori tables, we only look at this statement 
  if (!m_gori)
    {
      if (!s)
	return false;
      // Otherwise use the outgoing edge as a LHS and try to calculate a range.
      return compute_operand_range (r, s, name, lhs);
    }

  if (!range_p (e->src, name))
    return false;

  gcc_checking_assert (s);
  return compute_operand_range (r, s, name, lhs);

}

// Return the range for NAME on entry to basic block BB in R.  
// Return false if no range can be calculated.
// Calculation is performed by unioning all the ranges on incoming edges.

bool
ssa_range::range_on_entry (irange &r, basic_block bb, tree name)
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
ssa_range::range_on_exit (irange &r, basic_block bb, tree name)
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

// Calculate the range for NAME if the lhs of statement S has the range LHS.
// NAME must be one of the two operands on S.  Return the result in R.
// Return false if no range can be calculated.

bool
ssa_range::compute_operand_range_on_stmt (irange &r, grange_op *s, tree name,
					  const irange &lhs)
{
  irange op1_range, op2_range;

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Operand 1 is the name being looked for, evaluate it.
  if (op1 == name)
    { 
      if (!op2)
	{
	  // The second parameter to a unary operation is the range for the type
	  // of operand1, but if it can be reduced further, the results will
	  // be better.  Start with what we know of the range of OP1.
	  if (range_of_expr (op1_range, op1, s))
	    return s->calc_op1_irange (r, lhs, op1_range);
	  else
	    return s->calc_op1_irange (r, lhs);
	}
      // If we need the second operand, get a value and evaluate.
      if (range_of_expr (op2_range, op2, s))
	return s->calc_op1_irange (r, lhs, op2_range);
      return false;
    }

  gcc_checking_assert (op2 == name);
  if (range_of_expr (op1_range, op1, s))
    return s->calc_op2_irange (r, lhs, op1_range);
  return false;
}

/* GORI_MAP is used to determine what ssa-names in a block can generate range
   information, and provides tools for the block ranger to enable it to
   efficiently calculate these ranges.
   GORI stands for "Generates Outgoing Range Information."

   information for a basic block is calculated once and stored. IT is only
   calculated the first time a query is made, so if no queries are made, there
   is little overhead.
   
   2 bitmaps are maintained for each basic block:
   m_outgoing  : a set bit indicates a range can be generated for a name.
   m_incoming  : a set bit means a this name come from outside the block and is
	       used in the calculation of some outgoing range.

   The def_chain bitmaps is indexed by ssa_name. Bit are set within this 
   bitmap to indicate ssa_names which are defined in the SAME block and used
   to calculate this ssa_name.
    <bb 2> :
      _1 = x_4(D) + -2;
      _2 = _1 * 4;
      j_7 = foo ();
      q_5 = _2 + 3;
      if (q_5 <= 13)

    _1  : (single import : x_4(D))  :x_4(D)
    _2  : (single import : x_4(D))  :_1  x_4(D)
    q_5  : (single import : x_4(D))  :_1  _2  x_4(D)
    BB2 incoming: x_4(D)
    BB2 outgoing: _1  _2  x_4(D)  q_5

    This dump indicates the bits set in the incoming and outgoing vectors, as
    well as demonstrates the def_chain bits for the related ssa_names.
    The def chain is sued to determine which ssa_names are used in the
    calulcation of a the value.    checking the chain for _2 indicates that _1
    and x_4 are used in its evaluation, and with x_4 being an import, 
    an incoming value can change the result.

    For the purpose of defining an import, PHI node defintions  are considered
    imports as the dont really reside in the block, but rather are
    accumulators of values from incoming edges.
    
    Def chains also only include statements which are valid grange_op's so
    a def chain will only span statements for which the range engine
    implements operations.  */

class gori_map
{
public:
  gori_map ();
  ~gori_map ();

  tree terminal_name (tree name);
  bitmap get_def_chain (tree name);

  bool in_chain_p (tree name, tree def);
  bool is_export_p (tree name, basic_block bb);
  bool is_import_p (tree name, basic_block bb);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb);
private:
  vec<bitmap> m_outgoing;	// BB: Outgoing ranges generated.
  vec<bitmap> m_incoming;	// BB: ranges coming in.
  vec<bitmap> m_def_chain;	// SSA_NAME : def chain components.
  vec<tree> m_terminal;	        // SSA_NAME : chain terminal name.
  void maybe_add_gori (tree name, basic_block bb);
  void calculate_gori (basic_block bb);
  bitmap imports (basic_block bb);
  bitmap exports (basic_block bb);
  tree build_def_chain (tree name, bitmap result, basic_block bb);
//  void process_stmt (gimple *s, bitmap result, basic_block bb);
};


// Initialize a gori-map structure.

gori_map::gori_map ()
{
  m_outgoing.create (0);
  m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_incoming.create (0);
  m_incoming.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_def_chain.create (0);
  m_def_chain.safe_grow_cleared (num_ssa_names);
  m_terminal.create (0);
  m_terminal.safe_grow_cleared (num_ssa_names);

}

// Free any memory the GORI map allocated.

gori_map::~gori_map ()
{
  unsigned x, bb;
  for (bb = 0; bb < m_outgoing.length (); ++bb)
    if (m_outgoing[bb])
      BITMAP_FREE (m_outgoing[bb]);
  m_outgoing.release ();

  for (bb = 0; bb < m_incoming.length (); ++bb)
    if (m_incoming[bb])
      BITMAP_FREE (m_incoming[bb]);
  m_incoming.release ();

  for (x = 0; x < m_def_chain.length (); ++x)
    if (m_def_chain[x])
      BITMAP_FREE (m_def_chain[x]);
  m_def_chain.release ();
  m_terminal.release ();
}

// Return the bitmap vector of all imports to BB. Calculate if necessary

bitmap
gori_map::imports (basic_block bb)
{
  if (!m_incoming[bb->index])
    calculate_gori (bb);
  return m_incoming[bb->index];
}

// Return true if NAME is an import to basic block BB

bool
gori_map::is_import_p (tree name, basic_block bb)
{
  return bitmap_bit_p (imports (bb), SSA_NAME_VERSION (name));
}

// Return the bitmap vector of all export from BB. Calculate if necessary

bitmap
gori_map::exports (basic_block bb)
{
  if (!m_outgoing[bb->index])
    calculate_gori (bb);
  return m_outgoing[bb->index];
}

// Return true if NAME is can have ranges generated for it from basic block BB.

bool
gori_map::is_export_p (tree name, basic_block bb)
{
  return bitmap_bit_p (exports (bb), SSA_NAME_VERSION (name));
}

// Return true if NAME is in the def chain of DEF.  If BB is provided, only
// return true if the defining statement of DEF is in BB.

bool
gori_map::in_chain_p (tree name, tree def)
{
  gcc_checking_assert (ssa_range::valid_ssa_p (def));
  gcc_checking_assert (ssa_range::valid_ssa_p (name));

  // Get the defintion chain for DEF
  bitmap chain = get_def_chain (def);

  if (chain == NULL)
    return false;
  return bitmap_bit_p (chain, SSA_NAME_VERSION (name));
}

// If NAME has a definition chain, and the chain has a single import into
// the block, return the name of that import.

tree
gori_map::terminal_name (tree name)
{
  // Ensure the def chain has been calculated.
  get_def_chain (name);
  return m_terminal[SSA_NAME_VERSION (name)];
}

// Given up to 3 ssa names, return the common name or NULL_TREE.   NULL_TREE's
// passed in can be ignored, but all specified ssa-names must be the same name.

static inline tree
pick_import (tree ssa1, tree ssa2, tree ssa3)
{
  if (ssa1)
    {
      if (ssa2 && ssa1 != ssa2)
        return NULL_TREE;	// No match.
      // Either ssa2 is NULL, or it is the same as ssa1.
      if (!ssa3 || ssa1 == ssa3)
        return ssa1;	// ssa1 is the import.
      return NULL_TREE;
    }
  if (ssa2)
    {
      // If there is no ssa3 or ssa3 is thr same as ssa2, thats the import.
      if (!ssa3 || ssa2 == ssa3)
        return ssa2;
      // They must both be different, so no import.
      return NULL_TREE;
    }
  return ssa3;
}

// Build def_chains for NAME if it is in BB.. copy the def chain into RESULT.
// Return the import for name, or NAME if it is an import.

tree
gori_map::build_def_chain (tree name, bitmap result, basic_block bb)
{
  bitmap b;
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  // Add this operand into the result.
  bitmap_set_bit (result, SSA_NAME_VERSION (name));

  if (gimple_bb (def_stmt) == bb)
    {
      // Get the def chain for the operand
      b = get_def_chain (name);
      // If there was one, copy it into result and retuirn the terminal name.
      if (b)
        {
	  bitmap_ior_into (result, b);
	  return m_terminal [SSA_NAME_VERSION (name)];
	}
      // If there is no def chain, this terminal is within the same BB.
    }
  return name;	// This is an import.
}

// Calculate the def chain for NAME and all of its dependent operands. Only
// using names in the same BB.  Return the bitmap of all names in the 
// m_def_chain.   This only works for supported range statements.

bitmap
gori_map::get_def_chain (tree name)
{
  tree ssa1, ssa2, ssa3;
  unsigned v = SSA_NAME_VERSION (name);

  // If it has already been processed, just return the cached value.
  if (m_def_chain[v])
    return m_def_chain[v];

  // No definition chain for default defs.
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return NULL;

  gimple *s = SSA_NAME_DEF_STMT (name);
  if (is_a<grange_op *> (s))
    { 
      grange_op *stmt = as_a<grange_op *> (s);
      ssa1 = ssa_range::valid_ssa_p (stmt->operand1 ());
      ssa2 = ssa_range::valid_ssa_p (stmt->operand2 ());
      ssa3 = NULL_TREE;
    }
  else if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    {
      gassign *st = as_a<gassign *> (s);
      ssa1 = ssa_range::valid_ssa_p (gimple_assign_rhs1 (st));
      ssa2 = ssa_range::valid_ssa_p (gimple_assign_rhs2 (st));
      ssa3 = ssa_range::valid_ssa_p (gimple_assign_rhs3 (st));
    }
  else
    return NULL;

  basic_block bb = gimple_bb (s);

  // Allocate a new bitmap and initialize it.
  m_def_chain[v] = BITMAP_ALLOC (NULL);

  // build_def_chain returns the terminal name. If we have more than one unique
  // terminal name, then this statement will have no terminal.
  if (ssa1)
    ssa1 = build_def_chain (ssa1, m_def_chain[v], bb);
  if (ssa2)
    ssa2 = build_def_chain (ssa2, m_def_chain[v], bb);
  if (ssa3)
    ssa3 = build_def_chain (ssa3, m_def_chain[v], bb);

  m_terminal[v] = pick_import (ssa1, ssa2, ssa3);
  // If we run into pathological cases where the defintion chains are huge
  // (I'm thinking fppp for instance.. huge basic block fully unrolled)
  // we might be able to limit this by deciding here that if there is no
  // import AND 2 or more ssa names, we change the def_chain back to be
  // just the ssa-names.  that should prevent  a_2 = b_6 + a_8 from creating
  // a pathological case yet allow us to still handle it when b_6 and a_8 are
  // derived from the same base name.
  // thoughts?
  return m_def_chain[v];
}


// If NAME is non-NULL and defined in block BB, calculate the def chain
// and add it to m_outgoing, and any imports to m_incoming.

void
gori_map::maybe_add_gori (tree name, basic_block bb)
{
  if (name)
    {
      bitmap r = get_def_chain (name);
      if (r)
        {
	  bitmap_copy (m_outgoing[bb->index], r);
	  tree im = m_terminal[SSA_NAME_VERSION (name)];
	  if (im)
	    bitmap_set_bit (m_incoming[bb->index], SSA_NAME_VERSION (im));
	}
      else
        {
	  // IF there is no def chain, and name originates outside this block
	  // then this name is also an import.
	  if (gimple_bb (SSA_NAME_DEF_STMT (name)) != bb)
	    bitmap_set_bit (m_incoming[bb->index], SSA_NAME_VERSION (name));
	}
      // Def chain doesn't include itself, and even if there isn't a def
      // chain, this name should be added to exports.
      bitmap_set_bit (m_outgoing[bb->index], SSA_NAME_VERSION (name));
    }
}

// Calculate all the required information for BB.

void
gori_map::calculate_gori (basic_block bb)
{
  tree name;
  gcc_assert (m_outgoing[bb->index] == NULL);
  m_outgoing[bb->index] = BITMAP_ALLOC (NULL);
  m_incoming[bb->index] = BITMAP_ALLOC (NULL);

  // If this block's last statement may generate range informaiton, 
  // go calculate it.
  gimple *s = gimple_outgoing_range_stmt_p (bb);
  if (!s)
    return;
  if (is_a<gcond *> (s))
    {
      gcond *gc = as_a<gcond *>(s);
      name = ssa_range::valid_ssa_p (gimple_cond_lhs (gc));
      maybe_add_gori (name, gimple_bb (s));

      name = ssa_range::valid_ssa_p (gimple_cond_rhs (gc));
      maybe_add_gori (name, gimple_bb (s));
    }
  else
    {
      gswitch *gs = as_a<gswitch *>(s);
      name = ssa_range::valid_ssa_p (gimple_switch_index (gs));
      maybe_add_gori (name, gimple_bb (s));
    }
}

// Dump the table information for BB to file F.

void
gori_map::dump(FILE *f, basic_block bb)
{
  tree t;
  unsigned x, y;
  bitmap_iterator bi;

  if (!m_outgoing[bb->index])
    {
      fprintf (f, "BB%d was not processed.\n", bb->index);
      return;
    }

  // Dump the def chain for each SSA_NAME defined in BB.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (!name)
	continue;
      gimple *stmt = SSA_NAME_DEF_STMT (name);
      if (stmt && gimple_bb (stmt) == bb && m_def_chain[x] &&
	  !bitmap_empty_p (m_def_chain[x]))
        {
	  print_generic_expr (f, name, TDF_SLIM);
	  if ((t = terminal_name (name)))
	    {
	      fprintf (f, "  : (single import : ");
	      print_generic_expr (f, t, TDF_SLIM);
	      fprintf (f, ")");
	    }
	  fprintf (f, "  :");
	  EXECUTE_IF_SET_IN_BITMAP (m_def_chain[x], 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }

  // Now dump the incoming vector.
  fprintf (f, "BB%d imports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (m_incoming[bb->index], 0, y, bi)
    {
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }

  // Now dump the export vector.
  fprintf (f, "\nBB%d exports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (m_outgoing[bb->index], 0, y, bi)
    {
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }
  fprintf (f, "\n");
}

// Dump the entire GORI map structure to file F.
void
gori_map::dump(FILE *f)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      fprintf (f, "----BB %d----\n", bb->index);
      dump (f, bb);
      fprintf (f, "\n");
    }
}

/* -------------------------------------------------------------------------*/

// Initialize a block ranger.

ssa_range::ssa_range (bool support_calculations)
{
  if (support_calculations)
    {
      // Create a boolean_type true and false range.
      m_bool_zero = irange (boolean_type_node, boolean_false_node,
			    boolean_false_node);
      m_bool_one = irange (boolean_type_node, boolean_true_node,
			   boolean_true_node);
      m_gori = new gori_map ();
    }
  else
    m_gori = NULL;
}

// Destruct a block ranger.

ssa_range::~ssa_range ()
{
  if (m_gori)
    delete m_gori;
}

static inline bool
is_gimple_logical_p (const gimple *gs)
{
  /* Look for boolean and/or condition.  */
  if (gimple_code (gs) == GIMPLE_ASSIGN)
    switch (gimple_expr_code (gs))
      {
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	  return true;

	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	  // Bitwise operations on single bits are logical too.
	  if (types_compatible_p (TREE_TYPE (gimple_assign_rhs1 (gs)),
				  boolean_type_node))
	    return true;
	  break;

	default:
	  break;
      }
  return false;

}
// Given the statement S, return an evaluation in R for NAME
// when the lhs evaluates to LHS.  Returning false means the name being
// looked for was not resolvable. 

bool
ssa_range::compute_operand_range (irange &r, gimple *s, tree name,
				   const irange &lhs)
{
  if (is_a<grange_op *> (s))
    return compute_operand_range_op (r, as_a<grange_op *> (s), name, lhs);
  if (is_a<gswitch *> (s))
    return compute_operand_range_switch (r, as_a<gswitch *> (s), name, lhs);
  return false;
}
    
// Given the switch S, return an evaluation in R for NAME
// when the lhs evaluates to LHS.  Returning false means the name being
// looked for was not resolvable. 

bool
ssa_range::compute_operand_range_switch (irange &r, gswitch *s, tree name,
				   const irange &lhs)
{
  tree op1 = gimple_switch_index (s);

  // If name matches, the range is simply the range from the edge.
  // Empty ranges are viral as they are on a path which isn't executable.
  if (op1 == name || lhs.undefined_p ())
    {
      r = lhs;
      return true;
    }

  // If op1 is in the defintion chain, pass lhs back.
  if (m_gori && valid_ssa_p (op1) && m_gori->in_chain_p (name, op1))
    return compute_operand_range (r, SSA_NAME_DEF_STMT (op1), name, lhs);

  return false;
}

// Given the range_op S, return an evaluation in R for NAME
// when the lhs evaluates to LHS.  Returning false means the name being
// looked for was not resolvable. 

bool
ssa_range::compute_operand_range_op (irange &r, grange_op *stmt, tree name,
				     const irange &lhs)
{
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;

  // Empty ranges are viral as they are on a path which isn't executable.
  if (lhs.undefined_p ())
    {
      r.set_undefined (TREE_TYPE (name));
      return true;
    }

  op1 = valid_ssa_p (stmt->operand1 ());
  op2 = valid_ssa_p (stmt->operand2 ());

  // The base ranger handles NAME on this statement.
  if (op1 == name || op2 == name)
    return compute_operand_range_on_stmt (r, stmt, name, lhs);

  // Check for logical combination cases which require developing ranges 
  // and combining the results based on the operation. 
  if (is_gimple_logical_p (stmt))
    return compute_logical_operands (stmt, r, name, lhs);

  // Reaching this point means NAME is not in this stmt, but one of the
  // names in it ought to be derived from it. 
  op1_in_chain = op1 && m_gori->in_chain_p (name, op1);
  op2_in_chain = op2 && m_gori->in_chain_p (name, op2);

  if (op2_in_chain)
    { 
      if (op1_in_chain)
	return compute_operand1_and_operand2_range (stmt, r, name, lhs);
      else
	return compute_operand2_range (stmt, r, name, lhs);
    }
  else
    if (op1_in_chain)
      return compute_operand1_range (stmt, r, name, lhs);

  // If neither operand is derived, then this stmt tells us nothing.
  return false;
}

// Evaluate a binary logical expression by combining the true and false
// ranges for each of the operands based on the result value in the LHS. 

bool
logical_combine (irange &r, enum tree_code code, const irange &lhs,
		 const irange &op1_true, const irange &op1_false,
		 const irange &op2_true, const irange &op2_false)
{
  // This is not a simple fold of a logical expression, rather it determines
  // ranges which flow through the logical expression.
  // Assuming x_8 is an unsigned char, and relational statements:
  //	      b_1 = x_8 < 20
  //	      b_2 = x_8 > 5
  // consider the logical expression and branch:
  //          c_2 = b_1 && b_2
  //          if (c_2)
  // To determine the range of x_8 on either edge of the branch,
  // one must first determine what the range of x_8 is when the boolean
  // values of b_1 and b_2 are both true and false.
  //    b_1 TRUE      x_8 = [0, 19]
  //    b_1 FALSE     x_8 = [20, 255]
  //    b_2 TRUE      x_8 = [6, 255]
  //    b_2 FALSE     x_8 = [0,5]. 
  //
  // These ranges are then combined based on the expected outcome of the branch
  // The range on the TRUE side of the branch must satisfy 
  //     b_1 == true && b_2 == true
  // in terms of x_8, that means both x_8 == [0, 19] and x_8 = [6, 255]
  // must be true.  The range of x_8 on the true side must be the intersection
  // of both ranges since both must be true.  Thus the range of x_8
  // on the true side is [6, 19]
  // 
  // To determine the ranges on the FALSE side, all 3 combinations of
  // failing ranges must be considered, and combined as any of them can cause
  // the false result.
  
  // If the LHS can be TRUE OR FALSE, then evaluate both a TRUE and FALSE
  // results and combine them.  If we fell back to VARYING any range
  // restrictions that have been discovered up to this point would be lost.  */
  if (!lhs.singleton_p ())
    {
      irange r1;
      irange bool_zero (boolean_type_node, boolean_false_node,
			boolean_false_node);
      irange bool_one (boolean_type_node, boolean_true_node, boolean_true_node);
      if (logical_combine (r1, code, bool_zero, op1_true, op1_false, op2_true,
			   op2_false) &&
	  logical_combine (r, code, bool_one, op1_true, op1_false, op2_true,
			   op2_false))
	{
	  r.union_ (r1);
	  return true;
	}
      return false;

    }

  switch (code)
    {
      //  A logical AND combines ranges from 2 boolean conditions.
      //       c_2 = b_1 && b_2
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  // The TRUE side is the intersection of the the 2 true ranges.
	  r = range_intersect (op1_true, op2_true);
	else
	  {
	    // The FALSE side is the union of the other 3 cases. 
	    irange ff = range_intersect (op1_false, op2_false);
	    irange tf = range_intersect (op1_true, op2_false);
	    irange ft = range_intersect (op1_false, op2_true);
	    r = range_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      //  A logical OR combines ranges from 2 boolean conditons.
      // 	c_2 = b_1 || b_2
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  // An OR operation will only take the FALSE path if both operands
	  // are false. so [20, 255] intersect [0, 5] is the 
	  // union: [0,5][20,255].  */
	  r = range_intersect (op1_false, op2_false);
	else
	  {
	    // The TRUE side of an OR operation will be the union of the other
	    // three combinations.
	    irange tt = range_intersect (op1_true, op2_true);
	    irange tf = range_intersect (op1_true, op2_false);
	    irange ft = range_intersect (op1_false, op2_true);
	    r = range_union (tt, tf);
	    r.union_ (ft);
	  }
	break;

      default:
        gcc_unreachable ();
    }

  return true;
}

// Given a logical STMT, calculate true and false for each potential path 
// using NAME and resolve the outcome based on the logical operator.  

bool
ssa_range::compute_logical_operands (grange_op *s, irange &r, tree name,
				        const irange &lhs)
{
  irange op1_range, op2_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;
  bool ret = true;

  irange op1_true, op1_false, op2_true, op2_false;

  // Reaching this point means NAME is not in this stmt, but one of the
  // names in it ought to be derived from it.  */
  op1 = s->operand1 ();
  op2 = s->operand2 ();
  gcc_checking_assert (op1 != name && op2 != name);

  op1_in_chain = valid_ssa_p (op1) && m_gori->in_chain_p (name, op1);
  op2_in_chain = valid_ssa_p (op2) && m_gori->in_chain_p (name, op2);

  /* If neither operand is derived, then this stmt tells us nothing. */
  if (!op1_in_chain && !op2_in_chain)
    return false;

  /* The false path is not always a simple inversion of the true side.
     Calulate ranges for true and false on both sides. */
  if (op1_in_chain)
    {
      ret = compute_operand_range (op1_true, SSA_NAME_DEF_STMT (op1), name,
				   m_bool_one);
      ret &= compute_operand_range (op1_false, SSA_NAME_DEF_STMT (op1), name,
				    m_bool_zero);
    }
  else
    {
      // Otherwise just get the value for name in operand 1 position
      gcc_assert (range_of_expr (op1_true, name, s));
      op1_false = op1_true;
    }

  /* If operand1 evaluated OK, move on to operand 2.  */
  if (ret)
    {
      if (op2_in_chain)
	{
	  ret &= compute_operand_range (op2_true, SSA_NAME_DEF_STMT (op2),
				      name, m_bool_one);
	  ret &= compute_operand_range (op2_false, SSA_NAME_DEF_STMT (op2),
				      name, m_bool_zero);
	}
      else
	{
	  // Otherwise just get the value for name in operand 2 position
	  gcc_assert (range_of_expr (op2_true, name, s));
	  op2_false = op2_true; 
	}
    }
  if (!ret || !logical_combine (r, gimple_expr_code (s), lhs, op1_true,
				op1_false, op2_true, op2_false))
    r.set_varying (TREE_TYPE (name));
  return true;
}


// Calculate a range for NAME from the operand 1 position of S assuming the 
// result of the statement is LHS.  Return the range in R, or false if no
// range could be calculated.

bool
ssa_range::compute_operand1_range (grange_op *s, irange &r, tree name,
				      const irange &lhs)
{
  irange op1_range, op2_range;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Determine a known range for operand1 ().
  if (!range_of_expr (op1_range, op1, s))
    return false;

  // Now calcuated the operand and put that result in r.
  if (!op2)
    {
      // we pass op1_range to the unary operation. Nomally it's a hidden
      // range_for_type paraemter, but sometimes having the actual range
      // can result in better information.
      if (!s->calc_op1_irange (r, lhs, op1_range))
	return false;
    }
  else
    {
      if (!range_of_expr (op2_range, op2, s))
	return false;
      if (!s->calc_op1_irange (r, lhs, op2_range))
	return false;
    }

  // Intersect the calculated result with the known result.
  op1_range.intersect (r);

  // Then feed this range back as the LHS of the defining statement.
  return compute_operand_range (r, SSA_NAME_DEF_STMT (op1), name, op1_range);
}


// Calculate a range for NAME from the operand 2 position of S assuming the 
// result of the statement is LHS.  Return the range in R, or false if no
// range could be calculated.

bool
ssa_range::compute_operand2_range (grange_op *s, irange &r, tree name,
				      const irange &lhs)
{
  irange op1_range, op2_range;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Get a range for op1.
  if (!range_of_expr (op1_range, op1, s))
    return false;

  // calculate the range for op2 based on lhs and op1.
  if (!s->calc_op2_irange (op2_range, lhs, op1_range))
    return false;

  // Also pick up what is known about op2's range at this point
  if (!range_of_expr (r, op2, s))
    return false;

  // And intersect it with the calculated result.
  op2_range.intersect (r);

  // Then feed this range back as the LHS of the defining statement.
  return compute_operand_range (r, SSA_NAME_DEF_STMT (op2), name, op2_range);
}

// Calculate a range for NAME from both operand positions of S assuming the 
// result of the statement is LHS.  Return the range in R, or false if no
// range could be calculated.

bool
ssa_range::compute_operand1_and_operand2_range (grange_op *s, irange &r,
						   tree name, const irange &lhs)
{
  irange op_range;

  // Calculate a good a range for op2. Since op1 == op2, this will have
  // already included whatever the actual range of name is.
  if (!compute_operand2_range (s, op_range, name, lhs))
    return false;

  // Now get the range thru op1... 
  if (!compute_operand1_range (s, r, name, lhs))
    return false;

  // Whichever range is the most permissive is the one we need to use. (?)
  // OR is that true?  Maybe this should be intersection?
  r.union_ (op_range);
  return true;
}
 


// Dump the block rangers data structures.

void
ssa_range::dump (FILE *f)
{
  if (!f || !m_gori)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  m_gori->dump (f);
  fprintf (f, "\n");
}

// If NAME's derived chain has a termnial name, return it otherwise NULL_TREE.

tree
ssa_range::terminal_name (tree name)
{
  gcc_checking_assert (m_gori);
  return m_gori->terminal_name (name);
}

// Return the definition chain, if any, for NAME.

bitmap
ssa_range::def_chain (tree name)
{
  gcc_checking_assert (m_gori);
  return m_gori->get_def_chain (name);
}

// Return TRUE if the outgoing edges of block BB define a range for NAME.
bool
ssa_range::range_p (basic_block bb, tree name)
{
  return m_gori->is_export_p (name, bb);
}

// This routine will loop through all the edges and block in the program and
// ask for the range of each ssa-name. THis exercises the ranger to make sure
// there are no ICEs for unexpected questions, as well as provides a
// convenient way to show all the ranges that could be calculated.

void
ssa_range::exercise (FILE *output)
{

  basic_block bb;
  irange range;
  
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge_iterator ei;
      edge e;
      bool printed = false;
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  unsigned x;
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && outgoing_edge_range_p (range, e, name))
		{
		  if (output)
		    {
		      printed = true;
		      fprintf (output, "BB%3d: ", bb->index);
		      if (e->flags & EDGE_TRUE_VALUE)
			fprintf (output, " T: ");
		      else if (e->flags & EDGE_FALSE_VALUE)
			fprintf (output, " F: ");
		      print_generic_expr (output, name, TDF_SLIM);
		      fprintf(output, "  \t");
		      range.dump(output);
		    }
		}
	    }
	}
      if (printed)
        fprintf (output, "\n");

    }
    
  if (output)
    dump (output);

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

// Allocate an populate the bitmap for NAME.

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
  for ( x = 1; x < num_ssa_names; x++)
    if (ssa_range::valid_ssa_p (ssa_name (x)) &&
	get_global_range (r, ssa_name (x)))
      {
        print_generic_expr (f, ssa_name (x), TDF_NONE);
	fprintf (f, "  : ");
        r.dump (f);
      }
}



// -------------------------------------------------------------------------

// Initialize a global_ranger.

global_ranger::global_ranger () : ssa_range (true)
{
  m_block_cache = new block_range_cache ();
  m_globals = new ssa_global_cache ();
  m_non_null = new non_null_ref ();
}

// Deallocate global_ranger members.

global_ranger::~global_ranger () 
{
  delete m_block_cache;
  delete m_globals;
  delete m_non_null;
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

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back thourgh the CFG to DEF_BB ensuring the range is calculated 
// on the block/edges leading back to that point.

void
global_ranger::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  irange er;
  irange block_result;

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == def_bb)
    return;

  // If the block cache is set, then we've already visited this block.
  if (m_block_cache->bb_range_p (name, bb))
    return;

  // Avoid infinite recursion by marking this block as calculated now.
  m_block_cache->set_bb_varying (name, bb);

  // Visit each predecessor to resolve them.
  FOR_EACH_EDGE (e, ei, bb->preds)
    fill_block_cache (name, e->src, def_bb);

  // Start with an empty range.
  block_result.set_undefined (TREE_TYPE (name));
  // And union all the ranges on the incoming edges.
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      irange pred_range;
      basic_block src = e->src;
      // If this is the defintion BB, force evaluation of the intial value.
      if (src == def_bb)
        get_global_ssa_range (pred_range, name);
      else
        {
	  bool res;
	  // We know this has been evaluated, just get the range on entry to 
	  // the predecessor block.
	  res = m_block_cache->get_bb_range (pred_range, name, src);
	  gcc_assert (res);
	}

      // If there is no range yet, and the previous block has a pointer
      // dereference, adjust the range.
      if (pred_range.varying_p ())
        non_null_deref_in_block (pred_range, name, src);

      // If the predecessor block refines the range on the incoming edge,
      // apply it to the range on entry value from that block.
      if (outgoing_edge_range_p (er, e, name))
        {
	  pred_range.intersect (er);
	  er.intersect (pred_range);
	}

      // Union the range on that edge with our accumulating range on all edges.
      block_result.union_ (pred_range);

      // If we get to varying, we can stop looking.
      if (block_result.varying_p ())
        break;
    }

  if (block_result.varying_p ())
    m_block_cache->set_bb_varying (name, bb);
  else
    m_block_cache->set_bb_range (name, bb, block_result);
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

  // Start with any known range. 
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


// Calculate a range for statement S and return it in R.  Check the globhal
// cache first to see if the evaluation can be avoided.  If a range cannot
// be calculated, return false.

bool
global_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  // If no name, simply call the base routine.
  if (!name)
    return ssa_range::range_of_stmt (r, s, name);

  gcc_checking_assert (supports_ssa_p (name));

  // If this STMT has already been processed, return that value. 
  if (has_global_ssa_range (r, name))
    return true;
 
  // Avoid infinite recursion by initializing global cache
  irange tmp;
  tmp = range_from_ssa (name);
  set_global_ssa_range (name, tmp);

  gcc_assert (ssa_range::range_of_stmt (r, s, name));

  if (is_a<gphi *> (s))
    r.intersect (tmp);
  set_global_ssa_range (name, r);
  return true;
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
	// Otherwise OP comes from outside this block, try range on entry.
	gcc_assert (range_on_entry (r, bb, op));

      // No range yet, see if there is a dereference in the block.
      // We don't care if it's between the def and a use within a block
      // because the entire block must be executed anyway.
      // FIXME:?? For non-call exceptions we could have a statement throw
      // which causes an early block exit. 
      // in which case we may need to walk from S back to the def/top of block
      // to make sure the deref happens between S and there before claiming 
      // there is a deref.   
      if (r.varying_p ())
	non_null_deref_in_block (r, op, bb);
      return true;
    }

  // Fall back to the default.
  return ssa_range::range_of_expr (r, op);
}


// Print the known table values to file F.

void
global_ranger::dump(FILE *f)
{
  ssa_range::dump (f);
  dump_global_ssa_range (f);
}


// Exercise the ranger by visiting every block and asking for the range of 
// every ssa_name on each outgoing edge, and optionally providing a dump
// of everything than can be calculated by the ranger to file OUTPUT.

void
global_ranger::exercise (FILE *output)
{

  basic_block bb;
  irange range;

  FOR_EACH_BB_FN (bb, cfun)
    {
      unsigned x;
      edge_iterator ei;
      edge e;
      bool printed = false;

      // This dramatically slows down builds, so only when printing. 
      if (output)
        {
	  fprintf (output, "========= BB%d =========\n", bb->index);
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && range_on_entry (range, bb, name))
		{
		  if (output && !range.varying_p ())
		    {
		      // avoid printing global values that are just global
		      irange gl;
		      if (has_global_ssa_range (gl, name) && gl == range)
		        continue;
			  
		      if (!printed)
			fprintf (output,"   Ranges on Entry :\n");
		      printed = true;
		      fprintf (output, "     ");
		      print_generic_expr (output, name, TDF_NONE);
		      fprintf (output, " : ");
		      range.dump (output);
		    }
		}
	    }
	  if (printed)
	    fprintf (output, "\n");
	  dump_bb (output, bb, 2, TDF_NONE);
	  printed = false;
	}

      // Run through the block once to visit each value.
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && range_p (bb, name))
		  range_on_edge (range, e, name);
	    }
	}
      // Now calculate any globals in this block if we asked for details
      if (dump_file && (dump_flags & TDF_DETAILS))
	for (x = 1; x < num_ssa_names; x++)
	  {
	    tree name = ssa_name (x);
	    if (name && TREE_CODE (name) == SSA_NAME 
		&& !SSA_NAME_IS_VIRTUAL_OPERAND (name)
		&& SSA_NAME_DEF_STMT (name) &&
		gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
		!get_global_ssa_range (range, name))
	      range_of_stmt (range, SSA_NAME_DEF_STMT (name), name);
	  }

      if (output)
        {
	  // Dump any globals defined.
	  printed = false;
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      if (ssa_name(x) && has_global_ssa_range (range, ssa_name (x))
		  && !range.varying_p ())
	        {
		  gimple *s = SSA_NAME_DEF_STMT (ssa_name (x));
		  if (s && gimple_bb (s) == bb)
		    {
		      if (!printed)
			fprintf (output, "     -- Ranges Defined -- :\n");
		      fprintf (output, "     ");
		      print_generic_expr (output, ssa_name (x), TDF_NONE);
		      fprintf (output, "  : ");
		      range.dump (output);
		      printed = true;
		    }
		}
	      else
		// Check for a pointer and it is dereferences in this block
		// and it had no range coming in.
		if (ssa_name (x)
		    && m_non_null->non_null_deref_p (ssa_name(x), bb)
		    && !range_on_entry (range, bb, ssa_name (x)))
		  {
		    if (!printed)
		      fprintf (output, "     -- Ranges Defined -- :\n");
		    fprintf (output, "     ");
		    print_generic_expr (output, ssa_name (x), TDF_NONE);
		    fprintf (output, "  : non-null due to deref in block\n");
		    printed = true;
		  }
	    }
	  if (printed)
	    fprintf (output, "\n");

	  // Now print any edge values.
          printed = false;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      for (x = 1; x < num_ssa_names; x++)
		{
		  tree name = ssa_name (x);
		  if (name && range_p (bb, name))
		    {
		      if (range_on_edge (range, e, name))
			{
			  if (!range.varying_p ())
			    {
			      printed = true;
			      fprintf (output, "     %d->%d ", e->src->index,
				       e->dest->index);
			      if (e->flags & EDGE_TRUE_VALUE)
				fprintf (output, " (T) ");
			      else if (e->flags & EDGE_FALSE_VALUE)
				fprintf (output, " (F) ");
			      else
				fprintf (output, "     ");
			      print_generic_expr (output, name, TDF_SLIM);
			      fprintf(output, "  \t");
			      range.dump(output);
			    }
			}
		    }
		}
	    }
	  if (printed)
	    fprintf (output, "\n");
	}
    }

  if (output && (dump_flags & TDF_DETAILS))
    dump (output);

}
