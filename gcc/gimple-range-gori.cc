/* Gimple range GORI functions.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-range.h"

// Return TRUE if GS is a logical && or || expression.

static inline bool
is_gimple_logical_p (const gimple *gs)
{
  // Look for boolean and/or condition.
  if (is_gimple_assign (gs))
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

/* RANGE_DEF_CHAIN is used to determine what SSA names in a block can
   have range information calculated for them, and what the
   dependencies on each other are.

   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made, so if no queries
   are made, there is little overhead.

   The def_chain bitmap is indexed by SSA_NAME_VERSION.  Bits are set
   within this bitmap to indicate SSA names that are defined in the
   SAME block and used to calculate this SSA name.


    <bb 2> :
      _1 = x_4(D) + -2;
      _2 = _1 * 4;
      j_7 = foo ();
      q_5 = _2 + 3;
      if (q_5 <= 13)

    _1  : x_4(D)
    _2  : 1  x_4(D)
    q_5  : _1  _2  x_4(D)

    This dump indicates the bits set in the def_chain vector.
    as well as demonstrates the def_chain bits for the related ssa_names.

    Checking the chain for _2 indicates that _1 and x_4 are used in
    its evaluation.

    Def chains also only include statements which are valid gimple
    so a def chain will only span statements for which the range
    engine implements operations for.  */


class range_def_chain
{
public:
  range_def_chain ();
  ~range_def_chain ();
  bool has_def_chain (tree name);
  bitmap get_def_chain (tree name);
  bool in_chain_p (tree name, tree def);
private:
  vec<bitmap> m_def_chain;	// SSA_NAME : def chain components.
  void build_def_chain (tree name, bitmap result, basic_block bb);
  int m_logical_depth;
};


// Construct a range_def_chain.

range_def_chain::range_def_chain ()
{
  m_def_chain.create (0);
  m_def_chain.safe_grow_cleared (num_ssa_names);
  m_logical_depth = 0;
}

// Destruct a range_def_chain.

range_def_chain::~range_def_chain ()
{
  unsigned x;
  for (x = 0; x < m_def_chain.length (); ++x)
    if (m_def_chain[x])
      BITMAP_FREE (m_def_chain[x]);
  m_def_chain.release ();
}

// Return true if NAME is in the def chain of DEF.  If BB is provided,
// only return true if the defining statement of DEF is in BB.

bool
range_def_chain::in_chain_p (tree name, tree def)
{
  gcc_checking_assert (gimple_range_ssa_p (def));
  gcc_checking_assert (gimple_range_ssa_p (name));

  // Get the defintion chain for DEF.
  bitmap chain = get_def_chain (def);

  if (chain == NULL)
    return false;
  return bitmap_bit_p (chain, SSA_NAME_VERSION (name));
}

// Build def_chains for NAME if it is in BB.  Copy the def chain into RESULT.

void
range_def_chain::build_def_chain (tree name, bitmap result, basic_block bb)
{
  bitmap b;
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  // Add this operand into the result.
  bitmap_set_bit (result, SSA_NAME_VERSION (name));

  if (gimple_bb (def_stmt) == bb && !is_a<gphi *>(def_stmt))
    {
      // Get the def chain for the operand.
      b = get_def_chain (name);
      // If there was one, copy it into result.
      if (b)
	bitmap_ior_into (result, b);
    }
}

// Return TRUE if NAME has been processed for a def_chain.

inline bool
range_def_chain::has_def_chain (tree name)
{
  // Ensure there is an entry in the internal vector.
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_def_chain.length ())
    m_def_chain.safe_grow_cleared (num_ssa_names + 1);
  return (m_def_chain[v] != NULL);
}

// Calculate the def chain for NAME and all of its dependent
// operands. Only using names in the same BB.  Return the bitmap of
// all names in the m_def_chain.  This only works for supported range
// statements.

bitmap
range_def_chain::get_def_chain (tree name)
{
  tree ssa1, ssa2, ssa3;
  unsigned v = SSA_NAME_VERSION (name);
  bool is_logical = false;

  // If it has already been processed, just return the cached value.
  if (has_def_chain (name))
    return m_def_chain[v];

  // No definition chain for default defs.
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    return NULL;

  gimple *stmt = SSA_NAME_DEF_STMT (name);
  if (gimple_range_handler (stmt))
    {
      is_logical = is_gimple_logical_p (stmt);
      // Terminate the def chains if we see too many cascading logical stmts.
      if (is_logical)
	{
	  if (m_logical_depth == param_ranger_logical_depth)
	    return NULL;
	  m_logical_depth++;
	}

      ssa1 = gimple_range_ssa_p (gimple_range_operand1 (stmt));
      ssa2 = gimple_range_ssa_p (gimple_range_operand2 (stmt));
      ssa3 = NULL_TREE;
    }
  else if (is_a<gassign *> (stmt)
	   && gimple_assign_rhs_code (stmt) == COND_EXPR)
    {
      gassign *st = as_a<gassign *> (stmt);
      ssa1 = gimple_range_ssa_p (gimple_assign_rhs1 (st));
      ssa2 = gimple_range_ssa_p (gimple_assign_rhs2 (st));
      ssa3 = gimple_range_ssa_p (gimple_assign_rhs3 (st));
    }
  else
    return NULL;

  basic_block bb = gimple_bb (stmt);

  m_def_chain[v] = BITMAP_ALLOC (NULL);

  if (ssa1)
    build_def_chain (ssa1, m_def_chain[v], bb);
  if (ssa2)
    build_def_chain (ssa2, m_def_chain[v], bb);
  if (ssa3)
    build_def_chain (ssa3, m_def_chain[v], bb);

  if (is_logical)
    m_logical_depth--;

  // If we run into pathological cases where the defintion chains are
  // huge (ie  huge basic block fully unrolled) we might be able to limit
  // this by deciding here that if some criteria is satisfied, we change the
  // def_chain back to be just the ssa-names.  That will help prevent chains
  // of a_2 = b_6 + a_8 from creating a pathological case.
  return m_def_chain[v];
}

// -------------------------------------------------------------------

/* GORI_MAP is used to accumulate what SSA names in a block can
   generate range information, and provides tools for the block ranger
   to enable it to efficiently calculate these ranges.

   GORI stands for "Generates Outgoing Range Information."

   It utilizes the range_def_chain class to contruct def_chains.
   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made.  If no queries are
   made, there is little overhead.

   one bitmap is maintained for each basic block:
   m_outgoing  : a set bit indicates a range can be generated for a name.

   Generally speaking, the m_outgoing vector is the union of the
   entire def_chain of all SSA names used in the last statement of the
   block which generate ranges.  */

class gori_map : public range_def_chain
{
public:
  gori_map ();
  ~gori_map ();

  bool is_export_p (tree name, basic_block bb = NULL);
  bool def_chain_in_export_p (tree name, basic_block bb);
  bitmap exports (basic_block bb);
  void set_range_invariant (tree name);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb);
private:
  bitmap_obstack m_bitmaps;
  vec<bitmap> m_outgoing;	// BB: Outgoing ranges calculatable on edges
  bitmap m_maybe_variant;	// Names which might have outgoing ranges.
  void maybe_add_gori (tree name, basic_block bb);
  void calculate_gori (basic_block bb);
};


// Initialize a gori-map structure.

gori_map::gori_map ()
{
  m_outgoing.create (0);
  m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  bitmap_obstack_initialize (&m_bitmaps);
  m_maybe_variant = BITMAP_ALLOC (&m_bitmaps);
}

// Free any memory the GORI map allocated.

gori_map::~gori_map ()
{
  bitmap_obstack_release (&m_bitmaps);
  m_outgoing.release ();
}

// Return the bitmap vector of all export from BB.  Calculate if necessary.

bitmap
gori_map::exports (basic_block bb)
{
  if (!m_outgoing[bb->index])
    calculate_gori (bb);
  return m_outgoing[bb->index];
}

// Return true if NAME is can have ranges generated for it from basic
// block BB.

bool
gori_map::is_export_p (tree name, basic_block bb)
{
  // If no BB is specified, test if it is exported anywhere in the IL.
  if (!bb)
    return bitmap_bit_p (m_maybe_variant, SSA_NAME_VERSION (name));
  return bitmap_bit_p (exports (bb), SSA_NAME_VERSION (name));
}

// Clear the m_maybe_variant bit so ranges will not be tracked for NAME.

void
gori_map::set_range_invariant (tree name)
{
  bitmap_clear_bit (m_maybe_variant, SSA_NAME_VERSION (name));
}

// Return true if any element in the def chain of NAME is in the
// export list for BB.

bool
gori_map::def_chain_in_export_p (tree name, basic_block bb)
{
  bitmap a = exports (bb);
  bitmap b = get_def_chain (name);
  if (a && b)
    return bitmap_intersect_p (a, b);
  return false;
}

// If NAME is non-NULL and defined in block BB, calculate the def
// chain and add it to m_outgoing.

void
gori_map::maybe_add_gori (tree name, basic_block bb)
{
  if (name)
    {
      gimple *s = SSA_NAME_DEF_STMT (name);
      bitmap r = get_def_chain (name);
      // Check if there is a def chain, and it is in this block.
      if (r && gimple_bb (s) == bb)
	bitmap_copy (m_outgoing[bb->index], r);
      // Def chain doesn't include itself, and even if there isn't a
      // def chain, this name should be added to exports.
      bitmap_set_bit (m_outgoing[bb->index], SSA_NAME_VERSION (name));
    }
}

// Calculate all the required information for BB.

void
gori_map::calculate_gori (basic_block bb)
{
  tree name;
  if (bb->index >= (signed int)m_outgoing.length ())
    m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  gcc_checking_assert (m_outgoing[bb->index] == NULL);
  m_outgoing[bb->index] = BITMAP_ALLOC (&m_bitmaps);

  // If this block's last statement may generate range informaiton, go
  // calculate it.
  gimple *stmt = gimple_outgoing_range_stmt_p (bb);
  if (!stmt)
    return;
  if (is_a<gcond *> (stmt))
    {
      gcond *gc = as_a<gcond *>(stmt);
      name = gimple_range_ssa_p (gimple_cond_lhs (gc));
      maybe_add_gori (name, gimple_bb (stmt));

      name = gimple_range_ssa_p (gimple_cond_rhs (gc));
      maybe_add_gori (name, gimple_bb (stmt));
    }
  else
    {
      gswitch *gs = as_a<gswitch *>(stmt);
      name = gimple_range_ssa_p (gimple_switch_index (gs));
      maybe_add_gori (name, gimple_bb (stmt));
    }
  // Add this bitmap to the aggregate list of all outgoing names.
  bitmap_ior_into (m_maybe_variant, m_outgoing[bb->index]);
}

// Dump the table information for BB to file F.

void
gori_map::dump (FILE *f, basic_block bb)
{
  bool header = false;
  const char *header_string = "bb%-4d ";
  const char *header2 = "       ";
  bool printed_something = false;;
  unsigned x, y;
  bitmap_iterator bi;

  // BB was not processed.
  if (!m_outgoing[bb->index])
    return;

  // Dump the def chain for each SSA_NAME defined in BB.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (!name)
	continue;
      gimple *stmt = SSA_NAME_DEF_STMT (name);
      bitmap chain = (has_def_chain (name) ? get_def_chain (name) : NULL);
      if (stmt && gimple_bb (stmt) == bb && chain && !bitmap_empty_p (chain))
        {
	  fprintf (f, header_string, bb->index);
	  header_string = header2;
	  header = true;
	  print_generic_expr (f, name, TDF_SLIM);
	  fprintf (f, " : ");
	  EXECUTE_IF_SET_IN_BITMAP (chain, 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }

  printed_something |= header;

  // Now dump the export vector.
  header = false;
  EXECUTE_IF_SET_IN_BITMAP (m_outgoing[bb->index], 0, y, bi)
    {
      if (!header)
        {
	  fprintf (f, header_string, bb->index);
	  fprintf (f, "exports: ");
	  header_string = header2;
	  header = true;
	}
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }
  if (header)
    fputc ('\n', f);

  printed_something |= header;
  if (printed_something)
    fprintf (f, "\n");
}

// Dump the entire GORI map structure to file F.

void
gori_map::dump (FILE *f)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      dump (f, bb);
      if (m_outgoing[bb->index])
	fprintf (f, "\n");
    }
}

DEBUG_FUNCTION void
debug (gori_map &g)
{
  g.dump (stderr);
}

// -------------------------------------------------------------------

// Construct a gori_compute object.

gori_compute::gori_compute ()
{
  // Create a boolean_type true and false range.
  m_bool_zero = int_range<2> (boolean_false_node, boolean_false_node);
  m_bool_one = int_range<2> (boolean_true_node, boolean_true_node);
  m_gori_map = new gori_map;
  unsigned x, lim = last_basic_block_for_fn (cfun);
  // Calculate outgoing range info upfront.  This will fully populate the
  // m_maybe_variant bitmap which will help eliminate processing of names
  // which never have their ranges adjusted.
  for (x = 0; x < lim ; x++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, x);
      if (bb)
	m_gori_map->exports (bb);
    }
}

// Destruct a gori_compute_object.

gori_compute::~gori_compute ()
{
  delete m_gori_map;
}

// Provide a default of VARYING for all incoming SSA names.

void
gori_compute::ssa_range_in_bb (irange &r, tree name, basic_block)
{
  r.set_varying (TREE_TYPE (name));
}

void
gori_compute::expr_range_in_bb (irange &r, tree expr, basic_block bb)
{
  if (gimple_range_ssa_p (expr))
    ssa_range_in_bb (r, expr, bb);
  else
    get_tree_range (r, expr);
}

// Calculate the range for NAME if the lhs of statement S has the
// range LHS.  Return the result in R.  Return false if no range can be
// calculated.

bool
gori_compute::compute_name_range_op (irange &r, gimple *stmt,
				     const irange &lhs, tree name)
{
  int_range_max op1_range, op2_range;

  tree op1 = gimple_range_operand1 (stmt);
  tree op2 = gimple_range_operand2 (stmt);

  // Operand 1 is the name being looked for, evaluate it.
  if (op1 == name)
    {
      expr_range_in_bb (op1_range, op1, gimple_bb (stmt));
      if (!op2)
	{
	  // The second parameter to a unary operation is the range
	  // for the type of operand1, but if it can be reduced
	  // further, the results will be better.  Start with what we
	  // know of the range of OP1 instead of the full type.
	  return gimple_range_calc_op1 (r, stmt, lhs, op1_range);
	}
      // If we need the second operand, get a value and evaluate.
      expr_range_in_bb (op2_range, op2, gimple_bb (stmt));
      if (gimple_range_calc_op1 (r, stmt, lhs, op2_range))
	r.intersect (op1_range);
      else
        r = op1_range;
      return true;
    }

  if (op2 == name)
    {
      expr_range_in_bb (op1_range, op1, gimple_bb (stmt));
      expr_range_in_bb (r, op2, gimple_bb (stmt));
      if (gimple_range_calc_op2 (op2_range, stmt, lhs, op1_range))
        r.intersect (op2_range);
      return true;
    }
  return false;
}

// Given the switch S, return an evaluation in R for NAME when the lhs
// evaluates to LHS.  Returning false means the name being looked for
// was not resolvable.

bool
gori_compute::compute_operand_range_switch (irange &r, gswitch *s,
					    const irange &lhs,
					    tree name)
{
  tree op1 = gimple_switch_index (s);

  // If name matches, the range is simply the range from the edge.
  // Empty ranges are viral as they are on a path which isn't
  // executable.
  if (op1 == name || lhs.undefined_p ())
    {
      r = lhs;
      return true;
    }

  // If op1 is in the defintion chain, pass lhs back.
  if (gimple_range_ssa_p (op1) && m_gori_map->in_chain_p (name, op1))
    return compute_operand_range (r, SSA_NAME_DEF_STMT (op1), lhs, name);

  return false;
}


// Return an evaluation for NAME as it would appear in STMT when the
// statement's lhs evaluates to LHS.  If successful, return TRUE and
// store the evaluation in R, otherwise return FALSE.

bool
gori_compute::compute_operand_range (irange &r, gimple *stmt,
				     const irange &lhs, tree name)
{
  // Empty ranges are viral as they are on an unexecutable path.
  if (lhs.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  if (is_a<gswitch *> (stmt))
    return compute_operand_range_switch (r, as_a<gswitch *> (stmt), lhs, name);
  if (!gimple_range_handler (stmt))
    return false;

  tree op1 = gimple_range_ssa_p (gimple_range_operand1 (stmt));
  tree op2 = gimple_range_ssa_p (gimple_range_operand2 (stmt));

  // The base ranger handles NAME on this statement.
  if (op1 == name || op2 == name)
    return compute_name_range_op (r, stmt, lhs, name);

  if (is_gimple_logical_p (stmt))
    return compute_logical_operands (r, stmt, lhs, name);

  // NAME is not in this stmt, but one of the names in it ought to be
  // derived from it.
  bool op1_in_chain = op1 && m_gori_map->in_chain_p (name, op1);
  bool op2_in_chain = op2 && m_gori_map->in_chain_p (name, op2);
  if (op1_in_chain && op2_in_chain)
    return compute_operand1_and_operand2_range (r, stmt, lhs, name);
  if (op1_in_chain)
    return compute_operand1_range (r, stmt, lhs, name);
  if (op2_in_chain)
    return compute_operand2_range (r, stmt, lhs, name);

  // If neither operand is derived, this statement tells us nothing.
  return false;
}

// Return TRUE if range R is either a true or false compatible range.

static bool
range_is_either_true_or_false (const irange &r)
{
  if (r.undefined_p ())
    return false;

  // This is complicated by the fact that Ada has multi-bit booleans,
  // so true can be ~[0, 0] (i.e. [1,MAX]).
  tree type = r.type ();
  gcc_checking_assert (range_compatible_p (type, boolean_type_node));
  return (r.singleton_p () || !r.contains_p (build_zero_cst (type)));
}

// A pair of ranges for true/false paths.

struct tf_range
{
  tf_range () { }
  tf_range (const irange &t_range, const irange &f_range)
  {
    true_range = t_range;
    false_range = f_range;
  }
  int_range_max true_range, false_range;
};

// Evaluate a binary logical expression by combining the true and
// false ranges for each of the operands based on the result value in
// the LHS.

bool
gori_compute::logical_combine (irange &r, enum tree_code code,
			       const irange &lhs,
			       const tf_range &op1, const tf_range &op2)
{
  if (op1.true_range.varying_p ()
      && op1.false_range.varying_p ()
      && op2.true_range.varying_p ()
      && op2.false_range.varying_p ())
    return false;

  // This is not a simple fold of a logical expression, rather it
  // determines ranges which flow through the logical expression.
  //
  // Assuming x_8 is an unsigned char, and relational statements:
  //	      b_1 = x_8 < 20
  //	      b_2 = x_8 > 5
  // consider the logical expression and branch:
  //          c_2 = b_1 && b_2
  //          if (c_2)
  //
  // To determine the range of x_8 on either edge of the branch, one
  // must first determine what the range of x_8 is when the boolean
  // values of b_1 and b_2 are both true and false.
  //    b_1 TRUE      x_8 = [0, 19]
  //    b_1 FALSE     x_8 = [20, 255]
  //    b_2 TRUE      x_8 = [6, 255]
  //    b_2 FALSE     x_8 = [0,5].
  //
  // These ranges are then combined based on the expected outcome of
  // the branch.  The range on the TRUE side of the branch must satisfy
  //     b_1 == true && b_2 == true
  //
  // In terms of x_8, that means both x_8 == [0, 19] and x_8 = [6, 255]
  // must be true.  The range of x_8 on the true side must be the
  // intersection of both ranges since both must be true.  Thus the
  // range of x_8 on the true side is [6, 19].
  //
  // To determine the ranges on the FALSE side, all 3 combinations of
  // failing ranges must be considered, and combined as any of them
  // can cause the false result.
  //
  // If the LHS can be TRUE or FALSE, then evaluate both a TRUE and
  // FALSE results and combine them.  If we fell back to VARYING any
  // range restrictions that have been discovered up to this point
  // would be lost.
  if (!range_is_either_true_or_false (lhs))
    {
      int_range_max r1;
      if (logical_combine (r1, code, m_bool_zero, op1, op2)
	  && logical_combine (r, code, m_bool_one, op1, op2))
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
	  {
	    // The TRUE side is the intersection of the the 2 true ranges.
	    r = op1.true_range;
	    r.intersect (op2.true_range);
	  }
	else
	  {
	    // The FALSE side is the union of the other 3 cases.
	    int_range_max ff (op1.false_range);
	    ff.intersect (op2.false_range);
	    int_range_max tf (op1.true_range);
	    tf.intersect (op2.false_range);
	    int_range_max ft (op1.false_range);
	    ft.intersect (op2.true_range);
	    r = ff;
	    r.union_ (tf);
	    r.union_ (ft);
	  }
        break;
      //  A logical OR combines ranges from 2 boolean conditons.
      // 	c_2 = b_1 || b_2
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  {
	    // An OR operation will only take the FALSE path if both
	    // operands are false simlulateously, which means they should
	    // be intersected.  !(x || y) == !x && !y
	    r = op1.false_range;
	    r.intersect (op2.false_range);
	  }
	else
	  {
	    // The TRUE side of an OR operation will be the union of
	    // the other three combinations.
	    int_range_max tt (op1.true_range);
	    tt.intersect (op2.true_range);
	    int_range_max tf (op1.true_range);
	    tf.intersect (op2.false_range);
	    int_range_max ft (op1.false_range);
	    ft.intersect (op2.true_range);
	    r = tt;
	    r.union_ (tf);
	    r.union_ (ft);
	  }
	break;
      default:
        gcc_unreachable ();
    }

  return true;
}

// Helper function for compute_logical_operands_in_chain that computes
// the range of logical statements that can be computed without
// chasing down operands.  These are things like [0 = x | y] where we
// know neither operand can be non-zero, or [1 = x & y] where we know
// neither operand can be zero.

bool
gori_compute::optimize_logical_operands (tf_range &range,
					 gimple *stmt,
					 const irange &lhs,
					 tree name,
					 tree op)
{
  enum tree_code code = gimple_expr_code (stmt);

  // Optimize [0 = x | y], since neither operand can ever be non-zero.
  if ((code == BIT_IOR_EXPR || code == TRUTH_OR_EXPR) && lhs.zero_p ())
    {
      if (!compute_operand_range (range.false_range, SSA_NAME_DEF_STMT (op),
				  m_bool_zero, name))
	expr_range_in_bb (range.false_range, name, gimple_bb (stmt));
      range.true_range = range.false_range;
      return true;
    }
  // Optimize [1 = x & y], since neither operand can ever be zero.
  if ((code == BIT_AND_EXPR || code == TRUTH_AND_EXPR) && lhs == m_bool_one)
    {
      if (!compute_operand_range (range.true_range, SSA_NAME_DEF_STMT (op),
				  m_bool_one, name))
	expr_range_in_bb (range.true_range, name, gimple_bb (stmt));
      range.false_range = range.true_range;
      return true;
    }
  return false;
}

// Given a logical STMT, calculate true and false ranges for each
// potential path of NAME, assuming NAME came through the OP chain if
// OP_IN_CHAIN is true.

void
gori_compute::compute_logical_operands_in_chain (tf_range &range,
						 gimple *stmt,
						 const irange &lhs,
						 tree name,
						 tree op, bool op_in_chain)
{
  gimple *src_stmt = gimple_range_ssa_p (op) ? SSA_NAME_DEF_STMT (op) : NULL;
  basic_block bb = gimple_bb (stmt);
  if (!op_in_chain || (src_stmt != NULL && bb != gimple_bb (src_stmt)))
    {
      // If op is not in the def chain, or defined in this block,
      // use its known value on entry to the block.
      expr_range_in_bb (range.true_range, name, gimple_bb (stmt));
      range.false_range = range.true_range;
      return;
    }
  if (optimize_logical_operands (range, stmt, lhs, name, op))
    return;

  // Calculate ranges for true and false on both sides, since the false
  // path is not always a simple inversion of the true side.
  if (!compute_operand_range (range.true_range, src_stmt, m_bool_one, name))
    expr_range_in_bb (range.true_range, name, bb);
  if (!compute_operand_range (range.false_range, src_stmt, m_bool_zero, name))
    expr_range_in_bb (range.false_range, name, bb);
}

// Given a logical STMT, calculate true and false for each potential
// path using NAME, and resolve the outcome based on the logical
// operator.

bool
gori_compute::compute_logical_operands (irange &r, gimple *stmt,
					const irange &lhs,
					tree name)
{
  // Reaching this point means NAME is not in this stmt, but one of
  // the names in it ought to be derived from it.
  tree op1 = gimple_range_operand1 (stmt);
  tree op2 = gimple_range_operand2 (stmt);
  gcc_checking_assert (op1 != name && op2 != name);

  bool op1_in_chain = (gimple_range_ssa_p (op1)
		       && m_gori_map->in_chain_p (name, op1));
  bool op2_in_chain = (gimple_range_ssa_p (op2)
		       && m_gori_map->in_chain_p (name, op2));

  // If neither operand is derived, then this stmt tells us nothing.
  if (!op1_in_chain && !op2_in_chain)
    return false;

  tf_range op1_range, op2_range;
  compute_logical_operands_in_chain (op1_range, stmt, lhs,
				     name, op1, op1_in_chain);
  compute_logical_operands_in_chain (op2_range, stmt, lhs,
				     name, op2, op2_in_chain);
  return logical_combine (r, gimple_expr_code (stmt), lhs,
			  op1_range, op2_range);
}

// Calculate a range for NAME from the operand 1 position of STMT
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand1_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name)
{
  int_range_max op1_range, op2_range;
  tree op1 = gimple_range_operand1 (stmt);
  tree op2 = gimple_range_operand2 (stmt);

  expr_range_in_bb (op1_range, op1, gimple_bb (stmt));

  // Now calcuated the operand and put that result in r.
  if (op2)
    {
      expr_range_in_bb (op2_range, op2, gimple_bb (stmt));
      if (!gimple_range_calc_op1 (r, stmt, lhs, op2_range))
	return false;
    }
  else
    {
      // We pass op1_range to the unary operation.  Nomally it's a
      // hidden range_for_type parameter, but sometimes having the
      // actual range can result in better information.
      if (!gimple_range_calc_op1 (r, stmt, lhs, op1_range))
	return false;
    }

  // Intersect the calculated result with the known result.
  op1_range.intersect (r);

  gimple *src_stmt = SSA_NAME_DEF_STMT (op1);
  // If def stmt is outside of this BB, then name must be an import.
  if (!src_stmt || (gimple_bb (src_stmt) != gimple_bb (stmt)))
    {
      // If this isn't the right import statement, then abort calculation.
      if (!src_stmt || gimple_get_lhs (src_stmt) != name)
        return false;
      return compute_name_range_op (r, src_stmt, op1_range, name);
    }
  // Then feed this range back as the LHS of the defining statement.
  return compute_operand_range (r, src_stmt, op1_range, name);
}


// Calculate a range for NAME from the operand 2 position of S
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand2_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name)
{
  int_range_max op1_range, op2_range;
  tree op1 = gimple_range_operand1 (stmt);
  tree op2 = gimple_range_operand2 (stmt);

  expr_range_in_bb (op1_range, op1, gimple_bb (stmt));
  expr_range_in_bb (op2_range, op2, gimple_bb (stmt));

  // Intersect with range for op2 based on lhs and op1.
  if (!gimple_range_calc_op2 (r, stmt, lhs, op1_range))
    return false;
  op2_range.intersect (r);

  gimple *src_stmt = SSA_NAME_DEF_STMT (op2);
  // If def stmt is outside of this BB, then name must be an import.
  if (!src_stmt || (gimple_bb (src_stmt) != gimple_bb (stmt)))
    {
      // If  this isn't the right src statement, then abort calculation.
      if (!src_stmt || gimple_get_lhs (src_stmt) != name)
        return false;
      return compute_name_range_op (r, src_stmt, op2_range, name);
    }
  // Then feed this range back as the LHS of the defining statement.
  return compute_operand_range (r, src_stmt, op2_range, name);
}

// Calculate a range for NAME from both operand positions of S
// assuming the result of the statement is LHS.  Return the range in
// R, or false if no range could be calculated.

bool
gori_compute::compute_operand1_and_operand2_range
					(irange &r,
					 gimple *stmt,
					 const irange &lhs,
					 tree name)
{
  int_range_max op_range;

  // Calculate a good a range for op2.  Since op1 == op2, this will
  // have already included whatever the actual range of name is.
  if (!compute_operand2_range (op_range, stmt, lhs, name))
    return false;

  // Now get the range thru op1.
  if (!compute_operand1_range (r, stmt, lhs, name))
    return false;

  // Whichever range is the most permissive is the one we need to
  // use. (?)  OR is that true?  Maybe this should be intersection?
  r.union_ (op_range);
  return true;
}

// Return TRUE if a range can be calcalated for NAME on edge E.

bool
gori_compute::has_edge_range_p (tree name, edge e)
{
  // If no edge is specified, check if NAME is an export on any edge.
  if (!e)
    return m_gori_map->is_export_p (name);

  return (m_gori_map->is_export_p (name, e->src)
	  || m_gori_map->def_chain_in_export_p (name, e->src));
}

// Clear the m_maybe_variant bit so ranges will not be tracked for NAME.

void
gori_compute::set_range_invariant (tree name)
{
  m_gori_map->set_range_invariant (name);
}

// Dump what is known to GORI computes to listing file F.

void
gori_compute::dump (FILE *f)
{
  m_gori_map->dump (f);
}

// Calculate a range on edge E and return it in R.  Try to evaluate a
// range for NAME on this edge.  Return FALSE if this is either not a
// control edge or NAME is not defined by this edge.

bool
gori_compute::outgoing_edge_range_p (irange &r, edge e, tree name)
{
  int_range_max lhs;

  gcc_checking_assert (gimple_range_ssa_p (name));
  // Determine if there is an outgoing edge.
  gimple *stmt = outgoing.edge_range_p (lhs, e);
  if (!stmt)
    return false;

  // If NAME can be calculated on the edge, use that.
  if (m_gori_map->is_export_p (name, e->src))
    {
      if (compute_operand_range (r, stmt, lhs, name))
	{
	  // Sometimes compatible types get interchanged. See PR97360.
	  // Make sure we are returning the type of the thing we asked for.
	  if (!r.undefined_p () && r.type () != TREE_TYPE (name))
	    {
	      gcc_checking_assert (range_compatible_p (r.type (),
						       TREE_TYPE (name)));
	      range_cast (r, TREE_TYPE (name));
	    }
	  return true;
	}
    }
  return false;
}

// --------------------------------------------------------------------------

// Cache for SSAs that appear on the RHS of a boolean assignment.
//
// Boolean assignments of logical expressions (i.e. LHS = j_5 > 999)
// have SSA operands whose range depend on the LHS of the assigment.
// That is, the range of j_5 when LHS is true is different than when
// LHS is false.
//
// This class caches the TRUE/FALSE ranges of such SSAs to avoid
// recomputing.

class logical_stmt_cache
{
public:
  logical_stmt_cache ();
  ~logical_stmt_cache ();
  void set_range (tree lhs, tree name, const tf_range &);
  bool get_range (tf_range &r, tree lhs, tree name) const;
  bool cacheable_p (gimple *, const irange *lhs_range = NULL) const;
  void dump (FILE *, gimple *stmt) const;
  tree same_cached_name (tree lhs1, tree lh2) const;
private:
  tree cached_name (tree lhs) const;
  void slot_diagnostics (tree lhs, const tf_range &range) const;
  struct cache_entry
  {
    cache_entry (tree name, const irange &t_range, const irange &f_range);
    void dump (FILE *out) const;
    tree name;
    tf_range range;
  };
  vec<cache_entry *> m_ssa_cache;
};

logical_stmt_cache::cache_entry::cache_entry (tree name,
					      const irange &t_range,
					      const irange &f_range)
  : name (name), range (t_range, f_range)
{
}

logical_stmt_cache::logical_stmt_cache ()
{
  m_ssa_cache.create (num_ssa_names + num_ssa_names / 10);
  m_ssa_cache.safe_grow_cleared (num_ssa_names);
}

logical_stmt_cache::~logical_stmt_cache ()
{
  for (unsigned i = 0; i < m_ssa_cache.length (); ++i)
    if (m_ssa_cache[i])
      delete m_ssa_cache[i];
  m_ssa_cache.release ();
}

// Dump cache_entry to OUT.

void
logical_stmt_cache::cache_entry::dump (FILE *out) const
{
  fprintf (out, "name=");
  print_generic_expr (out, name, TDF_SLIM);
  fprintf (out, " ");
  range.true_range.dump (out);
  fprintf (out, ", ");
  range.false_range.dump (out);
  fprintf (out, "\n");
}

// Update range for cache entry of NAME as it appears in the defining
// statement of LHS.

void
logical_stmt_cache::set_range (tree lhs, tree name, const tf_range &range)
{
  unsigned version = SSA_NAME_VERSION (lhs);
  if (version >= m_ssa_cache.length ())
    m_ssa_cache.safe_grow_cleared (num_ssa_names + num_ssa_names / 10);

  cache_entry *slot = m_ssa_cache[version];
  slot_diagnostics (lhs, range);
  if (slot)
    {
      // The IL must have changed.  Update the carried SSA name for
      // consistency.  Testcase is libgomp.fortran/doacross1.f90.
      if (slot->name != name)
	slot->name = name;
      return;
    }
  m_ssa_cache[version]
    = new cache_entry (name, range.true_range, range.false_range);
}

// If there is a cached entry of NAME, set it in R and return TRUE,
// otherwise return FALSE.  LHS is the defining statement where NAME
// appeared.

bool
logical_stmt_cache::get_range (tf_range &r, tree lhs, tree name) const
{
  gcc_checking_assert (cacheable_p (SSA_NAME_DEF_STMT (lhs)));
  if (cached_name (lhs) == name)
    {
      unsigned version = SSA_NAME_VERSION (lhs);
      if (m_ssa_cache[version])
	{
	  r = m_ssa_cache[version]->range;
	  return true;
	}
    }
  return false;
}

// If the defining statement of LHS is in the cache, return the SSA
// operand being cached.  That is, return SSA for LHS = SSA .RELOP. OP2.

tree
logical_stmt_cache::cached_name (tree lhs) const
{
  unsigned version = SSA_NAME_VERSION (lhs);

  if (version >= m_ssa_cache.length ())
    return NULL;

  if (m_ssa_cache[version])
    return m_ssa_cache[version]->name;
  return NULL;
}

// Return TRUE if the cached name for LHS1 is the same as the
// cached name for LHS2.

tree
logical_stmt_cache::same_cached_name (tree lhs1, tree lhs2) const
{
  tree name = cached_name (lhs1);
  if (name && name == cached_name (lhs2))
    return name;
  return NULL;
}

// Return TRUE if STMT is a statement we are interested in caching.
// LHS_RANGE is any known range for the LHS of STMT.

bool
logical_stmt_cache::cacheable_p (gimple *stmt, const irange *lhs_range) const
{
  if (gimple_code (stmt) == GIMPLE_ASSIGN
      && types_compatible_p (TREE_TYPE (gimple_assign_lhs (stmt)),
			     boolean_type_node)
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      switch (gimple_expr_code (stmt))
	{
	case TRUTH_AND_EXPR:
	case BIT_AND_EXPR:
	case TRUTH_OR_EXPR:
	case BIT_IOR_EXPR:
	  return !lhs_range || range_is_either_true_or_false (*lhs_range);
	default:
	  return false;
	}
    }
  return false;
}

// Output debugging diagnostics for the cache entry for LHS.  RANGE is
// the new range that is being cached.

void
logical_stmt_cache::slot_diagnostics (tree lhs, const tf_range &range) const
{
  gimple *stmt = SSA_NAME_DEF_STMT (lhs);
  unsigned version = SSA_NAME_VERSION (lhs);
  cache_entry *slot = m_ssa_cache[version];

  if (!slot)
    {
      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file ? dump_file : stderr, "registering range for: ");
	  dump (dump_file ? dump_file : stderr, stmt);
	}
      return;
    }
  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file ? dump_file : stderr,
	     "reusing range for SSA #%d\n", version);
  if (CHECKING_P && (slot->range.true_range != range.true_range
		     || slot->range.false_range != range.false_range))
    {
      fprintf (stderr, "FATAL: range altered for cached: ");
      dump (stderr, stmt);
      fprintf (stderr, "Attempt to change to:\n");
      fprintf (stderr, "TRUE=");
      range.true_range.dump (stderr);
      fprintf (stderr, ", FALSE=");
      range.false_range.dump (stderr);
      fprintf (stderr, "\n");
      gcc_unreachable ();
    }
}

// Dump the cache information for STMT.

void
logical_stmt_cache::dump (FILE *out, gimple *stmt) const
{
  tree lhs = gimple_assign_lhs (stmt);
  cache_entry *entry = m_ssa_cache[SSA_NAME_VERSION (lhs)];

  print_gimple_stmt (out, stmt, 0, TDF_SLIM);
  if (entry)
    {
      fprintf (out, "\tname = ");
      print_generic_expr (out, entry->name);
      fprintf (out, " lhs(%d)= ", SSA_NAME_VERSION (lhs));
      print_generic_expr (out, lhs);
      fprintf (out, "\n\tTRUE=");
      entry->range.true_range.dump (out);
      fprintf (out, ", FALSE=");
      entry->range.false_range.dump (out);
      fprintf (out, "\n");
    }
  else
    fprintf (out, "[EMPTY]\n");
}

gori_compute_cache::gori_compute_cache ()
{
  m_cache = new logical_stmt_cache;
}

gori_compute_cache::~gori_compute_cache ()
{
  delete m_cache;
}

// Caching version of compute_operand_range.  If NAME, as it appears
// in STMT, has already been cached return it from the cache,
// otherwise compute the operand range as normal and cache it.

bool
gori_compute_cache::compute_operand_range (irange &r, gimple *stmt,
					   const irange &lhs_range, tree name)
{
  bool cacheable = m_cache->cacheable_p (stmt, &lhs_range);
  if (cacheable)
    {
      tree lhs = gimple_assign_lhs (stmt);
      tf_range range;
      if (m_cache->get_range (range, lhs, name))
	{
	  if (lhs_range.zero_p ())
	    r = range.false_range;
	  else
	    r = range.true_range;
	  return true;
	}
    }
  if (super::compute_operand_range (r, stmt, lhs_range, name))
    {
      if (cacheable)
	cache_stmt (stmt);
      return true;
    }
  return false;
}

// Cache STMT if possible.

void
gori_compute_cache::cache_stmt (gimple *stmt)
{
  gcc_checking_assert (m_cache->cacheable_p (stmt));
  enum tree_code code = gimple_expr_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree op1 = gimple_range_operand1 (stmt);
  tree op2 = gimple_range_operand2 (stmt);
  int_range_max r_true_side, r_false_side;

  // LHS = s_5 && 999.
  if (TREE_CODE (op2) == INTEGER_CST)
    {
      range_operator *handler = range_op_handler (code, TREE_TYPE (lhs));
      int_range_max op2_range;
      expr_range_in_bb (op2_range, op2, gimple_bb (stmt));
      tree type = TREE_TYPE (op1);
      handler->op1_range (r_true_side, type, m_bool_one, op2_range);
      handler->op1_range (r_false_side, type, m_bool_zero, op2_range);
      m_cache->set_range (lhs, op1, tf_range (r_true_side, r_false_side));
    }
  // LHS = s_5 && b_8.
  else if (tree cached_name = m_cache->same_cached_name (op1, op2))
    {
      tf_range op1_range, op2_range;
      bool ok = m_cache->get_range (op1_range, op1, cached_name);
      ok = ok && m_cache->get_range (op2_range, op2, cached_name);
      ok = ok && logical_combine (r_true_side, code, m_bool_one,
				  op1_range, op2_range);
      ok = ok && logical_combine (r_false_side, code, m_bool_zero,
				  op1_range, op2_range);
      gcc_checking_assert (ok);
      if (ok)
	m_cache->set_range (lhs, cached_name,
			    tf_range (r_true_side, r_false_side));
    }
}
