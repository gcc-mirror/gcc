/* On-demand ssa range generator for blocks.
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
#include "ssa-range-bb.h"

/* Is the last stmt in a block interesting to look at for range info.  */
static inline gimple *
last_stmt_gori (basic_block bb)
{
  gimple *stmt;

  stmt = last_stmt (bb);
  if (stmt && gimple_code (stmt) == GIMPLE_COND)
    return stmt;
  return NULL;
}
   
/* GORI_MAP is used to determine what ssa-names in a block can generate range
   information, and provides tools for the block ranger to enable it to
   efficiently calculate these ranges.
   GORI stands for "Generates Outgoing Range Information."

   information for a basic block is calculated once and stored. IT is only
   calculated the first time a query is made, so if no queries are made, there
   is little overhead.
   
   2 bitmaps are maintained for each basic block:
   outgoing  : a set bit indicates a range can be generated for a name.
   incoming  : a set bit means a this name come from outside the block and is
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
    
    Def chains also only include statements which are valie range_stmt's. so
    a def chain will only span statements for which the range engine
    implements operations.  */

class gori_map
{
  vec<bitmap> outgoing;		/* BB: Outgoing ranges generated.  */
  vec<bitmap> incoming;		/* BB: ranges coming in.  */
  vec<bitmap> def_chain;	/* SSA_NAME : def chain components. */
  void calculate_gori (basic_block bb);
  bool in_chain_p (unsigned name, unsigned def);
  bitmap imports (basic_block bb);
  bitmap exports (basic_block bb);
  bitmap calc_def_chain (tree name, basic_block bb);
  void process_stmt (range_stmt stmt, bitmap result, basic_block bb);
public:
  gori_map ();
  ~gori_map ();
  bool in_chain_p (tree name, tree def, basic_block bb = NULL);
  bool is_export_p (tree name, basic_block bb);
  bool is_import_p (tree name, basic_block bb);
  tree single_import (tree name);
  void dump (FILE *f);
  void dump (FILE *f, basic_block bb);
};


gori_map::gori_map ()
{
  outgoing.create (0);
  outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  incoming.create (0);
  incoming.safe_grow_cleared (last_basic_block_for_fn (cfun));
  def_chain.create (0);
  def_chain.safe_grow_cleared (num_ssa_names);
}

gori_map::~gori_map ()
{
  unsigned x, bb;
  for (bb = 0; bb < outgoing.length (); ++bb)
    if (outgoing[bb])
      BITMAP_FREE (outgoing[bb]);
  outgoing.release ();

  for (bb = 0; bb < incoming.length (); ++bb)
    if (incoming[bb])
      BITMAP_FREE (incoming[bb]);
  incoming.release ();

  for (x = 0; x < def_chain.length (); ++x)
    if (def_chain[x])
      BITMAP_FREE (def_chain[x]);
  def_chain.release ();
}

// Return the bitmap vector of all imports to BB. Calculate if necessary
bitmap
gori_map::imports (basic_block bb)
{
  if (!incoming[bb->index])
    calculate_gori (bb);
  return incoming[bb->index];
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
  if (!outgoing[bb->index])
    calculate_gori (bb);
  return outgoing[bb->index];
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
gori_map::in_chain_p (tree name, tree def, basic_block bb)
{
  if (TREE_CODE (def) != SSA_NAME || TREE_CODE (name) != SSA_NAME)
    return false;

  unsigned def_index = SSA_NAME_VERSION (def);
  unsigned name_index = SSA_NAME_VERSION (name);
  gimple *stmt;

  // Nothing is in the def chain of a default definition.
  if (SSA_NAME_IS_DEFAULT_DEF (def))
    return false;

  stmt = SSA_NAME_DEF_STMT (def);
  if (bb)
    {
      // If the definition is not in a specified BB, return false.
      if (gimple_bb (stmt) != bb)
        return false;
    }
  else
    bb = gimple_bb (stmt);

  // Calculate gori info for the block if it hasnt been done yet.
  if (outgoing[bb->index] == NULL)
    calculate_gori (bb);

  return in_chain_p (name_index, def_index);
}

// Return true if ssa_name version NAME is set in vector version DEF.
// This assumes all vectors have been created, so NULL means there is 
// nothing in the defintion chain.
bool
gori_map::in_chain_p (unsigned name, unsigned def)
{
  if (def_chain[def] == NULL)
    return false;
  return bitmap_bit_p (def_chain[def], name);
}

// If NAME has a definition chain, and the chain has a single import into
// the block, return the name of that import.
tree
gori_map::single_import (tree name)
{
  tree ret = NULL_TREE;
  unsigned name_index = SSA_NAME_VERSION (name);
  unsigned index;
  basic_block bb;
  bitmap_iterator bi;

  bb = gimple_bb (SSA_NAME_DEF_STMT (name));
  if (bb && !incoming[bb->index])
    calculate_gori (bb);
  if (def_chain [name_index] == NULL)
    return NULL_TREE;

  // Now make sure it is the ONLY import. 
  EXECUTE_IF_AND_IN_BITMAP (def_chain [name_index], incoming[bb->index], 0,
			    index, bi)
    {
      if (!ret)
        ret = ssa_name (index);
      else
	return NULL_TREE;
    }
  return ret;
}

// Process STMT to build def_chains in BB.. Recursively create def_chains
// for any operand contained in STMT, and set the def chain bits in RESULT.
void
gori_map::process_stmt (range_stmt stmt, bitmap result, basic_block bb)
{
  bitmap b;

  if (!stmt.valid ())
    return;

  tree ssa1 = stmt.ssa_operand1 ();
  tree ssa2 = stmt.ssa_operand2 ();

  if (ssa1)
    {
      // Get the def chain for the operand
      b = calc_def_chain (ssa1, bb);
      // If there was one, copy it into result.
      if (b)
	bitmap_copy (result, b);
      // Now add this operand into the result.
      bitmap_set_bit (result, SSA_NAME_VERSION (ssa1));
    }
  // Now do the second operand.
  if (ssa2)
    {
      b = calc_def_chain (ssa2, bb);
      // Have to ior def chain in now since thre are previous results present.
      if (b)
	bitmap_ior_into (result, b);
      bitmap_set_bit (result, SSA_NAME_VERSION (ssa2));
    }
  return;
}


// Calculate the def chain for NAME, but only using names in BB.  Return 
// the bimap of all names in the def_chain
bitmap
gori_map::calc_def_chain (tree name, basic_block bb)
{
  gimple *stmt = SSA_NAME_DEF_STMT (name);
  unsigned v = SSA_NAME_VERSION (name);
  range_stmt rn;

  if (!stmt || gimple_bb (stmt) != bb || is_a <gphi *> (stmt))
    {
      // If its an import, set the bit.
      bitmap_set_bit (incoming[bb->index], v);
      return NULL;
    }
  // If it has already been processed, just return the cached value.
  if (def_chain[v])
    return def_chain[v];

  // Allocate a new bitmap and initialize it.
  def_chain[v] = BITMAP_ALLOC (NULL);
  process_stmt (stmt, def_chain[v], bb);

  return def_chain[v];
}

// Calculate all the required information for BB.
void
gori_map::calculate_gori (basic_block bb)
{
  gimple *stmt;
  gcc_assert (outgoing[bb->index] == NULL);
  outgoing[bb->index] = BITMAP_ALLOC (NULL);
  incoming[bb->index] = BITMAP_ALLOC (NULL);

  // If this block's last statement may generate range informaiton, 
  // go calculate it.
  stmt = last_stmt_gori (bb);
  if (stmt)
    process_stmt (stmt, outgoing[bb->index], bb);
}

// Dump the table information for BB to file F.
void
gori_map::dump(FILE *f, basic_block bb)
{
  tree t;
  unsigned x, y;
  bitmap_iterator bi;

  if (!outgoing[bb->index])
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
      if (stmt && gimple_bb (stmt) == bb && def_chain[x] &&
	  !bitmap_empty_p (def_chain[x]))
        {
	  print_generic_expr (f, name, TDF_SLIM);
	  if ((t = single_import (name)))
	    {
	      fprintf (f, "  : (single import : ");
	      print_generic_expr (f, t, TDF_SLIM);
	      fprintf (f, ")");
	    }
	  fprintf (f, "  :");
	  EXECUTE_IF_SET_IN_BITMAP (def_chain[x], 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }

  // Now dump the incoming vector.
  fprintf (f, "BB%d imports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (incoming[bb->index], 0, y, bi)
    {
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }

  // Now dump the export vector.
  fprintf (f, "\nBB%d exports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (outgoing[bb->index], 0, y, bi)
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

block_ranger::block_ranger () : bool_zero (boolean_type_node, 0, 0),
				bool_one (boolean_type_node, 1, 1)
{
  gori = new gori_map ();
}

block_ranger::~block_ranger ()
{
  delete gori;
}

// This routine will return a range for any kind of operator possible. 
// It is virtual to allow a more globally aware version to get better results
// by looking a the CFG.
// Return true if there was a range generated.
bool
block_ranger::get_operand_range (irange& r, tree op,
				 gimple *s ATTRIBUTE_UNUSED)
{
  /* This check allows unary operations to be handled without having to 
     make an explicit check for the existence of a second operand.  */
  if (!op)
    return false;

  // Integers are simply a range [op,op].
  if (TREE_CODE (op) == INTEGER_CST)
    r.set_range (TREE_TYPE (op), op, op);
  else
    // IF its an ssa_name, query gcc's current global range, if one is known.
    if (TREE_CODE (op) == SSA_NAME)
      r = op;
    else
      // If an operand is an ADDR_EXPR constant (Which can happen in a phi
      // argument) simply check to see if its a non-zero.
      if (TREE_CODE (op) == ADDR_EXPR)
        {
	  bool ov;
	  // handle &var which can show up in phi arguments
	  if (tree_single_nonzero_warnv_p (op, &ov))
	    r.set_range (TREE_TYPE (op), 0, 0, irange::INVERSE);
	  else
	    r.set_range_for_type (TREE_TYPE (op));
	}
      else
        // If its a TYPE node, set the range for the type.
	if (TYPE_P (op))
	  r.set_range_for_type (op);
	else // Default to range for the type of the expression.   */
	  r.set_range_for_type (TREE_TYPE (op));

  return true;
}


// Given a logical STMT, calculate true and false for each potential path 
// using NAME and resolve the outcome based on the logical operator.  
bool
block_ranger::process_logical (range_stmt stmt, irange& r, tree name,
		       const irange& lhs)
{
  range_stmt op_stmt;
  irange op1_range, op2_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;
  bool ret;

  irange op1_true, op1_false, op2_true, op2_false;

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1 = stmt.operand1 ();
  op2 = stmt.operand2 ();

  op1_in_chain = gori->in_chain_p (name, op1);
  op2_in_chain = gori->in_chain_p (name, op2);

  /* If neither operand is derived, then this stmt tells us nothing. */
  if (!op1_in_chain && !op2_in_chain)
    return false;

  /* The false path is not always a simple inversion of the true side.
     Calulate ranges for true and false on both sides. */
  if (op1_in_chain)
    {
      ret = get_range_from_stmt (SSA_NAME_DEF_STMT (op1), op1_true, name,
				 bool_one);
      ret &= get_range_from_stmt (SSA_NAME_DEF_STMT (op1), op1_false, name,
				  bool_zero);
    }
  else
    {
      // Otherwise just get values for name in operand 1 position
      ret = get_operand_range (op1_true, name, stmt);
      ret &= get_operand_range (op1_false, name, stmt);
    }

  /* If operand1 evaluated OK, move on to operand 2.  */
  if (ret)
    {
      if (op2_in_chain)
	{
	  ret &= get_range_from_stmt (SSA_NAME_DEF_STMT (op2), op2_true,
				      name, bool_one);
	  ret &= get_range_from_stmt (SSA_NAME_DEF_STMT (op2), op2_false,
				      name, bool_zero);
	}
      else
	{
	  // Otherwise just get values for name in operand 1 position
	  ret &= get_operand_range (op2_true, name, stmt);
	  ret &= get_operand_range (op2_false, name, stmt);
	}
    }
  if (!ret || !stmt.fold_logical (r, lhs, op1_true, op1_false, op2_true,
				  op2_false))
    r.set_range_for_type (TREE_TYPE (name));
  return true;
}


// Given the expression in STMT, return an evaluation in R for NAME
// when the LJHS evaluates to LHS.  Returning false means the name being
// looked for was not resolvable. 
bool
block_ranger::get_range_from_stmt (range_stmt stmt, irange& r, tree name,
				   const irange& lhs)
{
  irange op1_range, op2_range, tmp_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;

  if (!stmt.valid ())
    return false;

  // Empty ranges are viral as they are on a path which isn't executable.
  if (lhs.empty_p ())
    {
      r.clear (TREE_TYPE (name));
      return true;
    }

  op1 = stmt.operand1 ();
  op2 = stmt.operand2 ();

  // Operand 1 is the name being looked for, evaluate it.
  if (op1 == name)
    { 
      if (!op2)
        return stmt.op1_irange (r, lhs);
      // If we need the second operand, get a value and evaluate.
      if (get_operand_range (op2_range, op2, stmt))
	return stmt.op1_irange (r, lhs, op2_range);
      else
        return false;
    }

  // Operand 2 is the name being looked for, evaluate it.
  if (op2 == name)
    {
      if (get_operand_range (op1_range, op1, stmt))
	return stmt.op2_irange (r, lhs, op1_range);
      else
        return false;
    }

  // Check for boolean cases which require developing ranges and combining. 
  if (stmt.logical_expr_p ())
    return process_logical (stmt, r, name, lhs);

  // Reaching this point means NAME is not in this stmt, but one of the
  // names in it ought to be derived from it. 
  op1_in_chain = gori->in_chain_p (name, op1);
  op2_in_chain = op2 && gori->in_chain_p (name, op2);

  // If neither operand is derived, then this stmt tells us nothing.
  if (!op1_in_chain && !op2_in_chain)
    return false;

  // Pick up an operand range for each argument.
  if (!get_operand_range (op1_range, op1, stmt))
    return false;
  if (op2 && !get_operand_range (op2_range, op2, stmt))
    return false;

  if (op1_in_chain && op2_in_chain)
    {
      // Get an op2_range based on the op1 range. 
      if (!stmt.op2_irange (tmp_range, lhs, op1_range))
        return false;
      // And combine it with the raw possibilty.
      op2_range.intersect (tmp_range);
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range))
        return false;

      // Now the same for operand 1
      if (!stmt.op1_irange (tmp_range, lhs, op2_range))
        return false;
      op1_range.intersect (tmp_range);
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name, op1_range))
        return false;
      // Do we need to possibly reevaluate op2 since op1 is now more accurate?
      return true;
    }
  else
    if (op1_in_chain)
      {
        // We know we only care about operand1.
        if (!op2)
	  {
	    if (!stmt.op1_irange (tmp_range, lhs))
	      return false;
	  }
	else
	  {
	    if (!stmt.op1_irange (tmp_range, lhs, op2_range))
	      return false;
	  }
	op1_range.intersect (tmp_range);
	return get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name,
				    op1_range);
      }
  // At this point it must be op2_in_chain
  if (!stmt.op2_irange (tmp_range, lhs, op1_range))
    return false;
  op2_range.intersect (tmp_range);
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range);
}
 
// Dump the bvlock rangers data structures.
void
block_ranger::dump (FILE *f)
{

  if (!f)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  gori->dump (f);
  fprintf (f, "\n");
}

tree
block_ranger::single_import (tree name)
{
  return gori->single_import (name);
}

bool
block_ranger::range_p (basic_block bb, tree name)
{
  return gori->is_export_p (name, bb);
}


// Calcualte a range for NAME on edge E, returning ther result in R.
bool
block_ranger::range_on_edge (irange& r, tree name, edge e)
{
  gimple *stmt;
  basic_block bb = e->src;

  if (!valid_irange_ssa (name))
    return false;

  // If this block doesnt produce ranges for NAME, bail now.
  if (!range_p (bb, name))
    return false;

  stmt = last_stmt_gori (bb);
  gcc_assert (stmt);

  // Generate a range for either the TRUE or FALSE edge.
  if (e->flags & EDGE_TRUE_VALUE)
    return get_range_from_stmt (stmt, r, name, bool_one);

  if (e->flags & EDGE_FALSE_VALUE)
    return get_range_from_stmt (stmt, r, name, bool_zero);

  return false;
}


// This routine will loop through all the edges and block in the program and
// ask for the range of each ssa-name. THis exercises the ranger to make sure
// there are no ICEs for unexpected questions, as well as provides a
// convenient way to show all the ranges that could be calculated.
void
block_ranger::exercise (FILE *output)
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
	      if (name && range_p (bb, name))
		{
		  if (range_on_edge (range, name, e))
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
	}
      if (printed)
        fprintf (output, "\n");

    }
    
  if (output)
    dump (output);

}


// Attempt to evaluate the epression using whatever is globally known about
// the operands.  If it can be evaluated, TRUE is returned
// and the range is returned in R.  */
bool
block_ranger::range_of_def (irange& r, gimple *g)
{
  range_stmt rn (g);
  irange r1, r2;

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  tree op1 = rn.operand1 ();
  tree op2 = rn.operand2 ();

  get_operand_range (r1, op1);
  if (op2)
    return rn.fold (r, r1);

  get_operand_range (r2, op2);
  return rn.fold (r, r1, r2);
}

// This method will attempt to evaluate the statement G by replacing any
// occurrence of ssa_name NAME with the RANGE_OF_NAME. If it can be
// evaluated, TRUE is returned and the resulting range returned in R. 
bool
block_ranger::range_of_def (irange& r, gimple *g, tree name,
			    const irange& range_of_name)
{
  range_stmt rn (g);

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  irange r1, r2;
  tree op1 = rn.operand1 ();
  tree op2 = rn.operand2 ();

  if (op1 == name)
    r1 = range_of_name;
  else
    get_operand_range (r1, op1);

  if (!op2)
    return rn.fold (r, r1);

  if (op2 == name)
    r2 = range_of_name;
  else
    get_operand_range (r2, op2);

  return rn.fold (r, r1, r2);
}

