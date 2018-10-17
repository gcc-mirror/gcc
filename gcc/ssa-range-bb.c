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

// If NAME is defined in block BB, return the gimple statement pointer, 
// otherwise return NULL>
inline gimple *
ssa_name_same_bb_p (tree name, basic_block bb)
{
  gimple *g = SSA_NAME_DEF_STMT (name);
  if (!g || gimple_bb (g) != bb)
   return NULL;
  return g;
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
    
    Def chains also only include statements which are valid range_stmt's. so
    a def chain will only span statements for which the range engine
    implements operations.  */

class gori_map
{
public:
  gori_map ();
  ~gori_map ();
  bool in_chain_p (tree name, tree def, basic_block bb = NULL);
  bool is_export_p (tree name, basic_block bb);
  bool is_import_p (tree name, basic_block bb);
  tree single_import (tree name);
  void dump (FILE *f);
  void dump (FILE *f, basic_block bb);
private:
  vec<bitmap> m_outgoing;	/* BB: Outgoing ranges generated.  */
  vec<bitmap> m_incoming;	/* BB: ranges coming in.  */
  vec<bitmap> m_def_chain;	/* SSA_NAME : def chain components. */
  void calculate_gori (basic_block bb);
  bool in_chain_p (unsigned name, unsigned def);
  bitmap imports (basic_block bb);
  bitmap exports (basic_block bb);
  bitmap calc_def_chain (tree name, basic_block bb);
  void process_stmt (gimple *s, bitmap result, basic_block bb);
};


gori_map::gori_map ()
{
  m_outgoing.create (0);
  m_outgoing.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_incoming.create (0);
  m_incoming.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_def_chain.create (0);
  m_def_chain.safe_grow_cleared (num_ssa_names);
}

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
  if (m_outgoing[bb->index] == NULL)
    calculate_gori (bb);

  return in_chain_p (name_index, def_index);
}

// Return true if ssa_name version NAME is set in vector version DEF.
// This assumes all vectors have been created, so NULL means there is 
// nothing in the defintion chain.
bool
gori_map::in_chain_p (unsigned name, unsigned def)
{
  if (m_def_chain[def] == NULL)
    return false;
  return bitmap_bit_p (m_def_chain[def], name);
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
  if (bb && !m_incoming[bb->index])
    calculate_gori (bb);
  if (m_def_chain [name_index] == NULL)
    return NULL_TREE;

  // Now make sure it is the ONLY import. 
  EXECUTE_IF_AND_IN_BITMAP (m_def_chain [name_index], m_incoming[bb->index], 0,
			    index, bi)
    {
      if (!ret)
        ret = ssa_name (index);
      else
	return NULL_TREE;
    }
  return ret;
}

// Process STMT to build m_def_chains in BB.. Recursively create m_def_chains
// for any operand contained in STMT, and set the def chain bits in RESULT.
void
gori_map::process_stmt (gimple *s, bitmap result, basic_block bb)
{
  bitmap b;
  grange_op *stmt = dyn_cast<grange_op *>(s);
  if (!stmt)
    return;

  tree ssa1 = stmt->ssa_operand1 ();
  tree ssa2 = stmt->ssa_operand2 ();

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
// the bimap of all names in the m_def_chain
bitmap
gori_map::calc_def_chain (tree name, basic_block bb)
{
  unsigned v = SSA_NAME_VERSION (name);
  gimple *s = SSA_NAME_DEF_STMT (name);
  grange_op *stmt = dyn_cast <grange_op *>(s);

  if (!stmt || gimple_bb (stmt) != bb)
    {
      // If its an import, set the bit. PHIs are also considere3d imports
      bitmap_set_bit (m_incoming[bb->index], v);
      return NULL;
    }
  // If it has already been processed, just return the cached value.
  if (m_def_chain[v])
    return m_def_chain[v];

  // Allocate a new bitmap and initialize it.
  m_def_chain[v] = BITMAP_ALLOC (NULL);
  process_stmt (stmt, m_def_chain[v], bb);

  return m_def_chain[v];
}

// Calculate all the required information for BB.
void
gori_map::calculate_gori (basic_block bb)
{
  gcc_assert (m_outgoing[bb->index] == NULL);
  m_outgoing[bb->index] = BITMAP_ALLOC (NULL);
  m_incoming[bb->index] = BITMAP_ALLOC (NULL);

  // If this block's last statement may generate range informaiton, 
  // go calculate it.
  gimple *s = last_stmt (bb);
  if (!s || !is_a<gcond *>(s))
    return;
  process_stmt (s, m_outgoing[bb->index], bb);
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
	  if ((t = single_import (name)))
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

block_ranger::block_ranger ()
{
  unsigned prec = TYPE_PRECISION (boolean_type_node);
  m_bool_zero = irange (boolean_type_node, wi::zero (prec), wi::zero (prec));
  m_bool_one = irange (boolean_type_node, wi::one (prec), wi::one (prec));
  m_gori = new gori_map ();
}

block_ranger::~block_ranger ()
{
  delete m_gori;
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
  if (op)
    return gimple_range_of_expr (r, op);
  return false;
}


// Given a logical STMT, calculate true and false for each potential path 
// using NAME and resolve the outcome based on the logical operator.  
bool
block_ranger::process_logical (glogical *s, irange& r, tree name,
			       const irange& lhs)
{
  irange op1_range, op2_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;
  bool ret;
  grange_op *stmt = dyn_cast<grange_op *> (s);

  gcc_assert (stmt);

  irange op1_true, op1_false, op2_true, op2_false;

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1 = stmt->operand1 ();
  op2 = stmt->operand2 ();

  op1_in_chain = m_gori->in_chain_p (name, op1);
  op2_in_chain = m_gori->in_chain_p (name, op2);

  /* If neither operand is derived, then this stmt tells us nothing. */
  if (!op1_in_chain && !op2_in_chain)
    return false;

  /* The false path is not always a simple inversion of the true side.
     Calulate ranges for true and false on both sides. */
  if (op1_in_chain)
    {
      ret = get_range_from_stmt (SSA_NAME_DEF_STMT (op1), op1_true, name,
				 m_bool_one);
      ret &= get_range_from_stmt (SSA_NAME_DEF_STMT (op1), op1_false, name,
				  m_bool_zero);
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
				      name, m_bool_one);
	  ret &= get_range_from_stmt (SSA_NAME_DEF_STMT (op2), op2_false,
				      name, m_bool_zero);
	}
      else
	{
	  // Otherwise just get values for name in operand 1 position
	  ret &= get_operand_range (op2_true, name, stmt);
	  ret &= get_operand_range (op2_false, name, stmt);
	}
    }
  if (!ret || !gimple_range_logical_fold (r, s, lhs, op1_true, op1_false,
					  op2_true, op2_false))
    r.set_varying (TREE_TYPE (name));
  return true;
}

bool
block_ranger::get_range_if_name_on_stmt (grange_op *s,
					 irange& r, tree name,
					 const irange& lhs)
{
  irange op1_range, op2_range;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Operand 1 is the name being looked for, evaluate it.
  if (op1 == name)
    { 
      if (!op2)
	{
	  if (get_operand_range (op1_range, op1, s))
	    return s->calc_op1_irange (r, lhs, op1_range);
	  else
	    return s->calc_op1_irange (r, lhs);
	}
      // If we need the second operand, get a value and evaluate.
      if (get_operand_range (op2_range, op2, s))
	return s->calc_op1_irange (r, lhs, op2_range);
      else
	return false;
    }

  gcc_checking_assert (op2 == name);

  if (get_operand_range (op1_range, op1, s))
    return s->calc_op2_irange (r, lhs, op1_range);
  return false;
}

bool
block_ranger::get_range_thru_op1 (grange_op *s, irange& r, tree name,
				  const irange& lhs)
{
  irange op1_range, op2_range;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Determine a known range for operand1 ().
  if (!get_operand_range (op1_range, op1, s))
    return false;

  // Put the calcuated result in r.
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
      if (!get_operand_range (op2_range, op2, s))
	return false;
      if (!s->calc_op1_irange (r, lhs, op2_range))
	return false;
    }
  // Intersect the calculated result with the known result.
  op1_range.intersect (r);

  // Then feed this range back as the LHS of the defining statement.
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name, op1_range);
}

bool
block_ranger::get_range_thru_op2 (grange_op *s, irange& r, tree name,
				  const irange& lhs)
{
  irange op1_range, op2_range;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Get a range for op1.
  if (!get_operand_range (op1_range, op1, s))
    return false;

  // calculate the range for op2 based on lhs and op1.
  if (!s->calc_op2_irange (op2_range, lhs, op1_range))
    return false;

  // also pick up what is known about op2's range at this point
  if (!get_operand_range (r, op2, s))
    return false;

  // And intersect it with the calculated result.
  op2_range.intersect (r);

  // Then feed this range back as the LHS of the defining statement.
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range);
}

bool
block_ranger::get_range_thru_op1_and_op2 (grange_op *s, irange& r, tree name,
					  const irange& lhs)
{
  irange op_range;

  // Calculate a good a range for op2. since op1 == op2, this will have
  // already included whatever the actual range of name is.
  if (!get_range_thru_op2 (s, op_range, name, lhs))
    return false;

  // Now get the range thru op1... 
  if (!get_range_thru_op1 (s, r, name, lhs))
    return false;

  // Whichever range is the most permissive is the one we need to use. (?)
  r.union_ (op_range);
  return true;
}
 
// Given the expression in STMT, return an evaluation in R for NAME
// when the lhs evaluates to LHS.  Returning false means the name being
// looked for was not resolvable. 
bool
block_ranger::get_range_from_stmt (gimple *s, irange& r, tree name,
				     const irange& lhs)
{
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;

  grange_op *stmt = dyn_cast<grange_op *>(s);
  if (!stmt)
    return false;

  // Empty ranges are viral as they are on a path which isn't executable.
  if (lhs.undefined_p ())
    {
      r.set_undefined (TREE_TYPE (name));
      return true;
    }

  op1 = stmt->operand1 ();
  op2 = stmt->operand2 ();

  if (op1 == name || op2 == name)
    return get_range_if_name_on_stmt (stmt, r, name, lhs);

  // Check for boolean combination cases which require developing ranges 
  // and combining the results based on the operation. 
  glogical *logic = dyn_cast<glogical *> (s);
  if (logic)
    return process_logical (logic, r, name, lhs);

  // Reaching this point means NAME is not in this stmt, but one of the
  // names in it ought to be derived from it. 
  op1_in_chain = m_gori->in_chain_p (name, op1);
  op2_in_chain = op2 && m_gori->in_chain_p (name, op2);

  if (op2_in_chain)
    { 
      if (op1_in_chain)
	return get_range_thru_op1_and_op2 (stmt, r, name, lhs);
      else
	return get_range_thru_op2 (stmt, r, name, lhs);
    }
  else
    if (op1_in_chain)
      return get_range_thru_op1 (stmt, r, name, lhs);

  // If neither operand is derived, then this stmt tells us nothing.
  return false;
}



// Dump the bvlock rangers data structures.
void
block_ranger::dump (FILE *f)
{

  if (!f)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  m_gori->dump (f);
  fprintf (f, "\n");
}

tree
block_ranger::single_import (tree name)
{
  return m_gori->single_import (name);
}

bool
block_ranger::range_p (basic_block bb, tree name)
{
  return m_gori->is_export_p (name, bb);
}


// Calculate a range for NAME on edge E, returning ther result in R.
bool
block_ranger::range_on_edge (irange& r, tree name, edge e)
{
  irange lhs_range;
  basic_block bb = e->src;

  if (!valid_irange_ssa (name))
    return false;

  // If this block doesnt produce ranges for NAME, bail now.
  if (!range_p (bb, name))
    return false;
  
  gimple_range_outgoing_edge (lhs_range, e);

  gimple *s = last_stmt (bb);
  return get_range_from_stmt (s, r, name, lhs_range);
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


// Attempt to evaluate NAME within the basic block it is defined assuming the
// block was entered via edge E.
bool
block_ranger::range_of_stmt (irange &r, gimple *g, edge e)
{
  basic_block bb = gimple_bb (g);

  /* The edge provided must be an incoming edge to this BB.  */
  gcc_assert (e->dest == bb);

  // Note that since we are remaining within BB, we do not attempt to further
  // evaluate any of the arguments of a PHI at this point.
  // For the moment, just pick up any cheap edge information.
  gphi *phi = dyn_cast<gphi *> (g);
  if (phi)
    {
      tree arg;
      gcc_assert (e->dest == bb);
      arg = gimple_phi_arg_def (phi, e->dest_idx);
      // Pick up anything simple we might know about the incoming edge. 
      if (!range_on_edge (r, arg, e))
	return gimple_range_of_expr (r, arg);
      return true;
    }

  grange_op *stmt = dyn_cast<grange_op *>(g);
  if (!stmt)
    return false; 
  irange range1, range2;

  if (!stmt->ssa_operand1 () ||
      !ssa_name_same_bb_p (stmt->ssa_operand1 (), bb) ||
      !range_of_stmt (range1, SSA_NAME_DEF_STMT (stmt->ssa_operand1 ()), e))
    if (!gimple_range_of_expr (range1, stmt->operand1 ()))
      return false;

  if (!stmt->operand2 ())
    return stmt->fold (r, range1);

  if (!stmt->ssa_operand2 () ||
      !ssa_name_same_bb_p (stmt->ssa_operand2 (), bb) ||
      !range_of_stmt (range2, SSA_NAME_DEF_STMT (stmt->ssa_operand2 ()), e))
    if (!gimple_range_of_expr (range2, stmt->operand2 ()))
      return false;
  return stmt->fold (r, range1, range2);
}

