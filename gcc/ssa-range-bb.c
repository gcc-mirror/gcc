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
#include "ssa-range-global.h"

/* Is the last stmt in a block interesting to look at for range info.  */

gimple *
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
   GORI stands for "Generates Outgoing Range Information."  */

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
  void process_stmt (gimple *stmt, bitmap result, basic_block bb);
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
  unsigned x;
  int bb;
  for (bb = 0; bb < last_basic_block_for_fn (cfun); ++bb)
    if (outgoing[bb])
      BITMAP_FREE (outgoing[bb]);
  outgoing.release ();

  for (bb = 0; bb < last_basic_block_for_fn (cfun); ++bb)
    if (incoming[bb])
      BITMAP_FREE (incoming[bb]);
  incoming.release ();

  for (x = 0; x < num_ssa_names; ++x)
    if (def_chain[x])
      BITMAP_FREE (def_chain[x]);
  def_chain.release ();
}

bitmap
gori_map::imports (basic_block bb)
{
  if (!incoming[bb->index])
    calculate_gori (bb);
  return incoming[bb->index];
}

bool
gori_map::is_import_p (tree name, basic_block bb)
{
  return bitmap_bit_p (imports (bb), SSA_NAME_VERSION (name));
}

bitmap
gori_map::exports (basic_block bb)
{
  if (!outgoing[bb->index])
    calculate_gori (bb);
  return outgoing[bb->index];
}

bool
gori_map::is_export_p (tree name, basic_block bb)
{
  return bitmap_bit_p (exports (bb), SSA_NAME_VERSION (name));
}

bool
gori_map::in_chain_p (tree name, tree def, basic_block bb)
{
  if (TREE_CODE (def) != SSA_NAME || TREE_CODE (name) != SSA_NAME)
    return false;

  unsigned def_index = SSA_NAME_VERSION (def);
  unsigned name_index = SSA_NAME_VERSION (name);
  gimple *stmt;

  if (SSA_NAME_IS_DEFAULT_DEF (def))
    return false;

  stmt = SSA_NAME_DEF_STMT (def);
  if (bb)
    {
      /* If the definition is not in a specified BB, return false;.  */
      if (gimple_bb (stmt) != bb)
        return false;
    }
  else
    bb = gimple_bb (stmt);

  if (outgoing[bb->index] == NULL)
    calculate_gori (bb);

  return in_chain_p (name_index, def_index);
}

bool
gori_map::in_chain_p (unsigned name, unsigned def)
{
  if (def_chain[def] == NULL)
    return false;
  return bitmap_bit_p (def_chain[def], name);
}

tree
gori_map::single_import (tree name)
{
  tree ret = NULL_TREE;
  unsigned name_index = SSA_NAME_VERSION (name);
  unsigned index;
  basic_block bb;
  bitmap_iterator bi;

  if (def_chain [name_index] == NULL)
    return NULL_TREE;
  bb = gimple_bb (SSA_NAME_DEF_STMT (name));

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

void
gori_map::process_stmt (gimple *stmt, bitmap result, basic_block bb)
{
  range_stmt rn (stmt);
  tree ssa1 = rn.ssa_operand1 ();
  tree ssa2 = rn.ssa_operand2 ();
  bitmap b;

  if (!rn.valid ())
    return;

  if (ssa1)
    {
      b = calc_def_chain (ssa1, bb);
      if (b)
	bitmap_copy (result, b);
      bitmap_set_bit (result, SSA_NAME_VERSION (ssa1));
    }
  if (ssa2)
    {
      b = calc_def_chain (ssa2, bb);
      if (b)
	bitmap_ior_into (result, b);
      bitmap_set_bit (result, SSA_NAME_VERSION (ssa2));
    }
  return;
}


bitmap
gori_map::calc_def_chain (tree name, basic_block bb)
{
  gimple *stmt = SSA_NAME_DEF_STMT (name);
  unsigned v = SSA_NAME_VERSION (name);
  range_stmt rn;

  if (!stmt || gimple_bb (stmt) != bb)
    {
      bitmap_set_bit (incoming[bb->index], v);
      return NULL;
    }
  if (def_chain[v])
    return def_chain[v];

  def_chain[v] = BITMAP_ALLOC (NULL);
  process_stmt (stmt, def_chain[v], bb);

  return def_chain[v];
}

void
gori_map::calculate_gori (basic_block bb)
{
  gimple *stmt;
  gcc_assert (outgoing[bb->index] == NULL);
  outgoing[bb->index] = BITMAP_ALLOC (NULL);
  incoming[bb->index] = BITMAP_ALLOC (NULL);

  stmt = last_stmt_gori (bb);
  if (stmt)
    process_stmt (stmt, outgoing[bb->index], bb);
}

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

  for (x = 1; x< num_ssa_names; x++)
    {
      tree name = ssa_name (x);
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

  fprintf (f, "BB%d imports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (incoming[bb->index], 0, y, bi)
    {
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }
  fprintf (f, "\nBB%d exports: ",bb->index);
  EXECUTE_IF_SET_IN_BITMAP (outgoing[bb->index], 0, y, bi)
    {
      print_generic_expr (f, ssa_name (y), TDF_SLIM);
      fprintf (f, "  ");
    }
  fprintf (f, "\n");
}

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
  initialize_global_ssa_range_cache ();

}

block_ranger::~block_ranger ()
{
  destroy_global_ssa_range_cache ();
  delete gori;
}

/* Return TRUE if CODE with operands of type TYPE is a boolean
   evaluation.  These are important to identify as both sides of a logical
   binary expression must be evaluated in order to calculate a range.  */
bool
block_ranger::logical_expr_p (tree_code code, tree type) const
{

  /* Look for boolean and/or condition.  */
  switch (code)
    {
      case TRUTH_AND_EXPR:
      case TRUTH_OR_EXPR:
        return true;

      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
        if (types_compatible_p (type, boolean_type_node))
	  return true;
	break;

      default:
        break;
    }
  return false;
}

/* Evaluate a binary logical expression given true and false ranges for each
   of the operands. Base the result on the value in the LHS.  */
bool
block_ranger::eval_logical (irange& r, range_stmt &stmt, const irange& lhs,
			    const irange& op1_true, const irange& op1_false,
			    const irange& op2_true,
			    const irange& op2_false) const
{
  gcc_checking_assert (logical_expr_p (stmt.get_code (),
				       TREE_TYPE (stmt.operand1 ())));
 
  /* If the LHS can be TRUE OR FALSE, then both need to be evalauted and
     combined, otherwise any range restrictions that have been determined
     leading up to this point would be lost.  */
  if (!wi::eq_p (lhs.lower_bound(), lhs.upper_bound()))
    {
      irange r1;
      if (eval_logical (r1, stmt, bool_zero, op1_true, op1_false, op2_true,
			op2_false) &&
	  eval_logical (r, stmt, bool_one, op1_true, op1_false, op2_true,
			op2_false))
	{
	  r.union_ (r1);
	  return true;
	}
      return false;

    }

  /* Now combine based on whether the result is TRUE or FALSE.  */
  switch (stmt.get_code ())
    {

      /* A logical operation on two ranges is executed with operand ranges that
	 have been determined for both a TRUE and FALSE result..
	 Assuming x_8 is an unsigned char:
		b_1 = x_8 < 20
		b_2 = x_8 > 5
	 if we are looking for the range of x_8, the operand ranges will be:
	 will be: 
	 b_1 TRUE	x_8 = [0, 19]
	 b_1 FALSE  	x_8 = [20, 255]
	 b_2 TRUE 	x_8 = [6, 255]
	 b_2 FALSE	x_8 = [0,5]. */
	       
      /*	c_2 = b_1 && b_2
	 The result of an AND operation with a TRUE result is the intersection
	 of the 2 TRUE ranges, [0,19] intersect [6,255]  ->   [6, 19]. */
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  r = irange_intersect (op1_true, op2_true);
	else
	  {
	    /* The FALSE side is the union of the other 3 cases.  */
	    irange ff = irange_intersect (op1_false, op2_false);
	    irange tf = irange_intersect (op1_true, op2_false);
	    irange ft = irange_intersect (op1_false, op2_true);
	    r = irange_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      /* 	c_2 = b_1 || b_2
	 An OR operation will only take the FALSE path if both operands are
	 false, so [20, 255] intersect [0, 5] is the union: [0,5][20,255].  */
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  r = irange_intersect (op1_false, op2_false);
	else
	  {
	    /* The TRUE side of the OR operation will be the union of the other
	       three combinations.  */
	    irange tt = irange_intersect (op1_true, op2_true);
	    irange tf = irange_intersect (op1_true, op2_false);
	    irange ft = irange_intersect (op1_false, op2_true);
	    r = irange_union (tt, tf);
	    r.union_ (ft);
	  }
	break;

      default:
        gcc_unreachable ();
    }

  return true;
}

/* Given a logical STMT, calculate true and false for each potential path 
   using NAME and resolve the outcome based on the logical operator.  */
bool
block_ranger::process_logical (range_stmt& stmt, irange& r, tree name,
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
      ret = get_operand_range (op1_true, name);
      ret &= get_operand_range (op1_false, name);
    }

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
	  ret &= get_operand_range (op2_true, name);
	  ret &= get_operand_range (op2_false, name);
	}
    }
  if (!ret || !eval_logical (r, stmt, lhs, op1_true, op1_false, op2_true,
			     op2_false))
    r.set_range_for_type (TREE_TYPE (name));
  return true;
}


/* Given the expression in STMT, return an evaluation in R for NAME.
   Returning false means the name being looked for is NOT resolvable, and
   can be removed from the GORI map to avoid future searches.  */
bool
block_ranger::get_range (range_stmt& stmt, irange& r, tree name,
		 const irange& lhs)
{
  range_stmt op_stmt;
  irange op1_range, op2_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;

  op1 = stmt.operand1 ();
  op2 = stmt.operand2 ();

  if (op1 == name)
    { 
      if (!op2)
        return stmt.op1_irange (r, lhs);
      if (get_operand_range (op2_range, op2))
	return stmt.op1_irange (r, lhs, op2_range);
      else
        return false;
    }

  if (op2 == name)
    {
      if (get_operand_range (op1_range, op1))
	return stmt.op2_irange (r, lhs, op1_range);
      else
        return false;
    }

  /* Check for boolean cases which require developing ranges and combining.  */
  if (logical_expr_p (stmt.get_code (), TREE_TYPE (op1)))
    return process_logical (stmt, r, name, lhs);

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1_in_chain = gori->in_chain_p (name, op1);
  op2_in_chain = op2 && gori->in_chain_p (name, op2);

  /* If neither operand is derived, then this stmt tells us nothing. */
  if (!op1_in_chain && !op2_in_chain)
    return false;

  /* Can't resolve both sides at once, so take a guess at operand 1, calculate
     operand 2 and check if the guess at operand 1 was good.  */
  if (op1_in_chain && op2_in_chain)
    {
      irange tmp_op1_range;
      if (!get_operand_range (tmp_op1_range, op1))
        return false;
      if (!stmt.op2_irange (op2_range, lhs, tmp_op1_range))
        return false;
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range))
        return false;
      if (!stmt.op1_irange (op1_range, lhs, op2_range))
        return false;
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name, op1_range))
        return false;

      /* If the guess is good, we're done. */
      if (op1_range == tmp_op1_range)
        return true;
      /* Otherwise fall thru and calculate op2_range with this range.  */
    }
  else
    if (op1_in_chain)
      {
        if (!op2)
	  {
	    if (!stmt.op1_irange (op1_range, lhs))
	      return false;
	  }
	else
	  {
	    if (!get_operand_range (op2_range, op2))
	      return false;
	    if (!stmt.op1_irange (op1_range, lhs, op2_range))
	      return false;
	  }
	return get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name,
				    op1_range);
      }
    else
      if (!get_operand_range (op1_range, op1))
        return false;

  if (!stmt.op2_irange (op2_range, lhs, op1_range))
    return false;
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range);
}
 
/* Given the expression in STMT, return an evaluation in R for NAME. */
bool
block_ranger::get_range_from_stmt (gimple *stmt, irange& r, tree name,
			   const irange& lhs)
{
  range_stmt rn;
  rn = stmt;

  /* If it isnt an expression that is understood, we know nothing.  */
  if (!rn.valid())
    return false;

  /* If the lhs has no range, ie , it cannot be executed, then the query
     has no range either.  */
  if (lhs.empty_p ())
    {
      r.clear (TREE_TYPE (name));
      return true;
    }

  return get_range (rn, r, name, lhs);
}

void
block_ranger::dump (FILE *f)
{

  if (!f)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  gori->dump (f);
  fprintf (f, "\n");

  fprintf (f, "\nDUMPING Globals table\n");
  dump_global_ssa_range_cache (f);
}


/* Name is not directly mentioned in the gori map, but if one of the
   compnents it is built from are, we can derive the value from that. ie
	 a_3 = c_2 * 2
	 b_6 = a_3 + 5
	 if (a_3 > 10)
   The GORI map will only have c_2 and a_3 since they comprise the
   calculation of a_3.   b_6 will not be there as the only way to know
   that b_6 can be calculated from a_3 would be to visit all the uses of
   a_3 and see whether it is used to help define b_6.
   Far easier now that b_6 is requested would be to simply check now
   if a_3 is used to construct b_6. If it is get the expression for a_3
   and adjust it to b_6.  */
   
#if 0
bool
block_ranger::get_derived_range_stmt (range_stmt& stmt, tree name, basic_block bb)
{
  gimple *s;
  tree n1,n2;
  unsigned name_v = SSA_NAME_VERSION (name);

  s = last_stmt_gori (bb);
  gcc_assert (s);

  stmt = s;
  if (!stmt.valid ())
    return false;

  n1 = stmt.ssa_operand1 ();
  n2 = stmt.ssa_operand2 ();

  if (n1 && !bitmap_bit_p (def_chain[name_v], SSA_NAME_VERSION (n1)))
    n1 = NULL_TREE;
    
  if (n2 && !bitmap_bit_p (def_chain[name_v], SSA_NAME_VERSION (n2)))
    n2 = NULL_TREE;
  
  /* Non-null n1 or n2 indicates it is used in the calculating name.  */

  /* Used on both sides too complicated.  */
  if (n1 && n2)
    return false;
  /* Well we aren't actually DOING it yet... :-)  */
  return false;
}
#endif


bool
block_ranger::range_p (basic_block bb, tree name)
{
  return gori->is_export_p (name, bb);
}


/* Known range on an edge.  */
bool
block_ranger::range_on_edge (irange& r, tree name, edge e)
{
  gimple *stmt;
  basic_block bb = e->src;

  if (!valid_irange_ssa (name))
    return false;

  if (!range_p (bb, name))
    return false;

  stmt = last_stmt_gori (bb);
  gcc_assert (stmt);

  if (e->flags & EDGE_TRUE_VALUE)
    return get_range_from_stmt (stmt, r, name, bool_one);

  if (e->flags & EDGE_FALSE_VALUE)
    return get_range_from_stmt (stmt, r, name, bool_zero);

  return false;
}



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



bool
block_ranger::range_on_stmt (irange& r, tree name, gimple *g)
{
  range_stmt rn (g);
  irange lhs;

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;

  /* If neither operand is what we are looking for, then return nothing.  */
  if (rn.operand1 () != name && rn.operand2 () != name)
    return false;

  /* So far only understand LHS if its an assignment.  */
  if (gimple_code (g) != GIMPLE_ASSIGN)
    return false;

  if (get_operand_range (lhs, gimple_get_lhs (g)))
    return get_range (rn, r, name, lhs);

  return false;
}

bool
block_ranger::range_of_def (irange& r, gimple *g)
{
  range_stmt rn (g);

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  return rn.fold (r);
}

bool
block_ranger::range_of_def (irange& r, gimple *g, tree name,
		    const irange& range_of_name)
{
  range_stmt rn (g);

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  return rn.fold (r, name, range_of_name);
}

