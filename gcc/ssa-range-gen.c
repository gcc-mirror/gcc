/* On-demand ssa range generator.
   Copyright (C) 2017 Free Software Foundation, Inc.
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
#include "domwalk.h"
#include "ssa-range-gen.h"
#include "ssa-range-stmt.h"
#include "ssa-range-global.h"


// Internally, the range operators all use boolen_type_node when comparisons
// and such are made to create ranges for logical operations.
// some languages, such as fortran, may use boolean types with different
// precisions and these are incompatible.   This routine will look at the 
// 2 ranges, and if there is a mismatch between the boolean types, will change
// the range generated from the default node to the other type.
static void
normalize_bool_type (irange& r1, irange& r2)
{
  const_tree t1 = r1.get_type ();
  const_tree t2 = r2.get_type ();
  if (TREE_CODE (t1) != BOOLEAN_TYPE || TREE_CODE (t2) != BOOLEAN_TYPE)
    return;

  if (t1 == t2)
    return;

  /* If neither is boolean_type_node, assume they are compatible.  */
  if (t1 == boolean_type_node)
      r1.cast (t2);
  else
    if (t2 == boolean_type_node)
      r2.cast (t1);
}


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


/* Return TRUE if a CODE expression with operands of type TYPE is a boolean
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

bool
block_ranger::eval_logical (irange& r, range_stmt &stmt, const irange& lhs,
			    const irange& op1_true, const irange& op1_false,
			    const irange& op2_true,
			    const irange& op2_false) const
{
  gcc_checking_assert (logical_expr_p (stmt.get_code (),
				       TREE_TYPE (stmt.operand1 ())));
 
  /* If the LHS can be TRUE OR FALSE, then we cant really tell anything.  */
  if (!wi::eq_p (lhs.lower_bound(), lhs.upper_bound()))
    return false;

  /* Now combine based on the result.  */
  switch (stmt.get_code ())
    {

      /* A logical AND of two ranges is executed when we are walking forward
	 with ranges that have been determined.   x_8 is an unsigned char.
	       b_1 = x_8 < 20
	       b_2 = x_8 > 5
	       c_2 = b_1 && b_2
	 if we are looking for the range of x_8, the ranges on each side 
	 will be:   b_1 carries x_8 = [0, 19],   b_2 carries [6, 255]
	 the result of the AND is the intersection of the 2 ranges, [6, 255]. */
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  r = irange_intersect (op1_true, op2_true);
	else
	  {
	    irange ff = irange_intersect (op1_false, op2_false);
	    irange tf = irange_intersect (op1_true, op2_false);
	    irange ft = irange_intersect (op1_false, op2_true);
	    r = irange_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      /* A logical OR of two ranges is executed when we are walking forward with
	 ranges that have been determined.   x_8 is an unsigned char.
	       b_1 = x_8 > 20
	       b_2 = x_8 < 5
	       c_2 = b_1 || b_2
	 if we are looking for the range of x_8, the ranges on each side
	 will be:   b_1 carries x_8 = [21, 255],   b_2 carries [0, 4]
	 the result of the OR is the union_ of the 2 ranges, [0,4][21,255].  */
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  r = irange_intersect (op1_false, op2_false);
	else
	  {
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
   and resolve the outcome based on the logical operator.  */
bool
block_ranger::process_logical (range_stmt& stmt, irange& r, tree name,
		       const irange& lhs)
{
  range_stmt op_stmt;
  irange op1_range, op2_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;
  bool ret;

  irange bool_zero (boolean_type_node, 0, 0);
  irange bool_one (boolean_type_node, 1, 1);
  irange op1_true, op1_false, op2_true, op2_false;

  /* If the lhs is not a true or false constant, we can't tell anything
     about the arguments.  */
  if (lhs.range_for_type_p ())
    {
      r.set_range_for_type (TREE_TYPE (name));
      return true;
    }

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1 = stmt.operand1 ();
  op2 = stmt.operand2 ();

  op1_in_chain = gori.in_chain_p (name, op1);
  op2_in_chain = gori.in_chain_p (name, op2);

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
  op1_in_chain = gori.in_chain_p (name, op1);
  op2_in_chain = op2 && gori.in_chain_p (name, op2);

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

/*  ---------------------------------------------------------------------  */


block_ranger::block_ranger ()
{
  initialize_global_ssa_range_cache ();
}

block_ranger::~block_ranger ()
{
  destroy_global_ssa_range_cache ();
}

void
block_ranger::dump (FILE *f)
{

  if (!f)
    return;

  fprintf (f, "\nDUMPING GORI MAP\n");
  gori.dump (f);
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
  return gori.is_export_p (name, bb);
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
    {
      irange bool_true (boolean_type_node,  1 , 1);
      return get_range_from_stmt (stmt, r, name, bool_true);
    }

  if (e->flags & EDGE_FALSE_VALUE)
    {
      irange bool_false (boolean_type_node,  0 , 0);
      return get_range_from_stmt (stmt, r, name, bool_false);
    }

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
// -------------------------------------------------------------------------

ssa_block_ranges::ssa_block_ranges (tree t)
{
  irange tr;
  gcc_assert (TYPE_P (t));
  type = t;

  tab.create (0);
  tab.safe_grow_cleared (last_basic_block_for_fn (cfun));

  tr.set_range_for_type (t);
  type_range = irange_storage::ggc_alloc_init (tr);

  tab[ENTRY_BLOCK_PTR_FOR_FN (cfun)->index] = type_range;
}

ssa_block_ranges::~ssa_block_ranges ()
{
  tab.release ();
}

void
ssa_block_ranges::set_bb_range (const basic_block bb, const irange& r)
{
  irange_storage *m = tab[bb->index];

  if (m && m != type_range)
    m->set_irange (r);
  else
    m = irange_storage::ggc_alloc_init (r);

  tab[bb->index] = m;
}

void
ssa_block_ranges::set_bb_range_for_type (const basic_block bb)
{
  tab[bb->index] = type_range;
}


bool
ssa_block_ranges::get_bb_range (irange& r, const basic_block bb)
{
  irange_storage *m = tab[bb->index];
  if (m)
    {
      r.set_range (m, type);
      return true;
    }
  return false;
}


// Returns true if a range is present
bool
ssa_block_ranges::bb_range_p (const basic_block bb)
{
  return tab[bb->index] != NULL;
}

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

block_range_cache::block_range_cache ()
{
  ssa_ranges.create (0);
  ssa_ranges.safe_grow_cleared (num_ssa_names);
}

block_range_cache::~block_range_cache ()
{
  unsigned x;
  for (x = 0; x < ssa_ranges.length (); ++x)
    {
      if (ssa_ranges[x])
	delete ssa_ranges[x];
    }
  ssa_ranges.release ();
}

ssa_block_ranges&
block_range_cache::operator[] (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!ssa_ranges[v])
    ssa_ranges[v] = new ssa_block_ranges (TREE_TYPE (name));

  return *(ssa_ranges[v]);
}

void
block_range_cache::dump (FILE *f)
{
  unsigned x;
  for (x = 0; x < num_ssa_names; ++x)
    {
      if (ssa_ranges[x])
        {
	  fprintf (f, " Ranges for ");
	  print_generic_expr (f, ssa_name (x), 0);
	  fprintf (f, ":\n");
	  ssa_ranges[x]->dump (f);
	}
    }
  
}
// -------------------------------------------------------------------------

path_ranger::path_ranger () 
{
}

void
path_ranger::range_for_bb (irange &r, tree name, basic_block bb,
			   basic_block def_bb)
{
  bool res;
  determine_block (name, bb, def_bb);
  res = block_cache[name].get_bb_range (r, bb);
  gcc_assert (res);
}

bool
path_ranger::path_range_entry (irange& r, tree name, basic_block bb)
{
  gimple *def_stmt;
  basic_block def_bb;

  if (!valid_irange_ssa (name))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (name);
  if (!def_stmt)
    return false;

  def_bb = gimple_bb (def_stmt);;
  if (!def_bb)
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  /* Start with any known range.  */
  get_global_ssa_range (r, name);

  /* If its defined in this basic block, then there is no range on entry,
     otherwise, go figure out what is known in predecessor blocks.  */
  if (def_bb != bb)
    {
      irange block_range;
      range_for_bb (block_range, name, bb, def_bb);
      r.intersect (block_range);
    }

  return true;
}


/* Known range on an edge on the path back to the DEF of name.  */
bool
path_ranger::path_range_edge (irange& r, tree name, edge e)
{
  basic_block bb = e->src;

  if (!valid_irange_ssa (name))
    return false;

  gimple *stmt = SSA_NAME_DEF_STMT (name);
  if (stmt && gimple_bb (stmt) == e->src)
    {
      if (!path_range_of_def (r, stmt))
        get_global_ssa_range (r, name);
    }
  else
    /* Get the range for the def and possible basic block.  */
    if (!path_range_entry (r, name, bb))
      return false;

  /* Now intersect it with what we know about this edge.  */
  irange edge_range;
  if (range_on_edge (edge_range, name, e))
    {
      normalize_bool_type (edge_range, r);
      r.intersect (edge_range);
    }
  return true;

}

void
path_ranger::determine_block (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  irange er;
  irange block_result;

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == def_bb)
    return;

  /* If the block cache is set, then we've already visited this block.  */
  if (block_cache[name].bb_range_p (bb))
    return;

  /* Avoid infinite recursion by marking this block as calculated.  */
  block_cache[name].set_bb_range_for_type (bb);

  /* Visit each predecessor to reseolve them.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      determine_block (name, e->src, def_bb);
    }

  block_result.clear (TREE_TYPE (name));
  /* Now Union all the ranges on the incoming edges.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      irange pred_range;
      basic_block src = e->src;
      // Should be using range_on_def
      if (src == def_bb)
        get_global_ssa_range (pred_range, name);
      else
        {
	  bool res = block_cache[name].get_bb_range (pred_range, src);
	  gcc_assert (res);
	}

      if (range_on_edge (er, name, e))
        {
	  pred_range.intersect (er);
	  er.intersect (pred_range);
	}

      block_result.union_ (pred_range);

      if (block_result.range_for_type_p ())
        break;
    }

  if (block_result.range_for_type_p ())
    block_cache[name].set_bb_range_for_type (bb);
  else
    block_cache[name].set_bb_range (bb, block_result);
}

bool
path_ranger::path_range_stmt (irange& r, tree name, gimple *g)
{
#if 0
  if (is_a <gphi *> (g))
    {
      gphi *phi = as_a <gphi *> (g);
      tree phi_def = gimple_phi_result (phi);
      irange tmp;
      unsigned x;

      /* Only calculate ranges for PHI defs.  */
      if (phi_def != name)
        return false;

      r.clear (TREE_TYPE (name));

      for (x = 0; x < gimple_phi_num_args (phi); x++)
        {
	  bool res;
	  tree arg = gimple_phi_arg_def (phi, x);
	  if (TREE_CODE (arg) == SSA_NAME)
	    res = path_range_edge (tmp, arg, gimple_phi_arg_edge (phi, x));
	  else
	    res = get_operand_range (tmp, arg);
          if (res)
	    r.union_ (tmp);
	  else
	    {
	      r.set_range_for_type (name);
	      return false;
	    }
	  if (r.range_for_type_p ())
	    return true;
	}
      return true;

    }
#endif
  return range_on_stmt (r, name, g);
}


static inline bool
ssa_name_same_bb_p (tree name, basic_block bb)
{
  gimple *g = SSA_NAME_DEF_STMT (name);
  if (!g || gimple_bb (g) != bb)
   return false;
  return true;
}

bool
path_ranger::path_fold_stmt (irange &r, range_stmt &rn, basic_block bb, edge e)
{
  irange range_op1, range_op2;

  if (!rn.ssa_operand1 () || !ssa_name_same_bb_p (rn.ssa_operand1 (), bb) ||
      !path_range_of_def (range_op1, SSA_NAME_DEF_STMT (rn.ssa_operand1 ()), e))
    get_operand_range (range_op1, rn.operand1 ());

  // If this is a unary operation, call fold now.  
  if (!rn.operand2 ())
    return rn.fold (r, range_op1);

  if (!rn.ssa_operand2 () || !ssa_name_same_bb_p (rn.ssa_operand2 (), bb) ||
      !path_range_of_def (range_op2,
			  SSA_NAME_DEF_STMT (rn.ssa_operand2 ()), e))
    get_operand_range (range_op2, rn.operand2 ());

  normalize_bool_type (range_op1, range_op2);
  return rn.fold (r, range_op1, range_op2);
}

// Attempt to evaluate NAME within the basic block it is defined as far
// as possible. With an edge provided, we must do the calculation on demand
// since the global cache involves collating ALL the incoming edges.  This
// can potentially change all the values in the block.
bool
path_ranger::path_range_of_def (irange &r, gimple *g, edge e)
{
  if (!e)
    return path_range_of_def (r, g);

  basic_block bb = gimple_bb (g);
  tree arg;

  /* If an edge is provided, it must be an incoming edge to this BB.  */
  gcc_assert (e->dest == bb);

  // Note that since we are remaining within BB, we do not attempt to further
  // evaluate any of the arguments of a PHI at this point.
  // a recursive call could be made to evaluate any SSA_NAMEs on their
  // repsective edgesin PATH form, but we leave that as something to look into
  // later.  For the moment, just pick up any edge information since its cheap.
  if (is_a <gphi *> (g))
    {
      gphi *phi = as_a <gphi *> (g);
      gcc_assert (e->dest == bb);
      arg = gimple_phi_arg_def (phi, e->dest_idx);
      // Pick up anything simple we might know about the incoming edge. 
      if (!range_on_edge (r, arg, e))
	return get_operand_range (r, arg);
      return true;
    }

  range_stmt rn(g);
  if (!rn.valid())
    return false;

  return path_fold_stmt (r, rn, bb, e);

}

// Attempt to evaluate NAME within the basic block it is defined as far
// as possible. IF a PHI is encountered at the beginning of the block, either
// fully evalaute it, or if E is provided, use just the value from that edge.
bool
path_ranger::path_range_of_def (irange &r, gimple *g)
{
  tree name = gimple_get_lhs (g);
  basic_block bb = gimple_bb (g);
  tree arg;
  irange range_op1, range_op2;

  // Note that since we are remaining within BB, we do not attempt to further
  // evaluate any of the arguments of a PHI at this point.
  // a recursive call could be made to evaluate any SSA_NAMEs on their
  // repsective edgesin PATH form, but we leave that as something to look into
  // later.  For the moment, just pick up any edge information since its cheap.
  if (is_a <gphi *> (g))
    {
      gphi *phi = as_a <gphi *> (g);
      tree phi_def = gimple_phi_result (phi);
      irange tmp;
      unsigned x;
      edge e;

      if (get_global_ssa_range (r, phi_def))
        return true;

      // avoid infinite recursion by initializing global cache 
      r.set_range (phi_def);
      set_global_ssa_range (phi_def, r);

      r.clear (TREE_TYPE (phi_def));
      for (x = 0; x < gimple_phi_num_args (phi); x++)
        {
	  arg = gimple_phi_arg_def (phi, x);
	  e = gimple_phi_arg_edge (phi, x);
	  if (!path_range_edge (range_op2, arg, e))
	    if (!get_operand_range (range_op2, arg))
	      return false;

	  normalize_bool_type (r, range_op2);
	  r.union_ (range_op2);
	  if (r.range_for_type_p ())
	    return true;
	}

      set_global_ssa_range (phi_def, r);
      return true;
    }

  range_stmt rn(g);
  if (!rn.valid())
    return false;

  name = gimple_get_lhs (g);
  gcc_checking_assert (name);

  if (get_global_ssa_range (r, name))
    return true;

  // avoid infinite recursion by initializing global cache 
  r.set_range (name);
  set_global_ssa_range (name, r);

  bool res = path_fold_stmt (r, rn, bb);

  if (res)
    set_global_ssa_range (name, r);
  return res;
}

/* Calculate the known range for NAME on a path of basic blocks in
   BBS.  If such a range exists, store it in R and return TRUE,
   otherwise return FALSE.

   DIR is FORWARD if BBS[0] is the definition and the last block is
   the use.  DIR is REVERSE if the blocks are in reverse order.

   If there is an edge leading into this path that we'd like to take
   into account, such edge is START_EDGE.  Otherwise, START_EDGE is
   set to NULL.  */

bool
path_ranger::path_range (irange &r, tree name, const vec<basic_block> &bbs,
			 enum path_range_direction dir, edge start_edge)
{
  if (bbs.is_empty ())
    return false;

  /* If the first block defines NAME and it has meaningful range
     information, use it, otherwise fall back to range for type.

     Note: The first block may not always define NAME because we may
     have pruned the paths such that the first block (bb1) is just the
     first block that contains range info (bb99).  For example:

     bb1:
       x = 55;
       ...
       ...
     bb99:
       if (x > blah).
  */
  basic_block first_bb = dir == FORWARD ? bbs[0] : bbs[bbs.length () - 1];
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (gimple_bb (def_stmt) == first_bb && start_edge)
    {
      if (!path_range_of_def (r, def_stmt, start_edge))
	get_global_ssa_range (r, name);
    }
  else
    get_global_ssa_range (r, name);

  if (dir == REVERSE)
    return path_range_reverse (r, name, bbs);

  for (unsigned i = 1; i < bbs.length (); ++i)
    {
      edge e = find_edge (bbs[i - 1], bbs[i]);
      gcc_assert (e);
      irange redge;
      if (range_on_edge (redge, name, e))
	r.intersect (redge);
    }

  if (r.range_for_type_p ())
    return false;
  return true;
}

/* The same as above, but handle the case where BBS are a path of
   basic blocks in reverse order.

   BBS[0] is the USE of NAME.
   BBS[LEN-1] is the DEF of NAME.  */

bool
path_ranger::path_range_reverse (irange &r, tree name,
				 const vec<basic_block> &bbs)
{
  for (int i = bbs.length () - 1; i > 0; --i)
    {
      edge e = find_edge (bbs[i], bbs[i - 1]);
      gcc_assert (e);
      irange redge;
      if (range_on_edge (redge, name, e))
	r.intersect (redge);
    }

  if (r.range_for_type_p ())
    return false;
  return true;
}

void
path_ranger::dump(FILE *f)
{
  block_ranger::dump (f);
}


void
path_ranger::exercise (FILE *output)
{

  basic_block bb;
  irange range;

  FOR_EACH_BB_FN (bb, cfun)
    {
      unsigned x;
      edge_iterator ei;
      edge e;
      bool printed = false;

      /* This dramatically slows down builds, so only when printing.  */
      if (output)
        {
	  fprintf (output, "----- BB%d -----\n", bb->index);
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && path_range_entry (range, name, bb))
		{
		  if (output && !range.range_for_type_p ())
		    {
		      if (!printed)
			fprintf (output,"   Ranges on entry :\n");
		      printed = true;
		      fprintf (output, "     ");
		      print_generic_expr (output, name, 0);
		      fprintf (output, " : ");
		      range.dump (output);
		    }
		}
	    }
	  if (printed)
	    fprintf (output, "\n");
	  dump_bb (output, bb, 2, 0);
	  printed = false;
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = ssa_name (x);
	      if (name && range_p (bb, name))
		{
		  if (path_range_edge (range, name, e))
		    {
		      if (output && !range.range_for_type_p ())
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

  if (output)
    dump (output);

}

