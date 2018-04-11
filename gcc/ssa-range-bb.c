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
  bitmap b;

  if (!rn.valid ())
    return;

  tree ssa1 = rn.ssa_operand1 ();
  tree ssa2 = rn.ssa_operand2 ();

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


// Internally, the range operators all use boolen_type_node when comparisons
// and such are made to create ranges for logical operations.
// some languages, such as fortran, may use boolean types with different
// precisions and these are incompatible.   This routine will look at the 
// 2 ranges, and if there is a mismatch between the boolean types, will change
// the range generated from the default node to the other type.
//
void
block_ranger::normalize_bool_type (irange& r1, irange& r2)
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

/* This routine will return what  is globally known about the range for an
   operand of any kind.  */
bool
block_ranger::get_operand_range (irange& r, tree op,
				 gimple *s ATTRIBUTE_UNUSED)
{
  /* This check allows unary operations to be handled without having to 
     make an explicit check for the existence of a second operand.  */
  if (!op)
    return false;

  if (TREE_CODE (op) == INTEGER_CST)
    r.set_range (TREE_TYPE (op), op, op);
  else
    if (TREE_CODE (op) == SSA_NAME)
      r = op;
    else
      if (TYPE_P (op))
	r.set_range_for_type (op);
      else
        /* Default to range for the type of the expression.   */
	r.set_range_for_type (TREE_TYPE (op));

  return true;
}


/* Given a logical STMT, calculate true and false for each potential path 
   using NAME and resolve the outcome based on the logical operator.  */
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
	  ret &= get_operand_range (op2_true, name, stmt);
	  ret &= get_operand_range (op2_false, name, stmt);
	}
    }

  if (!ret || !stmt.fold_logical (r, lhs, op1_true, op1_false, op2_true,
				  op2_false))
    r.set_range_for_type (TREE_TYPE (name));
  return true;
}


/* Given the expression in STMT, return an evaluation in R for NAME.
   Returning false means the name being looked for was NOT resolvable.  */
bool
block_ranger::get_range_from_stmt (range_stmt stmt, irange& r, tree name,
				   const irange& lhs)
{
  irange op1_range, op2_range, tmp_range;
  tree op1, op2;
  bool op1_in_chain, op2_in_chain;

  if (!stmt.valid ())
    return false;

  if (lhs.empty_p ())
    {
      r.clear (TREE_TYPE (name));
      return true;
    }

  op1 = stmt.operand1 ();
  op2 = stmt.operand2 ();

  if (op1 == name)
    { 
      if (!op2)
        return stmt.op1_irange (r, lhs);
      if (get_operand_range (op2_range, op2, stmt))
	return stmt.op1_irange (r, lhs, op2_range);
      else
        return false;
    }

  if (op2 == name)
    {
      if (get_operand_range (op1_range, op1, stmt))
	return stmt.op2_irange (r, lhs, op1_range);
      else
        return false;
    }

  /* Check for boolean cases which require developing ranges and combining.  */
  if (stmt.logical_expr_p ())
    return process_logical (stmt, r, name, lhs);

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1_in_chain = gori->in_chain_p (name, op1);
  op2_in_chain = op2 && gori->in_chain_p (name, op2);

  /* If neither operand is derived, then this stmt tells us nothing. */
  if (!op1_in_chain && !op2_in_chain)
    return false;

  /* Pick up an operand range for each argument.  */
  if (!get_operand_range (op1_range, op1, stmt))
    return false;
  if (op2 && !get_operand_range (op2_range, op2, stmt))
    return false;

  /* Can't resolve both sides at once, so take a guess at operand 1, calculate
     operand 2 and check if the guess at operand 1 was good.  */
  if (op1_in_chain && op2_in_chain)
    {
      // Get an op2_range based on the op1 range. 
      if (!stmt.op2_irange (tmp_range, lhs, op1_range))
        return false;
      // And combine it with the raw possibilty.
      normalize_bool_type (op2_range, tmp_range);
      op2_range.intersect (tmp_range);
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range))
        return false;

      // Now the same for operand 1
      if (!stmt.op1_irange (tmp_range, lhs, op2_range))
        return false;
      normalize_bool_type (op1_range, tmp_range);
      op1_range.intersect (tmp_range);
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name, op1_range))
        return false;
      // Do we need to possibly reevaluate op2?
      return true;
    }
  else
    if (op1_in_chain)
      {
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
	normalize_bool_type (op1_range, tmp_range);
	op1_range.intersect (tmp_range);
	return get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name,
				    op1_range);
      }
    else

  if (!stmt.op2_irange (tmp_range, lhs, op1_range))
    return false;
  normalize_bool_type (op2_range, tmp_range);
  op2_range.intersect (tmp_range);
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range);
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


/* Attempt to evaluate the epression using whatever is globally known about
   the operands.  If it can be evaluated, TRUE is returned
   and the range is returned in R.  */

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

/* This method will attempt to evaluate the expression by replacing any
   occurrence of ssa_name NAME with the range NAME_RANGE. If it can be
   evaluated, TRUE is returned and the resulting range returned in R.  */
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

