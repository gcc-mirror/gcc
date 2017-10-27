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
#include "ssa-range-gen.h"
#include "ssa-range-stmt.h"
#include "domwalk.h"

//#define trace_output dump_file
#define trace_output ((FILE *)0)

//#define trace_path  dump_file
#define trace_path  ((FILE *)0)

 /* Given a logical STMT, calculate true and false for each potential path 
    and resolve the outcome based on the logical operator.  */
bool
gori::process_logical (range_stmt& stmt, irange& r, tree name,
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

  op1_in_chain = def_chain.in_chain_p (op1, name);
  op2_in_chain = def_chain.in_chain_p (op2, name);

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
  if (!ret || !stmt.logical_expr (r, lhs, op1_true, op1_false, op2_true,
				 op2_false))
    r.set_range_for_type (TREE_TYPE (name));
  return true;
}


/* Given the expression in STMT, return an evaluation in R for NAME.
   Returning false means the name being looked for is NOT resolvable, and
   can be removed from the GORI map to avoid future searches.  */
bool
gori::get_range (range_stmt& stmt, irange& r, tree name,
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
      if (get_operand_range (op2_range, op2))
	return stmt.op1_irange (r, lhs, op2_range, trace_output);
      else
        return false;
    }

  if (op2 == name)
    {
      if (get_operand_range (op1_range, op1))
	return stmt.op2_irange (r, lhs, op1_range, trace_output);
      else
        return false;
    }

  /* Check for boolean cases which require developing ranges and combining.  */
  if (stmt.logical_expr_p (TREE_TYPE (op1)))
    return process_logical (stmt, r, name, lhs);

  /* Reaching this point means NAME is not in this stmt, but one of the
     names in it ought to be derived from it.  */
  op1_in_chain = def_chain.in_chain_p (op1, name);
  op2_in_chain = def_chain.in_chain_p (op2, name);

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
      if (!stmt.op2_irange (op2_range, lhs, tmp_op1_range, trace_output))
        return false;
      if (!get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range))
        return false;
      if (!stmt.op1_irange (op1_range, lhs, op2_range, trace_output))
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
	if (!get_operand_range (op2_range, op2))
	  return false;
	if (!stmt.op1_irange (op1_range, lhs, op2_range, trace_output))
	  return false;
	return get_range_from_stmt (SSA_NAME_DEF_STMT (op1), r, name,
				    op1_range);
      }
    else
      if (!get_operand_range (op1_range, op1))
        return false;

  if (!stmt.op2_irange (op2_range, lhs, op1_range, trace_output))
    return false;
  return get_range_from_stmt (SSA_NAME_DEF_STMT (op2), r, name, op2_range);
}
 
/* Given the expression in STMT, return an evaluation in R for NAME. */
bool
gori::get_range_from_stmt (gimple *stmt, irange& r, tree name,
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


gori::gori ()
{
  gori_map.create (0);
  gori_map.safe_grow_cleared (last_basic_block_for_fn (cfun));
}

gori::~gori ()
{
  gori_map.release ();
}

/* Is the last stmt in a block interesting to look at for range info.  */

gimple *
gori::last_stmt_gori (basic_block bb)
{
  gimple *stmt;

  stmt = last_stmt (bb);
  if (stmt && gimple_code (stmt) == GIMPLE_COND)
    return stmt;
  return NULL;
}

void
gori::build (basic_block bb)
{
  gimple *stmt;

  /* Non-NULL means its already been processed.  */
  if (gori_map[bb->index])
    return;

  gori_map[bb->index] = BITMAP_ALLOC (NULL);

  /* Look for a branch of some sort at the end of the block, and build the
     GORI map and definition dependcies for it.  */
  stmt = last_stmt_gori (bb);
  if (stmt)
    {
      range_stmt rn (stmt);
      if (rn.valid())
	{
	  tree ssa1 = rn.ssa_operand1 ();
	  tree ssa2 = rn.ssa_operand2 ();
	  if (ssa1)
	    {
	      bitmap_copy (gori_map[bb->index],
			   def_chain[SSA_NAME_VERSION (ssa1)]);
	      bitmap_set_bit (gori_map[bb->index], SSA_NAME_VERSION (ssa1));
	    }
	  if (ssa2)
	    {
	      bitmap_ior_into (gori_map[bb->index],
			       def_chain[SSA_NAME_VERSION (ssa2)]);
	      bitmap_set_bit (gori_map[bb->index], SSA_NAME_VERSION (ssa2));
	    }
	  return;
	}
    }
}


void
gori::build ()
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      build (bb);
    }
}

void
gori::dump (FILE *f)
{
  basic_block bb;
  bitmap_iterator bi;
  unsigned y;

  if (!f)
    return;

  fprintf (f, "\nDUMPING DEF_CHAIN\n");
  def_chain.dump (f);

  fprintf (f, "\nDUMPING GORI MAP\n");
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (gori_map[bb->index])
        {
          fprintf (f, "basic block %3d  : ", bb->index);
	  EXECUTE_IF_SET_IN_BITMAP (gori_map[bb->index], 0, y, bi)
	    {
	      print_generic_expr (f, ssa_name (y), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }
  fprintf (f, "\n");

}

bool 
gori::remove_from_gori_map (basic_block bb, tree name)
{
  bitmap_clear_bit (gori_map[bb->index], SSA_NAME_VERSION (name));
  return false;
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
   
bool
gori::get_derived_range_stmt (range_stmt& stmt, tree name, basic_block bb)
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



bool
gori::range_p (basic_block bb, tree name)
{
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);

  if (!gori_map[bb->index])
    build (bb);

  return bitmap_bit_p (gori_map[bb->index], SSA_NAME_VERSION (name));
}


/* Known range on an edge.  */
bool
gori::range_on_edge (irange& r, tree name, edge e)
{
  gimple *stmt;
  basic_block bb = e->src;

  if (!irange_ssa (name))
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
gori::exercise (FILE *output)
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
gori::range_on_stmt (irange& r, tree name, gimple *g)
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
gori::range_of_def (irange& r, gimple *g)
{
  range_stmt rn (g);

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  return rn.fold (r);
}

bool
gori::range_of_def (irange& r, gimple *g, tree name,
		    const irange& range_of_name)
{
  range_stmt rn (g);

  /* If we don't understand the stmt... */
  if (!rn.valid())
    return false;
  
  return rn.fold (r, name, range_of_name);
}
// -------------------------------------------------------------------------

ssa_range_cache::ssa_range_cache (tree t)
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

ssa_range_cache::~ssa_range_cache ()
{
  tab.release ();
}

void
ssa_range_cache::set_range (const basic_block bb, const irange& r)
{
  irange_storage *m = tab[bb->index];

  if (m)
    m->set_irange (r);
  else
    m = irange_storage::ggc_alloc_init (r);

  tab[bb->index] = m;
}

void
ssa_range_cache::set_range_for_type (const basic_block bb)
{
  tab[bb->index] = type_range;
}


bool
ssa_range_cache::get_range (irange& r, const basic_block bb)
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
ssa_range_cache::range_p (const basic_block bb)
{
  return tab[bb->index] != NULL;
}

void
ssa_range_cache::dump (FILE *f)
{
  basic_block bb;
  irange r;

  FOR_EACH_BB_FN (bb, cfun)
    if (get_range (r, bb))
      {
	fprintf (f, "BB%d  -> ", bb->index);
	r.dump (f);
      }
}

// -------------------------------------------------------------------------

range_cache::range_cache ()
{
  ssa_ranges.create (0);
  ssa_ranges.safe_grow_cleared (num_ssa_names);
}

range_cache::~range_cache ()
{
  unsigned x;
  for (x = 0; x < num_ssa_names; ++x)
    {
      if (ssa_ranges[x])
	delete ssa_ranges[x];
    }
  ssa_ranges.release ();
}

ssa_range_cache&
range_cache::operator[] (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!ssa_ranges[v])
    ssa_ranges[v] = new ssa_range_cache (TREE_TYPE (name));

  return *(ssa_ranges[v]);
}

void
range_cache::dump (FILE *f)
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
  res = block_cache[name].get_range (r, bb);
  gcc_assert (res);
}

bool
path_ranger::path_range_entry (irange& r, tree name, basic_block bb)
{
  gimple *def_stmt;
  basic_block def_bb;

  if (!irange_ssa (name))
    return false;

  def_stmt = SSA_NAME_DEF_STMT (name);
  if (!def_stmt)
    return false;

  def_bb = gimple_bb (def_stmt);;
  if (!def_bb)
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  if (trace_path)
    {
      fprintf (dump_file, "path_range_entry BB%d on path for ", bb->index);
      print_generic_expr (dump_file, name, 0);
      fprintf (dump_file,"\n");
    }

  /* Start with any known range.  */
  r.set_range (name);

  /* If its defined in this basic block, then there is no range on entry,
     otherwise, go figure out what is known in predecessor blocks.  */
  if (def_bb != bb)
    {
      irange block_range;
      range_for_bb (block_range, name, bb, def_bb);
      r.intersect (block_range);
    }

  if (trace_path)
    {
      fprintf (dump_file, "range on entry query for");
      print_generic_expr (dump_file, name, 0);
      fprintf (dump_file, " in BB%d : returns ",bb->index);
      r.dump (dump_file);
      block_cache.dump (dump_file);
      fprintf(dump_file, "\n");
    }

  return true;
}


/* Known range on an edge on the path back to the DEF of name.  */
bool
path_ranger::path_range_edge (irange& r, tree name, edge e)
{
  basic_block bb = e->src;

  /* Get the range for the def and possible basic block.  */
  if (!path_range_entry (r, name, bb))
    return false;

  /* Now intersect it with what we know about this edge.  */
  irange edge_range;
  if (range_on_edge (edge_range, name, e))
    r.intersect (edge_range);
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

  if (trace_path)
    fprintf (dump_file, "determine_block for BB%d\n", bb->index);

  /* If the block cache is set, then we've already visited this block.  */
  if (block_cache[name].range_p (bb))
    return;

  /* Avoid infinite recursion by marking this block as calculated.  */
  block_cache[name].set_range_for_type (bb);

  if (trace_path)
    fprintf (dump_file, "Visiting preds of BB%d\n", bb->index);

  /* Visit each predecessor to reseolve them.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      determine_block (name, e->src, def_bb);
    }

  if (trace_path)
    fprintf (dump_file, "Re-Visiting preds of BB%d\n", bb->index);

  block_result.clear (TREE_TYPE (name));
  /* Now Union all the ranges on the incoming edges.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      irange pred_range;
      basic_block src = e->src;
      if (trace_path)
        fprintf (dump_file, " processing pred BB%d ", src->index);

      // Should be using range_on_def
      if (src == def_bb)
        pred_range.set_range (name);
      else
        {
	  bool res = block_cache[name].get_range (pred_range, src);
	  gcc_assert (res);
	}

      if (trace_path)
	{
	  fprintf (dump_file, " No range on edge  pred block range is: ");
	  pred_range.dump (dump_file);
	}

      if (range_on_edge (er, name, e))
        {
	  if (trace_path)
	    {
	      fprintf (dump_file, " edge has range : ");
	      er.dump (dump_file);
	    }
	  pred_range.intersect (er);
	  er.intersect (pred_range);
	  if (trace_path)
	    {
	      fprintf (dump_file, " Edge intersect pred : ");
	      pred_range.dump (dump_file);
	    }
	}

      block_result.union_ (pred_range);

      if (trace_path)
	{
	  fprintf (dump_file, " result union range : ");
	  block_result.dump (dump_file);
	}
      if (block_result.range_for_type_p ())
        break;
    }

  if (trace_path)
    {
      fprintf (dump_file, "Finished processing preds.  final result: ");
      block_result.dump (dump_file);
    }
  if (block_result.range_for_type_p ())
    block_cache[name].set_range_for_type (bb);
  else
    block_cache[name].set_range (bb, block_result);
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

/* Calculate the known range for NAME on a path of basic blocks in
   BBS.  If such a range exists, store it in R and return TRUE,
   otherwise return FALSE.  */

bool
path_ranger::path_range (irange &r, tree name, const vec<basic_block> &bbs)
{
  /* Handle paths in which the blocks are reversed.  */
  if (bbs.length () > 1 && find_edge (bbs[1], bbs[0]))
    return path_range_reverse (r, name, bbs);

  /* ?? Should we start with a known range for NAME?  */
  r.set_range_for_type (TREE_TYPE (name));

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
   basic blocks in reverse order.  */

bool
path_ranger::path_range_reverse (irange &r, tree name,
				 const vec<basic_block> &bbs)
{
  /* ?? Should we start with a known range for NAME?  */
  r.set_range_for_type (TREE_TYPE (name));

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
  gori::dump (f);
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

