/* On-demand ssa range generator.
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
#include "domwalk.h"
#include "ssa-range.h"
#include "ssa-range-global.h"


// Internally, the range operators all use boolen_type_node when comparisons
// and such are made to create ranges for logical operations.
// some languages, such as fortran, may use boolean types with different
// precisions and these are incompatible.   This routine will look at the 
// 2 ranges, and if there is a mismatch between the boolean types, will change
// the range generated from the default node to the other type.
//
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

