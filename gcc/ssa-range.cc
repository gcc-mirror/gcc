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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "dbgcnt.h"

// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.

bool
gimple_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res = false;
  // If name is specified, make sure it is a LHS of S.
  gcc_checking_assert (name ? SSA_NAME_DEF_STMT (name) == s : true);

  if (gimple_range_handler (s))
    res = range_of_range_op (r, s);
  else if (is_a<gphi *>(s))
    res = range_of_phi (r, as_a<gphi *> (s));
  else if (is_a<gcall *>(s))
    res = range_of_call (r, as_a<gcall *> (s));
  else if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    res = range_of_cond_expr (r, as_a<gassign *> (s));
  else
    {
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
      r = gimple_range_global (name);
      return true;
    }
  if (res)
    {
      if (r.undefined_p ())
	return true;
      if (name && TREE_TYPE (name) != r.type ())
	range_cast (r, TREE_TYPE (name));
      return true;
    }
  return false;
}


// Calculate a range for NAME on edge E and return it in R.
// Return false if no range can be determined.

void
gimple_ranger::range_on_edge (irange &r, edge e, tree name)
{
  widest_irange edge_range;
  gcc_checking_assert (irange::supports_type_p (TREE_TYPE (name)));

  // PHI arguments can be constants, catch these here.
  if (!gimple_range_ssa_p (name))
    {
      gcc_assert (range_of_expr (r, name));
      return;
    }

  range_on_exit (r, e->src, name);
  gcc_checking_assert  (r.undefined_p ()
			|| types_compatible_p (r.type(), TREE_TYPE (name)));

  // Check to see if NAME is defined on edge e.
  if (outgoing_edge_range_p (edge_range, e, name, &r))
    r = edge_range;
}

// Return the range for NAME on entry to block BB in R.
// At the statement level, this amounts to whatever the global value is.

void
gimple_ranger::range_on_entry (irange &r, basic_block bb ATTRIBUTE_UNUSED,
			       tree name)
{
  range_of_ssa_name (r, name);
}


// Return the range for NAME on exit from block BB in R.
// At the statement level, this amounts to whatever the global value is.

void
gimple_ranger::range_on_exit (irange &r, basic_block bb ATTRIBUTE_UNUSED,
			      tree name)
{
  range_of_ssa_name (r, name);
}


// Calculate a range for range_op statement S and return it in R.  If any
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_range_op (irange &r, gimple *s)
{
  widest_irange range1, range2;
  tree type = gimple_expr_type (s);
  gcc_checking_assert (irange::supports_type_p (type));

  tree op1 = gimple_range_operand1 (s);
  tree op2 = gimple_range_operand2 (s);

  if (range_of_expr (range1, op1, s))
    {
      if (!op2)
	return gimple_range_fold (s, r, range1);

      if (range_of_expr (range2, op2, s))
	return gimple_range_fold (s, r, range1, range2);
    }
  r.set_varying (type);
  return true;
}


// Calculate a range for phi statement S and return it in R.
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_phi (irange &r, gphi *phi)
{
  tree phi_def = gimple_phi_result (phi);
  tree type = TREE_TYPE (phi_def);
  widest_irange phi_range;
  unsigned x;

  if (!irange::supports_type_p (type))
    return false;

  // And start with an empty range, unioning in each argument's range.
  r.set_undefined ();
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      widest_irange arg_range;
      tree arg = gimple_phi_arg_def (phi, x);
      edge e = gimple_phi_arg_edge (phi, x);

      range_on_edge (arg_range, e, arg);
      r.union_ (arg_range);
      // Once the value reaches varying, stop looking.
      if (r.varying_p ())
	break;
    }

  return true;
}


// Calculate a range for call statement S and return it in R.
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_call (irange &r, gcall *call)
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


// Calculate a range for COND_EXPR statement S and return it in R.
// If a range cannot be calculated, return false.

bool
gimple_ranger::range_of_cond_expr  (irange &r, gassign *s)
{
  widest_irange cond_range, range1, range2;
  tree cond = gimple_assign_rhs1 (s);
  tree op1 = gimple_assign_rhs2 (s);
  tree op2 = gimple_assign_rhs3 (s);

  gcc_checking_assert (gimple_assign_rhs_code (s) == COND_EXPR);
  gcc_checking_assert (useless_type_conversion_p  (TREE_TYPE (op1),
						   TREE_TYPE (op2)));
  if (!irange::supports_type_p (TREE_TYPE (op1)))
    return false;

  gcc_assert (range_of_expr (cond_range, cond, s));
  gcc_assert (range_of_expr (range1, op1, s));
  gcc_assert (range_of_expr (range2, op2, s));

  // If the condition is known, choose the appropriate expression.
  if (cond_range.singleton_p ())
    {
      // False, pick second operand
      if (cond_range.zero_p ())
	r = range2;
      else
	r = range1;
    }
  else
    {
      r = range1;
      r.union_ (range2);
    }
  return true;
}


// ------------------------------------------------------------------------


// Construct a global_ranger object.

global_ranger::global_ranger ()
{
  m_workback.create (0);
  m_workback.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.truncate (0);
}

// Destruct a global_ranger object.

global_ranger::~global_ranger ()
{
  m_workback.release ();
  m_update_list.release ();
}

// Return true if NAME has a non-null dereference in block BB.

bool
global_ranger::non_null_deref_p (tree name, basic_block bb)
{
  return m_non_null.non_null_deref_p (name, bb);
}

void
global_ranger::dump_block (FILE *f, basic_block bb)
{
  m_on_entry.dump (f, bb);
}


// Return the range of NAME on entry to block BB in R.

void
global_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  widest_irange entry_range;
  gcc_checking_assert (gimple_range_ssa_p (name));

  // Start with any known range
  gcc_assert (range_of_stmt (r, SSA_NAME_DEF_STMT (name), name));

  // Now see if there is any on_entry value which may refine it .
  if (block_range (entry_range, bb, name))
    r.intersect (entry_range);
}


// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

void
global_ranger::range_on_exit (irange &r, basic_block bb, tree name)
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
  gcc_checking_assert (r.undefined_p ()
		       || types_compatible_p (r.type(), TREE_TYPE (name)));
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
	return gimple_ranger::range_of_stmt (r, s, name);
    }

  gcc_checking_assert (TREE_CODE (name) == SSA_NAME &&
		       irange::supports_type_p (TREE_TYPE (name)));

  // If this STMT has already been processed, return that value.
  if (m_globals.get_global_range (r, name))
    return true;
 
  // Avoid infinite recursion by initializing global cache
  widest_irange tmp = gimple_range_global (name);
  m_globals.set_global_range (name, tmp);

  gcc_assert (gimple_ranger::range_of_stmt (r, s, name));

  if (is_a<gphi *> (s))
    r.intersect (tmp);
  m_globals.set_global_range (name, r);
  return true;
}


// Determine a range for OP on stmt S, returning the result in R.
// If OP is not defined in BB, find the range on entry to this block.

void
global_ranger::range_of_ssa_name (irange &r, tree name, gimple *s)
{
  // If there is no statement, just get the global value.
  if (!s)
    {
      gimple_ranger::range_of_ssa_name (r, name);
      return;
    }

  basic_block bb = gimple_bb (s);
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);

  // If name is defined in this block, try to get an range from S.
  if (def_stmt && gimple_bb (def_stmt) == bb)
    gcc_assert (range_of_stmt (r, def_stmt, name));
  else
    // Otherwise OP comes from outside this block, use range on entry.
    range_on_entry (r, bb, name);

  // No range yet, see if there is a dereference in the block.
  // We don't care if it's between the def and a use within a block
  // because the entire block must be executed anyway.
  // FIXME:?? For non-call exceptions we could have a statement throw
  // which causes an early block exit.
  // in which case we may need to walk from S back to the def/top of block
  // to make sure the deref happens between S and there before claiming
  // there is a deref.   Punt for now.
  if (!cfun->can_throw_non_call_exceptions && r.varying_p () &&
      non_null_deref_p (name, bb))
    r = range_nonzero (TREE_TYPE (name));
}



bool
global_ranger::range_from_import (irange &r, tree name, irange &import_range)
{
  widest_irange r1, r2;
  bool res = true;
  tree import = m_gori_map.terminal_name (name);

  // This probably means the IL has changed underneath... just return false
  // until we have a more comprehensive solution
  if (!import || (import_range.undefined_p () ||
		  useless_type_conversion_p (TREE_TYPE (import),
					     import_range.type ())))
    return false;

  // Only handling range_ops until we find a cond-expr that matters.
  // We process this specially so we can handle self-referencing chains. ie:
  //   b_3 = b_1 + 10
  //   b_4 = b_3 + b_1  // b_4 = b_1 * 2 + 10 really
  //   if (b_4 < 20)
  //
  // import b_1 = [0,0]
  // we want to make sure b_4 evaluates both b_3 and b_1 with this import value
  // Due to the nature of def chains, there can only be one import in the chain.
  // its possible 2 different chains occur in one stmt, ie:
  // if (b_4 < d_6), but there is no DEF for this stmt, so it can't happen.
  // f_5 = b_4 + d_6 would have no import since there are 2 symbolics.

  gimple *s = SSA_NAME_DEF_STMT (name);
  if (!s || !gimple_range_handler (s))
    return false;

  tree op1 = gimple_range_operand1 (s);
  tree op2 = gimple_range_operand2 (s);

  // Evaluate op1
  if (gimple_range_ssa_p (op1))
    {
      if (op1 == import)
	r1 = import_range;
      else
	res = range_from_import (r1, op1, import_range);
    }
  else
    gcc_assert (range_of_expr (r1, op1));

  if (!res)
    return false;
  if (!op2)
    return gimple_range_fold (s, r, r1);

  // Now evaluate op2.
  if (gimple_range_ssa_p (op2))
    {
      if (op2 == import)
	r2 = import_range;
      else
	res = range_from_import (r2, op2, import_range);
    }
  else
    gcc_assert (range_of_expr (r2, op2));

  if (res)
    return gimple_range_fold (s, r, r1, r2);

  return false;
}



// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO fields.

void
global_ranger::export_global_ranges ()
{
  unsigned x;
  widest_irange r;
  if (dump_file)
    {
      fprintf (dump_file, "Exported global range table\n");
      fprintf (dump_file, "===========================\n");
    }

  for ( x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (name && !SSA_NAME_IN_FREE_LIST (name) &&
	  gimple_range_ssa_p (name) && m_globals.get_global_range (r, name) &&
	  !r.varying_p())
	{
	  // Make sure the new range is a subset of the old range.
	  widest_irange old_range;
	  old_range = gimple_range_global (name);
	  old_range.intersect (r);
	  /* Disable this while we fix tree-ssa/pr61743-2.c.  */
	  //gcc_checking_assert (old_range == r);

	  // WTF? Can't write non-null pointer ranges?? stupid set_range_info!
	  if (!POINTER_TYPE_P (TREE_TYPE (name)) && !r.undefined_p ())
	    {
	      if (!dbg_cnt (ranger_export_count))
		return;

	      value_range vr = r;
	      set_range_info (name, vr);
	      if (dump_file)
		{
		  print_generic_expr (dump_file, name , TDF_SLIM);
		  fprintf (dump_file, " --> ");
		  vr.dump (dump_file);
		  fprintf (dump_file, "\n");
		  fprintf (dump_file, "         irange : ");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
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
      widest_irange range;
      fprintf (f, "\n=========== BB %d ============\n", bb->index);
      dump_block (f, bb);

      dump_bb (f, bb, 4, TDF_NONE);

      // Now find any globals defined in this block
      for (x = 1; x < num_ssa_names; x++)
	{
	  tree name = ssa_name (x);
	  if (gimple_range_ssa_p (name) && SSA_NAME_DEF_STMT (name) &&
	      gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
	      m_globals.get_global_range (range, name))
	    {
	      if (!range.varying_p ())
	       {
		 print_generic_expr (f, name, TDF_SLIM);
		 fprintf (f, " : ");
		 range.dump (f);
		 fprintf (f, "\n");
	       }

	    }
	}

      // And now outgoing edges, if they define anything.
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  for (x = 1; x < num_ssa_names; x++)
	    {
	      tree name = gimple_range_ssa_p (ssa_name (x));
	      if (name && outgoing_edge_range_p (range, e, name))
		{
		  gimple *s = SSA_NAME_DEF_STMT (name);
		  // Only print the range if this is the def block,
		  // or the on entry cache for either end of the edge is set.
		  if ((s && bb == gimple_bb (s)) ||
		      block_range (range, bb, name, false) ||
		      block_range (range, e->dest, name, false))
		    {
		      range_on_edge (range, e, name);
		      if (!range.varying_p ())
			{
			  fprintf (f, "%d->%d ", e->src->index,
				   e->dest->index);
			  char c = (m_gori_map.is_export_p (name, bb) ? ' ' : '*');
			  if (e->flags & EDGE_TRUE_VALUE)
			    fprintf (f, " (T)%c", c);
			  else if (e->flags & EDGE_FALSE_VALUE)
			    fprintf (f, " (F)%c", c);
			  else
			    fprintf (f, "     ");
			  print_generic_expr (f, name, TDF_SLIM);
			  fprintf(f, " : \t");
			  range.dump(f);
			  fprintf (f, "\n");
			}
		    }
		}
	    }
	}
    }

  m_globals.dump (dump_file);
  fprintf (f, "\n");

  if (dump_flags & TDF_DETAILS)
    {
      fprintf (f, "\nDUMPING GORI MAP\n");
      m_gori_map.dump (f);
      fprintf (f, "\n");
    }
}

// Calculate all ranges by visiting every block and asking for the range of
// each ssa_name on each statement, and then dump those ranges to OUTPUT.

void
global_ranger::calculate_and_dump (FILE *output)
{
  basic_block bb;
  widest_irange r;

  //  Walk every statement asking for a range.
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator gpi = gsi_start_phis (bb); !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  tree phi_def = gimple_phi_result (phi);
	  if (gimple_range_ssa_p (phi_def))
	    gcc_assert (range_of_stmt (r, phi));
	}

      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  ssa_op_iter iter;
	  use_operand_p use_p;

	  // Calculate a range for the LHS if there is one.
	  if (gimple_range_ssa_p (gimple_get_lhs (stmt)))
	    range_of_stmt (r, stmt);
	  // and make sure to query every operand.
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	    {
	      tree use = gimple_range_ssa_p (USE_FROM_PTR (use_p));
	      if (use)
		range_of_expr (r, use, stmt);
	    }
	}
    }
  // The dump it.
  dump (output);
  fprintf (output, "\n");
}

// Return a static range for NAME on entry to basic block BB in R.
// If calc is true, Fill any cache entries required between BB and the def
// block for NAME.  Otherwise, return false if the cache is empty.

bool
global_ranger::block_range (irange &r, basic_block bb, tree name, bool calc)
{

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = NULL;

  gcc_checking_assert (gimple_range_ssa_p (name));

  if (calc)
    {
      if (def_stmt)
	def_bb = gimple_bb (def_stmt);;
      if (!def_bb)
	{
	  // IF we get to the entry block, this better be a default def
	  // or range_on_entry was called fo a block not dominated by
	  // the def.  This would be a bug.
	  gcc_checking_assert (SSA_NAME_IS_DEFAULT_DEF (name));
	  def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
	}

      // There is no range on entry for the defintion block.
      if (def_bb == bb)
	return false;

      // Otherwise, go figure out what is known in predecessor blocks.
      fill_block_cache (name, bb, def_bb);
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
    }
  return m_on_entry.get_bb_range (r, name, bb);
}


// Return the static range for NAME on edge E in R. If there is no
// range-on-entry cache for E->src, then return false.
// If this is the def block, then see if the DEF can be evaluated with them
// import name, otherwise use varying as the range.
// If there is any outgoing range information on edge E, incorporate it
// into the results.

bool
global_ranger::edge_range (irange &r, edge e, tree name)
{
  basic_block src = e->src;
  widest_irange er, tmp;
  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = ((s && gimple_bb (s)) ? gimple_bb (s) :
					       ENTRY_BLOCK_PTR_FOR_FN (cfun));

  if (src == def_bb)
    {
      // Check to see if the import has a cache_entry, and if it does use that
      // in an evaluation to get a static starting value.
      // The import should have a range if the global range is requested
      // before any other lookups.
      tree term = (has_edge_range_p (e, name) ? m_gori_map.terminal_name (name)
		   : NULL_TREE);
      if (!term || !(m_on_entry.get_bb_range (tmp, term, src) &&
		     range_from_import (r, name, tmp)))
	{
	  // Try to pick up any known value first.
	  if (!m_globals.get_global_range (r, name))
	    r = gimple_range_global (name);
	}
    }
  else
    if (!m_on_entry.get_bb_range (r, name, src))
      return false;

  // Check if pointers have any non-null dereferences.
  // Non-call exceptions mean we could throw in the middle of he block,
  // so just punt for now on those.
  if (r.varying_p () && m_non_null.non_null_deref_p (name, src) &&
      !cfun->can_throw_non_call_exceptions)
    r = range_nonzero (TREE_TYPE (name));

  if (outgoing_edge_range_p (er, e, name, &r))
    r = er;
  return true;
}

void
global_ranger::add_to_update (basic_block bb)
{
  if (!m_update_list.contains (bb))
    m_update_list.quick_push (bb);
}

#define DEBUG_CACHE (0 && dump_file)

// If there is anything in the iterative update_list, continue processing NAME
// until the list of blocks is empty.

void
global_ranger::iterative_cache_update (tree name)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  widest_irange new_range;
  widest_irange current_range;
  widest_irange e_range;

  // Process each block by seeing if it's calculated range on entry is the same
  // as it's cached value. IF there is a difference, update the cache to
  // reflect the new value, and check to see if any successors have cache
  // entries which may need to be checked for updates.

  while (m_update_list.length () > 0)
    {
      bb = m_update_list.pop ();
if (DEBUG_CACHE) fprintf (dump_file, "FWD visiting block %d\n", bb->index);

      gcc_assert (m_on_entry.get_bb_range (current_range, name, bb));
      // Calculate the "new" range on entry by unioning the pred edges..
      new_range.set_undefined ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  gcc_assert (edge_range (e_range, e, name));
	  new_range.union_ (e_range);
	  if (new_range.varying_p ())
	    break;
	}
      // If the range on entry has changed, update it.
      if (new_range != current_range)
	{
if (DEBUG_CACHE) { fprintf (dump_file, "updating range from/to "); current_range.dump (dump_file); new_range.dump (dump_file); }
	  m_on_entry.set_bb_range (name, bb, new_range);
	  // Mark each successor that has a range to re-check it's range
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (m_on_entry.bb_range_p (name, e->dest))
	      add_to_update (e->dest);
	}
    }
if (DEBUG_CACHE) fprintf (dump_file, "DONE visiting blocks \n\n");
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back thourgh the CFG to DEF_BB ensuring the range is calculated
// on the block/edges leading back to that point.

void
global_ranger::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  widest_irange block_result;
  widest_irange undefined;

  // At this point we shouldnt be looking at the def, entry or exit block.
  gcc_checking_assert (bb != def_bb && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) &&
		       bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  // If the block cache is set, then we've already visited this block.
  if (m_on_entry.bb_range_p (name, bb))
    return;

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.truncate (0);
  m_workback.quick_push (bb);
  undefined.set_undefined ();
  m_on_entry.set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update_list.length () == 0);

if (DEBUG_CACHE) { fprintf (dump_file, "\n"); print_generic_expr (dump_file, name, TDF_SLIM); fprintf (dump_file, " : "); }

  while (m_workback.length () > 0)
    {
      basic_block node = m_workback.pop ();
if (DEBUG_CACHE)  fprintf (dump_file, "BACK visiting block %d\n", node->index);

      FOR_EACH_EDGE (e, ei, node->preds)
        {
	  basic_block pred = e->src;
	  widest_irange r;
	  // If the pred block is the def block add this BB to update list.
	  if (pred == def_bb)
	    {
	      add_to_update (node);
	      continue;
	    }

	  // If the pred is entry but NOT def, then it is used before defined,
	  // It'll get set to []. and no need to update it.
	  if (pred == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    continue;

	  // Regardless of whther we have visited pred or not, if the pred has
	  // a non-null reference, revisit this block.
	  if (m_non_null.non_null_deref_p (name, pred))
	    add_to_update (node);

	  // If the pred block already has a range, or if it can contribute
	  // something new. Ie, the edge generates a range of some sort.
	  if (m_on_entry.get_bb_range (r, name, pred))
	    {
	      if (!r.undefined_p () || has_edge_range_p (e, name))
		add_to_update (node);
	      continue;
	    }

	  // If the pred hasn't been visited (has no range), add it to the list.
	  gcc_checking_assert (!m_on_entry.bb_range_p (name, pred));
	  m_on_entry.set_bb_range (name, pred, undefined);
	  m_workback.quick_push (pred);
	}
    }

  iterative_cache_update (name);
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
      value_range vl = r;
      value_range_equiv vr = vl;
      m_vr_values->adjust_range_with_scev (&vr, l, phi, phi_result);
      if (vr.constant_p ())
	{
	  widest_irange old = r;
	  r = vr;
	  if (old != r
	      && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "LOOP_RANGER refined this PHI result: ");
	      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	      fprintf (dump_file, "  from this range: ");
	      old.dump (dump_file);
	      fprintf (dump_file, "\n");
	      fprintf (dump_file, "  into this range: ");
	      r.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

// Virtual override of global_ranger::range_of_phi() that adjusts the
// result with loop information if available.

bool
loop_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool result = global_ranger::range_of_stmt (r, s, name);
  if (result && is_a<gphi *> (s) && scev_initialized_p ())
    adjust_phi_with_loop_info (r, as_a<gphi *> (s));
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
	{
	  r.dump (dump_file);
	  fputc('\n', dump_file);
	}
      else
	fputc('\n', dump_file);
    }
  // Marks the end of a request.
  if (indent == 0)
    fputc('\n', dump_file);
  return result;
}

// Tracing version of range_of_expr.  Call it with printing wrappers.

void
trace_ranger::range_of_ssa_name (irange &r, tree name, gimple *s)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_ssa_name (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") at stmt ");
      if (s)
	print_gimple_stmt (dump_file, s , 0, TDF_SLIM);
      else
	fprintf (dump_file, " NULL\n");
      indent += bump;
    }

  super::range_of_ssa_name (r, name, s);

  trailer (idx, "range_of_ssa_name", true, name, r);
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
				     const irange *name_range)
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
	{
	  name_range->dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      else
	fputs ("NULL\n", dump_file);
      indent += bump;
    }

  res = super::outgoing_edge_range_p (r, e, name, name_range);

  return trailer (idx, "outgoing_edge_range_p", res, name, r);
}
