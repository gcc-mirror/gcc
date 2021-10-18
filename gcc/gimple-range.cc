/* Code for GIMPLE range related routines.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-range.h"

gimple_ranger::gimple_ranger () :
	non_executable_edge_flag (cfun),
	m_cache (non_executable_edge_flag),
	tracer ("")
{
  // If the cache has a relation oracle, use it.
  m_oracle = m_cache.oracle ();
  if (dump_file && (param_evrp_mode & EVRP_MODE_TRACE))
    tracer.enable_trace ();

  // Ensure the not_executable flag is clear everywhere.
  if (flag_checking)
    {
      basic_block bb;
      FOR_ALL_BB_FN (bb, cfun)
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    gcc_checking_assert ((e->flags & non_executable_edge_flag) == 0);
	}
    }
}

bool
gimple_ranger::range_of_expr (irange &r, tree expr, gimple *stmt)
{
  unsigned idx;
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  if ((idx = tracer.header ("range_of_expr(")))
    {
      print_generic_expr (dump_file, expr, TDF_SLIM);
      fputs (")", dump_file);
      if (stmt)
	{
	  fputs (" at stmt ", dump_file);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      else
	fputs ("\n", dump_file);
    }

  // If there is no statement, just get the global value.
  if (!stmt)
    {
      if (!m_cache.get_global_range (r, expr))
        r = gimple_range_global (expr);
    }
  // For a debug stmt, pick the best value currently available, do not
  // trigger new value calculations.  PR 100781.
  else if (is_gimple_debug (stmt))
    m_cache.range_of_expr (r, expr, stmt);
  else
    {
      basic_block bb = gimple_bb (stmt);
      gimple *def_stmt = SSA_NAME_DEF_STMT (expr);

      // If name is defined in this block, try to get an range from S.
      if (def_stmt && gimple_bb (def_stmt) == bb)
	{
	  range_of_stmt (r, def_stmt, expr);
	  m_cache.m_non_null.adjust_range (r, expr, bb, true);
	}
      // Otherwise OP comes from outside this block, use range on entry.
      else
	range_on_entry (r, bb, expr);
    }
  if (idx)
    tracer.trailer (idx, "range_of_expr", true, expr, r);
  return true;
}

// Return the range of NAME on entry to block BB in R.

void
gimple_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  int_range_max entry_range;
  gcc_checking_assert (gimple_range_ssa_p (name));

  unsigned idx;
  if ((idx = tracer.header ("range_on_entry (")))
    {
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") to BB %d\n", bb->index);
    }

  // Start with any known range
  range_of_stmt (r, SSA_NAME_DEF_STMT (name), name);

  // Now see if there is any on_entry value which may refine it.
  if (m_cache.block_range (entry_range, bb, name))
    r.intersect (entry_range);

  m_cache.m_non_null.adjust_range (r, name, bb, true);

  if (idx)
    tracer.trailer (idx, "range_on_entry", true, name, r);
}

// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

void
gimple_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  // on-exit from the exit block?
  gcc_checking_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
  gcc_checking_assert (gimple_range_ssa_p (name));

  unsigned idx;
  if ((idx = tracer.header ("range_on_exit (")))
    {
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") from BB %d\n", bb->index);
    }

  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (s);
  // If this is not the definition block, get the range on the last stmt in
  // the block... if there is one.
  if (def_bb != bb)
    s = last_stmt (bb);
  // If there is no statement provided, get the range_on_entry for this block.
  if (s)
    range_of_expr (r, name, s);
  else
    range_on_entry (r, bb, name);
  gcc_checking_assert (r.undefined_p ()
		       || range_compatible_p (r.type (), TREE_TYPE (name)));
  
  if (idx)
    tracer.trailer (idx, "range_on_exit", true, name, r);
}

// Calculate a range for NAME on edge E and return it in R.

bool
gimple_ranger::range_on_edge (irange &r, edge e, tree name)
{
  int_range_max edge_range;
  gcc_checking_assert (irange::supports_type_p (TREE_TYPE (name)));

  // Do not process values along abnormal edges.
  if (e->flags & EDGE_ABNORMAL)
    return get_tree_range (r, name, NULL);

  unsigned idx;
  if ((idx = tracer.header ("range_on_edge (")))
    {
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
    }

  // Check to see if the edge is executable.
  if ((e->flags & non_executable_edge_flag))
    {
      r.set_undefined ();
      if (idx)
	tracer.trailer (idx, "range_on_edge [Unexecutable] ", true,
			name, r);
      return true;
    }

  bool res = true;
  if (!gimple_range_ssa_p (name))
    return get_tree_range (r, name, NULL);
  else
    {
      range_on_exit (r, e->src, name);
      gcc_checking_assert  (r.undefined_p ()
			    || range_compatible_p (r.type(), TREE_TYPE (name)));

      // Check to see if NAME is defined on edge e.
      if (m_cache.range_on_edge (edge_range, e, name))
	r.intersect (edge_range);
    }

  if (idx)
    tracer.trailer (idx, "range_on_edge", res, name, r);
  return true;
}

// fold_range wrapper for range_of_stmt to use as an internal client.

bool
gimple_ranger::fold_range_internal (irange &r, gimple *s, tree name)
{
  fold_using_range f;
  fur_depend src (s, &(gori ()), this);
  return f.fold_stmt (r, s, src, name);
}

// Calculate a range for statement S and return it in R.  If NAME is
// provided it represents the SSA_NAME on the LHS of the statement.
// It is only required if there is more than one lhs/output.  Check
// the global cache for NAME first to see if the evaluation can be
// avoided.  If a range cannot be calculated, return false and UNDEFINED.

bool
gimple_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res;
  r.set_undefined ();

  unsigned idx;
  if ((idx = tracer.header ("range_of_stmt (")))
    {
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") at stmt ", dump_file);
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
    }

  if (!name)
    name = gimple_get_lhs (s);

  // If no name, simply call the base routine.
  if (!name)
    res = fold_range_internal (r, s, NULL_TREE);
  else if (!gimple_range_ssa_p (name))
    res = get_tree_range (r, name, NULL);
  // Check if the stmt has already been processed, and is not stale.
  else if (m_cache.get_non_stale_global_range (r, name))
    {
      if (idx)
	tracer.trailer (idx, " cached", true, name, r);
      return true;
    }
  else
    {
      // Otherwise calculate a new value.
      int_range_max tmp;
      fold_range_internal (tmp, s, name);

      // Combine the new value with the old value.  This is required because
      // the way value propagation works, when the IL changes on the fly we
      // can sometimes get different results.  See PR 97741.
      r.intersect (tmp);
      m_cache.set_global_range (name, r);
      res = true;
    }

  if (idx)
    tracer.trailer (idx, "range_of_stmt", res, name, r);
  return res;
}

// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO and SSA_NAME_PTR_INFO fields.

void
gimple_ranger::export_global_ranges ()
{
  /* Cleared after the table header has been printed.  */
  bool print_header = true;
  for (unsigned x = 1; x < num_ssa_names; x++)
    {
      int_range_max r;
      tree name = ssa_name (x);
      if (name && !SSA_NAME_IN_FREE_LIST (name)
	  && gimple_range_ssa_p (name)
	  && m_cache.get_global_range (r, name)
	  && !r.varying_p())
	{
	  bool updated = update_global_range (r, name);
	  if (!updated || !dump_file || !(dump_flags & TDF_DETAILS))
	    continue;

	  if (print_header)
	    {
	      /* Print the header only when there's something else
		 to print below.  */
	      fprintf (dump_file, "Exported global range table:\n");
	      fprintf (dump_file, "============================\n");
	      print_header = false;
	    }

	  value_range vr = r;
	  print_generic_expr (dump_file, name , TDF_SLIM);
	  fprintf (dump_file, "  : ");
	  vr.dump (dump_file);
	  fprintf (dump_file, "\n");
	  int_range_max same = vr;
	  if (same != r)
	    {
	      fprintf (dump_file, "         irange : ");
	      r.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

// Print the known table values to file F.

void
gimple_ranger::dump_bb (FILE *f, basic_block bb)
{
  unsigned x;
  edge_iterator ei;
  edge e;
  int_range_max range, tmp_range;
  fprintf (f, "\n=========== BB %d ============\n", bb->index);
  m_cache.dump_bb (f, bb);

  ::dump_bb (f, bb, 4, TDF_NONE);

  // Now find any globals defined in this block.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (gimple_range_ssa_p (name) && SSA_NAME_DEF_STMT (name) &&
	  gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
	  m_cache.get_global_range (range, name))
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
	  if (name && gori ().has_edge_range_p (name, e)
	      && m_cache.range_on_edge (range, e, name))
	    {
	      gimple *s = SSA_NAME_DEF_STMT (name);
	      // Only print the range if this is the def block, or
	      // the on entry cache for either end of the edge is
	      // set.
	      if ((s && bb == gimple_bb (s)) ||
		  m_cache.block_range (tmp_range, bb, name, false) ||
		  m_cache.block_range (tmp_range, e->dest, name, false))
		{
		  if (!range.varying_p ())
		    {
		      fprintf (f, "%d->%d ", e->src->index,
			       e->dest->index);
		      char c = ' ';
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

// Print the known table values to file F.

void
gimple_ranger::dump (FILE *f)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    dump_bb (f, bb);

  m_cache.dump (f);
}

void
gimple_ranger::debug ()
{
  dump (stderr);
}

/* Create a new ranger instance and associate it with function FUN.
   Each call must be paired with a call to disable_ranger to release
   resources.  */

gimple_ranger *
enable_ranger (struct function *fun)
{
  gimple_ranger *r;

  r = new gimple_ranger;
  fun->x_range_query = r;

  return r;
}

/* Destroy and release the ranger instance associated with function FUN
   and replace it the global ranger.  */

void
disable_ranger (struct function *fun)
{
  delete fun->x_range_query;

  fun->x_range_query = &global_ranges;
}
