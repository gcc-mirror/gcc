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

gimple_ranger::gimple_ranger ()
{
  // If the cache has a relation oracle, use it.
  m_oracle = m_cache.oracle ();
}

bool
gimple_ranger::range_of_expr (irange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  // If there is no statement, just get the global value.
  if (!stmt)
    {
      if (!m_cache.get_global_range (r, expr))
        r = gimple_range_global (expr);
      return true;
    }

  // For a debug stmt, pick the best value currently available, do not
  // trigger new value calculations.  PR 100781.
  if (is_gimple_debug (stmt))
    {
      m_cache.range_of_expr (r, expr, stmt);
      return true;
    }
  basic_block bb = gimple_bb (stmt);
  gimple *def_stmt = SSA_NAME_DEF_STMT (expr);

  // If name is defined in this block, try to get an range from S.
  if (def_stmt && gimple_bb (def_stmt) == bb)
    {
      range_of_stmt (r, def_stmt, expr);
      m_cache.m_non_null.adjust_range (r, expr, bb, true);
    }
  else
    // Otherwise OP comes from outside this block, use range on entry.
    range_on_entry (r, bb, expr);

  return true;
}

// Return the range of NAME on entry to block BB in R.

void
gimple_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  int_range_max entry_range;
  gcc_checking_assert (gimple_range_ssa_p (name));

  // Start with any known range
  range_of_stmt (r, SSA_NAME_DEF_STMT (name), name);

  // Now see if there is any on_entry value which may refine it.
  if (m_cache.block_range (entry_range, bb, name))
    r.intersect (entry_range);

  m_cache.m_non_null.adjust_range (r, name, bb, true);
}

// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

void
gimple_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  // on-exit from the exit block?
  gcc_checking_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
  gcc_checking_assert (gimple_range_ssa_p (name));

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
}

// Calculate a range for NAME on edge E and return it in R.

bool
gimple_ranger::range_on_edge (irange &r, edge e, tree name)
{
  int_range_max edge_range;
  gcc_checking_assert (irange::supports_type_p (TREE_TYPE (name)));

  // PHI arguments can be constants, catch these here.
  if (!gimple_range_ssa_p (name))
    return range_of_expr (r, name);

  range_on_exit (r, e->src, name);
  gcc_checking_assert  (r.undefined_p ()
			|| range_compatible_p (r.type(), TREE_TYPE (name)));

  // Check to see if NAME is defined on edge e.
  if (m_cache.range_on_edge (edge_range, e, name))
    r.intersect (edge_range);

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
  r.set_undefined ();

  if (!name)
    name = gimple_get_lhs (s);

  // If no name, simply call the base routine.
  if (!name)
    return fold_range_internal (r, s, NULL_TREE);

  if (!gimple_range_ssa_p (name))
    return false;

  // Check if the stmt has already been processed, and is not stale.
  if (m_cache.get_non_stale_global_range (r, name))
    return true;

  // Otherwise calculate a new value.
  int_range_max tmp;
  fold_range_internal (tmp, s, name);

  // Combine the new value with the old value.  This is required because
  // the way value propagation works, when the IL changes on the fly we
  // can sometimes get different results.  See PR 97741.
  r.intersect (tmp);
  m_cache.set_global_range (name, r);

  return true;
}

// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO and SSA_NAME_PTR_INFO fields.

void
gimple_ranger::export_global_ranges ()
{
  unsigned x;
  int_range_max r;
  if (dump_file)
    {
      fprintf (dump_file, "Exported global range table\n");
      fprintf (dump_file, "===========================\n");
    }

  for ( x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (name && !SSA_NAME_IN_FREE_LIST (name)
	  && gimple_range_ssa_p (name)
	  && m_cache.get_global_range (r, name)
	  && !r.varying_p())
	{
	  bool updated = update_global_range (r, name);

	  if (updated && dump_file)
	    {
	      value_range vr = r;
	      print_generic_expr (dump_file, name , TDF_SLIM);
	      fprintf (dump_file, " --> ");
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
}

// Print the known table values to file F.

void
gimple_ranger::dump_bb (FILE *f, basic_block bb)
{
  unsigned x;
  edge_iterator ei;
  edge e;
  int_range_max range;
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
		  m_cache.block_range (range, bb, name, false) ||
		  m_cache.block_range (range, e->dest, name, false))
		{
		  m_cache.range_on_edge (range, e, name);
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

// trace_ranger implementation.


trace_ranger::trace_ranger ()
{
  indent = 0;
  trace_count = 0;
}

// If dumping, return true and print the prefix for the next output line.

bool
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
  if (dumping (counter, true))
    {
      indent -= bump;
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
      // Marks the end of a request.
      if (indent == 0)
	fputc('\n', dump_file);
    }
  return result;
}

// Tracing version of range_on_edge.  Call it with printing wrappers.

bool
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

  bool res = gimple_ranger::range_on_edge (r, e, name);
  trailer (idx, "range_on_edge", true, name, r);
  return res;
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

  gimple_ranger::range_on_entry (r, bb, name);

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

  gimple_ranger::range_on_exit (r, bb, name);

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

  res = gimple_ranger::range_of_stmt (r, s, name);

  return trailer (idx, "range_of_stmt", res, name, r);
}

// Tracing version of range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree name, gimple *s)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_expr(");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (")", dump_file);
      if (s)
	{
	  fputs (" at stmt ", dump_file);
	  print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
	}
      else
	fputs ("\n", dump_file);
      indent += bump;
    }

  res = gimple_ranger::range_of_expr (r, name, s);

  return trailer (idx, "range_of_expr", res, name, r);
}

gimple_ranger *
enable_ranger (struct function *fun)
{
  gimple_ranger *r;

  if (param_evrp_mode & EVRP_MODE_TRACE)
    r = new trace_ranger;
  else
    r = new gimple_ranger;

  fun->x_range_query = r;

  return r;
}

void
disable_ranger (struct function *fun)
{
  delete fun->x_range_query;

  fun->x_range_query = &global_ranges;
}

// =========================================
// Debugging helpers.
// =========================================

// Query all statements in the IL to precalculate computable ranges in RANGER.

static DEBUG_FUNCTION void
debug_seed_ranger (gimple_ranger &ranger)
{
  // Recalculate SCEV to make sure the dump lists everything.
  if (scev_initialized_p ())
    {
      scev_finalize ();
      scev_initialize ();
    }

  basic_block bb;
  int_range_max r;
  gimple_stmt_iterator gsi;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	if (is_gimple_debug (stmt))
	  continue;

	ranger.range_of_stmt (r, stmt);
      }
}

// Dump all that ranger knows for the current function.

DEBUG_FUNCTION void
dump_ranger (FILE *out)
{
  gimple_ranger ranger;
  debug_seed_ranger (ranger);
  ranger.dump (out);
}

DEBUG_FUNCTION void
debug_ranger ()
{
  dump_ranger (stderr);
}

// Dump all that ranger knows on a path of BBs.
//
// Note that the blocks are in reverse order, thus the exit block is
// path[0].

DEBUG_FUNCTION void
dump_ranger (FILE *dump_file, const vec<basic_block> &path)
{
  if (path.length () == 0)
    {
      fprintf (dump_file, "empty\n");
      return;
    }

  gimple_ranger ranger;
  debug_seed_ranger (ranger);

  unsigned i = path.length ();
  do
    {
      i--;
      ranger.dump_bb (dump_file, path[i]);
    }
  while (i > 0);
}

DEBUG_FUNCTION void
debug_ranger (const vec<basic_block> &path)
{
  dump_ranger (stderr, path);
}

#include "gimple-range-tests.cc"
