/* Code for GIMPLE range related routines.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
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
#include "gimple-fold.h"
#include "gimple-walk.h"

gimple_ranger::gimple_ranger (bool use_imm_uses) :
	non_executable_edge_flag (cfun),
	m_cache (non_executable_edge_flag, use_imm_uses),
	tracer (""),
	current_bb (NULL)
{
  // Share the oracle from the cache.
  share_query (m_cache);
  if (dump_file && (param_ranger_debug & RANGER_DEBUG_TRACE))
    tracer.enable_trace ();
  m_stmt_list.create (0);
  m_stmt_list.safe_grow (num_ssa_names);
  m_stmt_list.truncate (0);

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

gimple_ranger::~gimple_ranger ()
{
  m_stmt_list.release ();
}

// Return a range_query which accesses just the known global values.

range_query &
gimple_ranger::const_query ()
{
  return m_cache.const_query ();
}

bool
gimple_ranger::range_of_expr (vrange &r, tree expr, gimple *stmt)
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
      value_range tmp (TREE_TYPE (expr));
      // If there is no global range for EXPR yet, try to evaluate it.
      // This call sets R to a global range regardless.
      if (!m_cache.get_global_range (r, expr))
	{
	  gimple *s = SSA_NAME_DEF_STMT (expr);
	  // Calculate a range for S if it is safe to do so.
	  if (s && gimple_bb (s) && gimple_get_lhs (s) == expr)
	    return range_of_stmt (r, s);
	}
      // Pick up implied context information from the on-entry cache
      // if current_bb is set.  Do not attempt any new calculations.
      if (current_bb && m_cache.block_range (tmp, current_bb, expr, false))
	{
	  r.intersect (tmp);
	  char str[80];
	  sprintf (str, "picked up range from bb %d\n",current_bb->index);
	  if (idx)
	    tracer.print (idx, str);
	}
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
	  // Declared in this block, if it has a global set, check for an
	  // override from a block walk, otherwise calculate it.
	  if (m_cache.get_global_range (r, expr))
	    m_cache.block_range (r, bb, expr, false);
	  else
	    range_of_stmt (r, def_stmt, expr);
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

bool
gimple_ranger::range_on_entry (vrange &r, basic_block bb, tree name)
{
  if (!gimple_range_ssa_p (name))
    return get_tree_range (r, name, NULL, bb, NULL);

  value_range entry_range (TREE_TYPE (name));

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

  if (idx)
    tracer.trailer (idx, "range_on_entry", true, name, r);
  return true;
}

// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

bool
gimple_ranger::range_on_exit (vrange &r, basic_block bb, tree name)
{
  if (!gimple_range_ssa_p (name))
    return get_tree_range (r, name, NULL, NULL, bb);

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
    s = last_nondebug_stmt (bb);
  // If there is no statement provided, get the range_on_entry for this block.
  if (s)
    range_of_expr (r, name, s);
  else
    range_on_entry (r, bb, name);
  gcc_checking_assert (r.undefined_p ()
		       || range_compatible_p (r.type (), TREE_TYPE (name)));

  if (idx)
    tracer.trailer (idx, "range_on_exit", true, name, r);
  return true;
}

// Calculate a range for NAME on edge E and return it in R.

bool
gimple_ranger::range_on_edge (vrange &r, edge e, tree name)
{
  value_range edge_range (TREE_TYPE (name));

  if (!r.supports_type_p (TREE_TYPE (name)))
    return false;

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
    res = get_tree_range (r, name, NULL);
  else
    {
      range_on_exit (r, e->src, name);
      // If this is not an abnormal edge, check for a non-null exit .
      if ((e->flags & (EDGE_EH | EDGE_ABNORMAL)) == 0)
	infer_oracle ().maybe_adjust_range (r, name, e->src);
      gcc_checking_assert  (r.undefined_p ()
			    || range_compatible_p (r.type(), TREE_TYPE (name)));

      // Check to see if NAME is defined on edge e.
      if (m_cache.range_on_edge (edge_range, e, name))
	r.intersect (edge_range);
    }

  if (idx)
    tracer.trailer (idx, "range_on_edge", res, name, r);
  return res;
}

// fold_range wrapper for range_of_stmt to use as an internal client.

bool
gimple_ranger::fold_range_internal (vrange &r, gimple *s, tree name)
{
  fold_using_range f;
  fur_depend src (s, this);
  return f.fold_stmt (r, s, src, name);
}

// Calculate a range for statement S and return it in R.  If NAME is
// provided it represents the SSA_NAME on the LHS of the statement.
// It is only required if there is more than one lhs/output.  Check
// the global cache for NAME first to see if the evaluation can be
// avoided.  If a range cannot be calculated, return false and UNDEFINED.

bool
gimple_ranger::range_of_stmt (vrange &r, gimple *s, tree name)
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
    {
      res = fold_range_internal (r, s, NULL_TREE);
      if (res && is_a <gcond *> (s))
	{
	  // Update any exports in the cache if this is a gimple cond statement.
	  tree exp;
	  basic_block bb = gimple_bb (s);
	  FOR_EACH_GORI_EXPORT_NAME (gori_ssa (), bb, exp)
	    m_cache.propagate_updated_value (exp, bb);
	}
    }
  else if (!gimple_range_ssa_p (name))
    res = get_tree_range (r, name, NULL);
  else
    {
      bool current;
      // Check if the stmt has already been processed.
      if (m_cache.get_global_range (r, name, current))
	{
	  // If it isn't stale, use this cached value.
	  if (current)
	    {
	      if (idx)
		tracer.trailer (idx, " cached", true, name, r);
	      return true;
	    }
	}
      else
	prefill_stmt_dependencies (name);

      // Calculate a new value.
      value_range tmp (TREE_TYPE (name));
      fold_range_internal (tmp, s, name);

      // Combine the new value with the old value.  This is required because
      // the way value propagation works, when the IL changes on the fly we
      // can sometimes get different results.  See PR 97741.
      bool changed = r.intersect (tmp);
      m_cache.set_global_range (name, r, changed);
      res = true;
    }

  if (idx)
    tracer.trailer (idx, "range_of_stmt", res, name, r);
  return res;
}


// Check if NAME is a dependency that needs resolving, and push it on the
// stack if so.  R is a scratch range.

inline void
gimple_ranger::prefill_name (vrange &r, tree name)
{
  if (!gimple_range_ssa_p (name))
    return;
  gimple *stmt = SSA_NAME_DEF_STMT (name);
  if (!gimple_range_op_handler::supported_p (stmt) && !is_a<gphi *> (stmt))
    return;

  // If this op has not been processed yet, then push it on the stack
  if (!m_cache.get_global_range (r, name))
    {
      bool current;
      // Set the global cache value and mark as alway_current.
      m_cache.get_global_range (r, name, current);
      m_stmt_list.safe_push (name);
    }
}

// This routine will seed the global cache with most of the dependencies of
// NAME.  This prevents excessive call depth through the normal API.

void
gimple_ranger::prefill_stmt_dependencies (tree ssa)
{
  if (SSA_NAME_IS_DEFAULT_DEF (ssa))
    return;

  unsigned idx;
  gimple *stmt = SSA_NAME_DEF_STMT (ssa);
  gcc_checking_assert (stmt && gimple_bb (stmt));

  // Only pre-process range-ops and phis.
  if (!gimple_range_op_handler::supported_p (stmt) && !is_a<gphi *> (stmt))
    return;

  // Mark where on the stack we are starting.
  unsigned start = m_stmt_list.length ();
  m_stmt_list.safe_push (ssa);

  idx = tracer.header ("ROS dependence fill\n");

  // Loop until back at the start point.
  while (m_stmt_list.length () > start)
    {
      tree name = m_stmt_list.last ();
      // NULL is a marker which indicates the next name in the stack has now
      // been fully resolved, so we can fold it.
      if (!name)
	{
	  // Pop the NULL, then pop the name.
	  m_stmt_list.pop ();
	  name = m_stmt_list.pop ();
	  // Don't fold initial request, it will be calculated upon return.
	  if (m_stmt_list.length () > start)
	    {
	      // Fold and save the value for NAME.
	      stmt = SSA_NAME_DEF_STMT (name);
	      value_range r (TREE_TYPE (name));
	      fold_range_internal (r, stmt, name);
	      // Make sure we don't lose any current global info.
	      value_range tmp (TREE_TYPE (name));
	      m_cache.get_global_range (tmp, name);
	      bool changed = tmp.intersect (r);
	      m_cache.set_global_range (name, tmp, changed);
	    }
	  continue;
	}

      // Add marker indicating previous NAME in list should be folded
      // when we get to this NULL.
      m_stmt_list.safe_push (NULL_TREE);
      stmt = SSA_NAME_DEF_STMT (name);

      if (idx)
	{
	  tracer.print (idx, "ROS dep fill (");
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fputs (") at stmt ", dump_file);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}

      gphi *phi = dyn_cast <gphi *> (stmt);
      if (phi)
	{
	  value_range r (TREE_TYPE (gimple_phi_result (phi)));
	  for (unsigned x = 0; x < gimple_phi_num_args (phi); x++)
	    prefill_name (r, gimple_phi_arg_def (phi, x));
	}
      else
	{
	  gimple_range_op_handler handler (stmt);
	  if (handler)
	    {
	      tree op = handler.operand2 ();
	      if (op)
		{
		  value_range r (TREE_TYPE (op));
		  prefill_name (r, op);
		}
	      op = handler.operand1 ();
	      if (op)
		{
		  value_range r (TREE_TYPE (op));
		  prefill_name (r, op);
		}
	    }
	}
    }
  if (idx)
    {
      unsupported_range r;
      tracer.trailer (idx, "ROS ", false, ssa, r);
    }
}


// This routine will invoke the gimple fold_stmt routine, providing context to
// range_of_expr calls via an private internal API.

bool
gimple_ranger::fold_stmt (gimple_stmt_iterator *gsi, tree (*valueize) (tree))
{
  gimple *stmt = gsi_stmt (*gsi);
  current_bb = gimple_bb (stmt);
  bool ret = ::fold_stmt (gsi, valueize);
  current_bb = NULL;
  return ret;
}

// Called during dominator walks to register any inferred ranges that take
// effect from this point forward.

void
gimple_ranger::register_inferred_ranges (gimple *s)
{
  // First, export the LHS if it is a new global range.
  tree lhs = gimple_get_lhs (s);
  if (lhs)
    {
      value_range tmp (TREE_TYPE (lhs));
      if (range_of_stmt (tmp, s, lhs) && !tmp.varying_p ())
	set_range_info (lhs, tmp);
    }
  m_cache.apply_inferred_ranges (s);
}

// This function will walk the statements in BB to determine if any
// discovered inferred ranges in the block have any transitive effects,
// and if so, register those effects in BB.

void
gimple_ranger::register_transitive_inferred_ranges (basic_block bb)
{
  // Return if there are no inferred ranges in BB.
  if (!infer_oracle ().has_range_p (bb))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Checking for transitive inferred ranges in BB %d\n",
	     bb->index);

  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
       gsi_next (&si))
    {
      gimple *s = gsi_stmt (si);
      tree lhs = gimple_get_lhs (s);
      // If the LHS already has an inferred effect, leave it be.
      if (!gimple_range_ssa_p (lhs) || infer_oracle ().has_range_p (bb, lhs))
	continue;
      // Pick up global value.
      value_range g (TREE_TYPE (lhs));
      range_of_expr (g, lhs);

      // If either dependency has an inferred range, check if recalculating
      // the LHS is different than the global value. If so, register it as
      // an inferred range as well.
      value_range r (TREE_TYPE (lhs));
      r.set_undefined ();
      tree name1 = gori_ssa ()->depend1 (lhs);
      tree name2 = gori_ssa ()->depend2 (lhs);
      if ((name1 && infer_oracle ().has_range_p (bb, name1))
	  || (name2 && infer_oracle ().has_range_p (bb, name2)))
	{
	  // Check if folding S produces a different result.
	  if (fold_range (r, s, this) && g != r)
	    {
	      gimple_infer_range ir (lhs, r);
	      infer_oracle ().add_ranges (s, ir);
	      m_cache.register_inferred_value (r, lhs, bb);
	    }
	}
    }
}

// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO and SSA_NAME_PTR_INFO fields.

void
gimple_ranger::export_global_ranges ()
{
  if (dump_file)
    {
      /* Print the header only when there's something else
	 to print below.  */
      fprintf (dump_file, "Exporting new  global ranges:\n");
      fprintf (dump_file, "============================\n");
    }
  for (unsigned x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (!name)
	continue;
      value_range r (TREE_TYPE (name));
      if (name && !SSA_NAME_IN_FREE_LIST (name) && gimple_range_ssa_p (name)
	  && m_cache.get_global_range (r, name) && !r.varying_p())
	set_range_info (name, r);
    }
  if (dump_file)
    fprintf (dump_file, "========= Done =============\n");
}

// Print the known table values to file F.

void
gimple_ranger::dump_bb (FILE *f, basic_block bb)
{
  unsigned x;
  edge_iterator ei;
  edge e;
  fprintf (f, "\n=========== BB %d ============\n", bb->index);
  m_cache.dump_bb (f, bb);

  ::dump_bb (f, bb, 4, TDF_NONE);

  // Now find any globals defined in this block.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (!gimple_range_ssa_p (name) || !SSA_NAME_DEF_STMT (name))
	continue;
      value_range range (TREE_TYPE (name));
      if (gimple_bb (SSA_NAME_DEF_STMT (name)) == bb
	  && m_cache.get_global_range (range, name))
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
	  if (!name || !gori ().has_edge_range_p (name, e))
	    continue;

	  value_range range (TREE_TYPE (name));
	  if (m_cache.range_on_edge (range, e, name))
	    {
	      gimple *s = SSA_NAME_DEF_STMT (name);
	      value_range tmp_range (TREE_TYPE (name));
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
enable_ranger (struct function *fun, bool use_imm_uses)
{
  gimple_ranger *r;

  gcc_checking_assert (!fun->x_range_query);
  r = new gimple_ranger (use_imm_uses);
  fun->x_range_query = r;

  return r;
}

/* Destroy and release the ranger instance associated with function FUN
   and replace it the global ranger.  */

void
disable_ranger (struct function *fun)
{
  gcc_checking_assert (fun->x_range_query);
  delete fun->x_range_query;
  fun->x_range_query = NULL;
}

// ---------------------------------------------------------------------------
//
// The DOM based ranger assumes a single DOM walk through the IL, and is
// used by the fvrp_folder as a fast VRP.
// During the dom walk, the current block has an ssa_lazy_cache pointer
// m_bb[bb->index] which represents all the cumulative contextual ranges
// active in the block.
// These ranges are pure static ranges generated by branches, and must be
// combined with the equivlaent global range to produce the final range.
// A NULL pointer means there are no contextual ranges.

// Create a DOM based ranger for use by a DOM walk pass.

dom_ranger::dom_ranger () : m_global ()
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_freelist.create (0);
  m_freelist.truncate (0);
  m_bb.create (0);
  m_bb.safe_grow_cleared (last_basic_block_for_fn (cfun));
  if (dump_file && (param_ranger_debug & RANGER_DEBUG_TRACE))
    tracer.enable_trace ();
}

// Dispose of a DOM ranger.

dom_ranger::~dom_ranger ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Non-varying global ranges:\n");
      fprintf (dump_file, "=========================:\n");
      m_global.dump (dump_file);
    }
  m_bb.release ();
  m_freelist.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Implement range of EXPR on stmt S, and return it in R.
// Return false if no range can be calculated.

bool
dom_ranger::range_of_expr (vrange &r, tree expr, gimple *s)
{
  unsigned idx;
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, s);

  if ((idx = tracer.header ("range_of_expr ")))
    {
      print_generic_expr (dump_file, expr, TDF_SLIM);
      if (s)
	{
	  fprintf (dump_file, " at ");
	  print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
	}
      else
	  fprintf (dump_file, "\n");
    }

  // If there is a statement, return the range in that statements block.
  if (s)
    range_in_bb (r, gimple_bb (s), expr);
  else
    m_global.range_of_expr (r, expr, s);

  if (idx)
    tracer.trailer (idx, " ", true, expr, r);
  return true;
}

// Return the range of EXPR on edge E in R.
// Return false if no range can be calculated.

bool
dom_ranger::range_on_edge (vrange &r, edge e, tree expr)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, NULL);

  basic_block bb = e->src;
  unsigned idx;
  if ((idx = tracer.header ("range_on_edge ")))
    {
      fprintf (dump_file, "%d->%d for ",e->src->index, e->dest->index);
      print_generic_expr (dump_file, expr, TDF_SLIM);
      fputc ('\n',dump_file);
    }

  range_in_bb (r, bb, expr);
  value_range vr(TREE_TYPE (expr));
  if (gori_name_on_edge (vr, expr, e, this))
    r.intersect (vr);

  if (idx)
    tracer.trailer (idx, " ", true, expr, r);
  return true;
}

// Return the range of NAME as it exists at the end of block BB in R.

void
dom_ranger::range_in_bb (vrange &r, basic_block bb, tree name)
{
  // Start with the global value.
  m_global.range_of_expr (r, name);

  // If there is a contextual range, intersect it with the global range
  ssa_lazy_cache *context = m_bb[bb->index];
  if (context && context->has_range (name))
    {
      value_range cr (TREE_TYPE (name));
      context->get_range (cr, name);
      r.intersect (cr);
    }
}

// Calculate the range of NAME, as the def of stmt S and return it in R.
// Return FALSE if no range can be calculated.
// Also set the global range for NAME as this should only be called within
// the def block during a DOM walk.

bool
dom_ranger::range_of_stmt (vrange &r, gimple *s, tree name)
{
  unsigned idx;
  bool ret;
  if (!name)
    name = gimple_get_lhs (s);

  if (name && !gimple_range_ssa_p (name))
    return get_tree_range (r, name, NULL);

  if ((idx = tracer.header ("range_of_stmt ")))
    print_gimple_stmt (dump_file, s, 0, TDF_SLIM);

  // Its already been calculated.
  if (name && m_global.has_range (name))
    {
      ret = m_global.range_of_expr (r, name, s);
      if (idx)
	tracer.trailer (idx, " Already had value ", ret, name, r);
      return ret;
    }

  // Fold using a fur_depend object so that relations are registered.
  fold_using_range f;
  fur_depend src (s, this);
  ret = f.fold_stmt (r, s, src, name);

  // If there is a new calculated range and it is not varying, set
  // a global range.
  if (ret && name && m_global.merge_range (name, r) && !r.varying_p ())
    set_range_info (name, r);

  if (idx)
    tracer.trailer (idx, " ", ret, name, r);
  return ret;
}

// Preprocess block BB.  If there is a single predecessor, start with any
// contextual ranges on the incoming edge, otherwise the initial list
// of ranges i empty for this block.  Then Merge in any contextual ranges
// from the dominator block.  Tihs will become the contextual ranges
// that apply to this block.

void
dom_ranger::pre_bb (basic_block bb)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "#FVRP entering BB %d\n", bb->index);

  m_bb[bb->index] = NULL;
  basic_block dom_bb  = get_immediate_dominator (CDI_DOMINATORS, bb);

  ssa_lazy_cache *e_cache;
  if (!m_freelist.is_empty ())
    e_cache = m_freelist.pop ();
  else
    e_cache = new ssa_lazy_cache (&m_bitmaps);
  gcc_checking_assert (e_cache->empty_p ());

  // If there is a single pred, check if there are any ranges on
  // the edge and start with those.
  if (single_pred_p (bb))
    {
      gori_on_edge (*e_cache, EDGE_PRED (bb, 0), this);
      if (!e_cache->empty_p () && dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nEdge ranges BB %d->%d\n",
		   EDGE_PRED (bb, 0)->src->index, bb->index);
	  e_cache->dump(dump_file);
	}
    }
  // If the dominator had any ranges registered, integrate those.
  if (dom_bb && m_bb [dom_bb->index])
    e_cache->merge (*(m_bb[dom_bb->index]));

  // If there are no ranges, this block has no contextual ranges.
  if (e_cache->empty_p ())
    m_freelist.safe_push (e_cache);
  else
    m_bb[bb->index] = e_cache;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (m_bb[bb->index])
	{
	  fprintf (dump_file, "all contextual ranges active:\n");
	  m_bb[bb->index]->dump (dump_file);
	}
      else
	fprintf (dump_file, " NO contextual ranges active:\n");
    }
}

// Perform any post block processing.

void
dom_ranger::post_bb (basic_block bb)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "#FVRP POST BB %d\n", bb->index);
  // If there were contextual ranges, clear them and put the
  // object on the freelist.
  if (m_bb[bb->index])
    {
      m_bb[bb->index]->clear ();
      m_freelist.safe_push (m_bb[bb->index]);
      m_bb[bb->index] = NULL;
    }
}
