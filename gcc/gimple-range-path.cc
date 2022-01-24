/* Basic block path solver.
   Copyright (C) 2021-2022 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfganal.h"
#include "value-range.h"
#include "gimple-range.h"
#include "tree-pretty-print.h"
#include "gimple-range-path.h"
#include "ssa.h"
#include "tree-cfg.h"
#include "gimple-iterator.h"

// Internal construct to help facilitate debugging of solver.
#define DEBUG_SOLVER (dump_file && (param_threader_debug == THREADER_DEBUG_ALL))

path_range_query::path_range_query (bool resolve, gimple_ranger *ranger)
  : m_cache (new ssa_global_cache),
    m_has_cache_entry (BITMAP_ALLOC (NULL)),
    m_resolve (resolve),
    m_alloced_ranger (!ranger)
{
  if (m_alloced_ranger)
    m_ranger = new gimple_ranger;
  else
    m_ranger = ranger;

  m_oracle = new path_oracle (m_ranger->oracle ());
}

path_range_query::~path_range_query ()
{
  delete m_oracle;
  if (m_alloced_ranger)
    delete m_ranger;
  BITMAP_FREE (m_has_cache_entry);
  delete m_cache;
}

// Return TRUE if NAME is in the import bitmap.

bool
path_range_query::import_p (tree name)
{
  return (TREE_CODE (name) == SSA_NAME
	  && bitmap_bit_p (m_imports, SSA_NAME_VERSION (name)));
}

// Mark cache entry for NAME as unused.

void
path_range_query::clear_cache (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  bitmap_clear_bit (m_has_cache_entry, v);
}

// If NAME has a cache entry, return it in R, and return TRUE.

inline bool
path_range_query::get_cache (irange &r, tree name)
{
  if (!gimple_range_ssa_p (name))
    return get_global_range_query ()->range_of_expr (r, name);

  unsigned v = SSA_NAME_VERSION (name);
  if (bitmap_bit_p (m_has_cache_entry, v))
    return m_cache->get_global_range (r, name);

  return false;
}

// Set the cache entry for NAME to R.

void
path_range_query::set_cache (const irange &r, tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  bitmap_set_bit (m_has_cache_entry, v);
  m_cache->set_global_range (name, r);
}

void
path_range_query::dump (FILE *dump_file)
{
  push_dump_file save (dump_file, dump_flags & ~TDF_DETAILS);

  if (m_path.is_empty ())
    return;

  unsigned i;
  bitmap_iterator bi;

  dump_ranger (dump_file, m_path);

  fprintf (dump_file, "Imports:\n");
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  m_cache->dump (dump_file);
}

void
path_range_query::debug ()
{
  dump (stderr);
}

// Return TRUE if NAME is defined outside the current path.

bool
path_range_query::defined_outside_path (tree name)
{
  gimple *def = SSA_NAME_DEF_STMT (name);
  basic_block bb = gimple_bb (def);

  return !bb || !m_path.contains (bb);
}

// Return the range of NAME on entry to the path.

void
path_range_query::range_on_path_entry (irange &r, tree name)
{
  gcc_checking_assert (defined_outside_path (name));
  basic_block entry = entry_bb ();

  // Prefer to use range_of_expr if we have a statement to look at,
  // since it has better caching than range_on_edge.
  gimple *last = last_stmt (entry);
  if (last)
    {
      if (m_ranger->range_of_expr (r, name, last))
	return;
      gcc_unreachable ();
    }

  // If we have no statement, look at all the incoming ranges to the
  // block.  This can happen when we're querying a block with only an
  // outgoing edge (no statement but the fall through edge), but for
  // which we can determine a range on entry to the block.
  int_range_max tmp;
  bool changed = false;
  r.set_undefined ();
  for (unsigned i = 0; i < EDGE_COUNT (entry->preds); ++i)
    {
      edge e = EDGE_PRED (entry, i);
      if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && m_ranger->range_on_edge (tmp, e, name))
	{
	  r.union_ (tmp);
	  changed = true;
	}
    }

  // Make sure we don't return UNDEFINED by mistake.
  if (!changed)
    r.set_varying (TREE_TYPE (name));
}

// Return the range of NAME at the end of the path being analyzed.

bool
path_range_query::internal_range_of_expr (irange &r, tree name, gimple *stmt)
{
  if (!irange::supports_type_p (TREE_TYPE (name)))
    return false;

  if (get_cache (r, name))
    return true;

  if (m_resolve && defined_outside_path (name))
    {
      range_on_path_entry (r, name);
      set_cache (r, name);
      return true;
    }

  if (stmt
      && range_defined_in_block (r, name, gimple_bb (stmt)))
    {
      if (TREE_CODE (name) == SSA_NAME)
	r.intersect (gimple_range_global (name));

      set_cache (r, name);
      return true;
    }

  r = gimple_range_global (name);
  return true;
}

bool
path_range_query::range_of_expr (irange &r, tree name, gimple *stmt)
{
  if (internal_range_of_expr (r, name, stmt))
    {
      if (r.undefined_p ())
	m_undefined_path = true;

      return true;
    }
  return false;
}

bool
path_range_query::unreachable_path_p ()
{
  return m_undefined_path;
}

// Initialize the current path to PATH.  The current block is set to
// the entry block to the path.
//
// Note that the blocks are in reverse order, so the exit block is
// path[0].

void
path_range_query::set_path (const vec<basic_block> &path)
{
  gcc_checking_assert (path.length () > 1);
  m_path = path.copy ();
  m_pos = m_path.length () - 1;
  bitmap_clear (m_has_cache_entry);
}

bool
path_range_query::ssa_defined_in_bb (tree name, basic_block bb)
{
  return (TREE_CODE (name) == SSA_NAME
	  && SSA_NAME_DEF_STMT (name)
	  && gimple_bb (SSA_NAME_DEF_STMT (name)) == bb);
}

// Return the range of the result of PHI in R.
//
// Since PHIs are calculated in parallel at the beginning of the
// block, we must be careful to never save anything to the cache here.
// It is the caller's responsibility to adjust the cache.  Also,
// calculating the PHI's range must not trigger additional lookups.

void
path_range_query::ssa_range_in_phi (irange &r, gphi *phi)
{
  tree name = gimple_phi_result (phi);
  basic_block bb = gimple_bb (phi);
  unsigned nargs = gimple_phi_num_args (phi);

  if (at_entry ())
    {
      if (m_resolve && m_ranger->range_of_expr (r, name, phi))
	return;

      // Try to fold the phi exclusively with global or cached values.
      // This will get things like PHI <5(99), 6(88)>.  We do this by
      // calling range_of_expr with no context.
      int_range_max arg_range;
      r.set_undefined ();
      for (size_t i = 0; i < nargs; ++i)
	{
	  tree arg = gimple_phi_arg_def (phi, i);
	  if (range_of_expr (arg_range, arg, /*stmt=*/NULL))
	    r.union_ (arg_range);
	  else
	    {
	      r.set_varying (TREE_TYPE (name));
	      return;
	    }
	}
      return;
    }

  basic_block prev = prev_bb ();
  edge e_in = find_edge (prev, bb);

  for (size_t i = 0; i < nargs; ++i)
    if (e_in == gimple_phi_arg_edge (phi, i))
      {
	tree arg = gimple_phi_arg_def (phi, i);
	// Avoid using the cache for ARGs defined in this block, as
	// that could create an ordering problem.
	if (ssa_defined_in_bb (arg, bb) || !get_cache (r, arg))
	  {
	    if (m_resolve)
	      {
		int_range_max tmp;
		// Using both the range on entry to the path, and the
		// range on this edge yields significantly better
		// results.
		if (defined_outside_path (arg))
		  range_on_path_entry (r, arg);
		else
		  r.set_varying (TREE_TYPE (name));
		m_ranger->range_on_edge (tmp, e_in, arg);
		r.intersect (tmp);
		return;
	      }
	    r.set_varying (TREE_TYPE (name));
	  }
	return;
      }
  gcc_unreachable ();
}

// If NAME is defined in BB, set R to the range of NAME, and return
// TRUE.  Otherwise, return FALSE.

bool
path_range_query::range_defined_in_block (irange &r, tree name, basic_block bb)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (def_stmt);

  if (def_bb != bb)
    return false;

  if (get_cache (r, name))
    return true;

  if (gimple_code (def_stmt) == GIMPLE_PHI)
    ssa_range_in_phi (r, as_a<gphi *> (def_stmt));
  else
    {
      if (name)
	get_path_oracle ()->killing_def (name);

      if (!range_of_stmt (r, def_stmt, name))
	r.set_varying (TREE_TYPE (name));
    }

  if (bb)
    m_non_null.adjust_range (r, name, bb);

  if (DEBUG_SOLVER && (bb || !r.varying_p ()))
    {
      fprintf (dump_file, "range_defined_in_block (BB%d) for ", bb ? bb->index : -1);
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " is ");
      r.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  return true;
}

// Compute ranges defined in the PHIs in this block.

void
path_range_query::compute_ranges_in_phis (basic_block bb)
{
  int_range_max r;
  auto_bitmap phi_set;

  // PHIs must be resolved simultaneously on entry to the block
  // because any dependencies must be satistifed with values on entry.
  // Thus, we calculate all PHIs first, and then update the cache at
  // the end.

  for (auto iter = gsi_start_phis (bb); !gsi_end_p (iter); gsi_next (&iter))
    {
      gphi *phi = iter.phi ();
      tree name = gimple_phi_result (phi);

      if (import_p (name) && range_defined_in_block (r, name, bb))
	{
	  unsigned v = SSA_NAME_VERSION (name);
	  set_cache (r, name);
	  bitmap_set_bit (phi_set, v);
	  // Pretend we don't have a cache entry for this name until
	  // we're done with all PHIs.
	  bitmap_clear_bit (m_has_cache_entry, v);
	}
    }
  bitmap_ior_into (m_has_cache_entry, phi_set);
}

// Compute ranges defined in the current block, or exported to the
// next block.

void
path_range_query::compute_ranges_in_block (basic_block bb)
{
  bitmap_iterator bi;
  int_range_max r, cached_range;
  unsigned i;

  if (m_resolve && !at_entry ())
    compute_phi_relations (bb, prev_bb ());

  // Force recalculation of any names in the cache that are defined in
  // this block.  This can happen on interdependent SSA/phis in loops.
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      if (ssa_defined_in_bb (name, bb))
	clear_cache (name);
    }

  // Solve imports defined in this block, starting with the PHIs...
  compute_ranges_in_phis (bb);
  // ...and then the rest of the imports.
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);

      if (gimple_code (SSA_NAME_DEF_STMT (name)) != GIMPLE_PHI
	  && range_defined_in_block (r, name, bb))
	set_cache (r, name);
    }

  if (at_exit ())
    return;

  // Solve imports that are exported to the next block.
  basic_block next = next_bb ();
  edge e = find_edge (bb, next);
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      gori_compute &g = m_ranger->gori ();
      bitmap exports = g.exports (bb);

      if (bitmap_bit_p (exports, i))
	{
	  if (g.outgoing_edge_range_p (r, e, name, *this))
	    {
	      if (get_cache (cached_range, name))
		r.intersect (cached_range);

	      set_cache (r, name);
	      if (DEBUG_SOLVER)
		{
		  fprintf (dump_file, "outgoing_edge_range_p for ");
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " on edge %d->%d ",
			   e->src->index, e->dest->index);
		  fprintf (dump_file, "is ");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	}
    }

  if (m_resolve)
    compute_outgoing_relations (bb, next);
}

// Adjust all pointer imports in BB with non-null information.

void
path_range_query::adjust_for_non_null_uses (basic_block bb)
{
  int_range_max r;
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);

      if (!POINTER_TYPE_P (TREE_TYPE (name)))
	continue;

      if (get_cache (r, name))
	{
	  if (r.nonzero_p ())
	    continue;
	}
      else
	r.set_varying (TREE_TYPE (name));

      if (m_non_null.adjust_range (r, name, bb))
	set_cache (r, name);
    }
}

// If NAME is a supported SSA_NAME, add it the bitmap in IMPORTS.

bool
path_range_query::add_to_imports (tree name, bitmap imports)
{
  if (TREE_CODE (name) == SSA_NAME
      && irange::supports_type_p (TREE_TYPE (name)))
    return bitmap_set_bit (imports, SSA_NAME_VERSION (name));
  return false;
}

// Compute the imports to the path ending in EXIT.  These are
// essentially the SSA names used to calculate the final conditional
// along the path.
//
// They are hints for the solver.  Adding more elements doesn't slow
// us down, because we don't solve anything that doesn't appear in the
// path.  On the other hand, not having enough imports will limit what
// we can solve.

void
path_range_query::compute_imports (bitmap imports, basic_block exit)
{
  // Start with the imports from the exit block...
  gori_compute &gori = m_ranger->gori ();
  bitmap r_imports = gori.imports (exit);
  bitmap_copy (imports, r_imports);

  auto_vec<tree> worklist (bitmap_count_bits (imports));
  bitmap_iterator bi;
  unsigned i;
  EXECUTE_IF_SET_IN_BITMAP (imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      worklist.quick_push (name);
    }

  // ...and add any operands used to define these imports.
  while (!worklist.is_empty ())
    {
      tree name = worklist.pop ();
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);

      if (is_gimple_assign (def_stmt))
	{
	  add_to_imports (gimple_assign_rhs1 (def_stmt), imports);
	  tree rhs = gimple_assign_rhs2 (def_stmt);
	  if (rhs && add_to_imports (rhs, imports))
	    worklist.safe_push (rhs);
	  rhs = gimple_assign_rhs3 (def_stmt);
	  if (rhs && add_to_imports (rhs, imports))
	    worklist.safe_push (rhs);
	}
      else if (gphi *phi = dyn_cast <gphi *> (def_stmt))
	{
	  for (size_t i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      edge e = gimple_phi_arg_edge (phi, i);
	      tree arg = gimple_phi_arg (phi, i)->def;

	      if (TREE_CODE (arg) == SSA_NAME
		  && m_path.contains (e->src)
		  && bitmap_set_bit (imports, SSA_NAME_VERSION (arg)))
		worklist.safe_push (arg);
	    }
	}
    }
  // Exported booleans along the path, may help conditionals.
  if (m_resolve)
    for (i = 0; i < m_path.length (); ++i)
      {
	basic_block bb = m_path[i];
	tree name;
	FOR_EACH_GORI_EXPORT_NAME (gori, bb, name)
	  if (TREE_CODE (TREE_TYPE (name)) == BOOLEAN_TYPE)
	    bitmap_set_bit (imports, SSA_NAME_VERSION (name));
      }
}

// Compute the ranges for IMPORTS along PATH.
//
// IMPORTS are the set of SSA names, any of which could potentially
// change the value of the final conditional in PATH.  Default to the
// imports of the last block in the path if none is given.

void
path_range_query::compute_ranges (const vec<basic_block> &path,
				  const bitmap_head *imports)
{
  if (DEBUG_SOLVER)
    fprintf (dump_file, "\n==============================================\n");

  set_path (path);
  m_undefined_path = false;

  if (imports)
    bitmap_copy (m_imports, imports);
  else
    compute_imports (m_imports, exit_bb ());

  if (m_resolve)
    get_path_oracle ()->reset_path ();

  if (DEBUG_SOLVER)
    {
      fprintf (dump_file, "path_range_query: compute_ranges for path: ");
      for (unsigned i = path.length (); i > 0; --i)
	{
	  basic_block bb = path[i - 1];
	  fprintf (dump_file, "%d", bb->index);
	  if (i > 1)
	    fprintf (dump_file, "->");
	}
      fprintf (dump_file, "\n");
    }

  while (1)
    {
      basic_block bb = curr_bb ();

      compute_ranges_in_block (bb);
      adjust_for_non_null_uses (bb);

      if (at_exit ())
	break;

      move_next ();
    }

  if (DEBUG_SOLVER)
    {
      get_path_oracle ()->dump (dump_file);
      dump (dump_file);
    }
}

// Convenience function to compute ranges along a path consisting of
// E->SRC and E->DEST.

void
path_range_query::compute_ranges (edge e)
{
  auto_vec<basic_block> bbs (2);
  bbs.quick_push (e->dest);
  bbs.quick_push (e->src);
  compute_ranges (bbs);
}

// A folding aid used to register and query relations along a path.
// When queried, it returns relations as they would appear on exit to
// the path.
//
// Relations are registered on entry so the path_oracle knows which
// block to query the root oracle at when a relation lies outside the
// path.  However, when queried we return the relation on exit to the
// path, since the root_oracle ignores the registered.

class jt_fur_source : public fur_depend
{
public:
  jt_fur_source (gimple *s, path_range_query *, gori_compute *,
		 const vec<basic_block> &);
  relation_kind query_relation (tree op1, tree op2) override;
  void register_relation (gimple *, relation_kind, tree op1, tree op2) override;
  void register_relation (edge, relation_kind, tree op1, tree op2) override;
private:
  basic_block m_entry;
};

jt_fur_source::jt_fur_source (gimple *s,
			      path_range_query *query,
			      gori_compute *gori,
			      const vec<basic_block> &path)
  : fur_depend (s, gori, query)
{
  gcc_checking_assert (!path.is_empty ());

  m_entry = path[path.length () - 1];

  if (dom_info_available_p (CDI_DOMINATORS))
    m_oracle = query->oracle ();
  else
    m_oracle = NULL;
}

// Ignore statement and register relation on entry to path.

void
jt_fur_source::register_relation (gimple *, relation_kind k, tree op1, tree op2)
{
  if (m_oracle)
    m_oracle->register_relation (m_entry, k, op1, op2);
}

// Ignore edge and register relation on entry to path.

void
jt_fur_source::register_relation (edge, relation_kind k, tree op1, tree op2)
{
  if (m_oracle)
    m_oracle->register_relation (m_entry, k, op1, op2);
}

relation_kind
jt_fur_source::query_relation (tree op1, tree op2)
{
  if (!m_oracle)
    return VREL_NONE;

  if (TREE_CODE (op1) != SSA_NAME || TREE_CODE (op2) != SSA_NAME)
    return VREL_NONE;

  return m_oracle->query_relation (m_entry, op1, op2);
}

// Return the range of STMT at the end of the path being analyzed.

bool
path_range_query::range_of_stmt (irange &r, gimple *stmt, tree)
{
  tree type = gimple_range_type (stmt);

  if (!irange::supports_type_p (type))
    return false;

  // If resolving unknowns, fold the statement making use of any
  // relations along the path.
  if (m_resolve)
    {
      fold_using_range f;
      jt_fur_source src (stmt, this, &m_ranger->gori (), m_path);
      if (!f.fold_stmt (r, stmt, src))
	r.set_varying (type);
    }
  // Otherwise, fold without relations.
  else if (!fold_range (r, stmt, this))
    r.set_varying (type);

  return true;
}

void
path_range_query::maybe_register_phi_relation (gphi *phi, tree arg)
{
  basic_block bb = gimple_bb (phi);
  tree result = gimple_phi_result (phi);

  // Avoid recording the equivalence if the arg is defined in this
  // block, as that could create an ordering problem.
  if (ssa_defined_in_bb (arg, bb))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  from bb%d:", bb->index);

  get_path_oracle ()->killing_def (result);
  m_oracle->register_relation (entry_bb (), EQ_EXPR, arg, result);
}

// Compute relations for each PHI in BB.  For example:
//
//   x_5 = PHI<y_9(5),...>
//
// If the path flows through BB5, we can register that x_5 == y_9.

void
path_range_query::compute_phi_relations (basic_block bb, basic_block prev)
{
  if (prev == NULL)
    return;

  edge e_in = find_edge (prev, bb);

  for (gphi_iterator iter = gsi_start_phis (bb); !gsi_end_p (iter);
       gsi_next (&iter))
    {
      gphi *phi = iter.phi ();
      tree result = gimple_phi_result (phi);
      unsigned nargs = gimple_phi_num_args (phi);

      if (!import_p (result))
	continue;

      for (size_t i = 0; i < nargs; ++i)
	if (e_in == gimple_phi_arg_edge (phi, i))
	  {
	    tree arg = gimple_phi_arg_def (phi, i);

	    if (gimple_range_ssa_p (arg))
	      maybe_register_phi_relation (phi, arg);
	    break;
	  }
    }
}

// Compute outgoing relations from BB to NEXT.

void
path_range_query::compute_outgoing_relations (basic_block bb, basic_block next)
{
  gimple *stmt = last_stmt (bb);

  if (stmt
      && gimple_code (stmt) == GIMPLE_COND
      && (import_p (gimple_cond_lhs (stmt))
	  || import_p (gimple_cond_rhs (stmt))))
    {
      int_range<2> r;
      gcond *cond = as_a<gcond *> (stmt);
      edge e0 = EDGE_SUCC (bb, 0);
      edge e1 = EDGE_SUCC (bb, 1);

      if (e0->dest == next)
	gcond_edge_range (r, e0);
      else if (e1->dest == next)
	gcond_edge_range (r, e1);
      else
	gcc_unreachable ();

      jt_fur_source src (NULL, this, &m_ranger->gori (), m_path);
      src.register_outgoing_edges (cond, r, e0, e1);
    }
}
