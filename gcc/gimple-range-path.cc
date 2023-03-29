/* Basic block path solver.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
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

path_range_query::path_range_query (gimple_ranger &ranger,
				    const vec<basic_block> &path,
				    const bitmap_head *dependencies,
				    bool resolve)
  : m_cache (new ssa_global_cache),
    m_has_cache_entry (BITMAP_ALLOC (NULL)),
    m_ranger (ranger),
    m_resolve (resolve)
{
  m_oracle = new path_oracle (m_ranger.oracle ());

  reset_path (path, dependencies);
}

path_range_query::path_range_query (gimple_ranger &ranger, bool resolve)
  : m_cache (new ssa_global_cache),
    m_has_cache_entry (BITMAP_ALLOC (NULL)),
    m_ranger (ranger),
    m_resolve (resolve)
{
  m_oracle = new path_oracle (m_ranger.oracle ());
}

path_range_query::~path_range_query ()
{
  delete m_oracle;
  BITMAP_FREE (m_has_cache_entry);
  delete m_cache;
}

// Return TRUE if NAME is an exit dependency for the path.

bool
path_range_query::exit_dependency_p (tree name)
{
  return (TREE_CODE (name) == SSA_NAME
	  && bitmap_bit_p (m_exit_dependencies, SSA_NAME_VERSION (name)));
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
path_range_query::get_cache (vrange &r, tree name)
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
path_range_query::set_cache (const vrange &r, tree name)
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

  fprintf (dump_file, "Exit dependencies:\n");
  EXECUTE_IF_SET_IN_BITMAP (m_exit_dependencies, 0, i, bi)
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
path_range_query::range_on_path_entry (vrange &r, tree name)
{
  gcc_checking_assert (defined_outside_path (name));
  basic_block entry = entry_bb ();
  m_ranger.range_on_entry (r, entry, name);
}

// Return the range of NAME at the end of the path being analyzed.

bool
path_range_query::internal_range_of_expr (vrange &r, tree name, gimple *stmt)
{
  if (!r.supports_type_p (TREE_TYPE (name)))
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
	{
	  Value_Range glob (TREE_TYPE (name));
	  gimple_range_global (glob, name);
	  r.intersect (glob);
	}

      set_cache (r, name);
      return true;
    }

  gimple_range_global (r, name);
  return true;
}

bool
path_range_query::range_of_expr (vrange &r, tree name, gimple *stmt)
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

// Reset the current path to PATH.

void
path_range_query::reset_path (const vec<basic_block> &path,
			      const bitmap_head *dependencies)
{
  gcc_checking_assert (path.length () > 1);
  m_path = path.copy ();
  m_pos = m_path.length () - 1;
  m_undefined_path = false;
  bitmap_clear (m_has_cache_entry);

  compute_ranges (dependencies);
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
path_range_query::ssa_range_in_phi (vrange &r, gphi *phi)
{
  tree name = gimple_phi_result (phi);

  if (at_entry ())
    {
      if (m_resolve && m_ranger.range_of_expr (r, name, phi))
	return;

      // Try to fold the phi exclusively with global or cached values.
      // This will get things like PHI <5(99), 6(88)>.  We do this by
      // calling range_of_expr with no context.
      unsigned nargs = gimple_phi_num_args (phi);
      Value_Range arg_range (TREE_TYPE (name));
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

  basic_block bb = gimple_bb (phi);
  basic_block prev = prev_bb ();
  edge e_in = find_edge (prev, bb);
  tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e_in);
  // Avoid using the cache for ARGs defined in this block, as
  // that could create an ordering problem.
  if (ssa_defined_in_bb (arg, bb) || !get_cache (r, arg))
    {
      if (m_resolve)
	{
	  Value_Range tmp (TREE_TYPE (name));
	  // Using both the range on entry to the path, and the
	  // range on this edge yields significantly better
	  // results.
	  if (TREE_CODE (arg) == SSA_NAME
	      && defined_outside_path (arg))
	    range_on_path_entry (r, arg);
	  else
	    r.set_varying (TREE_TYPE (name));
	  m_ranger.range_on_edge (tmp, e_in, arg);
	  r.intersect (tmp);
	  return;
	}
      r.set_varying (TREE_TYPE (name));
    }
}

// If NAME is defined in BB, set R to the range of NAME, and return
// TRUE.  Otherwise, return FALSE.

bool
path_range_query::range_defined_in_block (vrange &r, tree name, basic_block bb)
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

  if (bb && POINTER_TYPE_P (TREE_TYPE (name)))
    m_ranger.m_cache.m_exit.maybe_adjust_range (r, name, bb);

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
  auto_bitmap phi_set;

  // PHIs must be resolved simultaneously on entry to the block
  // because any dependencies must be satisfied with values on entry.
  // Thus, we calculate all PHIs first, and then update the cache at
  // the end.

  for (auto iter = gsi_start_phis (bb); !gsi_end_p (iter); gsi_next (&iter))
    {
      gphi *phi = iter.phi ();
      tree name = gimple_phi_result (phi);

      if (!exit_dependency_p (name))
	continue;

      Value_Range r (TREE_TYPE (name));
      if (range_defined_in_block (r, name, bb))
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

// Return TRUE if relations may be invalidated after crossing edge E.

bool
path_range_query::relations_may_be_invalidated (edge e)
{
  // As soon as the path crosses a back edge, we can encounter
  // definitions of SSA_NAMEs that may have had a use in the path
  // already, so this will then be a new definition.  The relation
  // code is all designed around seeing things in dominator order, and
  // crossing a back edge in the path violates this assumption.
  return (e->flags & EDGE_DFS_BACK);
}

// Compute ranges defined in the current block, or exported to the
// next block.

void
path_range_query::compute_ranges_in_block (basic_block bb)
{
  bitmap_iterator bi;
  unsigned i;

  if (m_resolve && !at_entry ())
    compute_phi_relations (bb, prev_bb ());

  // Force recalculation of any names in the cache that are defined in
  // this block.  This can happen on interdependent SSA/phis in loops.
  EXECUTE_IF_SET_IN_BITMAP (m_exit_dependencies, 0, i, bi)
    {
      tree name = ssa_name (i);
      if (ssa_defined_in_bb (name, bb))
	clear_cache (name);
    }

  // Solve dependencies defined in this block, starting with the PHIs...
  compute_ranges_in_phis (bb);
  // ...and then the rest of the dependencies.
  EXECUTE_IF_SET_IN_BITMAP (m_exit_dependencies, 0, i, bi)
    {
      tree name = ssa_name (i);
      Value_Range r (TREE_TYPE (name));

      if (gimple_code (SSA_NAME_DEF_STMT (name)) != GIMPLE_PHI
	  && range_defined_in_block (r, name, bb))
	set_cache (r, name);
    }

  if (at_exit ())
    return;

  // Solve dependencies that are exported to the next block.
  basic_block next = next_bb ();
  edge e = find_edge (bb, next);

  if (m_resolve && relations_may_be_invalidated (e))
    {
      if (DEBUG_SOLVER)
	fprintf (dump_file,
		 "Resetting relations as they may be invalidated in %d->%d.\n",
		 e->src->index, e->dest->index);

      path_oracle *p = get_path_oracle ();
      // ?? Instead of nuking the root oracle altogether, we could
      // reset the path oracle to search for relations from the top of
      // the loop with the root oracle.  Something for future development.
      p->reset_path ();
    }

  gori_compute &g = m_ranger.gori ();
  bitmap exports = g.exports (bb);
  EXECUTE_IF_AND_IN_BITMAP (m_exit_dependencies, exports, 0, i, bi)
    {
      tree name = ssa_name (i);
      Value_Range r (TREE_TYPE (name));
      if (g.outgoing_edge_range_p (r, e, name, *this))
	{
	  Value_Range cached_range (TREE_TYPE (name));
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

  if (m_resolve)
    compute_outgoing_relations (bb, next);
}

// Adjust all pointer exit dependencies in BB with non-null information.

void
path_range_query::adjust_for_non_null_uses (basic_block bb)
{
  int_range_max r;
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (m_exit_dependencies, 0, i, bi)
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

      if (m_ranger.m_cache.m_exit.maybe_adjust_range (r, name, bb))
	set_cache (r, name);
    }
}

// If NAME is a supported SSA_NAME, add it to the bitmap in dependencies.

bool
path_range_query::add_to_exit_dependencies (tree name, bitmap dependencies)
{
  if (TREE_CODE (name) == SSA_NAME
      && Value_Range::supports_type_p (TREE_TYPE (name)))
    return bitmap_set_bit (dependencies, SSA_NAME_VERSION (name));
  return false;
}

// Compute the exit dependencies to PATH.  These are essentially the
// SSA names used to calculate the final conditional along the path.

void
path_range_query::compute_exit_dependencies (bitmap dependencies)
{
  // Start with the imports from the exit block...
  basic_block exit = m_path[0];
  gori_compute &gori = m_ranger.gori ();
  bitmap_copy (dependencies, gori.imports (exit));

  auto_vec<tree> worklist (bitmap_count_bits (dependencies));
  bitmap_iterator bi;
  unsigned i;
  EXECUTE_IF_SET_IN_BITMAP (dependencies, 0, i, bi)
    {
      tree name = ssa_name (i);
      worklist.quick_push (name);
    }

  // ...and add any operands used to define these imports.
  while (!worklist.is_empty ())
    {
      tree name = worklist.pop ();
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      if (SSA_NAME_IS_DEFAULT_DEF (name)
	  || !m_path.contains (gimple_bb (def_stmt)))
	continue;

      if (gphi *phi = dyn_cast <gphi *> (def_stmt))
	{
	  for (size_t i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      edge e = gimple_phi_arg_edge (phi, i);
	      tree arg = gimple_phi_arg (phi, i)->def;

	      if (TREE_CODE (arg) == SSA_NAME
		  && m_path.contains (e->src)
		  && bitmap_set_bit (dependencies, SSA_NAME_VERSION (arg)))
		worklist.safe_push (arg);
	    }
	}
      else if (gassign *ass = dyn_cast <gassign *> (def_stmt))
	{
	  tree ssa[3];
	  unsigned count = gimple_range_ssa_names (ssa, 3, ass);
	  for (unsigned j = 0; j < count; ++j)
	    if (add_to_exit_dependencies (ssa[j], dependencies))
	      worklist.safe_push (ssa[j]);
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
	    bitmap_set_bit (dependencies, SSA_NAME_VERSION (name));
      }
}

// Compute the ranges for DEPENDENCIES along PATH.
//
// DEPENDENCIES are path exit dependencies.  They are the set of SSA
// names, any of which could potentially change the value of the final
// conditional in PATH.  If none is given, the exit dependencies are
// calculated from the final conditional in the path.

void
path_range_query::compute_ranges (const bitmap_head *dependencies)
{
  if (DEBUG_SOLVER)
    fprintf (dump_file, "\n==============================================\n");

  if (dependencies)
    bitmap_copy (m_exit_dependencies, dependencies);
  else
    compute_exit_dependencies (m_exit_dependencies);

  if (m_resolve)
    {
      path_oracle *p = get_path_oracle ();
      p->reset_path (m_ranger.oracle ());
    }

  if (DEBUG_SOLVER)
    {
      fprintf (dump_file, "path_range_query: compute_ranges for path: ");
      for (unsigned i = m_path.length (); i > 0; --i)
	{
	  basic_block bb = m_path[i - 1];
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
    return VREL_VARYING;

  if (TREE_CODE (op1) != SSA_NAME || TREE_CODE (op2) != SSA_NAME)
    return VREL_VARYING;

  return m_oracle->query_relation (m_entry, op1, op2);
}

// Return the range of STMT at the end of the path being analyzed.

bool
path_range_query::range_of_stmt (vrange &r, gimple *stmt, tree)
{
  tree type = gimple_range_type (stmt);

  if (!type || !r.supports_type_p (type))
    return false;

  // If resolving unknowns, fold the statement making use of any
  // relations along the path.
  if (m_resolve)
    {
      fold_using_range f;
      jt_fur_source src (stmt, this, &m_ranger.gori (), m_path);
      if (!f.fold_stmt (r, stmt, src))
	r.set_varying (type);
    }
  // Otherwise, fold without relations.
  else if (!fold_range (r, stmt, this))
    r.set_varying (type);

  return true;
}

// If possible, register the relation on the incoming edge E into PHI.

void
path_range_query::maybe_register_phi_relation (gphi *phi, edge e)
{
  tree arg = gimple_phi_arg_def (phi, e->dest_idx);

  if (!gimple_range_ssa_p (arg))
    return;

  if (relations_may_be_invalidated (e))
    return;

  basic_block bb = gimple_bb (phi);
  tree result = gimple_phi_result (phi);

  // Avoid recording the equivalence if the arg is defined in this
  // block, as that could create an ordering problem.
  if (ssa_defined_in_bb (arg, bb))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "maybe_register_phi_relation in bb%d:", bb->index);

  get_path_oracle ()->killing_def (result);
  m_oracle->register_relation (entry_bb (), VREL_EQ, arg, result);
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

      if (!exit_dependency_p (result))
	continue;

      for (size_t i = 0; i < nargs; ++i)
	if (e_in == gimple_phi_arg_edge (phi, i))
	  {
	    maybe_register_phi_relation (phi, e_in);
	    break;
	  }
    }
}

// Compute outgoing relations from BB to NEXT.

void
path_range_query::compute_outgoing_relations (basic_block bb, basic_block next)
{
  if (gcond *cond = safe_dyn_cast <gcond *> (last_stmt (bb)))
    {
      int_range<2> r;
      edge e0 = EDGE_SUCC (bb, 0);
      edge e1 = EDGE_SUCC (bb, 1);

      if (e0->dest == next)
	gcond_edge_range (r, e0);
      else if (e1->dest == next)
	gcond_edge_range (r, e1);
      else
	gcc_unreachable ();

      jt_fur_source src (NULL, this, &m_ranger.gori (), m_path);
      src.register_outgoing_edges (cond, r, e0, e1);
    }
}
