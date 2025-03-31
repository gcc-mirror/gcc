/* Calculate prime path coverage.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "memmodel.h"
#include "target.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "tree-cfg.h"
#include "tree-nested.h"
#include "cfg.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "coverage.h"
#include "ssa.h"
#include "vec.h"
#include "sbitmap.h"
#include "graphds.h"
#include "hash-map.h"
#include "bitmap.h"
#include "cfganal.h"

bool mark_dfs_back_edges (struct function *);
vec<vec<int>> prime_paths (struct graph*, size_t);

namespace
{

/* Check if all the successors of BB are abnormal, e.g. setjmp.  */
bool
all_abnormal_succs_p (basic_block bb)
{
  for (edge e : bb->succs)
    if (!(e->flags & EDGE_ABNORMAL))
      return false;
  return true;
}

/* Build a struct graph equivalent to the CFG for the function FN.  The caller
   must free the returned graph with free_graph.  The data field of every
   vertex and edge point to the basic blocks and edges in the CFG.

   The CFG recording and gcov is not aware of abnormal edges, so they are
   ignored here, too.  This means some paths are lost, e.g. those that involve
   setjmp/longjmp.  They are still paths but would need more support from
   profile.cc and gcov.cc to work.  */
struct graph*
cfg_as_graph (struct function* fn)
{
  struct graph *g = new_graph (n_basic_blocks_for_fn (fn));
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (fn);
  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (fn);

  g->vertices[entry->index].data = entry;
  g->vertices[exit->index].data = exit;

  const unsigned ignore = EDGE_FAKE | EDGE_ABNORMAL | EDGE_ABNORMAL_CALL;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      g->vertices[bb->index].data = bb;
      for (edge e : bb->succs)
	if (!(e->flags & ignore) && e->dest != exit)
	  add_edge (g, e->src->index, e->dest->index)->data = e;
    }
  return g;
}

/* Check if BB's predecessor is the ENTRY_BLOCK, i.e. BB is the first real
   block in the function.  */
bool
pred_entry_p (const_basic_block bb)
{
  return single_pred_p (bb) && single_pred (bb)->index == ENTRY_BLOCK;
}

/* Find the prime paths for the function FN with the ENTRY and EXIT blocks
   removed.  This can lead to empty paths when there is a fake edge to the exit
   block, for example for abort functions or infinite loops.  Empty paths are
   removed because the length of the returned vec is important.  */
vec<vec<int>>
find_prime_paths (struct function *fn)
{
  struct graph *cfg = cfg_as_graph (fn);
  vec<vec<int>> paths = prime_paths (cfg, path_coverage_limit);

  bool any_empty = false;
  for (vec<int> &path : paths)
    {
      /* setjmp calls will be isolated basic blocks when ABNORMAL_EDGEs are
	 removed.  If a path is made up of just such a vertex it is pointless
	 and can be removed.  If a function is only __builtin_exit() (see
	 gcov-22.C) the CFG will only have one block and look like such an
	 island, and we want to preserve it.  */
      if (path.length () == 1
	  && !pred_entry_p (BASIC_BLOCK_FOR_FN (fn, path[0]))
	  && all_abnormal_succs_p (BASIC_BLOCK_FOR_FN (fn, path[0])))
	path.pop ();
      if (!path.is_empty () && path[0] == ENTRY_BLOCK)
	path.ordered_remove (0);
      if (!path.is_empty () && path.last () == EXIT_BLOCK)
	path.pop ();

      if (path.is_empty ())
	{
	  any_empty = true;
	  path.release ();
	}
    }

  unsigned ix1, ix2;
  vec<int> *ptr;
  if (any_empty)
    VEC_ORDERED_REMOVE_IF (paths, ix1, ix2, ptr, ptr->is_empty ());

  return paths;
}

/* Return the edge between SRC and DST.  */
edge
edge_between (struct function *fn, int src, int dst)
{
  basic_block bbsrc = BASIC_BLOCK_FOR_FN (fn, src);
  basic_block bbdst = BASIC_BLOCK_FOR_FN (fn, dst);
  for (edge e : bbsrc->succs)
    if (e->dest == bbdst)
      return e;
  gcc_unreachable ();
}

/* Get the basic blocks of FN in topological order so that all predecessors
   come before the vertex, while ignoring back edges.  */
vec<basic_block>
topsort (struct function *fn)
{
  vec<basic_block> blocks {};
  auto_vec<int> order {};
  blocks.reserve (n_basic_blocks_for_fn (fn));
  order.safe_grow (n_basic_blocks_for_fn (fn));

  const int n = post_order_compute (order.address (), false, false);
  for (int i = 0; i < n; ++i)
    blocks.quick_push (BASIC_BLOCK_FOR_FN (fn, order[i]));
  blocks.reverse ();
  return blocks;
}

/* Hasher for maps where the key is a pair of edge/basic_block and bucket id
   (size_t).  */
template <typename T>
using bucket_hash = pair_hash <nofree_ptr_hash <T>,
			       int_hash <size_t, size_t(-1)>>;

/* Hasher for {edge, bucket-id} keys.  */
using edge_hash = bucket_hash <edge_def>;
/* Hasher for {basic_block, bucket-id} keys.  */
using block_hash = bucket_hash <basic_block_def>;

/* Find the union of the bitsets sets on the incoming edges of BB at BUCKET.
   If no paths go through BB the returned bitset would be empty.  Bitsets are
   looked up in ANDS, and if the nth bit is set then the nth path contains that
   edge.  */
uint64_t
union_incoming_bit_and (hash_map <edge_hash, uint64_t> &ands,
			const basic_block bb, size_t bucket)
{
  uint64_t inc = 0;
  for (edge e : bb->preds)
    {
      const uint64_t *val = ands.get ({e, bucket});
      inc |= (val ? *val : 0);
    }
  return inc;
}


/* Check if the incoming edges have the power to modify the paths in flight for
   BUCKET.  If all the incoming edges would apply the same bitmask the
   BB+BUCKET will not actually update the path set, and we don't need to emit
   an AND_EXPR and have a later fork distinguish the paths.  */
bool
can_change_path_p (hash_map <edge_hash, uint64_t> &ands,
		   const basic_block bb, size_t bucket, uint64_t all_and)
{
  for (edge e : bb->preds)
    {
      const uint64_t *e_and = ands.get ({e, bucket});
      if (!e_and || *e_and != all_and)
	return true;
    }
  return false;
}

/* Check if all bits in BITSET are 1 for the target size TARGET_SIZE.  For a
   32-bit target only the bits in the lower half should be set, and this should
   return true when all bits in the lower half are set, even if the bitset type
   have room for more bits.  */
bool
all_bits_set_p (uint64_t bitset, size_t target_size)
{
  return (size_t)popcount_hwi (bitset) == target_size;
}

/* Create a constant or SSA name of GCOV_TYPE_NODE type and zero-assign to it
   safely on the edge E.  If the edge is abnormal it is assumed the phi is
   abnormal and we need an SSA name, otherwise fall back to a constant.  The
   returned value is safe to use with add_phi_arg ().

   If the edge is abnormal we cannot insert on it directly, and instead
   carefully add the assignment on the source block.  If that source block ends
   with control flow (like those produced by _setjmp) we must insert before to
   not break that invariant, otherwise insert after so that things like the
   setjmp receiver is the first element of the basic block.  Doing the assign
   is necessary as phis cannot resolve to constants.  */
tree
safe_insert_zero_phi_arg (edge e, tree gcov_type_node)
{
  tree cst = build_zero_cst (gcov_type_node);
  if (!(e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL)))
    return cst;

  tree zero = make_ssa_name (gcov_type_node);
  gassign *put = gimple_build_assign (zero, cst);
  gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (e->src);
  if (gimple_call_internal_p (*gsi, IFN_ABNORMAL_DISPATCHER)
      || stmt_ends_bb_p (*gsi))
    gsi_insert_before (&gsi, put, GSI_NEW_STMT);
  else
    gsi_insert_after (&gsi, put, GSI_NEW_STMT);

  return zero;
}

/* Check if SSA is a constant value (created by build_int_cst) and can be
   folded.  */
bool
constant_p (tree ssa)
{
  return tree_fits_uhwi_p (ssa);
}

/* A fixup task.  When resolving the exit SSA for a back edge arg to a phi
   node, the exit SSA has not been created yet.  Record what needs to be done
   when it is created, and tie the phi to the right SSA name once it is
   guaranteed it is created.  If MASK is nullptr the predecessor's SSA should
   be used as-is, and does not need an AND.  This should only be created with
   the helpers fixup_noop and fixup_and.  */
struct fixup
{
  gphi *phi;
  edge e;
  tree lhs;
  tree mask;
  size_t bucket;
};

/* Create a fixup with a no-op for the PHI in BUCKET.  Use this when the edge E
   does not need an AND applied and should rather propagate the predecessor's
   SSA name.  */
fixup
fixup_noop (gphi *phi, edge e, size_t bucket)
{
  fixup todo;
  todo.phi = phi;
  todo.e = e;
  todo.bucket = bucket;
  todo.lhs = nullptr;
  todo.mask = nullptr;
  return todo;
}

/* Create a fixup for PHI through BUCKET with the exit SSA name E->src ANDed
   with MASK (E->src & MASK).  GCOV_TYPE_NODE should be a tree of the gcov type
   node for creating SSA names.  */
fixup
fixup_and (gphi *phi, edge e, size_t bucket, uint64_t mask,
	   tree gcov_type_node)
{
  fixup todo;
  todo.phi = phi;
  todo.e = e;
  todo.bucket = bucket;
  todo.lhs = make_ssa_name (gcov_type_node);
  todo.mask = build_int_cst (gcov_type_node, mask);
  return todo;
}

/* Insert LOCAL |= MASK on BB safely, even when BB is a returns_twice block.

   This is a helper to safely emit code for updating the path bitmask in the
   presence of abnormal edges and returns_twice blocks, since they have special
   restrictions on edge splits and first/last instructions on the block.  */
tree
safe_insert_ior (basic_block bb, tree local, tree mask, gphi *phi,
		 tree gcov_type_node)
{
  gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (bb);
  gimple *stmt = gsi_stmt (gsi);

  /* This is a returns_twice block (e.g. setjmp) which does not really expect
     anything before or after, so we cannot insert the IOR on the block itself.
     We move the IORs to the predecessors instead and update the phi.  The
     abnormal edge cannot be split, so in that case we carefully put the IOR
     after the ABNORMAL_DISPATCHER.  */
  if (stmt && is_gimple_call (stmt)
      && (gimple_call_flags (stmt) & ECF_RETURNS_TWICE))
    {
      for (edge e : bb->preds)
	{
	  gcc_checking_assert (phi);
	  tree next = make_ssa_name (gcov_type_node);
	  tree prev = gimple_phi_arg_def_from_edge (phi, e);
	  gassign *put = prev
	    ? gimple_build_assign (next, BIT_IOR_EXPR, prev, mask)
	    : gimple_build_assign (next, mask);

	  gimple_stmt_iterator gsi = gsi_last_bb (e->src);
	  add_phi_arg (phi, next, e, UNKNOWN_LOCATION);

	  if (e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	    gsi_insert_before (&gsi, put, GSI_SAME_STMT);
	  else
	    gsi_insert_on_edge (e, put);
	}
    }
  else
    {
      tree next = make_ssa_name (gcov_type_node);
      gassign *put = gimple_build_assign (next, BIT_IOR_EXPR, local, mask);
      gsi_insert_before (&gsi, put, GSI_SAME_STMT);
      local = next;
    }
  return local;
}

/* Insert instructions updating the global counter at BUCKET with the contents
   of (LOCAL & MASK).  LOCAL is the SSA counter for this bucket, MASK the bits
   that should be updated (that is, the paths that end in this basic block).
   If ATOMIC_IOR is non-null the it uses atomics.  GCOV_TYPE_NODE should be a
   tree of the gcov type node for creating SSA names.

   global[BUCKET] |= (LOCAL & MASK)

   If MASK is null, no mask is applied and it becomes:

   global[BUCKET] |= LOCAL

   This function should only be necessary for the successor of an
   ABNORMAL_DISPATCHER e.g. the destination of a longjmp.  Paths starting at a
   longjmp do not have anything to flush, so those edges are simply ignored.
   Since this is a returns_twice block we cannot put anything before (or really
   after), so we instrument on the edge itself, rather than messing with
   splitting and changing the graph now.

   This is similar to gsi_safe_insert_before, but this function does not
   immediately commit edge inserts and does not split blocks.  */
void
flush_on_edges (basic_block bb, size_t bucket, tree local, tree mask,
		tree atomic_ior, tree gcov_type_node)
{
  gimple *def = SSA_NAME_DEF_STMT (local);
  gphi *phi = dyn_cast <gphi *> (def);

  tree relaxed = nullptr;
  if (atomic_ior)
    relaxed = build_int_cst (integer_type_node, MEMMODEL_RELAXED);

  for (edge e : bb->preds)
    {
      if (e->flags & (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL))
	continue;

      tree global = tree_coverage_counter_ref (GCOV_COUNTER_PATHS, bucket);
      if (phi)
	local = gimple_phi_arg_def_from_edge (phi, e);

      tree global_copy = make_ssa_name (gcov_type_node);
      gassign *ga1 = gimple_build_assign (global_copy, global);
      gsi_insert_on_edge (e, ga1);

      tree masked;
      if (mask)
	{
	  masked = make_ssa_name (gcov_type_node);
	  gassign *ga2 = gimple_build_assign (masked, BIT_AND_EXPR, local,
					      mask);
	  gsi_insert_on_edge (e, ga2);
	}
      else
	masked = local;

      if (atomic_ior)
	{
	  global = unshare_expr (global);
	  gcall *call = gimple_build_call (atomic_ior, 3, build_addr (global),
					   masked, relaxed);
	  gsi_insert_on_edge (e, call);
	}
      else
	{
	  tree tmp = make_ssa_name (gcov_type_node);
	  gassign *ga3 = gimple_build_assign (tmp, BIT_IOR_EXPR, global_copy,
					      masked);
	  gassign *ga4 = gimple_build_assign (unshare_expr (global), tmp);
	  gsi_insert_on_edge (e, ga3);
	  gsi_insert_on_edge (e, ga4);
	}
    }
}

/* Insert instructions update the global counter at BUCKET with the contents of
   (LOCAL & MASK).  LOCAL is the SSA counter for this bucket, MASK the bits
   that should be updated (that is, the paths that end in this basic block).
   If ATOMIC_IOR is non-null the it uses atomics.  GCOV_TYPE_NODE should be a
   tree of the gcov type node for creating SSA names.

   global[BUCKET] |= (LOCAL & MASK)

   If MASK is null, no mask is applied and it becomes:

   global[BUCKET] |= LOCAL

   This function should be used on every block except returns_twice blocks (see
   flush_on_edge) as all incoming edges can use the same flushing code.  */
void
flush_on_gsi (gimple_stmt_iterator *gsi, size_t bucket, tree local, tree mask,
	      tree atomic_ior, tree gcov_type_node)
{
  tree relaxed = nullptr;
  if (atomic_ior)
    relaxed = build_int_cst (integer_type_node, MEMMODEL_RELAXED);
  tree global = tree_coverage_counter_ref (GCOV_COUNTER_PATHS, bucket);

  tree global_copy = make_ssa_name (gcov_type_node);
  gassign *ga1 = gimple_build_assign (global_copy, global);
  gsi_insert_before (gsi, ga1, GSI_SAME_STMT);

  tree masked;
  if (mask)
    {
      masked = make_ssa_name (gcov_type_node);
      gassign *ga2 = gimple_build_assign (masked, BIT_AND_EXPR, local, mask);
      gsi_insert_before (gsi, ga2, GSI_SAME_STMT);
    }
  else
    masked = local;

  if (atomic_ior)
    {
      global = unshare_expr (global);
      gcall *call = gimple_build_call (atomic_ior, 3, build_addr (global),
				       masked, relaxed);
      gsi_insert_before (gsi, call, GSI_SAME_STMT);
    }
  else
    {
      tree tmp = make_ssa_name (gcov_type_node);
      gassign *ga3 = gimple_build_assign (tmp, BIT_IOR_EXPR, global_copy,
					  masked);
      gassign *ga4 = gimple_build_assign (unshare_expr (global), tmp);
      gsi_insert_before (gsi, ga3, GSI_SAME_STMT);
      gsi_insert_before (gsi, ga4, GSI_SAME_STMT);
    }
}

} // namespace


/* Instrument FN for prime path coverage, enabled by -fpath-coverage.  The
   number of paths grows very fast with the complexity (control flow) which
   explodes compile times and object file size.  Giving up is controlled by the
   -fpath-coverage-limit flag.  The paths are sorted lexicographically by basic
   block id, and each path is identified by its index in the sorted set of
   paths, which in turn corresponds to a bit in a large bitset associated with
   FN.  The monitoring code is a few bitwise operations added to edges and
   basic blocks to maintain a set of live paths (note that many paths may
   overlap and that many paths may be covered by the same input).  When
   reaching the final vertex of a path the global counters are updated.

   This is made more complicated by the gcov type having very limited size.  To
   support functions with more than 32/64 paths the bitmap is implemented on
   top of a sequence of gcov integers, "buckets", where path N is recorded as
   bit N%64 in bucket N/64.  For large functions, an individual basic block
   will only be part of a small subset of paths, and by extension buckets and
   local counters.  Only the necessary counters are read and written.  */
unsigned
instrument_prime_paths (struct function *fn)
{
  mark_dfs_back_edges (fn);
  vec<vec<int>> paths = find_prime_paths (fn);

  if (paths.is_empty ())
    {
      warning_at (fn->function_start_locus, OPT_Wcoverage_too_many_paths,
		  "paths exceeding limit, giving up path coverage");
      release_vec_vec (paths);
      return 0;
    }

  tree gcov_type_node = get_gcov_type ();
  const size_t bucketsize = TYPE_PRECISION (gcov_type_node);
  const size_t nbuckets = (paths.length () + (bucketsize - 1)) / bucketsize;
  gcc_checking_assert (sizeof (uint64_t) * BITS_PER_UNIT >= bucketsize);

  if (!coverage_counter_alloc (GCOV_COUNTER_PATHS, nbuckets))
    {
      release_vec_vec (paths);
      return 0;
    }

  hash_map <edge_hash, uint64_t> ands;
  hash_map <block_hash, uint64_t> iors;
  hash_map <block_hash, uint64_t> flushes;

  /* Now that we have all paths we must figure out what bitmasks must be
     applied on the edges.  We also record in which basic block the path starts
     (e.g. accumulator resets) and ends (accumulator flushes).  */
  for (size_t pathno = 0; pathno != paths.length (); ++pathno)
    {
      const vec<int> &path = paths[pathno];
      const size_t bucket = pathno / bucketsize;
      const uint64_t bit = uint64_t (1) << (pathno % bucketsize);

      basic_block first = BASIC_BLOCK_FOR_FN (fn, path[0]);
      basic_block last = BASIC_BLOCK_FOR_FN (fn, path[path.length () - 1]);

      for (unsigned i = 1; i != path.length (); ++i)
	{
	  edge e = edge_between (fn, path[i-1], path[i]);
	  ands.get_or_insert ({e, bucket}) |= bit;
	}

      iors.get_or_insert ({first, bucket}) |= bit;
      flushes.get_or_insert ({last, bucket}) |= bit;
    }

  /* The basic blocks (except entry, exit) for this function, in topological
     order.  Processing in this order means that the predecessor(s) exit SSAs
     will have been created by the time a block is processed, unless it is a
     loop/back edge.  This simplifies processing a bit.  */
  vec<basic_block> blocks = topsort (fn);

  /* The exit SSA names for the BLOCK, the SSA name the BLOCK successors should
     use as inputs.  */
  hash_map<block_hash, tree> SSAex;
  /* The entry SSA name for the BLOCK.  This name forms the basis for the
     flushing to the global accumulators.  In the presence of phi nodes this is
     the resolved phi, otherwise it is some predecessor's exit SSA name.  */
  hash_map<block_hash, tree> SSAen;

  auto_vec<fixup, 4> todos;

  /* Generate the operations for each basic block.  */
  for (basic_block bb : blocks)
    {
      for (size_t bucket = 0; bucket != nbuckets; ++bucket)
	{
	  tree ssa = nullptr;
	  gphi *phi = nullptr;
	  uint64_t all_and = union_incoming_bit_and (ands, bb, bucket);

	  if (all_and && single_pred_p (bb))
	    {
	      /* There is a path on this edge through the bucket, but since
		 there is only one predecessor there it has no decisive power.
		 Just push the predecessor's exit and have later ANDs sort it
		 out.  */
	      tree *prev = SSAex.get ({single_pred (bb), bucket});
	      gcc_checking_assert (prev);
	      ssa = *prev;
	    }
	  else if (all_and)
	    {
	      /* There are multiple predecessors, so we need a phi.  */
	      ssa = make_ssa_name (gcov_type_node);
	      phi = create_phi_node (ssa, bb);
	    }

	  if (ssa)
	    SSAen.put ({bb, bucket}, ssa);

	  if (single_pred_p (bb) && single_succ_p (bb))
	    {
	      /* Straight line -- the AND mask will already have been applied
		 to the first ancestor of this chain, so we don't need to apply
		 it here.  */
	    }
	  else if (!can_change_path_p (ands, bb, bucket, all_and))
	    {
	      /* All incoming edges would apply the same mask, so applying the
		 AND here would not actually distinguish paths.  Such an AND
		 will be applied later anyway so we don't need to apply it
		 here.  This is a huge improvement for large programs.  */
	    }
	  else for (edge e : bb->preds)
	    {
	      const uint64_t *bitmask = ands.get ({e, bucket});
	      /* There is no phi, and there are no paths through this bucket.
		 Set the SSA name to nullptr so we don't contaminate it by
		 pushing unrelated predecessors.  */
	      if (!bitmask && !phi)
		ssa = nullptr;
	      else if (!bitmask && phi)
		{
		  tree zero = safe_insert_zero_phi_arg (e, gcov_type_node);
		  add_phi_arg (phi, zero, e, UNKNOWN_LOCATION);
		}
	      else if (all_bits_set_p (*bitmask, bucketsize) && !phi)
		{
		  /* This reduces to a no-op (x & ~0) and there is no phi
		     selection, so just push the SSA.  */
		  gcc_checking_assert (ssa);
		}
	      else if (all_bits_set_p (*bitmask, bucketsize) && phi)
		{
		  /* This reduces to a no-op (x & ~0).  Reusing the SSA and not
		     emitting an unecessary AND is a big improvement for large
		     programs.  */
		  tree *prev = SSAex.get ({e->src, bucket});
		  if (prev)
		    add_phi_arg (phi, *prev, e, UNKNOWN_LOCATION);
		  else
		    todos.safe_push (fixup_noop (phi, e, bucket));
		}
	      else if (SSAex.get ({e->src, bucket}))
		{
		  /* We need to apply a mask, folding if possible.  If there is
		     a phi it is already the latest-touched ssa.  */
		  tree prev = *SSAex.get ({e->src, bucket});
		  gcc_assert (prev);
		  if (constant_p (prev))
		    {
		      const uint64_t x = tree_to_uhwi (prev);
		      tree cst = build_int_cst (gcov_type_node, x & *bitmask);
		      if (phi)
			add_phi_arg (phi, cst, e, UNKNOWN_LOCATION);
		      else
			ssa = cst;
		    }
		  else
		    {
		      tree lhs = make_ssa_name (gcov_type_node);
		      tree mask = build_int_cst (gcov_type_node, *bitmask);
		      gassign *put = gimple_build_assign (lhs, BIT_AND_EXPR,
							  prev, mask);
		      gsi_insert_on_edge (e, put);
		      if (phi)
			add_phi_arg (phi, lhs, e, UNKNOWN_LOCATION);
		      else
			ssa = lhs;
		    }
		}
	      else
		{
		  /* There is a phi and this edge is a back edge,
		     which means the predecessor (and descendant) exit
		     SSA has not been created yet.  */
		  gcc_assert (phi);
		  gcc_assert (e->flags & EDGE_DFS_BACK);
		  fixup todo = fixup_and (phi, e, bucket, *bitmask,
					  gcov_type_node);
		  todos.safe_push (todo);
		}
	    }

	  /* Bitwise IOR.  Unlike the AND this assignment can always be created
	     right away as this should be applied to the result of the phi,
	     AND, or single predecessor's exit SSA, and all of those have
	     already been created.  */
	  const uint64_t *ior = iors.get ({bb, bucket});
	  if (ior && !ssa)
	    {
	      /* In case there was no predecessor, the IOR/initial state can
		 just be a constant.  In this case, the IOR also becomes the
		 block's entry node which means it will be considered for
		 flushing in single-vertex paths.  */
	      ssa = build_int_cst (gcov_type_node, *ior);
	      SSAen.put ({bb, bucket}, ssa);
	    }
	  else if (ior && all_bits_set_p (*ior, bucketsize))
	    ssa = build_all_ones_cst (gcov_type_node);
	  else if (ior)
	    {
	      gcc_assert (ssa);
	      tree mask = build_int_cst (gcov_type_node, *ior);
	      ssa = safe_insert_ior (bb, ssa, mask, phi, gcov_type_node);
	    }

	  if (ssa)
	    SSAex.put ({bb, bucket}, ssa);
	}
    }

  /* Apply fixups -- now that all exit SSA names are created we can properly
     set the phi argument if there is a phi node, and emit the (x & mask)
     instruction if necessary.  */
  for (fixup &todo : todos)
    {
      tree *exit = SSAex.get ({todo.e->src, todo.bucket});
      gcc_assert (exit && *exit);
      gcc_checking_assert (todo.phi);
      if (todo.mask)
	{
	  gassign *put = gimple_build_assign (todo.lhs, BIT_AND_EXPR, *exit,
					      todo.mask);
	  gsi_insert_on_edge (todo.e, put);
	}

      add_phi_arg (todo.phi, todo.lhs ? todo.lhs : *exit, todo.e,
		   UNKNOWN_LOCATION);
    }

  tree atomic_ior = nullptr;
  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    atomic_ior = builtin_decl_explicit
      (TYPE_PRECISION (gcov_type_node) > 32
       ? BUILT_IN_ATOMIC_FETCH_OR_8
       : BUILT_IN_ATOMIC_FETCH_OR_4);

  /* Finally, add instructions to update the global counters.  */
  for (basic_block bb : blocks)
    {
      gimple_stmt_iterator gsi = gsi_after_labels (bb);
      for (size_t bucket = 0; bucket != nbuckets; ++bucket)
	{
	  const uint64_t *bitmask = flushes.get ({bb, bucket});
	  if (!bitmask || !*bitmask)
	    continue;

	  tree *counter = SSAen.get ({bb, bucket});
	  gcc_checking_assert (counter);
	  if (!*counter)
	    continue;

	  tree mask = nullptr;
	  if (!all_bits_set_p (*bitmask, bucketsize))
	    mask = build_int_cst (gcov_type_node, *bitmask);

	  if (bb_has_abnormal_pred (bb))
	    flush_on_edges (bb, bucket, *counter, mask, atomic_ior,
			    gcov_type_node);
	  else
	    flush_on_gsi (&gsi, bucket, *counter, mask, atomic_ior,
			  gcov_type_node);
	}
    }

  const unsigned npaths = paths.length ();
  blocks.release ();
  release_vec_vec (paths);
  return npaths;
}
