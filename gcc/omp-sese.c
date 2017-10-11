/* Find single-entry, single-exit regions for OpenACC.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "omp-general.h"
#include "omp-low.h"
#include "omp-grid.h"
#include "gimple-pretty-print.h"
#include "cfghooks.h"
#include "insn-config.h"
#include "recog.h"
#include "internal-fn.h"
#include "bitmap.h"
#include "tree-nested.h"
#include "stor-layout.h"
#include "tree-ssa-threadupdate.h"
#include "tree-into-ssa.h"
#include "splay-tree.h"
#include "target.h"
#include "cfgloop.h"
#include "tree-cfg.h"
#include "omp-offload.h"
#include "attribs.h"

/* Loop structure of the function.  The entire function is described as
   a NULL loop.  */

struct parallel
{
  /* Parent parallel.  */
  parallel *parent;

  /* Next sibling parallel.  */
  parallel *next;

  /* First child parallel.  */
  parallel *inner;

  /* Partitioning mask of the parallel.  */
  unsigned mask;

  /* Partitioning used within inner parallels. */
  unsigned inner_mask;

  /* Location of parallel forked and join.  The forked is the first
     block in the parallel and the join is the first block after of
     the partition.  */
  basic_block forked_block;
  basic_block join_block;

  gimple *forked_stmt;
  gimple *join_stmt;

  gimple *fork_stmt;
  gimple *joining_stmt;

  /* Basic blocks in this parallel, but not in child parallels.  The
     FORKED and JOINING blocks are in the partition.  The FORK and JOIN
     blocks are not.  */
  auto_vec<basic_block> blocks;

  tree record_type;
  tree sender_decl;
  tree receiver_decl;

public:
  parallel (parallel *parent, unsigned mode);
  ~parallel ();
};

/* Constructor links the new parallel into it's parent's chain of
   children.  */

parallel::parallel (parallel *parent_, unsigned mask_)
  :parent (parent_), next (0), inner (0), mask (mask_), inner_mask (0)
{
  forked_block = join_block = 0;
  forked_stmt = join_stmt = NULL;
  fork_stmt = joining_stmt = NULL;

  record_type = NULL_TREE;
  sender_decl = NULL_TREE;
  receiver_decl = NULL_TREE;

  if (parent)
    {
      next = parent->inner;
      parent->inner = this;
    }
}

parallel::~parallel ()
{
  delete inner;
  delete next;
}

static bool
local_var_based_p (tree decl)
{
  switch (TREE_CODE (decl))
    {
    case VAR_DECL:
      return !is_global_var (decl);

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
      return local_var_based_p (TREE_OPERAND (decl, 0));

    default:
      return false;
    }
}

/* Map of basic blocks to gimple stmts.  */
typedef hash_map<basic_block, gimple *> bb_stmt_map_t;

/* Calls to OpenACC routines are made by all workers/wavefronts/warps, since
   the routine likely contains partitioned loops (else will do its own
   neutering and variable propagation). Return TRUE if a function call CALL
   should be made in (worker) single mode instead, rather than redundant
   mode.  */

static bool
omp_sese_active_worker_call (gcall *call)
{
#define GOMP_DIM_SEQ GOMP_DIM_MAX
  tree fndecl = gimple_call_fndecl (call);

  if (!fndecl)
    return true;

  tree attrs = oacc_get_fn_attrib (fndecl);

  if (!attrs)
    return true;

  int level = oacc_fn_attrib_level (attrs);

  /* Neither regular functions nor "seq" routines should be run by all threads
     in worker-single mode.  */
  return level == -1 || level == GOMP_DIM_SEQ;
#undef GOMP_DIM_SEQ
}

/* Split basic blocks such that each forked and join unspecs are at
   the start of their basic blocks.  Thus afterwards each block will
   have a single partitioning mode.  We also do the same for return
   insns, as they are executed by every thread.  Return the
   partitioning mode of the function as a whole.  Populate MAP with
   head and tail blocks.  We also clear the BB visited flag, which is
   used when finding partitions.  */

static void
omp_sese_split_blocks (bb_stmt_map_t *map)
{
  auto_vec<gimple *> worklist;
  basic_block block;

  /* Locate all the reorg instructions of interest.  */
  FOR_ALL_BB_FN (block, cfun)
    {
      /* Clear visited flag, for use by parallel locator  */
      block->flags &= ~BB_VISITED;

      for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (gimple_call_internal_p (stmt, IFN_UNIQUE))
	    {
	      enum ifn_unique_kind k = ((enum ifn_unique_kind)
		TREE_INT_CST_LOW (gimple_call_arg (stmt, 0)));
	      gcall *call = as_a <gcall *> (stmt);

	      if (k == IFN_UNIQUE_OACC_JOIN)
	        worklist.safe_push (stmt);
	      else if (k == IFN_UNIQUE_OACC_FORK)
	        {
		  gcc_assert (gsi_one_before_end_p (gsi));
		  basic_block forked_block = single_succ (block);
		  gimple_stmt_iterator gsi2 = gsi_start_bb (forked_block);

		  /* We push a NOP as a placeholder for the "forked" stmt.
		     This is then recognized in omp_sese_find_par.  */
		  gimple *nop = gimple_build_nop ();
		  gsi_insert_before (&gsi2, nop, GSI_SAME_STMT);

		  worklist.safe_push (nop);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_RETURN
		   || gimple_code (stmt) == GIMPLE_COND
		   || gimple_code (stmt) == GIMPLE_SWITCH
		   || (gimple_code (stmt) == GIMPLE_CALL
		       && !gimple_call_internal_p (stmt)
		       && !omp_sese_active_worker_call (as_a <gcall *> (stmt))))
	    worklist.safe_push (stmt);
	  else if (is_gimple_assign (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);

	      /* Force assignments to components/fields/elements of local
		 aggregates into fully-partitioned (redundant) mode.  This
		 avoids having to broadcast the whole aggregate.  The RHS of
		 the assignment will be propagated using the normal
		 mechanism.  */

	      switch (TREE_CODE (lhs))
	        {
		case COMPONENT_REF:
		case BIT_FIELD_REF:
		case ARRAY_REF:
		  {
		    tree aggr = TREE_OPERAND (lhs, 0);

		    if (local_var_based_p (aggr))
		      worklist.safe_push (stmt);
		  }
		  break;

		default:
		  ;
		}
	    }
	}
    }

  /* Split blocks on the worklist.  */
  unsigned ix;
  gimple *stmt;

  for (ix = 0; worklist.iterate (ix, &stmt); ix++)
    {
      basic_block block = gimple_bb (stmt);

      if (gimple_code (stmt) == GIMPLE_COND)
        {
	  gcond *orig_cond = as_a <gcond *> (stmt);
	  tree_code code = gimple_expr_code (orig_cond);
	  tree pred = make_ssa_name (boolean_type_node);
	  gimple *asgn = gimple_build_assign (pred, code,
			   gimple_cond_lhs (orig_cond),
			   gimple_cond_rhs (orig_cond));
	  gcond *new_cond
	    = gimple_build_cond (NE_EXPR, pred, boolean_false_node,
				 gimple_cond_true_label (orig_cond),
				 gimple_cond_false_label (orig_cond));

	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  gsi_insert_before (&gsi, asgn, GSI_SAME_STMT);
	  gsi_replace (&gsi, new_cond, true);

	  edge e = split_block (block, asgn);
	  block = e->dest;
	  map->get_or_insert (block) = new_cond;
	}
      else if ((gimple_code (stmt) == GIMPLE_CALL
		&& !gimple_call_internal_p (stmt))
	       || is_gimple_assign (stmt))
        {
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  gsi_prev (&gsi);

	  edge call = split_block (block, gsi_stmt (gsi));

	  gimple *call_stmt = gsi_stmt (gsi_start_bb (call->dest));

	  edge call_to_ret = split_block (call->dest, call_stmt);

	  map->get_or_insert (call_to_ret->src) = call_stmt;
	}
      else
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  gsi_prev (&gsi);

	  if (gsi_end_p (gsi))
	    map->get_or_insert (block) = stmt;
	  else
	    {
	      /* Split block before insn. The insn is in the new block.  */
	      edge e = split_block (block, gsi_stmt (gsi));

	      block = e->dest;
	      map->get_or_insert (block) = stmt;
	    }
	}
    }
}

static const char *
mask_name (unsigned mask)
{
  switch (mask)
    {
    case 0: return "gang redundant";
    case 1: return "gang partitioned";
    case 2: return "worker partitioned";
    case 3: return "gang+worker partitioned";
    case 4: return "vector partitioned";
    case 5: return "gang+vector partitioned";
    case 6: return "worker+vector partitioned";
    case 7: return "fully partitioned";
    default: return "<illegal>";
    }
}

/* Dump this parallel and all its inner parallels.  */

static void
omp_sese_dump_pars (parallel *par, unsigned depth)
{
  fprintf (dump_file, "%u: mask %d (%s) head=%d, tail=%d\n",
	   depth, par->mask, mask_name (par->mask),
	   par->forked_block ? par->forked_block->index : -1,
	   par->join_block ? par->join_block->index : -1);

  fprintf (dump_file, "    blocks:");

  basic_block block;
  for (unsigned ix = 0; par->blocks.iterate (ix, &block); ix++)
    fprintf (dump_file, " %d", block->index);
  fprintf (dump_file, "\n");
  if (par->inner)
    omp_sese_dump_pars (par->inner, depth + 1);

  if (par->next)
    omp_sese_dump_pars (par->next, depth);
}

/* If BLOCK contains a fork/join marker, process it to create or
   terminate a loop structure.  Add this block to the current loop,
   and then walk successor blocks.   */

static parallel *
omp_sese_find_par (bb_stmt_map_t *map, parallel *par, basic_block block)
{
  if (block->flags & BB_VISITED)
    return par;
  block->flags |= BB_VISITED;

  if (gimple **stmtp = map->get (block))
    {
      gimple *stmt = *stmtp;

      if (gimple_code (stmt) == GIMPLE_COND
	  || gimple_code (stmt) == GIMPLE_SWITCH
	  || gimple_code (stmt) == GIMPLE_RETURN
	  || (gimple_code (stmt) == GIMPLE_CALL
	      && !gimple_call_internal_p (stmt))
	  || is_gimple_assign (stmt))
	{
	  /* A single block that is forced to be at the maximum partition
	     level.  Make a singleton par for it.  */
	  par = new parallel (par, GOMP_DIM_MASK (GOMP_DIM_GANG)
				   | GOMP_DIM_MASK (GOMP_DIM_WORKER)
				   | GOMP_DIM_MASK (GOMP_DIM_VECTOR));
	  par->forked_block = block;
	  par->forked_stmt = stmt;
	  par->blocks.safe_push (block);
	  par = par->parent;
	  goto walk_successors;
	}
      else if (gimple_nop_p (stmt))
	{
	  basic_block pred = single_pred (block);
	  gcc_assert (pred);
	  gimple_stmt_iterator gsi = gsi_last_bb (pred);
	  gimple *final_stmt = gsi_stmt (gsi);

	  if (gimple_call_internal_p (final_stmt, IFN_UNIQUE))
	    {
	      gcall *call = as_a <gcall *> (final_stmt);
	      enum ifn_unique_kind k = ((enum ifn_unique_kind)
	        TREE_INT_CST_LOW (gimple_call_arg (call, 0)));

	      if (k == IFN_UNIQUE_OACC_FORK)
	        {
		  HOST_WIDE_INT dim
		    = TREE_INT_CST_LOW (gimple_call_arg (call, 2));
		  unsigned mask = (dim >= 0) ? GOMP_DIM_MASK (dim) : 0;

		  par = new parallel (par, mask);
		  par->forked_block = block;
		  par->forked_stmt = final_stmt;
		  par->fork_stmt = stmt;
		}
	      else
	        gcc_unreachable ();
	    }
	  else
	    gcc_unreachable ();
	}
      else if (gimple_call_internal_p (stmt, IFN_UNIQUE))
        {
	  gcall *call = as_a <gcall *> (stmt);
	  enum ifn_unique_kind k = ((enum ifn_unique_kind)
	    TREE_INT_CST_LOW (gimple_call_arg (call, 0)));
	  if (k == IFN_UNIQUE_OACC_JOIN)
	    {
	      HOST_WIDE_INT dim = TREE_INT_CST_LOW (gimple_call_arg (stmt, 2));
	      unsigned mask = (dim >= 0) ? GOMP_DIM_MASK (dim) : 0;

	      gcc_assert (par->mask == mask);
	      par->join_block = block;
	      par->join_stmt = stmt;
	      par = par->parent;
	    }
	  else
	    gcc_unreachable ();
	}
      else
        gcc_unreachable ();
    }

  if (par)
    /* Add this block onto the current loop's list of blocks.  */
    par->blocks.safe_push (block);
  else
    /* This must be the entry block.  Create a NULL parallel.  */
    par = new parallel (0, 0);

walk_successors:
  /* Walk successor blocks.  */
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, block->succs)
    omp_sese_find_par (map, par, e->dest);

  return par;
}

/* DFS walk the CFG looking for fork & join markers.  Construct
   loop structures as we go.  MAP is a mapping of basic blocks
   to head & tail markers, discovered when splitting blocks.  This
   speeds up the discovery.  We rely on the BB visited flag having
   been cleared when splitting blocks.  */

static parallel *
omp_sese_discover_pars (bb_stmt_map_t *map)
{
  basic_block block;

  /* Mark exit blocks as visited.  */
  block = EXIT_BLOCK_PTR_FOR_FN (cfun);
  block->flags |= BB_VISITED;

  /* And entry block as not.  */
  block = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  block->flags &= ~BB_VISITED;

  parallel *par = omp_sese_find_par (map, 0, block);

  if (dump_file)
    {
      fprintf (dump_file, "\nLoops\n");
      omp_sese_dump_pars (par, 0);
      fprintf (dump_file, "\n");
    }

  return par;
}

static void
populate_single_mode_bitmaps (parallel *par, bitmap worker_single,
			      bitmap vector_single, unsigned outer_mask,
			      int depth)
{
  unsigned mask = outer_mask | par->mask;

  basic_block block;

  for (unsigned i = 0; par->blocks.iterate (i, &block); i++)
    {
      if ((mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)) == 0)
        bitmap_set_bit (worker_single, block->index);

      if ((mask & GOMP_DIM_MASK (GOMP_DIM_VECTOR)) == 0)
        bitmap_set_bit (vector_single, block->index);
    }

  if (par->inner)
    populate_single_mode_bitmaps (par->inner, worker_single, vector_single,
				  mask, depth + 1);
  if (par->next)
    populate_single_mode_bitmaps (par->next, worker_single, vector_single,
				  outer_mask, depth);
}

/* A map from SSA names or var decls to record fields.  */

typedef hash_map<tree, tree> field_map_t;

/* For each propagation record type, this is a map from SSA names or var decls
   to propagate, to the field in the record type that should be used for
   transmission and reception.  */

typedef hash_map<tree, field_map_t *> record_field_map_t;

static GTY(()) record_field_map_t *field_map;

static void
install_var_field (tree var, tree record_type)
{
  field_map_t *fields = *field_map->get (record_type);
  tree name;
  char tmp[20];

  if (TREE_CODE (var) == SSA_NAME)
    name = SSA_NAME_IDENTIFIER (var);
  else if (TREE_CODE (var) == VAR_DECL)
    name = DECL_NAME (var);
  else
    gcc_unreachable ();

  gcc_assert (!fields->get (var));

  if (!name)
    {
      sprintf (tmp, "_%u", (unsigned) SSA_NAME_VERSION (var));
      name = get_identifier (tmp);
    }

  tree type = TREE_TYPE (var);

  if (POINTER_TYPE_P (type)
      && TYPE_RESTRICT (type))
    type = build_qualified_type (type, TYPE_QUALS (type) & ~TYPE_QUAL_RESTRICT);

  tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL, name, type);

  if (TREE_CODE (var) == VAR_DECL && type == TREE_TYPE (var))
    {
      SET_DECL_ALIGN (field, DECL_ALIGN (var));
      DECL_USER_ALIGN (field) = DECL_USER_ALIGN (var);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (var);
    }
  else
    SET_DECL_ALIGN (field, TYPE_ALIGN (type));

  fields->put (var, field);

  insert_field_into_struct (record_type, field);
}

/* Sets of SSA_NAMES or VAR_DECLs to propagate.  */
typedef hash_set<tree> propagation_set;

static void
find_ssa_names_to_propagate (parallel *par, unsigned outer_mask,
			     bitmap worker_single, bitmap vector_single,
			     vec<propagation_set *> *prop_set)
{
  unsigned mask = outer_mask | par->mask;

  if (par->inner)
    find_ssa_names_to_propagate (par->inner, mask, worker_single,
				 vector_single, prop_set);
  if (par->next)
    find_ssa_names_to_propagate (par->next, outer_mask, worker_single,
				 vector_single, prop_set);

  if (mask & GOMP_DIM_MASK (GOMP_DIM_WORKER))
    {
      basic_block block;
      int ix;

      for (ix = 0; par->blocks.iterate (ix, &block); ix++)
	{
	  for (gphi_iterator psi = gsi_start_phis (block);
	       !gsi_end_p (psi); gsi_next (&psi))
	    {
	      gphi *phi = psi.phi ();
	      use_operand_p use;
	      ssa_op_iter iter;

	      FOR_EACH_PHI_ARG (use, phi, iter, SSA_OP_USE)
	        {
		  tree var = USE_FROM_PTR (use);

		  if (TREE_CODE (var) != SSA_NAME)
		    continue;

		  gimple *def_stmt = SSA_NAME_DEF_STMT (var);

		  if (gimple_nop_p (def_stmt))
		    continue;

		  basic_block def_bb = gimple_bb (def_stmt);

		  if (bitmap_bit_p (worker_single, def_bb->index))
		    {
		      if (!(*prop_set)[def_bb->index])
		        (*prop_set)[def_bb->index] = new propagation_set;

		      propagation_set *ws_prop = (*prop_set)[def_bb->index];

		      ws_prop->add (var);
		    }
		}
	    }

	  for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	       !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      use_operand_p use;
	      ssa_op_iter iter;
	      gimple *stmt = gsi_stmt (gsi);

	      FOR_EACH_SSA_USE_OPERAND (use, stmt, iter, SSA_OP_USE)
	        {
		  tree var = USE_FROM_PTR (use);

		  gimple *def_stmt = SSA_NAME_DEF_STMT (var);

		  if (gimple_nop_p (def_stmt))
		    continue;

		  basic_block def_bb = gimple_bb (def_stmt);

		  if (bitmap_bit_p (worker_single, def_bb->index))
		    {
		      if (!(*prop_set)[def_bb->index])
		        (*prop_set)[def_bb->index] = new propagation_set;

		      propagation_set *ws_prop = (*prop_set)[def_bb->index];

		      ws_prop->add (var);
		    }
		}
	    }
	}
    }
}

/* Callback for walk_gimple_stmt to find RHS VAR_DECLs (uses) in a
   statement.  */

static tree
find_partitioned_var_uses_1 (tree *node, int *, void *data)
{
  walk_stmt_info *wi = (walk_stmt_info *) data;
  hash_set<tree> *partitioned_var_uses = (hash_set<tree> *) wi->info;

  if (!wi->is_lhs && VAR_P (*node))
    partitioned_var_uses->add (*node);

  return NULL_TREE;
}

static void
find_partitioned_var_uses (parallel *par, unsigned outer_mask,
			   hash_set<tree> *partitioned_var_uses)
{
  unsigned mask = outer_mask | par->mask;

  if (par->inner)
    find_partitioned_var_uses (par->inner, mask, partitioned_var_uses);
  if (par->next)
    find_partitioned_var_uses (par->next, outer_mask, partitioned_var_uses);

  if (mask & GOMP_DIM_MASK (GOMP_DIM_WORKER))
    {
      basic_block block;
      int ix;

      for (ix = 0; par->blocks.iterate (ix, &block); ix++)
	for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	     !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    walk_stmt_info wi;
	    memset (&wi, 0, sizeof (wi));
	    wi.info = (void *) partitioned_var_uses;
	    walk_gimple_stmt (&gsi, NULL, find_partitioned_var_uses_1, &wi);
	  }
    }
}

static void
find_local_vars_to_propagate (parallel *par, unsigned outer_mask,
			      hash_set<tree> *partitioned_var_uses,
			      vec<propagation_set *> *prop_set)
{
  unsigned mask = outer_mask | par->mask;

  if (par->inner)
    find_local_vars_to_propagate (par->inner, mask, partitioned_var_uses,
				  prop_set);
  if (par->next)
    find_local_vars_to_propagate (par->next, outer_mask, partitioned_var_uses,
				  prop_set);

  if (!(mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)))
    {
      basic_block block;
      int ix;

      for (ix = 0; par->blocks.iterate (ix, &block); ix++)
	{
	  for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	       !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      tree var;
	      unsigned i;

	      FOR_EACH_LOCAL_DECL (cfun, i, var)
		{
		  if (!VAR_P (var)
		      || is_global_var (var)
		      || AGGREGATE_TYPE_P (TREE_TYPE (var))
		      || !partitioned_var_uses->contains (var)
		      || lookup_attribute ("oacc gangprivate",
					   DECL_ATTRIBUTES (var)))
		    continue;

		  if (stmt_may_clobber_ref_p (stmt, var))
		    {
		      if (dump_file)
			{
			  fprintf (dump_file, "bb %u: local variable may be "
				   "clobbered in %s mode: ", block->index,
				   mask_name (mask));
			  print_generic_expr (dump_file, var, TDF_SLIM);
			  fprintf (dump_file, "\n");
			}

		      if (!(*prop_set)[block->index])
			(*prop_set)[block->index] = new propagation_set;

		      propagation_set *ws_prop
			= (*prop_set)[block->index];

		      ws_prop->add (var);
		    }
		}
	    }
	}
    }
}

/* Transform basic blocks FROM, TO (which may be the same block) into:
   if (GOACC_single_start ())
     BLOCK;
   GOACC_barrier ();
			      \  |  /
			      +----+
			      |    |        (new) predicate block
			      +----+--
   \  |  /   \  |  /	        |t    \
   +----+    +----+	      +----+  |
   |	|    |    |	===>  |    |  | f   (old) from block
   +----+    +----+	      +----+  |
     |       t/  \f	        |    /
			      +----+/
  (split  (split before       |    |        skip block
  at end)   condition)	      +----+
			      t/  \f
*/

static void
worker_single_simple (basic_block from, basic_block to,
		      hash_set<tree> *def_escapes_block)
{
  gimple *call, *cond;
  tree lhs, decl;
  basic_block skip_block;

  gimple_stmt_iterator gsi = gsi_last_bb (to);
  if (EDGE_COUNT (to->succs) > 1)
    {
      gcc_assert (gimple_code (gsi_stmt (gsi)) == GIMPLE_COND);
      gsi_prev (&gsi);
    }
  edge e = split_block (to, gsi_stmt (gsi));
  skip_block = e->dest;

  gimple_stmt_iterator start = gsi_after_labels (from);

  decl = builtin_decl_explicit (BUILT_IN_GOACC_SINGLE_START);
  lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (decl)));
  call = gimple_build_call (decl, 0);
  gimple_call_set_lhs (call, lhs);
  gsi_insert_before (&start, call, GSI_NEW_STMT);
  update_stmt (call);

  cond = gimple_build_cond (EQ_EXPR, lhs,
			    fold_convert_loc (UNKNOWN_LOCATION,
					      TREE_TYPE (lhs),
					      boolean_true_node),
			    NULL_TREE, NULL_TREE);
  gsi_insert_after (&start, cond, GSI_NEW_STMT);
  update_stmt (cond);

  edge et = split_block (from, cond);
  et->flags &= ~EDGE_FALLTHRU;
  et->flags |= EDGE_TRUE_VALUE;
  /* Make the active worker the more probable path so we prefer fallthrough
     (letting the idle workers jump around more).  */
  et->probability = profile_probability::likely ();

  edge ef = make_edge (from, skip_block, EDGE_FALSE_VALUE);
  ef->probability = et->probability.invert ();

  basic_block neutered = split_edge (ef);
  gimple_stmt_iterator neut_gsi = gsi_last_bb (neutered);

  for (gsi = gsi_start_bb (et->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      ssa_op_iter iter;
      tree var;

      FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_DEF)
        {
	  if (def_escapes_block->contains (var))
	    {
	      gphi *join_phi = create_phi_node (NULL_TREE, skip_block);
	      tree res = create_new_def_for (var, join_phi,
					     gimple_phi_result_ptr (join_phi));
	      add_phi_arg (join_phi, var, e, UNKNOWN_LOCATION);

	      tree neutered_def = copy_ssa_name (var, NULL);
	      /* We really want "don't care" or some value representing
		 undefined here, but optimizers will probably get rid of the
		 zero-assignments anyway.  */
	      gassign *zero = gimple_build_assign (neutered_def,
				build_zero_cst (TREE_TYPE (neutered_def)));

	      gsi_insert_after (&neut_gsi, zero, GSI_CONTINUE_LINKING);
	      update_stmt (zero);

	      add_phi_arg (join_phi, neutered_def, single_succ_edge (neutered),
			   UNKNOWN_LOCATION);
	      update_stmt (join_phi);
	    }
	}
    }

  gsi = gsi_start_bb (skip_block);

  decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
  gimple *acc_bar = gimple_build_call (decl, 0);

  gsi_insert_before (&gsi, acc_bar, GSI_SAME_STMT);
  update_stmt (acc_bar);
}

/* This is a copied and renamed omp-low.c:omp_build_component_ref.  */

static tree
oacc_build_component_ref (tree obj, tree field)
{
  tree ret = build3 (COMPONENT_REF, TREE_TYPE (field), obj, field, NULL);
  if (TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ret) |= 1;
  if (TREE_READONLY (field))
    TREE_READONLY (ret) |= 1;
  return ret;
}

static tree
build_receiver_ref (tree record_type, tree var, tree receiver_decl)
{
  field_map_t *fields = *field_map->get (record_type);
  tree x = build_simple_mem_ref (receiver_decl);
  tree field = *fields->get (var);
  TREE_THIS_NOTRAP (x) = 1;
  x = oacc_build_component_ref (x, field);
  return x;
}

static tree
build_sender_ref (tree record_type, tree var, tree sender_decl)
{
  field_map_t *fields = *field_map->get (record_type);
  tree field = *fields->get (var);
  return oacc_build_component_ref (sender_decl, field);
}

static int
sort_by_ssa_version_or_uid (const void *p1, const void *p2)
{
  const tree t1 = *(const tree *)p1;
  const tree t2 = *(const tree *)p2;

  if (TREE_CODE (t1) == SSA_NAME && TREE_CODE (t2) == SSA_NAME)
    return SSA_NAME_VERSION (t1) - SSA_NAME_VERSION (t2);
  else if (TREE_CODE (t1) == SSA_NAME && TREE_CODE (t2) != SSA_NAME)
    return -1;
  else if (TREE_CODE (t1) != SSA_NAME && TREE_CODE (t2) == SSA_NAME)
    return 1;
  else
    return DECL_UID (t1) - DECL_UID (t2);
}

static int
sort_by_size_then_ssa_version_or_uid (const void *p1, const void *p2)
{
  const tree t1 = *(const tree *)p1;
  const tree t2 = *(const tree *)p2;
  unsigned HOST_WIDE_INT s1 = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (t1)));
  unsigned HOST_WIDE_INT s2 = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (t2)));
  if (s1 != s2)
    return s2 - s1;
  else
    return sort_by_ssa_version_or_uid (p1, p2);
}

static void
worker_single_copy (basic_block from, basic_block to,
		    hash_set<tree> *def_escapes_block,
		    hash_set<tree> *worker_partitioned_uses,
		    tree record_type)
{
  /* If we only have virtual defs, we'll have no record type, but we still want
     to emit single_copy_start and (particularly) single_copy_end to act as
     a vdef source on the neutered edge representing memory writes on the
     non-neutered edge.  */
  if (!record_type)
    record_type = char_type_node;

  tree sender_decl
    = targetm.goacc.create_propagation_record (record_type, true,
					       ".oacc_worker_o");
  tree receiver_decl
    = targetm.goacc.create_propagation_record (record_type, false,
					       ".oacc_worker_i");

  gimple_stmt_iterator gsi = gsi_last_bb (to);
  if (EDGE_COUNT (to->succs) > 1)
    gsi_prev (&gsi);
  edge e = split_block (to, gsi_stmt (gsi));
  basic_block barrier_block = e->dest;

  gimple_stmt_iterator start = gsi_after_labels (from);

  tree decl = builtin_decl_explicit (BUILT_IN_GOACC_SINGLE_COPY_START);

  tree lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (decl)));

  gimple *call = gimple_build_call (decl, 1,
				    build_fold_addr_expr (sender_decl));
  gimple_call_set_lhs (call, lhs);
  gsi_insert_before (&start, call, GSI_NEW_STMT);
  update_stmt (call);

  tree conv_tmp = make_ssa_name (TREE_TYPE (receiver_decl));

  gimple *conv = gimple_build_assign (conv_tmp,
				      fold_convert (TREE_TYPE (receiver_decl),
						    lhs));
  update_stmt (conv);
  gsi_insert_after (&start, conv, GSI_NEW_STMT);
  gimple *asgn = gimple_build_assign (receiver_decl, conv_tmp);
  gsi_insert_after (&start, asgn, GSI_NEW_STMT);
  update_stmt (asgn);

  tree zero_ptr = build_int_cst (TREE_TYPE (receiver_decl), 0);

  tree recv_tmp = make_ssa_name (TREE_TYPE (receiver_decl));
  asgn = gimple_build_assign (recv_tmp, receiver_decl);
  gsi_insert_after (&start, asgn, GSI_NEW_STMT);
  update_stmt (asgn);

  gimple *cond = gimple_build_cond (EQ_EXPR, recv_tmp, zero_ptr, NULL_TREE,
				    NULL_TREE);
  update_stmt (cond);

  gsi_insert_after (&start, cond, GSI_NEW_STMT);

  edge et = split_block (from, cond);
  et->flags &= ~EDGE_FALLTHRU;
  et->flags |= EDGE_TRUE_VALUE;
  /* Make the active worker the more probable path so we prefer fallthrough
     (letting the idle workers jump around more).  */
  et->probability = profile_probability::likely ();

  basic_block body = et->dest;

  edge ef = make_edge (from, barrier_block, EDGE_FALSE_VALUE);
  ef->probability = et->probability.invert ();

  decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
  gimple *acc_bar = gimple_build_call (decl, 0);

  gimple_stmt_iterator bar_gsi = gsi_start_bb (barrier_block);
  gsi_insert_before (&bar_gsi, acc_bar, GSI_NEW_STMT);

  cond = gimple_build_cond (NE_EXPR, recv_tmp, zero_ptr, NULL_TREE, NULL_TREE);
  gsi_insert_after (&bar_gsi, cond, GSI_NEW_STMT);

  edge et2 = split_block (barrier_block, cond);
  et2->flags &= ~EDGE_FALLTHRU;
  et2->flags |= EDGE_TRUE_VALUE;
  et2->probability = profile_probability::unlikely ();

  basic_block exit_block = et2->dest;

  basic_block copyout_block = split_edge (et2);
  edge ef2 = make_edge (barrier_block, exit_block, EDGE_FALSE_VALUE);
  ef2->probability = et2->probability.invert ();

  gimple_stmt_iterator copyout_gsi = gsi_start_bb (copyout_block);

  edge copyout_to_exit = single_succ_edge (copyout_block);

  gimple_seq sender_seq = NULL;

  /* Make sure we iterate over definitions in a stable order.  */
  auto_vec<tree> escape_vec (def_escapes_block->elements ());
  for (hash_set<tree>::iterator it = def_escapes_block->begin ();
       it != def_escapes_block->end (); ++it)
    escape_vec.quick_push (*it);
  escape_vec.qsort (sort_by_ssa_version_or_uid);

  for (unsigned i = 0; i < escape_vec.length (); i++)
    {
      tree var = escape_vec[i];

      if (TREE_CODE (var) == SSA_NAME && SSA_NAME_IS_VIRTUAL_OPERAND (var))
        continue;

      tree barrier_def = 0;

      if (TREE_CODE (var) == SSA_NAME)
        {
	  gimple *def_stmt = SSA_NAME_DEF_STMT (var);

	  if (gimple_nop_p (def_stmt))
            continue;

	  /* The barrier phi takes one result from the actual work of the
	     block we're neutering, and the other result is constant zero of
	     the same type.  */

	  gphi *barrier_phi = create_phi_node (NULL_TREE, barrier_block);
	  barrier_def = create_new_def_for (var, barrier_phi,
			  gimple_phi_result_ptr (barrier_phi));

	  add_phi_arg (barrier_phi, var, e, UNKNOWN_LOCATION);
	  add_phi_arg (barrier_phi, build_zero_cst (TREE_TYPE (var)), ef,
		       UNKNOWN_LOCATION);

	  update_stmt (barrier_phi);
	}
      else
        gcc_assert (TREE_CODE (var) == VAR_DECL);

      /* If we had no record type, we will have no fields map.  */
      field_map_t **fields_p = field_map->get (record_type);
      field_map_t *fields = fields_p ? *fields_p : NULL;

      if (worker_partitioned_uses->contains (var)
	  && fields
	  && fields->get (var))
	{
	  tree neutered_def = make_ssa_name (TREE_TYPE (var));

	  /* Receive definition from shared memory block.  */

	  tree receiver_ref = build_receiver_ref (record_type, var,
						  receiver_decl);
	  gassign *recv = gimple_build_assign (neutered_def,
					       receiver_ref);
	  gsi_insert_after (&copyout_gsi, recv, GSI_CONTINUE_LINKING);
	  update_stmt (recv);

	  if (TREE_CODE (var) == VAR_DECL)
	    {
	      /* If it's a VAR_DECL, we only copied to an SSA temporary.  Copy
		 to the final location now.  */
	      gassign *asgn = gimple_build_assign (var, neutered_def);
	      gsi_insert_after (&copyout_gsi, asgn, GSI_CONTINUE_LINKING);
	      update_stmt (asgn);
	    }
	  else
	    {
	      /* If it's an SSA name, create a new phi at the join node to
	         represent either the output from the active worker (the
		 barrier) or the inactive workers (the copyout block).  */
	      gphi *join_phi = create_phi_node (NULL_TREE, exit_block);
	      create_new_def_for (barrier_def, join_phi,
				  gimple_phi_result_ptr (join_phi));
	      add_phi_arg (join_phi, barrier_def, ef2, UNKNOWN_LOCATION);
	      add_phi_arg (join_phi, neutered_def, copyout_to_exit,
			   UNKNOWN_LOCATION);
	      update_stmt (join_phi);
	    }

	  /* Send definition to shared memory block.  */

	  tree sender_ref = build_sender_ref (record_type, var, sender_decl);

	  if (TREE_CODE (var) == SSA_NAME)
	    {
	      gassign *send = gimple_build_assign (sender_ref, var);
	      gimple_seq_add_stmt (&sender_seq, send);
	      update_stmt (send);
	    }
	  else if (TREE_CODE (var) == VAR_DECL)
	    {
	      tree tmp = make_ssa_name (TREE_TYPE (var));
	      gassign *send = gimple_build_assign (tmp, var);
	      gimple_seq_add_stmt (&sender_seq, send);
	      update_stmt (send);
	      send = gimple_build_assign (sender_ref, tmp);
	      gimple_seq_add_stmt (&sender_seq, send);
	      update_stmt (send);
	    }
	  else
	    gcc_unreachable ();
	}
    }

  /* It's possible for the ET->DEST block (the work done by the active thread)
     to finish with a control-flow insn, e.g. a UNIQUE function call.  Split
     the block and add SENDER_SEQ in the latter part to avoid having control
     flow in the middle of a BB.  */

  decl = builtin_decl_explicit (BUILT_IN_GOACC_SINGLE_COPY_END);
  call = gimple_build_call (decl, 1, build_fold_addr_expr (sender_decl));
  gimple_seq_add_stmt (&sender_seq, call);

  gsi = gsi_last_bb (body);
  gimple *last = gsi_stmt (gsi);
  basic_block sender_block = split_block (body, last)->dest;
  gsi = gsi_last_bb (sender_block);
  gsi_insert_seq_after (&gsi, sender_seq, GSI_CONTINUE_LINKING);
}

static void
neuter_worker_single (parallel *par, unsigned outer_mask, bitmap worker_single,
		      bitmap vector_single, vec<propagation_set *> *prop_set,
		      hash_set<tree> *partitioned_var_uses)
{
  unsigned mask = outer_mask | par->mask;

  if ((mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)) == 0)
    {
      basic_block block;

      for (unsigned i = 0; par->blocks.iterate (i, &block); i++)
	{
	  bool has_defs = false;
	  hash_set<tree> def_escapes_block;
	  hash_set<tree> worker_partitioned_uses;
	  unsigned j;
	  tree var;

	  FOR_EACH_SSA_NAME (j, var, cfun)
            {
	      if (SSA_NAME_IS_VIRTUAL_OPERAND (var))
		{
		  has_defs = true;
		  continue;
		}

	      gimple *def_stmt = SSA_NAME_DEF_STMT (var);

	      if (gimple_nop_p (def_stmt))
		continue;

	      if (gimple_bb (def_stmt)->index != block->index)
	        continue;

	      gimple *use_stmt;
	      imm_use_iterator use_iter;
	      bool uses_outside_block = false;
	      bool worker_partitioned_use = false;

	      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, var)
	        {
		  int blocknum = gimple_bb (use_stmt)->index;

		  /* Don't propagate SSA names that are only used in the
		     current block, unless the usage is in a phi node: that
		     means the name left the block, then came back in at the
		     top.  */
		  if (blocknum != block->index
		      || gimple_code (use_stmt) == GIMPLE_PHI)
		    uses_outside_block = true;
		  if (!bitmap_bit_p (worker_single, blocknum))
		    worker_partitioned_use = true;
		}

	      if (uses_outside_block)
		def_escapes_block.add (var);

	      if (worker_partitioned_use)
		{
		  worker_partitioned_uses.add (var);
		  has_defs = true;
		}
	    }

	  propagation_set *ws_prop = (*prop_set)[block->index];

	  if (ws_prop)
	    {
	      for (propagation_set::iterator it = ws_prop->begin ();
		   it != ws_prop->end ();
		   ++it)
		{
	          tree var = *it;
		  if (TREE_CODE (var) == VAR_DECL)
		    {
		      def_escapes_block.add (var);
		      if (partitioned_var_uses->contains (var))
			{
			  worker_partitioned_uses.add (var);
			  has_defs = true;
			}
		    }
		}

	      delete ws_prop;
	      (*prop_set)[block->index] = 0;
	    }

	  tree record_type = (tree) block->aux;

	  if (has_defs)
	    worker_single_copy (block, block, &def_escapes_block,
				&worker_partitioned_uses, record_type);
	  else
	    worker_single_simple (block, block, &def_escapes_block);
	}
    }

  if ((outer_mask & GOMP_DIM_MASK (GOMP_DIM_WORKER)) == 0)
    {
      basic_block block;

      for (unsigned i = 0; par->blocks.iterate (i, &block); i++)
	for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	     !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);

	    if (gimple_code (stmt) == GIMPLE_CALL
		&& !gimple_call_internal_p (stmt)
		&& !omp_sese_active_worker_call (as_a <gcall *> (stmt)))
	      {
		/* If we have an OpenACC routine call in worker-single mode,
		   place barriers before and afterwards to prevent
		   clobbering re-used shared memory regions (as are used
		   for AMDGCN at present, for example).  */
		tree decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
		gsi_insert_before (&gsi, gimple_build_call (decl, 0),
				   GSI_SAME_STMT);
		gsi_insert_after (&gsi, gimple_build_call (decl, 0),
				  GSI_NEW_STMT);
	      }
	  }
    }

  if (par->inner)
    neuter_worker_single (par->inner, mask, worker_single, vector_single,
			  prop_set, partitioned_var_uses);
  if (par->next)
    neuter_worker_single (par->next, outer_mask, worker_single, vector_single,
			  prop_set, partitioned_var_uses);
}


void
oacc_do_neutering (void)
{
  bb_stmt_map_t bb_stmt_map;
  auto_bitmap worker_single, vector_single;

  omp_sese_split_blocks (&bb_stmt_map);

  if (dump_file)
    {
      fprintf (dump_file, "\n\nAfter splitting:\n\n");
      dump_function_to_file (current_function_decl, dump_file, dump_flags);
    }

  unsigned mask = 0;

  /* If this is a routine, calculate MASK as if the outer levels are already
     partitioned.  */
  tree attr = oacc_get_fn_attrib (current_function_decl);
  if (attr)
    {
      tree dims = TREE_VALUE (attr);
      unsigned ix;
      for (ix = 0; ix != GOMP_DIM_MAX; ix++, dims = TREE_CHAIN (dims))
	{
	  tree allowed = TREE_PURPOSE (dims);
	  if (allowed && integer_zerop (allowed))
	    mask |= GOMP_DIM_MASK (ix);
	}
    }

  parallel *par = omp_sese_discover_pars (&bb_stmt_map);
  populate_single_mode_bitmaps (par, worker_single, vector_single, mask, 0);

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    bb->aux = NULL;

  field_map = record_field_map_t::create_ggc (40);

  vec<propagation_set *> prop_set;
  prop_set.create (last_basic_block_for_fn (cfun));

  for (unsigned i = 0; i < last_basic_block_for_fn (cfun); i++)
    prop_set.quick_push (0);

  find_ssa_names_to_propagate (par, mask, worker_single, vector_single,
			       &prop_set);

  hash_set<tree> partitioned_var_uses;

  find_partitioned_var_uses (par, mask, &partitioned_var_uses);
  find_local_vars_to_propagate (par, mask, &partitioned_var_uses, &prop_set);

  FOR_ALL_BB_FN (bb, cfun)
    {
      propagation_set *ws_prop = prop_set[bb->index];
      if (ws_prop)
	{
	  tree record_type = lang_hooks.types.make_type (RECORD_TYPE);
	  tree name = create_tmp_var_name (".oacc_ws_data_s");
	  name = build_decl (UNKNOWN_LOCATION, TYPE_DECL, name, record_type);
	  DECL_ARTIFICIAL (name) = 1;
	  DECL_NAMELESS (name) = 1;
	  TYPE_NAME (record_type) = name;
	  TYPE_ARTIFICIAL (record_type) = 1;

	  auto_vec<tree> field_vec (ws_prop->elements ());
	  for (hash_set<tree>::iterator it = ws_prop->begin ();
	       it != ws_prop->end (); ++it)
	    field_vec.quick_push (*it);

	  field_vec.qsort (sort_by_size_then_ssa_version_or_uid);

	  field_map->put (record_type, field_map_t::create_ggc (17));

	  /* Insert var fields in reverse order, so the last inserted element
	     is the first in the structure.  */
	  for (int i = field_vec.length () - 1; i >= 0; i--)
	    install_var_field (field_vec[i], record_type);

	  layout_type (record_type);

	  bb->aux = (tree) record_type;
	}
    }

  neuter_worker_single (par, mask, worker_single, vector_single, &prop_set,
			&partitioned_var_uses);

  prop_set.release ();

  /* This doesn't seem to make a difference.  */
  loops_state_clear (LOOP_CLOSED_SSA);

  /* Neutering worker-single neutered blocks will invalidate dominance info.
     It may be possible to incrementally update just the affected blocks, but
     obliterate everything for now.  */
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  if (dump_file)
    {
      fprintf (dump_file, "\n\nAfter neutering:\n\n");
      dump_function_to_file (current_function_decl, dump_file, dump_flags);
    }
}

/* Analyse a group of BBs within a partitioned region and create N
   Single-Entry-Single-Exit regions.  Some of those regions will be
   trivial ones consisting of a single BB.  The blocks of a
   partitioned region might form a set of disjoint graphs -- because
   the region encloses a differently partitoned sub region.

   We use the linear time algorithm described in 'Finding Regions Fast:
   Single Entry Single Exit and control Regions in Linear Time'
   Johnson, Pearson & Pingali.  That algorithm deals with complete
   CFGs, where a back edge is inserted from END to START, and thus the
   problem becomes one of finding equivalent loops.

   In this case we have a partial CFG.  We complete it by redirecting
   any incoming edge to the graph to be from an arbitrary external BB,
   and similarly redirecting any outgoing edge to be to  that BB.
   Thus we end up with a closed graph.

   The algorithm works by building a spanning tree of an undirected
   graph and keeping track of back edges from nodes further from the
   root in the tree to nodes nearer to the root in the tree.  In the
   description below, the root is up and the tree grows downwards.

   We avoid having to deal with degenerate back-edges to the same
   block, by splitting each BB into 3 -- one for input edges, one for
   the node itself and one for the output edges.  Such back edges are
   referred to as 'Brackets'.  Cycle equivalent nodes will have the
   same set of brackets.

   Determining bracket equivalency is done by maintaining a list of
   brackets in such a manner that the list length and final bracket
   uniquely identify the set.

   We use coloring to mark all BBs with cycle equivalency with the
   same color.  This is the output of the 'Finding Regions Fast'
   algorithm.  Notice it doesn't actually find the set of nodes within
   a particular region, just unorderd sets of nodes that are the
   entries and exits of SESE regions.

   After determining cycle equivalency, we need to find the minimal
   set of SESE regions.  Do this with a DFS coloring walk of the
   complete graph.  We're either 'looking' or 'coloring'.  When
   looking, and we're in the subgraph, we start coloring the color of
   the current node, and remember that node as the start of the
   current color's SESE region.  Every time we go to a new node, we
   decrement the count of nodes with thet color.  If it reaches zero,
   we remember that node as the end of the current color's SESE region
   and return to 'looking'.  Otherwise we color the node the current
   color.

   This way we end up with coloring the inside of non-trivial SESE
   regions with the color of that region.  */

/* A pair of BBs.  We use this to represent SESE regions.  */
typedef std::pair<basic_block, basic_block> bb_pair_t;
typedef auto_vec<bb_pair_t> bb_pair_vec_t;

/* A node in the undirected CFG.  The discriminator SECOND indicates just
   above or just below the BB idicated by FIRST.  */
typedef std::pair<basic_block, int> pseudo_node_t;

/* A bracket indicates an edge towards the root of the spanning tree of the
   undirected graph.  Each bracket has a color, determined
   from the currrent set of brackets.  */
struct bracket
{
  pseudo_node_t back; /* Back target */

  /* Current color and size of set.  */
  unsigned color;
  unsigned size;

  bracket (pseudo_node_t back_)
  : back (back_), color (~0u), size (~0u)
  {
  }

  unsigned get_color (auto_vec<unsigned> &color_counts, unsigned length)
  {
    if (length != size)
      {
	size = length;
	color = color_counts.length ();
	color_counts.quick_push (0);
      }
    color_counts[color]++;
    return color;
  }
};

typedef auto_vec<bracket> bracket_vec_t;

/* Basic block info for finding SESE regions.    */

struct bb_sese
{
  int node;  /* Node number in spanning tree.  */
  int parent; /* Parent node number.  */

  /* The algorithm splits each node A into Ai, A', Ao. The incoming
     edges arrive at pseudo-node Ai and the outgoing edges leave at
     pseudo-node Ao.  We have to remember which way we arrived at a
     particular node when generating the spanning tree.  dir > 0 means
     we arrived at Ai, dir < 0 means we arrived at Ao.  */
  int dir;

  /* Lowest numbered pseudo-node reached via a backedge from thsis
     node, or any descendant.  */
  pseudo_node_t high;

  int color;  /* Cycle-equivalence color  */

  /* Stack of brackets for this node.  */
  bracket_vec_t brackets;

  bb_sese (unsigned node_, unsigned p, int dir_)
  :node (node_), parent (p), dir (dir_)
  {
  }
  ~bb_sese ();

  /* Push a bracket ending at BACK.  */
  void push (const pseudo_node_t &back)
  {
    if (dump_file)
      fprintf (dump_file, "Pushing backedge %d:%+d\n",
	       back.first ? back.first->index : 0, back.second);
    brackets.safe_push (bracket (back));
  }

  void append (bb_sese *child);
  void remove (const pseudo_node_t &);

  /* Set node's color.  */
  void set_color (auto_vec<unsigned> &color_counts)
  {
    color = brackets.last ().get_color (color_counts, brackets.length ());
  }
};

bb_sese::~bb_sese ()
{
}

/* Destructively append CHILD's brackets.  */

void
bb_sese::append (bb_sese *child)
{
  if (int len = child->brackets.length ())
    {
      int ix;

      if (dump_file)
	{
	  for (ix = 0; ix < len; ix++)
	    {
	      const pseudo_node_t &pseudo = child->brackets[ix].back;
	      fprintf (dump_file, "Appending (%d)'s backedge %d:%+d\n",
		       child->node, pseudo.first ? pseudo.first->index : 0,
		       pseudo.second);
	    }
	}
      if (!brackets.length ())
	std::swap (brackets, child->brackets);
      else
	{
	  brackets.reserve (len);
	  for (ix = 0; ix < len; ix++)
	    brackets.quick_push (child->brackets[ix]);
	}
    }
}

/* Remove brackets that terminate at PSEUDO.  */

void
bb_sese::remove (const pseudo_node_t &pseudo)
{
  unsigned removed = 0;
  int len = brackets.length ();

  for (int ix = 0; ix < len; ix++)
    {
      if (brackets[ix].back == pseudo)
	{
	  if (dump_file)
	    fprintf (dump_file, "Removing backedge %d:%+d\n",
		     pseudo.first ? pseudo.first->index : 0, pseudo.second);
	  removed++;
	}
      else if (removed)
	brackets[ix-removed] = brackets[ix];
    }
  while (removed--)
    brackets.pop ();
}

/* Accessors for BB's aux pointer.  */
#define BB_SET_SESE(B, S) ((B)->aux = (S))
#define BB_GET_SESE(B) ((bb_sese *)(B)->aux)

/* DFS walk creating SESE data structures.  Only cover nodes with
   BB_VISITED set.  Append discovered blocks to LIST.  We number in
   increments of 3 so that the above and below pseudo nodes can be
   implicitly numbered too.  */

static int
omp_sese_number (int n, int p, int dir, basic_block b,
		   auto_vec<basic_block> *list)
{
  if (BB_GET_SESE (b))
    return n;

  if (dump_file)
    fprintf (dump_file, "Block %d(%d), parent (%d), orientation %+d\n",
	     b->index, n, p, dir);

  BB_SET_SESE (b, new bb_sese (n, p, dir));
  p = n;

  n += 3;
  list->quick_push (b);

  /* First walk the nodes on the 'other side' of this node, then walk
     the nodes on the same side.  */
  for (unsigned ix = 2; ix; ix--)
    {
      vec<edge, va_gc> *edges = dir > 0 ? b->succs : b->preds;
      size_t offset = (dir > 0 ? offsetof (edge_def, dest)
		       : offsetof (edge_def, src));
      edge e;
      edge_iterator (ei);

      FOR_EACH_EDGE (e, ei, edges)
	{
	  basic_block target = *(basic_block *)((char *)e + offset);

	  if (target->flags & BB_VISITED)
	    n = omp_sese_number (n, p, dir, target, list);
	}
      dir = -dir;
    }
  return n;
}

/* Process pseudo node above (DIR < 0) or below (DIR > 0) ME.
   EDGES are the outgoing edges and OFFSET is the offset to the src
   or dst block on the edges.   */

static void
omp_sese_pseudo (basic_block me, bb_sese *sese, int depth, int dir,
		   vec<edge, va_gc> *edges, size_t offset)
{
  edge e;
  edge_iterator (ei);
  int hi_back = depth;
  pseudo_node_t node_back (0, depth);
  int hi_child = depth;
  pseudo_node_t node_child (0, depth);
  basic_block child = NULL;
  unsigned num_children = 0;
  int usd = -dir * sese->dir;

  if (dump_file)
    fprintf (dump_file, "\nProcessing %d(%d) %+d\n",
	     me->index, sese->node, dir);

  if (dir < 0)
    {
      /* This is the above pseudo-child.  It has the BB itself as an
	 additional child node.  */
      node_child = sese->high;
      hi_child = node_child.second;
      if (node_child.first)
	hi_child += BB_GET_SESE (node_child.first)->node;
      num_children++;
    }

  /* Examine each edge.
     - if it is a child (a) append its bracket list and (b) record
          whether it is the child with the highest reaching bracket.
     - if it is an edge to ancestor, record whether it's the highest
          reaching backlink.  */
  FOR_EACH_EDGE (e, ei, edges)
    {
      basic_block target = *(basic_block *)((char *)e + offset);

      if (bb_sese *t_sese = BB_GET_SESE (target))
	{
	  if (t_sese->parent == sese->node && !(t_sese->dir + usd))
	    {
	      /* Child node.  Append its bracket list. */
	      num_children++;
	      sese->append (t_sese);

	      /* Compare it's hi value.  */
	      int t_hi = t_sese->high.second;

	      if (basic_block child_hi_block = t_sese->high.first)
		t_hi += BB_GET_SESE (child_hi_block)->node;

	      if (hi_child > t_hi)
		{
		  hi_child = t_hi;
		  node_child = t_sese->high;
		  child = target;
		}
	    }
	  else if (t_sese->node < sese->node + dir
		   && !(dir < 0 && sese->parent == t_sese->node))
	    {
	      /* Non-parental ancestor node -- a backlink.  */
	      int d = usd * t_sese->dir;
	      int back = t_sese->node + d;

	      if (hi_back > back)
		{
		  hi_back = back;
		  node_back = pseudo_node_t (target, d);
		}
	    }
	}
      else
	{ /* Fallen off graph, backlink to entry node.  */
	  hi_back = 0;
	  node_back = pseudo_node_t (0, 0);
	}
    }

  /* Remove any brackets that terminate at this pseudo node.  */
  sese->remove (pseudo_node_t (me, dir));

  /* Now push any backlinks from this pseudo node.  */
  FOR_EACH_EDGE (e, ei, edges)
    {
      basic_block target = *(basic_block *)((char *)e + offset);
      if (bb_sese *t_sese = BB_GET_SESE (target))
	{
	  if (t_sese->node < sese->node + dir
	      && !(dir < 0 && sese->parent == t_sese->node))
	    /* Non-parental ancestor node - backedge from me.  */
	    sese->push (pseudo_node_t (target, usd * t_sese->dir));
	}
      else
	{
	  /* back edge to entry node */
	  sese->push (pseudo_node_t (0, 0));
	}
    }

  /* If this node leads directly or indirectly to a no-return region of
     the graph, then fake a backedge to entry node.  */
  if (!sese->brackets.length () || !edges || !edges->length ())
    {
      hi_back = 0;
      node_back = pseudo_node_t (0, 0);
      sese->push (node_back);
    }

  /* Record the highest reaching backedge from us or a descendant.  */
  sese->high = hi_back < hi_child ? node_back : node_child;

  if (num_children > 1)
    {
      /* There is more than one child -- this is a Y shaped piece of
	 spanning tree.  We have to insert a fake backedge from this
	 node to the highest ancestor reached by not-the-highest
	 reaching child.  Note that there may be multiple children
	 with backedges to the same highest node.  That's ok and we
	 insert the edge to that highest node.  */
      hi_child = depth;
      if (dir < 0 && child)
	{
	  node_child = sese->high;
	  hi_child = node_child.second;
	  if (node_child.first)
	    hi_child += BB_GET_SESE (node_child.first)->node;
	}

      FOR_EACH_EDGE (e, ei, edges)
	{
	  basic_block target = *(basic_block *)((char *)e + offset);

	  if (target == child)
	    /* Ignore the highest child. */
	    continue;

	  bb_sese *t_sese = BB_GET_SESE (target);
	  if (!t_sese)
	    continue;
	  if (t_sese->parent != sese->node)
	    /* Not a child. */
	    continue;

	  /* Compare its hi value.  */
	  int t_hi = t_sese->high.second;

	  if (basic_block child_hi_block = t_sese->high.first)
	    t_hi += BB_GET_SESE (child_hi_block)->node;

	  if (hi_child > t_hi)
	    {
	      hi_child = t_hi;
	      node_child = t_sese->high;
	    }
	}

      sese->push (node_child);
    }
}


/* DFS walk of BB graph.  Color node BLOCK according to COLORING then
   proceed to successors.  Set SESE entry and exit nodes of
   REGIONS.  */

static void
omp_sese_color (auto_vec<unsigned> &color_counts, bb_pair_vec_t &regions,
		  basic_block block, int coloring)
{
  bb_sese *sese = BB_GET_SESE (block);

  if (block->flags & BB_VISITED)
    {
      /* If we've already encountered this block, either we must not
	 be coloring, or it must have been colored the current color.  */
      gcc_assert (coloring < 0 || (sese && coloring == sese->color));
      return;
    }

  block->flags |= BB_VISITED;

  if (sese)
    {
      if (coloring < 0)
	{
	  /* Start coloring a region.  */
	  regions[sese->color].first = block;
	  coloring = sese->color;
	}

      if (!--color_counts[sese->color] && sese->color == coloring)
	{
	  /* Found final block of SESE region.  */
	  regions[sese->color].second = block;
	  coloring = -1;
	}
      else
	/* Color the node, so we can assert on revisiting the node
	   that the graph is indeed SESE.  */
	sese->color = coloring;
    }
  else
    /* Fallen off the subgraph, we cannot be coloring.  */
    gcc_assert (coloring < 0);

  /* Walk each successor block.  */
  if (block->succs && block->succs->length ())
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, block->succs)
	omp_sese_color (color_counts, regions, e->dest, coloring);
    }
  else
    gcc_assert (coloring < 0);
}

/* Find minimal set of SESE regions covering BLOCKS.  REGIONS might
   end up with NULL entries in it.  */

static void
omp_find_sese (auto_vec<basic_block> &blocks, bb_pair_vec_t &regions)
{
  basic_block block;
  int ix;

  /* First clear each BB of the whole function.  */
  FOR_EACH_BB_FN (block, cfun)
    {
      block->flags &= ~BB_VISITED;
      BB_SET_SESE (block, 0);
    }
  block = EXIT_BLOCK_PTR_FOR_FN (cfun);
  block->flags &= ~BB_VISITED;
  BB_SET_SESE (block, 0);
  block = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  block->flags &= ~BB_VISITED;
  BB_SET_SESE (block, 0);

  /* Mark blocks in the function that are in this graph.  */
  for (ix = 0; blocks.iterate (ix, &block); ix++)
    block->flags |= BB_VISITED;

  /* Counts of nodes assigned to each color.  There cannot be more
     colors than blocks (and hopefully there will be fewer).  */
  auto_vec<unsigned> color_counts;
  color_counts.reserve (blocks.length ());

  /* Worklist of nodes in the spanning tree.  Again, there cannot be
     more nodes in the tree than blocks (there will be fewer if the
     CFG of blocks is disjoint).  */
  auto_vec<basic_block> spanlist;
  spanlist.reserve (blocks.length ());

  /* Make sure every block has its cycle class determined.  */
  for (ix = 0; blocks.iterate (ix, &block); ix++)
    {
      if (BB_GET_SESE (block))
	/* We already met this block in an earlier graph solve.  */
	continue;

      if (dump_file)
	fprintf (dump_file, "Searching graph starting at %d\n", block->index);

      /* Number the nodes reachable from block initial DFS order.  */
      int depth = omp_sese_number (2, 0, +1, block, &spanlist);

      /* Now walk in reverse DFS order to find cycle equivalents.  */
      while (spanlist.length ())
	{
	  block = spanlist.pop ();
	  bb_sese *sese = BB_GET_SESE (block);

	  /* Do the pseudo node below.  */
	  omp_sese_pseudo (block, sese, depth, +1,
			     sese->dir > 0 ? block->succs : block->preds,
			     (sese->dir > 0 ? offsetof (edge_def, dest)
			      : offsetof (edge_def, src)));
	  sese->set_color (color_counts);
	  /* Do the pseudo node above.  */
	  omp_sese_pseudo (block, sese, depth, -1,
			     sese->dir < 0 ? block->succs : block->preds,
			     (sese->dir < 0 ? offsetof (edge_def, dest)
			      : offsetof (edge_def, src)));
	}
      if (dump_file)
	fprintf (dump_file, "\n");
    }

  if (dump_file)
    {
      unsigned count;
      const char *comma = "";

      fprintf (dump_file, "Found %d cycle equivalents\n",
	       color_counts.length ());
      for (ix = 0; color_counts.iterate (ix, &count); ix++)
	{
	  fprintf (dump_file, "%s%d[%d]={", comma, ix, count);

	  comma = "";
	  for (unsigned jx = 0; blocks.iterate (jx, &block); jx++)
	    if (BB_GET_SESE (block)->color == ix)
	      {
		block->flags |= BB_VISITED;
		fprintf (dump_file, "%s%d", comma, block->index);
		comma=",";
	      }
	  fprintf (dump_file, "}");
	  comma = ", ";
	}
      fprintf (dump_file, "\n");
   }

  /* Now we've colored every block in the subgraph.  We now need to
     determine the minimal set of SESE regions that cover that
     subgraph.  Do this with a DFS walk of the complete function.
     During the walk we're either 'looking' or 'coloring'.  When we
     reach the last node of a particular color, we stop coloring and
     return to looking.  */

  /* There cannot be more SESE regions than colors.  */
  regions.reserve (color_counts.length ());
  for (ix = color_counts.length (); ix--;)
    regions.quick_push (bb_pair_t (0, 0));

  for (ix = 0; blocks.iterate (ix, &block); ix++)
    block->flags &= ~BB_VISITED;

  omp_sese_color (color_counts, regions, ENTRY_BLOCK_PTR_FOR_FN (cfun), -1);

  if (dump_file)
    {
      const char *comma = "";
      int len = regions.length ();

      fprintf (dump_file, "SESE regions:");
      for (ix = 0; ix != len; ix++)
	{
	  basic_block from = regions[ix].first;
	  basic_block to = regions[ix].second;

	  if (from)
	    {
	      fprintf (dump_file, "%s %d{%d", comma, ix, from->index);
	      if (to != from)
		fprintf (dump_file, "->%d", to->index);

	      int color = BB_GET_SESE (from)->color;

	      /* Print the blocks within the region (excluding ends).  */
	      FOR_EACH_BB_FN (block, cfun)
		{
		  bb_sese *sese = BB_GET_SESE (block);

		  if (sese && sese->color == color
		      && block != from && block != to)
		    fprintf (dump_file, ".%d", block->index);
		}
	      fprintf (dump_file, "}");
	    }
	  comma = ",";
	}
      fprintf (dump_file, "\n\n");
    }

  for (ix = 0; blocks.iterate (ix, &block); ix++)
    delete BB_GET_SESE (block);
}

#undef BB_SET_SESE
#undef BB_GET_SESE
