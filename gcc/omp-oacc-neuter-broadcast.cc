/* OpenACC worker partitioning via middle end neutering/broadcasting scheme

   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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
#include "targhooks.h"
#include "diagnostic-core.h"

/* Loop structure of the function.  The entire function is described as
   a NULL loop.  */
/* Adapted from 'gcc/config/nvptx/nvptx.cc:struct parallel'.  */

struct parallel_g
{
  /* Parent parallel.  */
  parallel_g *parent;

  /* Next sibling parallel.  */
  parallel_g *next;

  /* First child parallel.  */
  parallel_g *inner;

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
  parallel_g (parallel_g *parent, unsigned mode);
  ~parallel_g ();
};

/* Constructor links the new parallel into it's parent's chain of
   children.  */

parallel_g::parallel_g (parallel_g *parent_, unsigned mask_)
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

parallel_g::~parallel_g ()
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
/* Adapted from 'gcc/config/nvptx/nvptx.cc:nvptx_split_blocks'.  */

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
/* Adapted from 'gcc/config/nvptx/nvptx.cc:nvptx_dump_pars'.  */

static void
omp_sese_dump_pars (parallel_g *par, unsigned depth)
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
/* Adapted from 'gcc/config/nvptx/nvptx.cc:nvptx_find_par'.  */

static parallel_g *
omp_sese_find_par (bb_stmt_map_t *map, parallel_g *par, basic_block block)
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
	  par = new parallel_g (par, GOMP_DIM_MASK (GOMP_DIM_GANG)
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

		  par = new parallel_g (par, mask);
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
    par = new parallel_g (0, 0);

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
/* Adapted from 'gcc/config/nvptx/nvptx.cc:nvptx_discover_pars'.  */

static parallel_g *
omp_sese_discover_pars (bb_stmt_map_t *map)
{
  basic_block block;

  /* Mark exit blocks as visited.  */
  block = EXIT_BLOCK_PTR_FOR_FN (cfun);
  block->flags |= BB_VISITED;

  /* And entry block as not.  */
  block = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  block->flags &= ~BB_VISITED;

  parallel_g *par = omp_sese_find_par (map, 0, block);

  if (dump_file)
    {
      fprintf (dump_file, "\nLoops\n");
      omp_sese_dump_pars (par, 0);
      fprintf (dump_file, "\n");
    }

  return par;
}

static void
populate_single_mode_bitmaps (parallel_g *par, bitmap worker_single,
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

typedef hash_map<tree, field_map_t> record_field_map_t;

static void
install_var_field (tree var, tree record_type, field_map_t *fields)
{
  tree name;
  char tmp[20];

  if (TREE_CODE (var) == SSA_NAME)
    {
      name = SSA_NAME_IDENTIFIER (var);
      if (!name)
	{
	  sprintf (tmp, "_%u", (unsigned) SSA_NAME_VERSION (var));
	  name = get_identifier (tmp);
	}
    }
  else if (VAR_P (var))
    {
      name = DECL_NAME (var);
      if (!name)
	{
	  sprintf (tmp, "D_%u", (unsigned) DECL_UID (var));
	  name = get_identifier (tmp);
	}
    }
  else
    gcc_unreachable ();

  gcc_assert (!fields->get (var));

  tree type = TREE_TYPE (var);

  if (POINTER_TYPE_P (type)
      && TYPE_RESTRICT (type))
    type = build_qualified_type (type, TYPE_QUALS (type) & ~TYPE_QUAL_RESTRICT);

  tree field = build_decl (BUILTINS_LOCATION, FIELD_DECL, name, type);

  if (VAR_P (var) && type == TREE_TYPE (var))
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
find_ssa_names_to_propagate (parallel_g *par, unsigned outer_mask,
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
find_partitioned_var_uses (parallel_g *par, unsigned outer_mask,
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

/* Gang-private variables (typically placed in a GPU's shared memory) do not
   need to be processed by the worker-propagation mechanism.  Populate the
   GANG_PRIVATE_VARS set with any such variables found in the current
   function.  */

static void
find_gang_private_vars (hash_set<tree> *gang_private_vars)
{
  basic_block block;

  FOR_EACH_BB_FN (block, cfun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (gimple_call_internal_p (stmt, IFN_UNIQUE))
	    {
	      enum ifn_unique_kind k = ((enum ifn_unique_kind)
		TREE_INT_CST_LOW (gimple_call_arg (stmt, 0)));
	      if (k == IFN_UNIQUE_OACC_PRIVATE)
		{
		  HOST_WIDE_INT level
		    = TREE_INT_CST_LOW (gimple_call_arg (stmt, 2));
		  if (level != GOMP_DIM_GANG)
		    continue;
		  for (unsigned i = 3; i < gimple_call_num_args (stmt); i++)
		    {
		      tree arg = gimple_call_arg (stmt, i);
		      gcc_assert (TREE_CODE (arg) == ADDR_EXPR);
		      tree decl = TREE_OPERAND (arg, 0);
		      gang_private_vars->add (decl);
		    }
		}
	    }
	}
    }
}

static void
find_local_vars_to_propagate (parallel_g *par, unsigned outer_mask,
			      hash_set<tree> *partitioned_var_uses,
			      hash_set<tree> *gang_private_vars,
			      bitmap writes_gang_private,
			      vec<propagation_set *> *prop_set)
{
  unsigned mask = outer_mask | par->mask;

  if (par->inner)
    find_local_vars_to_propagate (par->inner, mask, partitioned_var_uses,
				  gang_private_vars, writes_gang_private,
				  prop_set);
  if (par->next)
    find_local_vars_to_propagate (par->next, outer_mask, partitioned_var_uses,
				  gang_private_vars, writes_gang_private,
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
		      || !partitioned_var_uses->contains (var))
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

		      if (gang_private_vars->contains (var))
			{
			  /* If we write a gang-private variable, we want a
			     barrier at the end of the block.  */
			  bitmap_set_bit (writes_gang_private, block->index);
			  continue;
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
	      create_new_def_for (var, join_phi,
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
}

static tree
build_receiver_ref (tree var, tree receiver_decl, field_map_t *fields)
{
  tree x = build_simple_mem_ref (receiver_decl);
  tree field = *fields->get (var);
  TREE_THIS_NOTRAP (x) = 1;
  x = omp_build_component_ref (x, field);
  return x;
}

static tree
build_sender_ref (tree var, tree sender_decl, field_map_t *fields)
{
  if (POINTER_TYPE_P (TREE_TYPE (sender_decl)))
    sender_decl = build_simple_mem_ref (sender_decl);
  tree field = *fields->get (var);
  return omp_build_component_ref (sender_decl, field);
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
		    tree record_type, record_field_map_t *record_field_map,
		    unsigned HOST_WIDE_INT placement,
		    bool isolate_broadcasts, bool has_gang_private_write)
{
  /* If we only have virtual defs, we'll have no record type, but we still want
     to emit single_copy_start and (particularly) single_copy_end to act as
     a vdef source on the neutered edge representing memory writes on the
     non-neutered edge.  */
  if (!record_type)
    record_type = char_type_node;

  tree sender_decl
    = targetm.goacc.create_worker_broadcast_record (record_type, true,
						    ".oacc_worker_o",
						    placement);
  tree receiver_decl
    = targetm.goacc.create_worker_broadcast_record (record_type, false,
						    ".oacc_worker_i",
						    placement);

  gimple_stmt_iterator gsi = gsi_last_bb (to);
  if (EDGE_COUNT (to->succs) > 1)
    gsi_prev (&gsi);
  edge e = split_block (to, gsi_stmt (gsi));
  basic_block barrier_block = e->dest;

  gimple_stmt_iterator start = gsi_after_labels (from);

  tree decl = builtin_decl_explicit (BUILT_IN_GOACC_SINGLE_COPY_START);

  tree lhs = create_tmp_var (TREE_TYPE (TREE_TYPE (decl)));

  gimple *call
    = gimple_build_call (decl, 1,
			 POINTER_TYPE_P (TREE_TYPE (sender_decl))
			 ? sender_decl : build_fold_addr_expr (sender_decl));
  gimple_call_set_lhs (call, lhs);
  gsi_insert_before (&start, call, GSI_NEW_STMT);
  update_stmt (call);

  /* The shared-memory range for this block overflowed.  Add a barrier before
     the GOACC_single_copy_start call.  */
  if (isolate_broadcasts)
    {
      decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
      gimple *acc_bar = gimple_build_call (decl, 0);
      gsi_insert_before (&start, acc_bar, GSI_SAME_STMT);
    }

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

  gimple_stmt_iterator bar_gsi = gsi_start_bb (barrier_block);
  cond = gimple_build_cond (NE_EXPR, recv_tmp, zero_ptr, NULL_TREE, NULL_TREE);

  if (record_type != char_type_node || has_gang_private_write)
    {
      decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
      gimple *acc_bar = gimple_build_call (decl, 0);

      gsi_insert_before (&bar_gsi, acc_bar, GSI_NEW_STMT);
      gsi_insert_after (&bar_gsi, cond, GSI_NEW_STMT);
    }
  else
    gsi_insert_before (&bar_gsi, cond, GSI_NEW_STMT);

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
	gcc_assert (VAR_P (var));

      /* If we had no record type, we will have no fields map.  */
      field_map_t *fields = record_field_map->get (record_type);

      if (worker_partitioned_uses->contains (var)
	  && fields
	  && fields->get (var))
	{
	  tree neutered_def = make_ssa_name (TREE_TYPE (var));

	  /* Receive definition from shared memory block.  */

	  tree receiver_ref = build_receiver_ref (var, receiver_decl, fields);
	  gassign *recv = gimple_build_assign (neutered_def,
					       receiver_ref);
	  gsi_insert_after (&copyout_gsi, recv, GSI_CONTINUE_LINKING);
	  update_stmt (recv);

	  if (VAR_P (var))
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

	  tree sender_ref = build_sender_ref (var, sender_decl, fields);

	  if (TREE_CODE (var) == SSA_NAME)
	    {
	      gassign *send = gimple_build_assign (sender_ref, var);
	      gimple_seq_add_stmt (&sender_seq, send);
	      update_stmt (send);
	    }
	  else if (VAR_P (var))
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

  /* The shared-memory range for this block overflowed.  Add a barrier at the
     end.  */
  if (isolate_broadcasts)
    {
      gsi = gsi_start_bb (exit_block);
      decl = builtin_decl_explicit (BUILT_IN_GOACC_BARRIER);
      gimple *acc_bar = gimple_build_call (decl, 0);
      gsi_insert_before (&gsi, acc_bar, GSI_SAME_STMT);
    }

  /* It's possible for the ET->DEST block (the work done by the active thread)
     to finish with a control-flow insn, e.g. a UNIQUE function call.  Split
     the block and add SENDER_SEQ in the latter part to avoid having control
     flow in the middle of a BB.  */

  decl = builtin_decl_explicit (BUILT_IN_GOACC_SINGLE_COPY_END);
  call = gimple_build_call (decl, 1,
			    POINTER_TYPE_P (TREE_TYPE (sender_decl))
			    ? sender_decl
			    : build_fold_addr_expr (sender_decl));
  gimple_seq_add_stmt (&sender_seq, call);

  gsi = gsi_last_bb (body);
  gimple *last = gsi_stmt (gsi);
  basic_block sender_block = split_block (body, last)->dest;
  gsi = gsi_last_bb (sender_block);
  gsi_insert_seq_after (&gsi, sender_seq, GSI_CONTINUE_LINKING);
}

typedef hash_map<basic_block, std::pair<unsigned HOST_WIDE_INT, bool> >
  blk_offset_map_t;

static void
neuter_worker_single (parallel_g *par, unsigned outer_mask,
		      bitmap worker_single, bitmap vector_single,
		      vec<propagation_set *> *prop_set,
		      hash_set<tree> *partitioned_var_uses,
		      record_field_map_t *record_field_map,
		      blk_offset_map_t *blk_offset_map,
		      bitmap writes_gang_private)
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

	  bool only_marker_fns = true;
	  bool join_block = false;

	  for (gimple_stmt_iterator gsi = gsi_start_bb (block);
	       !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gimple_code (stmt) == GIMPLE_CALL
		  && gimple_call_internal_p (stmt, IFN_UNIQUE))
		{
		  enum ifn_unique_kind k = ((enum ifn_unique_kind)
		    TREE_INT_CST_LOW (gimple_call_arg (stmt, 0)));
		  if (k != IFN_UNIQUE_OACC_PRIVATE
		      && k != IFN_UNIQUE_OACC_JOIN
		      && k != IFN_UNIQUE_OACC_FORK
		      && k != IFN_UNIQUE_OACC_HEAD_MARK
		      && k != IFN_UNIQUE_OACC_TAIL_MARK)
		    only_marker_fns = false;
		  else if (k == IFN_UNIQUE_OACC_JOIN)
		    /* The JOIN marker is special in that it *cannot* be
		       predicated for worker zero, because it may be lowered
		       to a barrier instruction and all workers must typically
		       execute that barrier.  We shouldn't be doing any
		       broadcasts from the join block anyway.  */
		    join_block = true;
		}
	      else if (gimple_code (stmt) == GIMPLE_CALL
		       && gimple_call_internal_p (stmt, IFN_GOACC_LOOP))
		/* Empty.  */;
	      else if (gimple_nop_p (stmt))
		/* Empty.  */;
	      else
		only_marker_fns = false;
	    }

	  /* We can skip predicating this block for worker zero if the only
	     thing it contains is marker functions that will be removed in the
	     oaccdevlow pass anyway.
	     Don't do this if the block has (any) phi nodes, because those
	     might define SSA names that need broadcasting.
	     TODO: We might be able to skip transforming blocks that only
	     contain some other trivial statements too.  */
	  if (only_marker_fns && !phi_nodes (block))
	    continue;

	  gcc_assert (!join_block);

	  if (has_defs)
	    {
	      tree record_type = (tree) block->aux;
	      std::pair<unsigned HOST_WIDE_INT, bool> *off_rngalloc
		= blk_offset_map->get (block);
	      gcc_assert (!record_type || off_rngalloc);
	      unsigned HOST_WIDE_INT offset
		= off_rngalloc ? off_rngalloc->first : 0;
	      bool range_allocated
		= off_rngalloc ? off_rngalloc->second : true;
	      bool has_gang_private_write
		= bitmap_bit_p (writes_gang_private, block->index);
	      worker_single_copy (block, block, &def_escapes_block,
				  &worker_partitioned_uses, record_type,
				  record_field_map,
				  offset, !range_allocated,
				  has_gang_private_write);
	    }
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
			  prop_set, partitioned_var_uses, record_field_map,
			  blk_offset_map, writes_gang_private);
  if (par->next)
    neuter_worker_single (par->next, outer_mask, worker_single, vector_single,
			  prop_set, partitioned_var_uses, record_field_map,
			  blk_offset_map, writes_gang_private);
}

static void
dfs_broadcast_reachable_1 (basic_block bb, sbitmap reachable)
{
  if (bb->flags & BB_VISITED)
    return;

  bb->flags |= BB_VISITED;

  if (bb->succs)
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  basic_block dest = e->dest;
	  if (dest->aux)
	    bitmap_set_bit (reachable, dest->index);
	  else
	    dfs_broadcast_reachable_1 (dest, reachable);
	}
    }
}

typedef std::pair<int, tree> idx_decl_pair_t;

typedef auto_vec<splay_tree> used_range_vec_t;

static int
sort_size_descending (const void *a, const void *b)
{
  const idx_decl_pair_t *pa = (const idx_decl_pair_t *) a;
  const idx_decl_pair_t *pb = (const idx_decl_pair_t *) b;
  unsigned HOST_WIDE_INT asize = tree_to_uhwi (TYPE_SIZE_UNIT (pa->second));
  unsigned HOST_WIDE_INT bsize = tree_to_uhwi (TYPE_SIZE_UNIT (pb->second));
  return bsize - asize;
}

class addr_range
{
public:
  addr_range (unsigned HOST_WIDE_INT addr_lo, unsigned HOST_WIDE_INT addr_hi)
    : lo (addr_lo), hi (addr_hi)
    { }
  addr_range (const addr_range &ar) : lo (ar.lo), hi (ar.hi)
    { }
  addr_range () : lo (0), hi (0)
    { }

  bool invalid () { return lo == 0 && hi == 0; }

  unsigned HOST_WIDE_INT lo;
  unsigned HOST_WIDE_INT hi;
};

static int
splay_tree_compare_addr_range (splay_tree_key a, splay_tree_key b)
{
  addr_range *ar = (addr_range *) a;
  addr_range *br = (addr_range *) b;
  if (ar->lo == br->lo && ar->hi == br->hi)
    return 0;
  if (ar->hi <= br->lo)
    return -1;
  else if (ar->lo >= br->hi)
    return 1;
  return 0;
}

static void
splay_tree_free_key (splay_tree_key k)
{
  addr_range *ar = (addr_range *) k;
  delete ar;
}

static addr_range
first_fit_range (splay_tree s, unsigned HOST_WIDE_INT size,
		 unsigned HOST_WIDE_INT align, addr_range *bounds)
{
  splay_tree_node min = splay_tree_min (s);
  if (min)
    {
      splay_tree_node next;
      while ((next = splay_tree_successor (s, min->key)))
	{
	  unsigned HOST_WIDE_INT lo = ((addr_range *) min->key)->hi;
	  unsigned HOST_WIDE_INT hi = ((addr_range *) next->key)->lo;
	  unsigned HOST_WIDE_INT base = (lo + align - 1) & ~(align - 1);
	  if (base + size <= hi)
	    return addr_range (base, base + size);
	  min = next;
	}

      unsigned HOST_WIDE_INT base = ((addr_range *)min->key)->hi;
      base = (base + align - 1) & ~(align - 1);
      if (base + size <= bounds->hi)
	return addr_range (base, base + size);
      else
	return addr_range ();
    }
  else
    {
      unsigned HOST_WIDE_INT lo = bounds->lo;
      lo = (lo + align - 1) & ~(align - 1);
      if (lo + size <= bounds->hi)
	return addr_range (lo, lo + size);
      else
	return addr_range ();
    }
}

static int
merge_ranges_1 (splay_tree_node n, void *ptr)
{
  splay_tree accum = (splay_tree) ptr;
  addr_range ar = *(addr_range *) n->key;

  splay_tree_node old = splay_tree_lookup (accum, n->key);

  /* We might have an overlap.  Create a new range covering the
     overlapping parts.  */
  if (old)
    {
      addr_range *old_ar = (addr_range *) old->key;
      ar.lo = MIN (old_ar->lo, ar.lo);
      ar.hi = MAX (old_ar->hi, ar.hi);
      splay_tree_remove (accum, old->key);
    }

  addr_range *new_ar = new addr_range (ar);

  splay_tree_insert (accum, (splay_tree_key) new_ar, n->value);

  return 0;
}

static void
merge_ranges (splay_tree accum, splay_tree sp)
{
  splay_tree_foreach (sp, merge_ranges_1, (void *) accum);
}

static void
oacc_do_neutering (unsigned HOST_WIDE_INT bounds_lo,
		   unsigned HOST_WIDE_INT bounds_hi)
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
  {
    tree attr = oacc_get_fn_attrib (current_function_decl);
    tree dims = TREE_VALUE (attr);
    unsigned ix;
    for (ix = 0; ix != GOMP_DIM_MAX; ix++, dims = TREE_CHAIN (dims))
      {
	tree allowed = TREE_PURPOSE (dims);
	if (allowed && integer_zerop (allowed))
	  mask |= GOMP_DIM_MASK (ix);
      }
  }

  parallel_g *par = omp_sese_discover_pars (&bb_stmt_map);
  populate_single_mode_bitmaps (par, worker_single, vector_single, mask, 0);

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    bb->aux = NULL;

  vec<propagation_set *> prop_set (vNULL);
  prop_set.safe_grow_cleared (last_basic_block_for_fn (cfun), true);

  find_ssa_names_to_propagate (par, mask, worker_single, vector_single,
			       &prop_set);

  hash_set<tree> partitioned_var_uses;
  hash_set<tree> gang_private_vars;
  auto_bitmap writes_gang_private;

  find_gang_private_vars (&gang_private_vars);
  find_partitioned_var_uses (par, mask, &partitioned_var_uses);
  find_local_vars_to_propagate (par, mask, &partitioned_var_uses,
				&gang_private_vars, writes_gang_private,
				&prop_set);

  record_field_map_t record_field_map;

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

	  bool existed;
	  field_map_t *fields
	    = &record_field_map.get_or_insert (record_type, &existed);
	  gcc_checking_assert (!existed);

	  /* Insert var fields in reverse order, so the last inserted element
	     is the first in the structure.  */
	  for (int i = field_vec.length () - 1; i >= 0; i--)
	    install_var_field (field_vec[i], record_type, fields);

	  layout_type (record_type);

	  bb->aux = (tree) record_type;
	}
    }

  sbitmap *reachable
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    last_basic_block_for_fn (cfun));

  bitmap_vector_clear (reachable, last_basic_block_for_fn (cfun));

  auto_vec<std::pair<int, tree> > priority;

  FOR_ALL_BB_FN (bb, cfun)
    {
      if (bb->aux)
	{
	  tree record_type = (tree) bb->aux;

	  basic_block bb2;
	  FOR_ALL_BB_FN (bb2, cfun)
	    bb2->flags &= ~BB_VISITED;

	  priority.safe_push (std::make_pair (bb->index, record_type));
	  dfs_broadcast_reachable_1 (bb, reachable[bb->index]);
	}
    }

  sbitmap *inverted
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    last_basic_block_for_fn (cfun));

  bitmap_vector_clear (inverted, last_basic_block_for_fn (cfun));

  for (int i = 0; i < last_basic_block_for_fn (cfun); i++)
    {
      sbitmap_iterator bi;
      unsigned int j;
      EXECUTE_IF_SET_IN_BITMAP (reachable[i], 0, j, bi)
	bitmap_set_bit (inverted[j], i);
    }

  for (int i = 0; i < last_basic_block_for_fn (cfun); i++)
    bitmap_ior (reachable[i], reachable[i], inverted[i]);

  sbitmap_vector_free (inverted);

  used_range_vec_t used_ranges;

  used_ranges.safe_grow_cleared (last_basic_block_for_fn (cfun));

  blk_offset_map_t blk_offset_map;

  addr_range worker_shm_bounds (bounds_lo, bounds_hi);

  priority.qsort (sort_size_descending);
  for (unsigned int i = 0; i < priority.length (); i++)
    {
      idx_decl_pair_t p = priority[i];
      int blkno = p.first;
      tree record_type = p.second;
      HOST_WIDE_INT size = tree_to_uhwi (TYPE_SIZE_UNIT (record_type));
      HOST_WIDE_INT align = TYPE_ALIGN_UNIT (record_type);

      splay_tree conflicts = splay_tree_new (splay_tree_compare_addr_range,
					     splay_tree_free_key, NULL);

      if (!used_ranges[blkno])
	used_ranges[blkno] = splay_tree_new (splay_tree_compare_addr_range,
					     splay_tree_free_key, NULL);
      else
	merge_ranges (conflicts, used_ranges[blkno]);

      sbitmap_iterator bi;
      unsigned int j;
      EXECUTE_IF_SET_IN_BITMAP (reachable[blkno], 0, j, bi)
	if (used_ranges[j])
	  merge_ranges (conflicts, used_ranges[j]);

      addr_range ar
	= first_fit_range (conflicts, size, align, &worker_shm_bounds);

      splay_tree_delete (conflicts);

      if (ar.invalid ())
	{
	  unsigned HOST_WIDE_INT base
	    = (bounds_lo + align - 1) & ~(align - 1);
	  if (base + size > bounds_hi)
	    error_at (UNKNOWN_LOCATION, "shared-memory region overflow");
	  std::pair<unsigned HOST_WIDE_INT, bool> base_inrng
	    = std::make_pair (base, false);
	  blk_offset_map.put (BASIC_BLOCK_FOR_FN (cfun, blkno), base_inrng);
	}
      else
	{
	  splay_tree_node old = splay_tree_lookup (used_ranges[blkno],
						   (splay_tree_key) &ar);
	  if (old)
	    {
	      fprintf (stderr, "trying to map [%d..%d] but [%d..%d] is "
		       "already mapped in block %d\n", (int) ar.lo,
		       (int) ar.hi, (int) ((addr_range *) old->key)->lo,
		       (int) ((addr_range *) old->key)->hi, blkno);
	      abort ();
	    }

	  addr_range *arp = new addr_range (ar);
	  splay_tree_insert (used_ranges[blkno], (splay_tree_key) arp,
			     (splay_tree_value) blkno);
	  std::pair<unsigned HOST_WIDE_INT, bool> base_inrng
	    = std::make_pair (ar.lo, true);
	  blk_offset_map.put (BASIC_BLOCK_FOR_FN (cfun, blkno), base_inrng);
	}
    }

  sbitmap_vector_free (reachable);

  neuter_worker_single (par, mask, worker_single, vector_single, &prop_set,
			&partitioned_var_uses, &record_field_map,
			&blk_offset_map, writes_gang_private);

  record_field_map.empty ();

  /* These are supposed to have been 'delete'd by 'neuter_worker_single'.  */
  for (auto it : prop_set)
    gcc_checking_assert (!it);
  prop_set.release ();

  delete par;

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

static int
execute_omp_oacc_neuter_broadcast ()
{
  unsigned HOST_WIDE_INT reduction_size[GOMP_DIM_MAX];
  unsigned HOST_WIDE_INT private_size[GOMP_DIM_MAX];

  for (unsigned i = 0; i < GOMP_DIM_MAX; i++)
    {
      reduction_size[i] = 0;
      private_size[i] = 0;
    }

  /* Calculate shared memory size required for reduction variables and
     gang-private memory for this offloaded function.  */
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!is_gimple_call (stmt))
	    continue;
	  gcall *call = as_a <gcall *> (stmt);
	  if (!gimple_call_internal_p (call))
	    continue;
	  enum internal_fn ifn_code = gimple_call_internal_fn (call);
	  switch (ifn_code)
	    {
	    default: break;
	    case IFN_GOACC_REDUCTION:
	      if (integer_minus_onep (gimple_call_arg (call, 3)))
		continue;
	      else
		{
		  unsigned code = TREE_INT_CST_LOW (gimple_call_arg (call, 0));
		  /* Only count reduction variables once: the choice to pick
		     the setup call is fairly arbitrary.  */
		  if (code == IFN_GOACC_REDUCTION_SETUP)
		    {
		      int level = TREE_INT_CST_LOW (gimple_call_arg (call, 3));
		      tree var = gimple_call_arg (call, 2);
		      tree offset = gimple_call_arg (call, 5);
		      tree var_type = TREE_TYPE (var);
		      unsigned HOST_WIDE_INT limit
			= (tree_to_uhwi (offset)
			   + tree_to_uhwi (TYPE_SIZE_UNIT (var_type)));
		      reduction_size[level]
			= MAX (reduction_size[level], limit);
		    }
		}
	      break;
	    case IFN_UNIQUE:
	      {
		enum ifn_unique_kind kind
		  = ((enum ifn_unique_kind)
		     TREE_INT_CST_LOW (gimple_call_arg (call, 0)));

		if (kind == IFN_UNIQUE_OACC_PRIVATE)
		  {
		    HOST_WIDE_INT level
		      = TREE_INT_CST_LOW (gimple_call_arg (call, 2));
		    if (level == -1)
		      break;
		    for (unsigned i = 3;
			 i < gimple_call_num_args (call);
			 i++)
		      {
			tree arg = gimple_call_arg (call, i);
			gcc_assert (TREE_CODE (arg) == ADDR_EXPR);
			tree decl = TREE_OPERAND (arg, 0);
			unsigned HOST_WIDE_INT align = DECL_ALIGN_UNIT (decl);
			private_size[level] = ((private_size[level] + align - 1)
					       & ~(align - 1));
			unsigned HOST_WIDE_INT decl_size
			  = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (decl)));
			private_size[level] += decl_size;
		      }
		  }
	      }
	      break;
	    }
	}
    }

  int dims[GOMP_DIM_MAX];
  for (unsigned i = 0; i < GOMP_DIM_MAX; i++)
    dims[i] = oacc_get_fn_dim_size (current_function_decl, i);

  /* Find bounds of shared-memory buffer space we can use.  */
  unsigned HOST_WIDE_INT bounds_lo = 0, bounds_hi = 0;
  if (targetm.goacc.shared_mem_layout)
    targetm.goacc.shared_mem_layout (&bounds_lo, &bounds_hi, dims,
				     private_size, reduction_size);

  /* Perform worker partitioning unless we know 'num_workers(1)'.  */
  if (dims[GOMP_DIM_WORKER] != 1)
    oacc_do_neutering (bounds_lo, bounds_hi);

  return 0;
}

namespace {

const pass_data pass_data_omp_oacc_neuter_broadcast =
{
  GIMPLE_PASS, /* type */
  "omp_oacc_neuter_broadcast", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_omp_oacc_neuter_broadcast : public gimple_opt_pass
{
public:
  pass_omp_oacc_neuter_broadcast (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_oacc_neuter_broadcast, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
  {
    if (!flag_openacc)
      return false;

    if (!targetm.goacc.create_worker_broadcast_record)
      return false;

    /* Only relevant for OpenACC offloaded functions.  */
    tree attr = oacc_get_fn_attrib (fun->decl);
    if (!attr)
      return false;

    return true;
  }

  unsigned int execute (function *) final override
    {
      return execute_omp_oacc_neuter_broadcast ();
    }

}; // class pass_omp_oacc_neuter_broadcast

} // anon namespace

gimple_opt_pass *
make_pass_omp_oacc_neuter_broadcast (gcc::context *ctxt)
{
  return new pass_omp_oacc_neuter_broadcast (ctxt);
}
