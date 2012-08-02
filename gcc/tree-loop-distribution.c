/* Loop distribution.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Georges-Andre Silber <Georges-Andre.Silber@ensmp.fr>
   and Sebastian Pop <sebastian.pop@amd.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This pass performs loop distribution: for example, the loop

   |DO I = 2, N
   |    A(I) = B(I) + C
   |    D(I) = A(I-1)*E
   |ENDDO

   is transformed to

   |DOALL I = 2, N
   |   A(I) = B(I) + C
   |ENDDO
   |
   |DOALL I = 2, N
   |   D(I) = A(I-1)*E
   |ENDDO

   This pass uses an RDG, Reduced Dependence Graph built on top of the
   data dependence relations.  The RDG is then topologically sorted to
   obtain a map of information producers/consumers based on which it
   generates the new loops.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"

enum partition_kind { PKIND_NORMAL, PKIND_MEMSET, PKIND_MEMCPY };

typedef struct partition_s
{
  bitmap stmts;
  bool has_writes;
  enum partition_kind kind;
  /* data-references a kind != PKIND_NORMAL partition is about.  */
  data_reference_p main_dr;
  data_reference_p secondary_dr;
} *partition_t;

DEF_VEC_P (partition_t);
DEF_VEC_ALLOC_P (partition_t, heap);

/* Allocate and initialize a partition from BITMAP.  */

static partition_t
partition_alloc (bitmap stmts)
{
  partition_t partition = XCNEW (struct partition_s);
  partition->stmts = stmts ? stmts : BITMAP_ALLOC (NULL);
  partition->has_writes = false;
  partition->kind = PKIND_NORMAL;
  return partition;
}

/* Free PARTITION.  */

static void
partition_free (partition_t partition)
{
  BITMAP_FREE (partition->stmts);
  free (partition);
}

/* Returns true if the partition can be generated as a builtin.  */

static bool
partition_builtin_p (partition_t partition)
{
  return partition->kind != PKIND_NORMAL;
}

/* Returns true if the partition has an writes.  */

static bool
partition_has_writes (partition_t partition)
{
  return partition->has_writes;
}

/* If bit I is not set, it means that this node represents an
   operation that has already been performed, and that should not be
   performed again.  This is the subgraph of remaining important
   computations that is passed to the DFS algorithm for avoiding to
   include several times the same stores in different loops.  */
static bitmap remaining_stmts;

/* A node of the RDG is marked in this bitmap when it has as a
   predecessor a node that writes to memory.  */
static bitmap upstream_mem_writes;

/* Returns true when DEF is an SSA_NAME defined in LOOP and used after
   the LOOP.  */

static bool
ssa_name_has_uses_outside_loop_p (tree def, loop_p loop)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    if (loop != loop_containing_stmt (USE_STMT (use_p)))
      return true;

  return false;
}

/* Returns true when STMT defines a scalar variable used after the
   loop LOOP.  */

static bool
stmt_has_scalar_dependences_outside_loop (loop_p loop, gimple stmt)
{
  def_operand_p def_p;
  ssa_op_iter op_iter;

  if (gimple_code (stmt) == GIMPLE_PHI)
    return ssa_name_has_uses_outside_loop_p (gimple_phi_result (stmt), loop);

  FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, op_iter, SSA_OP_DEF)
    if (ssa_name_has_uses_outside_loop_p (DEF_FROM_PTR (def_p), loop))
      return true;

  return false;
}

/* Update the PHI nodes of NEW_LOOP.  NEW_LOOP is a duplicate of
   ORIG_LOOP.  */

static void
update_phis_for_loop_copy (struct loop *orig_loop, struct loop *new_loop)
{
  tree new_ssa_name;
  gimple_stmt_iterator si_new, si_orig;
  edge orig_loop_latch = loop_latch_edge (orig_loop);
  edge orig_entry_e = loop_preheader_edge (orig_loop);
  edge new_loop_entry_e = loop_preheader_edge (new_loop);

  /* Scan the phis in the headers of the old and new loops
     (they are organized in exactly the same order).  */
  for (si_new = gsi_start_phis (new_loop->header),
       si_orig = gsi_start_phis (orig_loop->header);
       !gsi_end_p (si_new) && !gsi_end_p (si_orig);
       gsi_next (&si_new), gsi_next (&si_orig))
    {
      tree def;
      source_location locus;
      gimple phi_new = gsi_stmt (si_new);
      gimple phi_orig = gsi_stmt (si_orig);

      /* Add the first phi argument for the phi in NEW_LOOP (the one
	 associated with the entry of NEW_LOOP)  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, orig_entry_e);
      locus = gimple_phi_arg_location_from_edge (phi_orig, orig_entry_e);
      add_phi_arg (phi_new, def, new_loop_entry_e, locus);

      /* Add the second phi argument for the phi in NEW_LOOP (the one
	 associated with the latch of NEW_LOOP)  */
      def = PHI_ARG_DEF_FROM_EDGE (phi_orig, orig_loop_latch);
      locus = gimple_phi_arg_location_from_edge (phi_orig, orig_loop_latch);

      if (TREE_CODE (def) == SSA_NAME)
	{
	  new_ssa_name = get_current_def (def);

	  if (!new_ssa_name)
	    /* This only happens if there are no definitions inside the
	       loop.  Use the the invariant in the new loop as is.  */
	    new_ssa_name = def;
	}
      else
	/* Could be an integer.  */
	new_ssa_name = def;

      add_phi_arg (phi_new, new_ssa_name, loop_latch_edge (new_loop), locus);
    }
}

/* Return a copy of LOOP placed before LOOP.  */

static struct loop *
copy_loop_before (struct loop *loop)
{
  struct loop *res;
  edge preheader = loop_preheader_edge (loop);

  initialize_original_copy_tables ();
  res = slpeel_tree_duplicate_loop_to_edge_cfg (loop, preheader);
  gcc_assert (res != NULL);
  free_original_copy_tables ();

  update_phis_for_loop_copy (loop, res);
  rename_variables_in_loop (res);

  return res;
}

/* Creates an empty basic block after LOOP.  */

static void
create_bb_after_loop (struct loop *loop)
{
  edge exit = single_exit (loop);

  if (!exit)
    return;

  split_edge (exit);
}

/* Generate code for PARTITION from the code in LOOP.  The loop is
   copied when COPY_P is true.  All the statements not flagged in the
   PARTITION bitmap are removed from the loop or from its copy.  The
   statements are indexed in sequence inside a basic block, and the
   basic blocks of a loop are taken in dom order.  */

static void
generate_loops_for_partition (struct loop *loop, partition_t partition,
			      bool copy_p)
{
  unsigned i, x;
  gimple_stmt_iterator bsi;
  basic_block *bbs;

  if (copy_p)
    {
      loop = copy_loop_before (loop);
      gcc_assert (loop != NULL);
      create_preheader (loop, CP_SIMPLE_PREHEADERS);
      create_bb_after_loop (loop);
    }

  /* Remove stmts not in the PARTITION bitmap.  The order in which we
     visit the phi nodes and the statements is exactly as in
     stmts_from_loop.  */
  bbs = get_loop_body_in_dom_order (loop);

  if (MAY_HAVE_DEBUG_STMTS)
    for (x = 0, i = 0; i < loop->num_nodes; i++)
      {
	basic_block bb = bbs[i];

	for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  if (!bitmap_bit_p (partition->stmts, x++))
	    reset_debug_uses (gsi_stmt (bsi));

	for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple stmt = gsi_stmt (bsi);
	    if (gimple_code (stmt) != GIMPLE_LABEL
		&& !is_gimple_debug (stmt)
		&& !bitmap_bit_p (partition->stmts, x++))
	      reset_debug_uses (stmt);
	  }
      }

  for (x = 0, i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi);)
	if (!bitmap_bit_p (partition->stmts, x++))
	  {
	    gimple phi = gsi_stmt (bsi);
	    if (!is_gimple_reg (gimple_phi_result (phi)))
	      mark_virtual_phi_result_for_renaming (phi);
	    remove_phi_node (&bsi, true);
	  }
	else
	  gsi_next (&bsi);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi);)
	{
	  gimple stmt = gsi_stmt (bsi);
	  if (gimple_code (stmt) != GIMPLE_LABEL
	      && !is_gimple_debug (stmt)
	      && !bitmap_bit_p (partition->stmts, x++))
	    {
	      unlink_stmt_vdef (stmt);
	      gsi_remove (&bsi, true);
	      release_defs (stmt);
	    }
	  else
	    gsi_next (&bsi);
	}
    }

  free (bbs);
}

/* Build the size argument for a memory operation call.  */

static tree
build_size_arg_loc (location_t loc, data_reference_p dr, tree nb_iter)
{
  tree size;
  size = fold_build2_loc (loc, MULT_EXPR, sizetype,
			  fold_convert_loc (loc, sizetype, nb_iter),
			  TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr))));
  return fold_convert_loc (loc, size_type_node, size);
}

/* Build an address argument for a memory operation call.  */

static tree
build_addr_arg_loc (location_t loc, data_reference_p dr, tree nb_bytes)
{
  tree addr_base;

  addr_base = size_binop_loc (loc, PLUS_EXPR, DR_OFFSET (dr), DR_INIT (dr));
  addr_base = fold_convert_loc (loc, sizetype, addr_base);

  /* Test for a negative stride, iterating over every element.  */
  if (tree_int_cst_sgn (DR_STEP (dr)) == -1)
    {
      addr_base = size_binop_loc (loc, MINUS_EXPR, addr_base,
				  fold_convert_loc (loc, sizetype, nb_bytes));
      addr_base = size_binop_loc (loc, PLUS_EXPR, addr_base,
				  TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr))));
    }

  return fold_build_pointer_plus_loc (loc, DR_BASE_ADDRESS (dr), addr_base);
}

/* Generate a call to memset for PARTITION in LOOP.  */

static void
generate_memset_builtin (struct loop *loop, partition_t partition)
{
  gimple_stmt_iterator gsi;
  gimple stmt, fn_call;
  tree nb_iter, mem, fn, nb_bytes;
  location_t loc;
  tree val;

  stmt = DR_STMT (partition->main_dr);
  loc = gimple_location (stmt);
  if (gimple_bb (stmt) == loop->latch)
    nb_iter = number_of_latch_executions (loop);
  else
    nb_iter = number_of_exit_cond_executions (loop);

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = build_size_arg_loc (loc, partition->main_dr, nb_iter);
  nb_bytes = force_gimple_operand_gsi (&gsi, nb_bytes, true, NULL_TREE,
				       false, GSI_CONTINUE_LINKING);
  mem = build_addr_arg_loc (loc, partition->main_dr, nb_bytes);
  mem = force_gimple_operand_gsi (&gsi, mem, true, NULL_TREE,
				  false, GSI_CONTINUE_LINKING);

  /* This exactly matches the pattern recognition in classify_partition.  */
  val = gimple_assign_rhs1 (stmt);
  if (integer_zerop (val)
      || real_zerop (val)
      || TREE_CODE (val) == CONSTRUCTOR)
    val = integer_zero_node;
  else if (integer_all_onesp (val))
    val = build_int_cst (integer_type_node, -1);
  else
    {
      if (TREE_CODE (val) == INTEGER_CST)
	val = fold_convert (integer_type_node, val);
      else if (!useless_type_conversion_p (integer_type_node, TREE_TYPE (val)))
	{
	  gimple cstmt;
	  tree tem = create_tmp_reg (integer_type_node, NULL);
	  tem = make_ssa_name (tem, NULL);
	  cstmt = gimple_build_assign_with_ops (NOP_EXPR, tem, val, NULL_TREE);
	  gsi_insert_after (&gsi, cstmt, GSI_CONTINUE_LINKING);
	  val = tem;
	}
    }

  fn = build_fold_addr_expr (builtin_decl_implicit (BUILT_IN_MEMSET));
  fn_call = gimple_build_call (fn, 3, mem, val, nb_bytes);
  gsi_insert_after (&gsi, fn_call, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "generated memset");
      if (integer_zerop (val))
	fprintf (dump_file, " zero\n");
      else if (integer_all_onesp (val))
	fprintf (dump_file, " minus one\n");
      else
	fprintf (dump_file, "\n");
    }
}

/* Generate a call to memcpy for PARTITION in LOOP.  */

static void
generate_memcpy_builtin (struct loop *loop, partition_t partition)
{
  gimple_stmt_iterator gsi;
  gimple stmt, fn_call;
  tree nb_iter, dest, src, fn, nb_bytes;
  location_t loc;
  enum built_in_function kind;

  stmt = DR_STMT (partition->main_dr);
  loc = gimple_location (stmt);
  if (gimple_bb (stmt) == loop->latch)
    nb_iter = number_of_latch_executions (loop);
  else
    nb_iter = number_of_exit_cond_executions (loop);

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = build_size_arg_loc (loc, partition->main_dr, nb_iter);
  nb_bytes = force_gimple_operand_gsi (&gsi, nb_bytes, true, NULL_TREE,
				       false, GSI_CONTINUE_LINKING);
  dest = build_addr_arg_loc (loc, partition->main_dr, nb_bytes);
  src = build_addr_arg_loc (loc, partition->secondary_dr, nb_bytes);
  if (ptr_derefs_may_alias_p (dest, src))
    kind = BUILT_IN_MEMMOVE;
  else
    kind = BUILT_IN_MEMCPY;

  dest = force_gimple_operand_gsi (&gsi, dest, true, NULL_TREE,
				   false, GSI_CONTINUE_LINKING);
  src = force_gimple_operand_gsi (&gsi, src, true, NULL_TREE,
				  false, GSI_CONTINUE_LINKING);
  fn = build_fold_addr_expr (builtin_decl_implicit (kind));
  fn_call = gimple_build_call (fn, 3, dest, src, nb_bytes);
  gsi_insert_after (&gsi, fn_call, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (kind == BUILT_IN_MEMCPY)
	fprintf (dump_file, "generated memcpy\n");
      else
	fprintf (dump_file, "generated memmove\n");
    }
}

/* Remove and destroy the loop LOOP.  */

static void
destroy_loop (struct loop *loop)
{
  unsigned nbbs = loop->num_nodes;
  edge exit = single_exit (loop);
  basic_block src = loop_preheader_edge (loop)->src, dest = exit->dest;
  basic_block *bbs;
  unsigned i;

  bbs = get_loop_body_in_dom_order (loop);

  redirect_edge_pred (exit, src);
  exit->flags &= ~(EDGE_TRUE_VALUE|EDGE_FALSE_VALUE);
  exit->flags |= EDGE_FALLTHRU;
  cancel_loop_tree (loop);
  rescan_loop_exit (exit, false, true);

  for (i = 0; i < nbbs; i++)
    {
      /* We have made sure to not leave any dangling uses of SSA
         names defined in the loop.  With the exception of virtuals.
	 Make sure we replace all uses of virtual defs that will remain
	 outside of the loop with the bare symbol as delete_basic_block
	 will release them.  */
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  if (!is_gimple_reg (gimple_phi_result (phi)))
	    mark_virtual_phi_result_for_renaming (phi);
	}
      for (gsi = gsi_start_bb (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree vdef = gimple_vdef (stmt);
	  if (vdef && TREE_CODE (vdef) == SSA_NAME)
	    mark_virtual_operand_for_renaming (vdef);
	}
      delete_basic_block (bbs[i]);
    }
  free (bbs);

  set_immediate_dominator (CDI_DOMINATORS, dest,
			   recompute_dominator (CDI_DOMINATORS, dest));
}

/* Generates code for PARTITION.  */

static void
generate_code_for_partition (struct loop *loop,
			     partition_t partition, bool copy_p)
{
  switch (partition->kind)
    {
    case PKIND_MEMSET:
      generate_memset_builtin (loop, partition);
      /* If this is the last partition for which we generate code, we have
	 to destroy the loop.  */
      if (!copy_p)
	destroy_loop (loop);
      break;

    case PKIND_MEMCPY:
      generate_memcpy_builtin (loop, partition);
      /* If this is the last partition for which we generate code, we have
	 to destroy the loop.  */
      if (!copy_p)
	destroy_loop (loop);
      break;

    case PKIND_NORMAL:
      generate_loops_for_partition (loop, partition, copy_p);
      break;

    default:
      gcc_unreachable ();
    }
}


/* Returns true if the node V of RDG cannot be recomputed.  */

static bool
rdg_cannot_recompute_vertex_p (struct graph *rdg, int v)
{
  if (RDG_MEM_WRITE_STMT (rdg, v))
    return true;

  return false;
}

/* Returns true when the vertex V has already been generated in the
   current partition (V is in PROCESSED), or when V belongs to another
   partition and cannot be recomputed (V is not in REMAINING_STMTS).  */

static inline bool
already_processed_vertex_p (bitmap processed, int v)
{
  return (bitmap_bit_p (processed, v)
	  || !bitmap_bit_p (remaining_stmts, v));
}

/* Returns NULL when there is no anti-dependence among the successors
   of vertex V, otherwise returns the edge with the anti-dep.  */

static struct graph_edge *
has_anti_dependence (struct vertex *v)
{
  struct graph_edge *e;

  if (v->succ)
    for (e = v->succ; e; e = e->succ_next)
      if (RDGE_TYPE (e) == anti_dd)
	return e;

  return NULL;
}

/* Returns true when V has an anti-dependence edge among its successors.  */

static bool
predecessor_has_mem_write (struct graph *rdg, struct vertex *v)
{
  struct graph_edge *e;

  if (v->pred)
    for (e = v->pred; e; e = e->pred_next)
      if (bitmap_bit_p (upstream_mem_writes, e->src)
	  /* Don't consider flow channels: a write to memory followed
	     by a read from memory.  These channels allow the split of
	     the RDG in different partitions.  */
	  && !RDG_MEM_WRITE_STMT (rdg, e->src))
	return true;

  return false;
}

/* Initializes the upstream_mem_writes bitmap following the
   information from RDG.  */

static void
mark_nodes_having_upstream_mem_writes (struct graph *rdg)
{
  int v, x;
  bitmap seen = BITMAP_ALLOC (NULL);

  for (v = rdg->n_vertices - 1; v >= 0; v--)
    if (!bitmap_bit_p (seen, v))
      {
	unsigned i;
	VEC (int, heap) *nodes = VEC_alloc (int, heap, 3);

	graphds_dfs (rdg, &v, 1, &nodes, false, NULL);

	FOR_EACH_VEC_ELT (int, nodes, i, x)
	  {
	    if (!bitmap_set_bit (seen, x))
	      continue;

	    if (RDG_MEM_WRITE_STMT (rdg, x)
		|| predecessor_has_mem_write (rdg, &(rdg->vertices[x]))
		/* In anti dependences the read should occur before
		   the write, this is why both the read and the write
		   should be placed in the same partition.  */
		|| has_anti_dependence (&(rdg->vertices[x])))
	      {
		bitmap_set_bit (upstream_mem_writes, x);
	      }
	  }

	VEC_free (int, heap, nodes);
      }
}

/* Returns true when vertex u has a memory write node as a predecessor
   in RDG.  */

static bool
has_upstream_mem_writes (int u)
{
  return bitmap_bit_p (upstream_mem_writes, u);
}

static void rdg_flag_vertex_and_dependent (struct graph *, int, partition_t,
					   bitmap, bitmap);

/* Flag the uses of U stopping following the information from
   upstream_mem_writes.  */

static void
rdg_flag_uses (struct graph *rdg, int u, partition_t partition, bitmap loops,
	       bitmap processed)
{
  use_operand_p use_p;
  struct vertex *x = &(rdg->vertices[u]);
  gimple stmt = RDGV_STMT (x);
  struct graph_edge *anti_dep = has_anti_dependence (x);

  /* Keep in the same partition the destination of an antidependence,
     because this is a store to the exact same location.  Putting this
     in another partition is bad for cache locality.  */
  if (anti_dep)
    {
      int v = anti_dep->dest;

      if (!already_processed_vertex_p (processed, v))
	rdg_flag_vertex_and_dependent (rdg, v, partition, loops,
				       processed);
    }

  if (gimple_code (stmt) != GIMPLE_PHI)
    {
      if ((use_p = gimple_vuse_op (stmt)) != NULL_USE_OPERAND_P)
	{
	  tree use = USE_FROM_PTR (use_p);

	  if (TREE_CODE (use) == SSA_NAME)
	    {
	      gimple def_stmt = SSA_NAME_DEF_STMT (use);
	      int v = rdg_vertex_for_stmt (rdg, def_stmt);

	      if (v >= 0
		  && !already_processed_vertex_p (processed, v))
		rdg_flag_vertex_and_dependent (rdg, v, partition, loops,
					       processed);
	    }
	}
    }

  if (is_gimple_assign (stmt) && has_upstream_mem_writes (u))
    {
      tree op0 = gimple_assign_lhs (stmt);

      /* Scalar channels don't have enough space for transmitting data
	 between tasks, unless we add more storage by privatizing.  */
      if (is_gimple_reg (op0))
	{
	  use_operand_p use_p;
	  imm_use_iterator iter;

	  FOR_EACH_IMM_USE_FAST (use_p, iter, op0)
	    {
	      int v = rdg_vertex_for_stmt (rdg, USE_STMT (use_p));

	      if (!already_processed_vertex_p (processed, v))
		rdg_flag_vertex_and_dependent (rdg, v, partition, loops,
					       processed);
	    }
	}
    }
}

/* Flag V from RDG as part of PARTITION, and also flag its loop number
   in LOOPS.  */

static void
rdg_flag_vertex (struct graph *rdg, int v, partition_t partition, bitmap loops)
{
  struct loop *loop;

  if (!bitmap_set_bit (partition->stmts, v))
    return;

  loop = loop_containing_stmt (RDG_STMT (rdg, v));
  bitmap_set_bit (loops, loop->num);

  if (rdg_cannot_recompute_vertex_p (rdg, v))
    {
      partition->has_writes = true;
      bitmap_clear_bit (remaining_stmts, v);
    }
}

/* Flag in the bitmap PARTITION the vertex V and all its predecessors.
   Also flag their loop number in LOOPS.  */

static void
rdg_flag_vertex_and_dependent (struct graph *rdg, int v, partition_t partition,
			       bitmap loops, bitmap processed)
{
  unsigned i;
  VEC (int, heap) *nodes = VEC_alloc (int, heap, 3);
  int x;

  bitmap_set_bit (processed, v);
  rdg_flag_uses (rdg, v, partition, loops, processed);
  graphds_dfs (rdg, &v, 1, &nodes, false, remaining_stmts);
  rdg_flag_vertex (rdg, v, partition, loops);

  FOR_EACH_VEC_ELT (int, nodes, i, x)
    if (!already_processed_vertex_p (processed, x))
      rdg_flag_vertex_and_dependent (rdg, x, partition, loops, processed);

  VEC_free (int, heap, nodes);
}

/* Initialize CONDS with all the condition statements from the basic
   blocks of LOOP.  */

static void
collect_condition_stmts (struct loop *loop, VEC (gimple, heap) **conds)
{
  unsigned i;
  edge e;
  VEC (edge, heap) *exits = get_loop_exit_edges (loop);

  FOR_EACH_VEC_ELT (edge, exits, i, e)
    {
      gimple cond = last_stmt (e->src);

      if (cond)
	VEC_safe_push (gimple, heap, *conds, cond);
    }

  VEC_free (edge, heap, exits);
}

/* Add to PARTITION all the exit condition statements for LOOPS
   together with all their dependent statements determined from
   RDG.  */

static void
rdg_flag_loop_exits (struct graph *rdg, bitmap loops, partition_t partition,
		     bitmap processed)
{
  unsigned i;
  bitmap_iterator bi;
  VEC (gimple, heap) *conds = VEC_alloc (gimple, heap, 3);

  EXECUTE_IF_SET_IN_BITMAP (loops, 0, i, bi)
    collect_condition_stmts (get_loop (i), &conds);

  while (!VEC_empty (gimple, conds))
    {
      gimple cond = VEC_pop (gimple, conds);
      int v = rdg_vertex_for_stmt (rdg, cond);
      bitmap new_loops = BITMAP_ALLOC (NULL);

      if (!already_processed_vertex_p (processed, v))
	rdg_flag_vertex_and_dependent (rdg, v, partition, new_loops, processed);

      EXECUTE_IF_SET_IN_BITMAP (new_loops, 0, i, bi)
	if (bitmap_set_bit (loops, i))
	  collect_condition_stmts (get_loop (i), &conds);

      BITMAP_FREE (new_loops);
    }

  VEC_free (gimple, heap, conds);
}

/* Returns a bitmap in which all the statements needed for computing
   the strongly connected component C of the RDG are flagged, also
   including the loop exit conditions.  */

static partition_t
build_rdg_partition_for_component (struct graph *rdg, rdgc c)
{
  int i, v;
  partition_t partition = partition_alloc (NULL);
  bitmap loops = BITMAP_ALLOC (NULL);
  bitmap processed = BITMAP_ALLOC (NULL);

  FOR_EACH_VEC_ELT (int, c->vertices, i, v)
    if (!already_processed_vertex_p (processed, v))
      rdg_flag_vertex_and_dependent (rdg, v, partition, loops, processed);

  rdg_flag_loop_exits (rdg, loops, partition, processed);

  BITMAP_FREE (processed);
  BITMAP_FREE (loops);
  return partition;
}

/* Free memory for COMPONENTS.  */

static void
free_rdg_components (VEC (rdgc, heap) *components)
{
  int i;
  rdgc x;

  FOR_EACH_VEC_ELT (rdgc, components, i, x)
    {
      VEC_free (int, heap, x->vertices);
      free (x);
    }

  VEC_free (rdgc, heap, components);
}

/* Build the COMPONENTS vector with the strongly connected components
   of RDG in which the STARTING_VERTICES occur.  */

static void
rdg_build_components (struct graph *rdg, VEC (int, heap) *starting_vertices,
		      VEC (rdgc, heap) **components)
{
  int i, v;
  bitmap saved_components = BITMAP_ALLOC (NULL);
  int n_components = graphds_scc (rdg, NULL);
  VEC (int, heap) **all_components = XNEWVEC (VEC (int, heap) *, n_components);

  for (i = 0; i < n_components; i++)
    all_components[i] = VEC_alloc (int, heap, 3);

  for (i = 0; i < rdg->n_vertices; i++)
    VEC_safe_push (int, heap, all_components[rdg->vertices[i].component], i);

  FOR_EACH_VEC_ELT (int, starting_vertices, i, v)
    {
      int c = rdg->vertices[v].component;

      if (bitmap_set_bit (saved_components, c))
	{
	  rdgc x = XCNEW (struct rdg_component);
	  x->num = c;
	  x->vertices = all_components[c];

	  VEC_safe_push (rdgc, heap, *components, x);
	}
    }

  for (i = 0; i < n_components; i++)
    if (!bitmap_bit_p (saved_components, i))
      VEC_free (int, heap, all_components[i]);

  free (all_components);
  BITMAP_FREE (saved_components);
}

/* Classifies the builtin kind we can generate for PARTITION of RDG and LOOP.
   For the moment we detect only the memset zero pattern.  */

static void
classify_partition (loop_p loop, struct graph *rdg, partition_t partition)
{
  bitmap_iterator bi;
  unsigned i;
  tree nb_iter;
  data_reference_p single_load, single_store;

  partition->kind = PKIND_NORMAL;
  partition->main_dr = NULL;
  partition->secondary_dr = NULL;

  if (!flag_tree_loop_distribute_patterns)
    return;

  /* Perform general partition disqualification for builtins.  */
  nb_iter = number_of_exit_cond_executions (loop);
  if (!nb_iter || nb_iter == chrec_dont_know)
    return;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple stmt = RDG_STMT (rdg, i);

      if (gimple_has_volatile_ops (stmt))
	return;

      /* If the stmt has uses outside of the loop fail.
	 ???  If the stmt is generated in another partition that
	 is not created as builtin we can ignore this.  */
      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "not generating builtin, partition has "
		     "scalar uses outside of the loop\n");
	  return;
	}
    }

  /* Detect memset and memcpy.  */
  single_load = NULL;
  single_store = NULL;
  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple stmt = RDG_STMT (rdg, i);
      data_reference_p dr;
      unsigned j;

      if (gimple_code (stmt) == GIMPLE_PHI)
	continue;

      /* Any scalar stmts are ok.  */
      if (!gimple_vuse (stmt))
	continue;

      /* Otherwise just regular loads/stores.  */
      if (!gimple_assign_single_p (stmt))
	return;

      /* But exactly one store and/or load.  */
      for (j = 0;
	   VEC_iterate (data_reference_p, RDG_DATAREFS (rdg, i), j, dr); ++j)
	{
	  if (DR_IS_READ (dr))
	    {
	      if (single_load != NULL)
		return;
	      single_load = dr;
	    }
	  else
	    {
	      if (single_store != NULL)
		return;
	      single_store = dr;
	    }
	}
    }

  if (single_store && !single_load)
    {
      gimple stmt = DR_STMT (single_store);
      tree rhs = gimple_assign_rhs1 (stmt);
      if (!(integer_zerop (rhs)
	    || integer_all_onesp (rhs)
	    || real_zerop (rhs)
	    || (TREE_CODE (rhs) == CONSTRUCTOR
		&& !TREE_CLOBBER_P (rhs))
	    || (INTEGRAL_TYPE_P (TREE_TYPE (rhs))
		&& (TYPE_MODE (TREE_TYPE (gimple_assign_lhs (stmt)))
		    == TYPE_MODE (unsigned_char_type_node)))))
	return;
      if (TREE_CODE (rhs) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (rhs)
	  && flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (rhs))))
	return;
      if (!adjacent_dr_p (single_store))
	return;
      partition->kind = PKIND_MEMSET;
      partition->main_dr = single_store;
    }
  else if (single_store && single_load)
    {
      gimple store = DR_STMT (single_store);
      gimple load = DR_STMT (single_load);
      /* Direct aggregate copy or via an SSA name temporary.  */
      if (load != store
	  && gimple_assign_lhs (load) != gimple_assign_rhs1 (store))
	return;
      if (!adjacent_dr_p (single_store)
	  || !adjacent_dr_p (single_load)
	  || !operand_equal_p (DR_STEP (single_store),
			       DR_STEP (single_load), 0))
	return;
      partition->kind = PKIND_MEMCPY;
      partition->main_dr = single_store;
      partition->secondary_dr = single_load;
    }
}

/* For a data reference REF, return the declaration of its base
   address or NULL_TREE if the base is not determined.  */

static tree
ref_base_address (data_reference_p dr)
{
  tree base_address = DR_BASE_ADDRESS (dr);
  if (base_address
      && TREE_CODE (base_address) == ADDR_EXPR)
    return TREE_OPERAND (base_address, 0);

  return base_address;
}

/* Returns true when PARTITION1 and PARTITION2 have similar memory
   accesses in RDG.  */

static bool
similar_memory_accesses (struct graph *rdg, partition_t partition1,
			 partition_t partition2)
{
  unsigned i, j, k, l;
  bitmap_iterator bi, bj;
  data_reference_p ref1, ref2;

  /* First check whether in the intersection of the two partitions are
     any loads or stores.  Common loads are the situation that happens
     most often.  */
  EXECUTE_IF_AND_IN_BITMAP (partition1->stmts, partition2->stmts, 0, i, bi)
    if (RDG_MEM_WRITE_STMT (rdg, i)
	|| RDG_MEM_READS_STMT (rdg, i))
      return true;

  /* Then check all data-references against each other.  */
  EXECUTE_IF_SET_IN_BITMAP (partition1->stmts, 0, i, bi)
    if (RDG_MEM_WRITE_STMT (rdg, i)
	|| RDG_MEM_READS_STMT (rdg, i))
      EXECUTE_IF_SET_IN_BITMAP (partition2->stmts, 0, j, bj)
	if (RDG_MEM_WRITE_STMT (rdg, j)
	    || RDG_MEM_READS_STMT (rdg, j))
	  {
	    FOR_EACH_VEC_ELT (data_reference_p, RDG_DATAREFS (rdg, i), k, ref1)
	      {
		tree base1 = ref_base_address (ref1);
		if (base1)
		  FOR_EACH_VEC_ELT (data_reference_p,
				    RDG_DATAREFS (rdg, j), l, ref2)
		    if (base1 == ref_base_address (ref2))
		      return true;
	      }
	  }

  return false;
}

/* Aggregate several components into a useful partition that is
   registered in the PARTITIONS vector.  Partitions will be
   distributed in different loops.  */

static void
rdg_build_partitions (struct graph *rdg, VEC (rdgc, heap) *components,
		      VEC (int, heap) **other_stores,
		      VEC (partition_t, heap) **partitions, bitmap processed)
{
  int i;
  rdgc x;
  partition_t partition = partition_alloc (NULL);

  FOR_EACH_VEC_ELT (rdgc, components, i, x)
    {
      partition_t np;
      int v = VEC_index (int, x->vertices, 0);

      if (bitmap_bit_p (processed, v))
	continue;

      np = build_rdg_partition_for_component (rdg, x);
      bitmap_ior_into (partition->stmts, np->stmts);
      partition->has_writes = partition_has_writes (np);
      bitmap_ior_into (processed, np->stmts);
      partition_free (np);

      if (partition_has_writes (partition))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "ldist useful partition:\n");
	      dump_bitmap (dump_file, partition->stmts);
	    }

	  VEC_safe_push (partition_t, heap, *partitions, partition);
	  partition = partition_alloc (NULL);
	}
    }

  /* Add the nodes from the RDG that were not marked as processed, and
     that are used outside the current loop.  These are scalar
     computations that are not yet part of previous partitions.  */
  for (i = 0; i < rdg->n_vertices; i++)
    if (!bitmap_bit_p (processed, i)
	&& rdg_defs_used_in_other_loops_p (rdg, i))
      VEC_safe_push (int, heap, *other_stores, i);

  /* If there are still statements left in the OTHER_STORES array,
     create other components and partitions with these stores and
     their dependences.  */
  if (VEC_length (int, *other_stores) > 0)
    {
      VEC (rdgc, heap) *comps = VEC_alloc (rdgc, heap, 3);
      VEC (int, heap) *foo = VEC_alloc (int, heap, 3);

      rdg_build_components (rdg, *other_stores, &comps);
      rdg_build_partitions (rdg, comps, &foo, partitions, processed);

      VEC_free (int, heap, foo);
      free_rdg_components (comps);
    }

  /* If there is something left in the last partition, save it.  */
  if (bitmap_count_bits (partition->stmts) > 0)
    VEC_safe_push (partition_t, heap, *partitions, partition);
  else
    partition_free (partition);
}

/* Dump to FILE the PARTITIONS.  */

static void
dump_rdg_partitions (FILE *file, VEC (partition_t, heap) *partitions)
{
  int i;
  partition_t partition;

  FOR_EACH_VEC_ELT (partition_t, partitions, i, partition)
    debug_bitmap_file (file, partition->stmts);
}

/* Debug PARTITIONS.  */
extern void debug_rdg_partitions (VEC (partition_t, heap) *);

DEBUG_FUNCTION void
debug_rdg_partitions (VEC (partition_t, heap) *partitions)
{
  dump_rdg_partitions (stderr, partitions);
}

/* Returns the number of read and write operations in the RDG.  */

static int
number_of_rw_in_rdg (struct graph *rdg)
{
  int i, res = 0;

  for (i = 0; i < rdg->n_vertices; i++)
    {
      if (RDG_MEM_WRITE_STMT (rdg, i))
	++res;

      if (RDG_MEM_READS_STMT (rdg, i))
	++res;
    }

  return res;
}

/* Returns the number of read and write operations in a PARTITION of
   the RDG.  */

static int
number_of_rw_in_partition (struct graph *rdg, partition_t partition)
{
  int res = 0;
  unsigned i;
  bitmap_iterator ii;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, ii)
    {
      if (RDG_MEM_WRITE_STMT (rdg, i))
	++res;

      if (RDG_MEM_READS_STMT (rdg, i))
	++res;
    }

  return res;
}

/* Returns true when one of the PARTITIONS contains all the read or
   write operations of RDG.  */

static bool
partition_contains_all_rw (struct graph *rdg, VEC (partition_t, heap) *partitions)
{
  int i;
  partition_t partition;
  int nrw = number_of_rw_in_rdg (rdg);

  FOR_EACH_VEC_ELT (partition_t, partitions, i, partition)
    if (nrw == number_of_rw_in_partition (rdg, partition))
      return true;

  return false;
}

/* Generate code from STARTING_VERTICES in RDG.  Returns the number of
   distributed loops.  */

static int
ldist_gen (struct loop *loop, struct graph *rdg,
	   VEC (int, heap) *starting_vertices)
{
  int i, nbp;
  VEC (rdgc, heap) *components = VEC_alloc (rdgc, heap, 3);
  VEC (partition_t, heap) *partitions = VEC_alloc (partition_t, heap, 3);
  VEC (int, heap) *other_stores = VEC_alloc (int, heap, 3);
  partition_t partition;
  bitmap processed = BITMAP_ALLOC (NULL);
  bool any_builtin;

  remaining_stmts = BITMAP_ALLOC (NULL);
  upstream_mem_writes = BITMAP_ALLOC (NULL);

  for (i = 0; i < rdg->n_vertices; i++)
    {
      bitmap_set_bit (remaining_stmts, i);

      /* Save in OTHER_STORES all the memory writes that are not in
	 STARTING_VERTICES.  */
      if (RDG_MEM_WRITE_STMT (rdg, i))
	{
	  int v;
	  unsigned j;
	  bool found = false;

	  FOR_EACH_VEC_ELT (int, starting_vertices, j, v)
	    if (i == v)
	      {
		found = true;
		break;
	      }

	  if (!found)
	    VEC_safe_push (int, heap, other_stores, i);
	}
    }

  mark_nodes_having_upstream_mem_writes (rdg);
  rdg_build_components (rdg, starting_vertices, &components);
  rdg_build_partitions (rdg, components, &other_stores, &partitions,
			processed);
  BITMAP_FREE (processed);

  any_builtin = false;
  FOR_EACH_VEC_ELT (partition_t, partitions, i, partition)
    {
      classify_partition (loop, rdg, partition);
      any_builtin |= partition_builtin_p (partition);
    }

  /* If we are only distributing patterns fuse all partitions that
     were not properly classified as builtins.  Else fuse partitions
     with similar memory accesses.  */
  if (!flag_tree_loop_distribution)
    {
      partition_t into;
      /* If we did not detect any builtin simply bail out.  */
      if (!any_builtin)
	{
	  nbp = 0;
	  goto ldist_done;
	}
      /* Only fuse adjacent non-builtin partitions, see PR53616.
         ???  Use dependence information to improve partition ordering.  */
      i = 0;
      do
	{
	  for (; VEC_iterate (partition_t, partitions, i, into); ++i)
	    if (!partition_builtin_p (into))
	      break;
	  for (++i; VEC_iterate (partition_t, partitions, i, partition); ++i)
	    if (!partition_builtin_p (partition))
	      {
		bitmap_ior_into (into->stmts, partition->stmts);
		VEC_ordered_remove (partition_t, partitions, i);
		i--;
	      }
	    else
	      break;
	}
      while ((unsigned) i < VEC_length (partition_t, partitions));
    }
  else
    {
      partition_t into;
      int j;
      for (i = 0; VEC_iterate (partition_t, partitions, i, into); ++i)
	{
	  if (partition_builtin_p (into))
	    continue;
	  for (j = i + 1;
	       VEC_iterate (partition_t, partitions, j, partition); ++j)
	    {
	      if (!partition_builtin_p (partition)
		  /* ???  The following is horribly inefficient,
		     we are re-computing and analyzing data-references
		     of the stmts in the partitions all the time.  */
		  && similar_memory_accesses (rdg, into, partition))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "fusing partitions\n");
		      dump_bitmap (dump_file, into->stmts);
		      dump_bitmap (dump_file, partition->stmts);
		      fprintf (dump_file, "because they have similar "
			       "memory accesses\n");
		    }
		  bitmap_ior_into (into->stmts, partition->stmts);
		  VEC_ordered_remove (partition_t, partitions, j);
		  j--;
		}
	    }
	}
    }

  nbp = VEC_length (partition_t, partitions);
  if (nbp == 0
      || (nbp == 1
	  && !partition_builtin_p (VEC_index (partition_t, partitions, 0)))
      || (nbp > 1
	  && partition_contains_all_rw (rdg, partitions)))
    {
      nbp = 0;
      goto ldist_done;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg_partitions (dump_file, partitions);

  FOR_EACH_VEC_ELT (partition_t, partitions, i, partition)
    generate_code_for_partition (loop, partition, i < nbp - 1);

 ldist_done:

  BITMAP_FREE (remaining_stmts);
  BITMAP_FREE (upstream_mem_writes);

  FOR_EACH_VEC_ELT (partition_t, partitions, i, partition)
    partition_free (partition);

  VEC_free (int, heap, other_stores);
  VEC_free (partition_t, heap, partitions);
  free_rdg_components (components);
  return nbp;
}

/* Distributes the code from LOOP in such a way that producer
   statements are placed before consumer statements.  When STMTS is
   NULL, performs the maximal distribution, if STMTS is not NULL,
   tries to separate only these statements from the LOOP's body.
   Returns the number of distributed loops.  */

static int
distribute_loop (struct loop *loop, VEC (gimple, heap) *stmts)
{
  int res = 0;
  struct graph *rdg;
  gimple s;
  unsigned i;
  VEC (int, heap) *vertices;
  VEC (ddr_p, heap) *dependence_relations;
  VEC (data_reference_p, heap) *datarefs;
  VEC (loop_p, heap) *loop_nest;

  datarefs = VEC_alloc (data_reference_p, heap, 10);
  dependence_relations = VEC_alloc (ddr_p, heap, 100);
  loop_nest = VEC_alloc (loop_p, heap, 3);
  rdg = build_rdg (loop, &loop_nest, &dependence_relations, &datarefs);

  if (!rdg)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FIXME: Loop %d not distributed: failed to build the RDG.\n",
		 loop->num);

      free_dependence_relations (dependence_relations);
      free_data_refs (datarefs);
      VEC_free (loop_p, heap, loop_nest);
      return res;
    }

  vertices = VEC_alloc (int, heap, 3);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg (dump_file, rdg);

  FOR_EACH_VEC_ELT (gimple, stmts, i, s)
    {
      int v = rdg_vertex_for_stmt (rdg, s);

      if (v >= 0)
	{
	  VEC_safe_push (int, heap, vertices, v);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "ldist asked to generate code for vertex %d\n", v);
	}
    }

  res = ldist_gen (loop, rdg, vertices);
  VEC_free (int, heap, vertices);
  free_rdg (rdg);
  free_dependence_relations (dependence_relations);
  free_data_refs (datarefs);
  VEC_free (loop_p, heap, loop_nest);
  return res;
}

/* Distribute all loops in the current function.  */

static unsigned int
tree_loop_distribution (void)
{
  struct loop *loop;
  loop_iterator li;
  bool changed = false;
  basic_block bb;

  FOR_ALL_BB (bb)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_uid (gsi_stmt (gsi), -1);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_uid (gsi_stmt (gsi), -1);
    }

  /* We can at the moment only distribute non-nested loops, thus restrict
     walking to innermost loops.  */
  FOR_EACH_LOOP (li, loop, LI_ONLY_INNERMOST)
    {
      VEC (gimple, heap) *work_list = NULL;
      basic_block *bbs;
      int num = loop->num;
      int nb_generated_loops = 0;
      unsigned int i;

      /* If the loop doesn't have a single exit we will fail anyway,
	 so do that early.  */
      if (!single_exit (loop))
	continue;

      /* Only distribute loops with a header and latch for now.  */
      if (loop->num_nodes > 2)
	continue;

      /* Initialize the worklist with stmts we seed the partitions with.  */
      bbs = get_loop_body_in_dom_order (loop);
      for (i = 0; i < loop->num_nodes; ++i)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_bb (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      /* Only distribute stores for now.
	         ???  We should also try to distribute scalar reductions,
		 thus SSA defs that have scalar uses outside of the loop.  */
	      if (!gimple_assign_single_p (stmt)
		  || is_gimple_reg (gimple_assign_lhs (stmt)))
		continue;

	      VEC_safe_push (gimple, heap, work_list, stmt);
	    }
	}
      free (bbs);

      if (VEC_length (gimple, work_list) > 0)
	nb_generated_loops = distribute_loop (loop, work_list);

      if (nb_generated_loops > 0)
	changed = true;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (nb_generated_loops > 1)
	    fprintf (dump_file, "Loop %d distributed: split to %d loops.\n",
		     num, nb_generated_loops);
	  else
	    fprintf (dump_file, "Loop %d is the same.\n", num);
	}

      VEC_free (gimple, heap, work_list);
    }

  if (changed)
    {
      mark_virtual_operands_for_renaming (cfun);
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
    }

#ifdef ENABLE_CHECKING
  verify_loop_structure ();
#endif

  return 0;
}

static bool
gate_tree_loop_distribution (void)
{
  return flag_tree_loop_distribution
    || flag_tree_loop_distribute_patterns;
}

struct gimple_opt_pass pass_loop_distribution =
{
 {
  GIMPLE_PASS,
  "ldist",			/* name */
  gate_tree_loop_distribution,  /* gate */
  tree_loop_distribution,       /* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_LOOP_DISTRIBUTION,    /* tv_id */
  PROP_cfg | PROP_ssa,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_ggc_collect
  | TODO_verify_ssa             /* todo_flags_finish */
 }
};
