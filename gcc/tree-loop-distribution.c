/* Loop distribution.
   Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"

#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "expr.h"
#include "optabs.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "lambda.h"
#include "langhooks.h"
#include "tree-vectorizer.h"

/* If bit I is not set, it means that this node represents an
   operation that has already been performed, and that should not be
   performed again.  This is the subgraph of remaining important
   computations that is passed to the DFS algorithm for avoiding to
   include several times the same stores in different loops.  */
static bitmap remaining_stmts;

/* A node of the RDG is marked in this bitmap when it has as a
   predecessor a node that writes to memory.  */
static bitmap upstream_mem_writes;

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

  if (!single_exit (loop))
    return NULL;

  initialize_original_copy_tables ();
  res = slpeel_tree_duplicate_loop_to_edge_cfg (loop, preheader);
  free_original_copy_tables ();

  if (!res)
    return NULL;

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
   basic blocks of a loop are taken in dom order.  Returns true when
   the code gen succeeded. */

static bool
generate_loops_for_partition (struct loop *loop, bitmap partition, bool copy_p)
{
  unsigned i, x;
  gimple_stmt_iterator bsi;
  basic_block *bbs;

  if (copy_p)
    {
      loop = copy_loop_before (loop);
      create_preheader (loop, CP_SIMPLE_PREHEADERS);
      create_bb_after_loop (loop);
    }

  if (loop == NULL)
    return false;

  /* Remove stmts not in the PARTITION bitmap.  The order in which we
     visit the phi nodes and the statements is exactly as in
     stmts_from_loop.  */
  bbs = get_loop_body_in_dom_order (loop);

  for (x = 0, i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi);)
	if (!bitmap_bit_p (partition, x++))
	  remove_phi_node (&bsi, true);
	else
	  gsi_next (&bsi);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi);)
	if (gimple_code (gsi_stmt (bsi)) != GIMPLE_LABEL
	    && !bitmap_bit_p (partition, x++))
	  gsi_remove (&bsi, false);
	else
	  gsi_next (&bsi);

	mark_virtual_ops_in_bb (bb);
    }

  free (bbs);
  return true;
}

/* Build the size argument for a memset call.  */

static inline tree
build_size_arg_loc (location_t loc, tree nb_iter, tree op,
		    gimple_seq *stmt_list)
{
  gimple_seq stmts;
  tree x;

  x = fold_build2_loc (loc, MULT_EXPR, size_type_node,
		       fold_convert_loc (loc, size_type_node, nb_iter),
		       fold_convert_loc (loc, size_type_node,
					 TYPE_SIZE_UNIT (TREE_TYPE (op))));
  x = force_gimple_operand (x, &stmts, true, NULL);
  gimple_seq_add_seq (stmt_list, stmts);

  return x;
}

/* Generate a call to memset.  Return true when the operation succeeded.  */

static bool
generate_memset_zero (gimple stmt, tree op0, tree nb_iter,
		      gimple_stmt_iterator bsi)
{
  tree addr_base, nb_bytes;
  bool res = false;
  gimple_seq stmt_list = NULL, stmts;
  gimple fn_call;
  tree mem, fn;
  gimple_stmt_iterator i;
  struct data_reference *dr = XCNEW (struct data_reference);
  location_t loc = gimple_location (stmt);

  DR_STMT (dr) = stmt;
  DR_REF (dr) = op0;
  if (!dr_analyze_innermost (dr))
    goto end;

  /* Test for a positive stride, iterating over every element.  */
  if (integer_zerop (size_binop (MINUS_EXPR,
				 fold_convert (sizetype, DR_STEP (dr)),
				 TYPE_SIZE_UNIT (TREE_TYPE (op0)))))
    {
      addr_base = fold_convert_loc (loc, sizetype,
				    size_binop_loc (loc, PLUS_EXPR,
						    DR_OFFSET (dr),
						    DR_INIT (dr)));
      addr_base = fold_build2_loc (loc, POINTER_PLUS_EXPR,
				   TREE_TYPE (DR_BASE_ADDRESS (dr)),
				   DR_BASE_ADDRESS (dr), addr_base);

      nb_bytes = build_size_arg_loc (loc, nb_iter, op0, &stmt_list);
    }

  /* Test for a negative stride, iterating over every element.  */
  else if (integer_zerop (size_binop (PLUS_EXPR,
				      TYPE_SIZE_UNIT (TREE_TYPE (op0)),
				      fold_convert (sizetype, DR_STEP (dr)))))
    {
      nb_bytes = build_size_arg_loc (loc, nb_iter, op0, &stmt_list);

      addr_base = size_binop_loc (loc, PLUS_EXPR, DR_OFFSET (dr), DR_INIT (dr));
      addr_base = fold_convert_loc (loc, sizetype, addr_base);
      addr_base = size_binop_loc (loc, MINUS_EXPR, addr_base,
				  fold_convert_loc (loc, sizetype, nb_bytes));
      addr_base = size_binop_loc (loc, PLUS_EXPR, addr_base,
				  TYPE_SIZE_UNIT (TREE_TYPE (op0)));
      addr_base = fold_build2_loc (loc, POINTER_PLUS_EXPR,
				   TREE_TYPE (DR_BASE_ADDRESS (dr)),
				   DR_BASE_ADDRESS (dr), addr_base);
    }
  else
    goto end;

  mem = force_gimple_operand (addr_base, &stmts, true, NULL);
  gimple_seq_add_seq (&stmt_list, stmts);

  fn = build_fold_addr_expr (implicit_built_in_decls [BUILT_IN_MEMSET]);
  fn_call = gimple_build_call (fn, 3, mem, integer_zero_node, nb_bytes);
  gimple_seq_add_stmt (&stmt_list, fn_call);

  for (i = gsi_start (stmt_list); !gsi_end_p (i); gsi_next (&i))
    {
      gimple s = gsi_stmt (i);
      update_stmt_if_modified (s);
    }

  gsi_insert_seq_after (&bsi, stmt_list, GSI_CONTINUE_LINKING);
  res = true;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "generated memset zero\n");

 end:
  free_data_ref (dr);
  return res;
}

/* Propagate phis in BB b to their uses and remove them.  */

static void
prop_phis (basic_block b)
{
  gimple_stmt_iterator psi;
  gimple_seq phis = phi_nodes (b);

  for (psi = gsi_start (phis); !gsi_end_p (psi); )
    {
      gimple phi = gsi_stmt (psi);
      tree def = gimple_phi_result (phi), use = gimple_phi_arg_def (phi, 0);

      gcc_assert (gimple_phi_num_args (phi) == 1);

      if (!is_gimple_reg (def))
	{
	  imm_use_iterator iter;
	  use_operand_p use_p;
	  gimple stmt;

	  FOR_EACH_IMM_USE_STMT (stmt, iter, def)
	    FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	      SET_USE (use_p, use);
	}
      else
	replace_uses_by (def, use);

      remove_phi_node (&psi, true);
    }
}

/* Tries to generate a builtin function for the instructions of LOOP
   pointed to by the bits set in PARTITION.  Returns true when the
   operation succeeded.  */

static bool
generate_builtin (struct loop *loop, bitmap partition, bool copy_p)
{
  bool res = false;
  unsigned i, x = 0;
  basic_block *bbs;
  gimple write = NULL;
  tree op0, op1;
  gimple_stmt_iterator bsi;
  tree nb_iter = number_of_exit_cond_executions (loop);

  if (!nb_iter || nb_iter == chrec_dont_know)
    return false;

  bbs = get_loop_body_in_dom_order (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	x++;

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);

	  if (bitmap_bit_p (partition, x++)
	      && is_gimple_assign (stmt)
	      && !is_gimple_reg (gimple_assign_lhs (stmt)))
	    {
	      /* Don't generate the builtins when there are more than
		 one memory write.  */
	      if (write != NULL)
		goto end;

	      write = stmt;
	      if (bb == loop->latch)
		nb_iter = number_of_latch_executions (loop);
	    }
	}
    }

  if (!write)
    goto end;

  op0 = gimple_assign_lhs (write);
  op1 = gimple_assign_rhs1 (write);

  if (!(TREE_CODE (op0) == ARRAY_REF
	|| TREE_CODE (op0) == INDIRECT_REF))
    goto end;

  /* The new statements will be placed before LOOP.  */
  bsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  if (gimple_assign_rhs_code (write) == INTEGER_CST
      && (integer_zerop (op1) || real_zerop (op1)))
    res = generate_memset_zero (write, op0, nb_iter, bsi);

  /* If this is the last partition for which we generate code, we have
     to destroy the loop.  */
  if (res && !copy_p)
    {
      unsigned nbbs = loop->num_nodes;
      basic_block src = loop_preheader_edge (loop)->src;
      basic_block dest = single_exit (loop)->dest;
      prop_phis (dest);
      make_edge (src, dest, EDGE_FALLTHRU);
      cancel_loop_tree (loop);

      for (i = 0; i < nbbs; i++)
	delete_basic_block (bbs[i]);

      set_immediate_dominator (CDI_DOMINATORS, dest,
			       recompute_dominator (CDI_DOMINATORS, dest));
    }

 end:
  free (bbs);
  return res;
}

/* Generates code for PARTITION.  For simple loops, this function can
   generate a built-in.  */

static bool
generate_code_for_partition (struct loop *loop, bitmap partition, bool copy_p)
{
  if (generate_builtin (loop, partition, copy_p))
    return true;

  return generate_loops_for_partition (loop, partition, copy_p);
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

	for (i = 0; VEC_iterate (int, nodes, i, x); i++)
	  {
	    if (bitmap_bit_p (seen, x))
	      continue;

	    bitmap_set_bit (seen, x);

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

static void rdg_flag_vertex_and_dependent (struct graph *, int, bitmap, bitmap,
					   bitmap, bool *);

/* Flag all the uses of U.  */

static void
rdg_flag_all_uses (struct graph *rdg, int u, bitmap partition, bitmap loops,
		   bitmap processed, bool *part_has_writes)
{
  struct graph_edge *e;

  for (e = rdg->vertices[u].succ; e; e = e->succ_next)
    if (!bitmap_bit_p (processed, e->dest))
      {
	rdg_flag_vertex_and_dependent (rdg, e->dest, partition, loops,
				       processed, part_has_writes);
	rdg_flag_all_uses (rdg, e->dest, partition, loops, processed,
			   part_has_writes);
      }
}

/* Flag the uses of U stopping following the information from
   upstream_mem_writes.  */

static void
rdg_flag_uses (struct graph *rdg, int u, bitmap partition, bitmap loops,
	       bitmap processed, bool *part_has_writes)
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
				       processed, part_has_writes);
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
					       processed, part_has_writes);
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
					       processed, part_has_writes);
	    }
	}
    }
}

/* Flag V from RDG as part of PARTITION, and also flag its loop number
   in LOOPS.  */

static void
rdg_flag_vertex (struct graph *rdg, int v, bitmap partition, bitmap loops,
		 bool *part_has_writes)
{
  struct loop *loop;

  if (bitmap_bit_p (partition, v))
    return;

  loop = loop_containing_stmt (RDG_STMT (rdg, v));
  bitmap_set_bit (loops, loop->num);
  bitmap_set_bit (partition, v);

  if (rdg_cannot_recompute_vertex_p (rdg, v))
    {
      *part_has_writes = true;
      bitmap_clear_bit (remaining_stmts, v);
    }
}

/* Flag in the bitmap PARTITION the vertex V and all its predecessors.
   Also flag their loop number in LOOPS.  */

static void
rdg_flag_vertex_and_dependent (struct graph *rdg, int v, bitmap partition,
			       bitmap loops, bitmap processed,
			       bool *part_has_writes)
{
  unsigned i;
  VEC (int, heap) *nodes = VEC_alloc (int, heap, 3);
  int x;

  bitmap_set_bit (processed, v);
  rdg_flag_uses (rdg, v, partition, loops, processed, part_has_writes);
  graphds_dfs (rdg, &v, 1, &nodes, false, remaining_stmts);
  rdg_flag_vertex (rdg, v, partition, loops, part_has_writes);

  for (i = 0; VEC_iterate (int, nodes, i, x); i++)
    if (!already_processed_vertex_p (processed, x))
      rdg_flag_vertex_and_dependent (rdg, x, partition, loops, processed,
				     part_has_writes);

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

  for (i = 0; VEC_iterate (edge, exits, i, e); i++)
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
rdg_flag_loop_exits (struct graph *rdg, bitmap loops, bitmap partition,
		     bitmap processed, bool *part_has_writes)
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
	rdg_flag_vertex_and_dependent (rdg, v, partition, new_loops, processed,
				       part_has_writes);

      EXECUTE_IF_SET_IN_BITMAP (new_loops, 0, i, bi)
	if (!bitmap_bit_p (loops, i))
	  {
	    bitmap_set_bit (loops, i);
	    collect_condition_stmts (get_loop (i), &conds);
	  }

      BITMAP_FREE (new_loops);
    }
}

/* Flag all the nodes of RDG containing memory accesses that could
   potentially belong to arrays already accessed in the current
   PARTITION.  */

static void
rdg_flag_similar_memory_accesses (struct graph *rdg, bitmap partition,
				  bitmap loops, bitmap processed,
				  VEC (int, heap) **other_stores)
{
  bool foo;
  unsigned i, n;
  int j, k, kk;
  bitmap_iterator ii;
  struct graph_edge *e;

  EXECUTE_IF_SET_IN_BITMAP (partition, 0, i, ii)
    if (RDG_MEM_WRITE_STMT (rdg, i)
	|| RDG_MEM_READS_STMT (rdg, i))
      {
	for (j = 0; j < rdg->n_vertices; j++)
	  if (!bitmap_bit_p (processed, j)
	      && (RDG_MEM_WRITE_STMT (rdg, j)
		  || RDG_MEM_READS_STMT (rdg, j))
	      && rdg_has_similar_memory_accesses (rdg, i, j))
	    {
	      /* Flag first the node J itself, and all the nodes that
		 are needed to compute J.  */
	      rdg_flag_vertex_and_dependent (rdg, j, partition, loops,
					     processed, &foo);

	      /* When J is a read, we want to coalesce in the same
		 PARTITION all the nodes that are using J: this is
		 needed for better cache locality.  */
	      rdg_flag_all_uses (rdg, j, partition, loops, processed, &foo);

	      /* Remove from OTHER_STORES the vertex that we flagged.  */
	      if (RDG_MEM_WRITE_STMT (rdg, j))
		for (k = 0; VEC_iterate (int, *other_stores, k, kk); k++)
		  if (kk == j)
		    {
		      VEC_unordered_remove (int, *other_stores, k);
		      break;
		    }
	    }

	/* If the node I has two uses, then keep these together in the
	   same PARTITION.  */
	for (n = 0, e = rdg->vertices[i].succ; e; e = e->succ_next, n++);

	if (n > 1)
	  rdg_flag_all_uses (rdg, i, partition, loops, processed, &foo);
      }
}

/* Returns a bitmap in which all the statements needed for computing
   the strongly connected component C of the RDG are flagged, also
   including the loop exit conditions.  */

static bitmap
build_rdg_partition_for_component (struct graph *rdg, rdgc c,
				   bool *part_has_writes,
				   VEC (int, heap) **other_stores)
{
  int i, v;
  bitmap partition = BITMAP_ALLOC (NULL);
  bitmap loops = BITMAP_ALLOC (NULL);
  bitmap processed = BITMAP_ALLOC (NULL);

  for (i = 0; VEC_iterate (int, c->vertices, i, v); i++)
    if (!already_processed_vertex_p (processed, v))
      rdg_flag_vertex_and_dependent (rdg, v, partition, loops, processed,
				     part_has_writes);

  /* Also iterate on the array of stores not in the starting vertices,
     and determine those vertices that have some memory affinity with
     the current nodes in the component: these are stores to the same
     arrays, i.e. we're taking care of cache locality.  */
  rdg_flag_similar_memory_accesses (rdg, partition, loops, processed,
				    other_stores);

  rdg_flag_loop_exits (rdg, loops, partition, processed, part_has_writes);

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

  for (i = 0; VEC_iterate (rdgc, components, i, x); i++)
    {
      VEC_free (int, heap, x->vertices);
      free (x);
    }
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

  for (i = 0; VEC_iterate (int, starting_vertices, i, v); i++)
    {
      int c = rdg->vertices[v].component;

      if (!bitmap_bit_p (saved_components, c))
	{
	  rdgc x = XCNEW (struct rdg_component);
	  x->num = c;
	  x->vertices = all_components[c];

	  VEC_safe_push (rdgc, heap, *components, x);
	  bitmap_set_bit (saved_components, c);
	}
    }

  for (i = 0; i < n_components; i++)
    if (!bitmap_bit_p (saved_components, i))
      VEC_free (int, heap, all_components[i]);

  free (all_components);
  BITMAP_FREE (saved_components);
}

/* Aggregate several components into a useful partition that is
   registered in the PARTITIONS vector.  Partitions will be
   distributed in different loops.  */

static void
rdg_build_partitions (struct graph *rdg, VEC (rdgc, heap) *components,
		      VEC (int, heap) **other_stores,
		      VEC (bitmap, heap) **partitions, bitmap processed)
{
  int i;
  rdgc x;
  bitmap partition = BITMAP_ALLOC (NULL);

  for (i = 0; VEC_iterate (rdgc, components, i, x); i++)
    {
      bitmap np;
      bool part_has_writes = false;
      int v = VEC_index (int, x->vertices, 0);

      if (bitmap_bit_p (processed, v))
	continue;

      np = build_rdg_partition_for_component (rdg, x, &part_has_writes,
					      other_stores);
      bitmap_ior_into (partition, np);
      bitmap_ior_into (processed, np);
      BITMAP_FREE (np);

      if (part_has_writes)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "ldist useful partition:\n");
	      dump_bitmap (dump_file, partition);
	    }

	  VEC_safe_push (bitmap, heap, *partitions, partition);
	  partition = BITMAP_ALLOC (NULL);
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
  if (bitmap_count_bits (partition) > 0)
    VEC_safe_push (bitmap, heap, *partitions, partition);
  else
    BITMAP_FREE (partition);
}

/* Dump to FILE the PARTITIONS.  */

static void
dump_rdg_partitions (FILE *file, VEC (bitmap, heap) *partitions)
{
  int i;
  bitmap partition;

  for (i = 0; VEC_iterate (bitmap, partitions, i, partition); i++)
    debug_bitmap_file (file, partition);
}

/* Debug PARTITIONS.  */
extern void debug_rdg_partitions (VEC (bitmap, heap) *);

void
debug_rdg_partitions (VEC (bitmap, heap) *partitions)
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
number_of_rw_in_partition (struct graph *rdg, bitmap partition)
{
  int res = 0;
  unsigned i;
  bitmap_iterator ii;

  EXECUTE_IF_SET_IN_BITMAP (partition, 0, i, ii)
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
partition_contains_all_rw (struct graph *rdg, VEC (bitmap, heap) *partitions)
{
  int i;
  bitmap partition;
  int nrw = number_of_rw_in_rdg (rdg);

  for (i = 0; VEC_iterate (bitmap, partitions, i, partition); i++)
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
  VEC (bitmap, heap) *partitions = VEC_alloc (bitmap, heap, 3);
  VEC (int, heap) *other_stores = VEC_alloc (int, heap, 3);
  bitmap partition, processed = BITMAP_ALLOC (NULL);

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

	  for (j = 0; VEC_iterate (int, starting_vertices, j, v); j++)
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
  nbp = VEC_length (bitmap, partitions);

  if (nbp <= 1
      || partition_contains_all_rw (rdg, partitions))
    goto ldist_done;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg_partitions (dump_file, partitions);

  for (i = 0; VEC_iterate (bitmap, partitions, i, partition); i++)
    if (!generate_code_for_partition (loop, partition, i < nbp - 1))
      goto ldist_done;

  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  update_ssa (TODO_update_ssa_only_virtuals | TODO_update_ssa);

 ldist_done:

  BITMAP_FREE (remaining_stmts);
  BITMAP_FREE (upstream_mem_writes);

  for (i = 0; VEC_iterate (bitmap, partitions, i, partition); i++)
    BITMAP_FREE (partition);

  VEC_free (int, heap, other_stores);
  VEC_free (bitmap, heap, partitions);
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

  if (loop->num_nodes > 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FIXME: Loop %d not distributed: it has more than two basic blocks.\n",
		 loop->num);

      return res;
    }

  rdg = build_rdg (loop);

  if (!rdg)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FIXME: Loop %d not distributed: failed to build the RDG.\n",
		 loop->num);

      return res;
    }

  vertices = VEC_alloc (int, heap, 3);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg (dump_file, rdg);

  for (i = 0; VEC_iterate (gimple, stmts, i, s); i++)
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

  return res;
}

/* Distribute all loops in the current function.  */

static unsigned int
tree_loop_distribution (void)
{
  struct loop *loop;
  loop_iterator li;
  int nb_generated_loops = 0;

  FOR_EACH_LOOP (li, loop, 0)
    {
      VEC (gimple, heap) *work_list = VEC_alloc (gimple, heap, 3);

      /* With the following working list, we're asking distribute_loop
	 to separate the stores of the loop: when dependences allow,
	 it will end on having one store per loop.  */
      stores_from_loop (loop, &work_list);

      /* A simple heuristic for cache locality is to not split stores
	 to the same array.  Without this call, an unrolled loop would
	 be split into as many loops as unroll factor, each loop
	 storing in the same array.  */
      remove_similar_memory_refs (&work_list);

      nb_generated_loops = distribute_loop (loop, work_list);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (nb_generated_loops > 1)
	    fprintf (dump_file, "Loop %d distributed: split to %d loops.\n",
		     loop->num, nb_generated_loops);
	  else
	    fprintf (dump_file, "Loop %d is the same.\n", loop->num);
	}

      verify_loop_structure ();

      VEC_free (gimple, heap, work_list);
    }

  return 0;
}

static bool
gate_tree_loop_distribution (void)
{
  return flag_tree_loop_distribution != 0;
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
  TODO_dump_func | TODO_verify_loops            /* todo_flags_finish */
 }
};
