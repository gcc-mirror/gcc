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
   loop.  */

static bool
stmt_has_scalar_dependences_outside_loop (gimple stmt)
{
  tree name;

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
    case GIMPLE_ASSIGN:
      name = gimple_get_lhs (stmt);
      break;

    case GIMPLE_PHI:
      name = gimple_phi_result (stmt);
      break;

    default:
      return false;
    }

  return (name
	  && TREE_CODE (name) == SSA_NAME
	  && ssa_name_has_uses_outside_loop_p (name,
					       loop_containing_stmt (stmt)));
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

  if (MAY_HAVE_DEBUG_STMTS)
    for (x = 0, i = 0; i < loop->num_nodes; i++)
      {
	basic_block bb = bbs[i];

	for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  if (!bitmap_bit_p (partition, x++))
	    reset_debug_uses (gsi_stmt (bsi));

	for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple stmt = gsi_stmt (bsi);
	    if (gimple_code (stmt) != GIMPLE_LABEL
		&& !is_gimple_debug (stmt)
		&& !bitmap_bit_p (partition, x++))
	      reset_debug_uses (stmt);
	  }
      }

  for (x = 0, i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi);)
	if (!bitmap_bit_p (partition, x++))
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
	      && !bitmap_bit_p (partition, x++))
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
  return true;
}

/* Build the size argument for a memset call.  */

static inline tree
build_size_arg_loc (location_t loc, tree nb_iter, tree op,
		    gimple_seq *stmt_list)
{
  gimple_seq stmts;
  tree x = fold_build2_loc (loc, MULT_EXPR, size_type_node,
			    fold_convert_loc (loc, size_type_node, nb_iter),
			    fold_convert_loc (loc, size_type_node,
					      TYPE_SIZE_UNIT (TREE_TYPE (op))));
  x = force_gimple_operand (x, &stmts, true, NULL);
  gimple_seq_add_seq (stmt_list, stmts);

  return x;
}

/* Generate a call to memset.  Return true when the operation succeeded.  */

static void
generate_memset_zero (gimple stmt, tree op0, tree nb_iter,
		      gimple_stmt_iterator bsi)
{
  tree addr_base, nb_bytes;
  bool res = false;
  gimple_seq stmt_list = NULL, stmts;
  gimple fn_call;
  tree mem, fn;
  struct data_reference *dr = XCNEW (struct data_reference);
  location_t loc = gimple_location (stmt);

  DR_STMT (dr) = stmt;
  DR_REF (dr) = op0;
  res = dr_analyze_innermost (dr, loop_containing_stmt (stmt));
  gcc_assert (res && stride_of_unit_type_p (DR_STEP (dr), TREE_TYPE (op0)));

  nb_bytes = build_size_arg_loc (loc, nb_iter, op0, &stmt_list);
  addr_base = size_binop_loc (loc, PLUS_EXPR, DR_OFFSET (dr), DR_INIT (dr));
  addr_base = fold_convert_loc (loc, sizetype, addr_base);

  /* Test for a negative stride, iterating over every element.  */
  if (tree_int_cst_sgn (DR_STEP (dr)) == -1)
    {
      addr_base = size_binop_loc (loc, MINUS_EXPR, addr_base,
				  fold_convert_loc (loc, sizetype, nb_bytes));
      addr_base = size_binop_loc (loc, PLUS_EXPR, addr_base,
				  TYPE_SIZE_UNIT (TREE_TYPE (op0)));
    }

  addr_base = fold_build_pointer_plus_loc (loc,
					   DR_BASE_ADDRESS (dr), addr_base);
  mem = force_gimple_operand (addr_base, &stmts, true, NULL);
  gimple_seq_add_seq (&stmt_list, stmts);

  fn = build_fold_addr_expr (builtin_decl_implicit (BUILT_IN_MEMSET));
  fn_call = gimple_build_call (fn, 3, mem, integer_zero_node, nb_bytes);
  gimple_seq_add_stmt (&stmt_list, fn_call);
  gsi_insert_seq_after (&bsi, stmt_list, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "generated memset zero\n");

  free_data_ref (dr);
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

	  if (gimple_code (stmt) == GIMPLE_LABEL
	      || is_gimple_debug (stmt))
	    continue;

	  if (!bitmap_bit_p (partition, x++))
	    continue;

	  /* If the stmt has uses outside of the loop fail.  */
	  if (stmt_has_scalar_dependences_outside_loop (stmt))
	    goto end;

	  if (is_gimple_assign (stmt)
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

  if (!stmt_with_adjacent_zero_store_dr_p (write))
    goto end;

  /* The new statements will be placed before LOOP.  */
  bsi = gsi_last_bb (loop_preheader_edge (loop)->src);
  generate_memset_zero (write, gimple_assign_lhs (write), nb_iter, bsi);
  res = true;

  /* If this is the last partition for which we generate code, we have
     to destroy the loop.  */
  if (!copy_p)
    {
      unsigned nbbs = loop->num_nodes;
      edge exit = single_exit (loop);
      basic_block src = loop_preheader_edge (loop)->src, dest = exit->dest;
      redirect_edge_pred (exit, src);
      exit->flags &= ~(EDGE_TRUE_VALUE|EDGE_FALSE_VALUE);
      exit->flags |= EDGE_FALLTHRU;
      cancel_loop_tree (loop);
      rescan_loop_exit (exit, false, true);

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

static void rdg_flag_vertex_and_dependent (struct graph *, int, bitmap, bitmap,
					   bitmap, bool *);

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

  if (!bitmap_set_bit (partition, v))
    return;

  loop = loop_containing_stmt (RDG_STMT (rdg, v));
  bitmap_set_bit (loops, loop->num);

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

  FOR_EACH_VEC_ELT (int, nodes, i, x)
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
	if (bitmap_set_bit (loops, i))
	  collect_condition_stmts (get_loop (i), &conds);

      BITMAP_FREE (new_loops);
    }

  VEC_free (gimple, heap, conds);
}

/* Returns a bitmap in which all the statements needed for computing
   the strongly connected component C of the RDG are flagged, also
   including the loop exit conditions.  */

static bitmap
build_rdg_partition_for_component (struct graph *rdg, rdgc c,
				   bool *part_has_writes)
{
  int i, v;
  bitmap partition = BITMAP_ALLOC (NULL);
  bitmap loops = BITMAP_ALLOC (NULL);
  bitmap processed = BITMAP_ALLOC (NULL);

  FOR_EACH_VEC_ELT (int, c->vertices, i, v)
    if (!already_processed_vertex_p (processed, v))
      rdg_flag_vertex_and_dependent (rdg, v, partition, loops, processed,
				     part_has_writes);

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

/* Returns true when it is possible to generate a builtin pattern for
   the PARTITION of RDG.  For the moment we detect only the memset
   zero pattern.  */

static bool
can_generate_builtin (struct graph *rdg, bitmap partition)
{
  unsigned i;
  bitmap_iterator bi;
  int nb_reads = 0;
  int nb_writes = 0;
  int stores_zero = 0;

  EXECUTE_IF_SET_IN_BITMAP (partition, 0, i, bi)
    if (RDG_MEM_READS_STMT (rdg, i))
      nb_reads++;
    else if (RDG_MEM_WRITE_STMT (rdg, i))
      {
	nb_writes++;
	if (stmt_with_adjacent_zero_store_dr_p (RDG_STMT (rdg, i)))
	  stores_zero++;
      }

  return stores_zero == 1 && nb_writes == 1 && nb_reads == 0;
}

/* Returns true when PARTITION1 and PARTITION2 have similar memory
   accesses in RDG.  */

static bool
similar_memory_accesses (struct graph *rdg, bitmap partition1,
			 bitmap partition2)
{
  unsigned i, j;
  bitmap_iterator bi, bj;

  EXECUTE_IF_SET_IN_BITMAP (partition1, 0, i, bi)
    if (RDG_MEM_WRITE_STMT (rdg, i)
	|| RDG_MEM_READS_STMT (rdg, i))
      EXECUTE_IF_SET_IN_BITMAP (partition2, 0, j, bj)
	if (RDG_MEM_WRITE_STMT (rdg, j)
	    || RDG_MEM_READS_STMT (rdg, j))
	  if (rdg_has_similar_memory_accesses (rdg, i, j))
	    return true;

  return false;
}

/* Fuse all the partitions from PARTITIONS that contain similar memory
   references, i.e., we're taking care of cache locality.  This
   function does not fuse those partitions that contain patterns that
   can be code generated with builtins.  */

static void
fuse_partitions_with_similar_memory_accesses (struct graph *rdg,
					      VEC (bitmap, heap) **partitions)
{
  int p1, p2;
  bitmap partition1, partition2;

  FOR_EACH_VEC_ELT (bitmap, *partitions, p1, partition1)
    if (!can_generate_builtin (rdg, partition1))
      FOR_EACH_VEC_ELT (bitmap, *partitions, p2, partition2)
	if (p1 != p2
	    && !can_generate_builtin (rdg, partition2)
	    && similar_memory_accesses (rdg, partition1, partition2))
	  {
	    bitmap_ior_into (partition1, partition2);
	    VEC_ordered_remove (bitmap, *partitions, p2);
	    p2--;
	  }
}

/* Returns true when STMT will be code generated in a partition of RDG
   different than PART and that will not be code generated as a
   builtin.  */

static bool
stmt_generated_in_another_partition (struct graph *rdg, gimple stmt, int part,
				     VEC (bitmap, heap) *partitions)
{
  int p;
  bitmap pp;
  unsigned i;
  bitmap_iterator bi;

  FOR_EACH_VEC_ELT (bitmap, partitions, p, pp)
    if (p != part
	&& !can_generate_builtin (rdg, pp))
      EXECUTE_IF_SET_IN_BITMAP (pp, 0, i, bi)
	if (stmt == RDG_STMT (rdg, i))
	  return true;

  return false;
}

/* For each partition in PARTITIONS that will be code generated using
   a builtin, add its scalar computations used after the loop to
   PARTITION.  */

static void
add_scalar_computations_to_partition (struct graph *rdg,
				      VEC (bitmap, heap) *partitions,
				      bitmap partition)
{
  int p;
  bitmap pp;
  unsigned i;
  bitmap_iterator bi;
  bitmap l = BITMAP_ALLOC (NULL);
  bitmap pr = BITMAP_ALLOC (NULL);
  bool f = false;

  FOR_EACH_VEC_ELT (bitmap, partitions, p, pp)
    if (can_generate_builtin (rdg, pp))
      EXECUTE_IF_SET_IN_BITMAP (pp, 0, i, bi)
	if (stmt_has_scalar_dependences_outside_loop (RDG_STMT (rdg, i))
	    && !stmt_generated_in_another_partition (rdg, RDG_STMT (rdg, i), p,
						     partitions))
	  rdg_flag_vertex_and_dependent (rdg, i, partition, l, pr, &f);

  rdg_flag_loop_exits (rdg, l, partition, pr, &f);

  BITMAP_FREE (pr);
  BITMAP_FREE (l);
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

  FOR_EACH_VEC_ELT (rdgc, components, i, x)
    {
      bitmap np;
      bool part_has_writes = false;
      int v = VEC_index (int, x->vertices, 0);

      if (bitmap_bit_p (processed, v))
	continue;

      np = build_rdg_partition_for_component (rdg, x, &part_has_writes);
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

  add_scalar_computations_to_partition (rdg, *partitions, partition);

  /* If there is something left in the last partition, save it.  */
  if (bitmap_count_bits (partition) > 0)
    VEC_safe_push (bitmap, heap, *partitions, partition);
  else
    BITMAP_FREE (partition);

  fuse_partitions_with_similar_memory_accesses (rdg, partitions);
}

/* Dump to FILE the PARTITIONS.  */

static void
dump_rdg_partitions (FILE *file, VEC (bitmap, heap) *partitions)
{
  int i;
  bitmap partition;

  FOR_EACH_VEC_ELT (bitmap, partitions, i, partition)
    debug_bitmap_file (file, partition);
}

/* Debug PARTITIONS.  */
extern void debug_rdg_partitions (VEC (bitmap, heap) *);

DEBUG_FUNCTION void
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

  FOR_EACH_VEC_ELT (bitmap, partitions, i, partition)
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
  nbp = VEC_length (bitmap, partitions);

  if (nbp <= 1
      || partition_contains_all_rw (rdg, partitions))
    goto ldist_done;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg_partitions (dump_file, partitions);

  FOR_EACH_VEC_ELT (bitmap, partitions, i, partition)
    if (!generate_code_for_partition (loop, partition, i < nbp - 1))
      goto ldist_done;

  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  mark_sym_for_renaming (gimple_vop (cfun));
  update_ssa (TODO_update_ssa_only_virtuals);

 ldist_done:

  BITMAP_FREE (remaining_stmts);
  BITMAP_FREE (upstream_mem_writes);

  FOR_EACH_VEC_ELT (bitmap, partitions, i, partition)
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
  VEC (ddr_p, heap) *dependence_relations;
  VEC (data_reference_p, heap) *datarefs;
  VEC (loop_p, heap) *loop_nest;

  if (loop->num_nodes > 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FIXME: Loop %d not distributed: it has more than two basic blocks.\n",
		 loop->num);

      return res;
    }

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
  int nb_generated_loops = 0;

  FOR_EACH_LOOP (li, loop, 0)
    {
      VEC (gimple, heap) *work_list = NULL;
      int num = loop->num;

      /* If the loop doesn't have a single exit we will fail anyway,
	 so do that early.  */
      if (!single_exit (loop))
	continue;

      /* If both flag_tree_loop_distribute_patterns and
	 flag_tree_loop_distribution are set, then only
	 distribute_patterns is executed.  */
      if (flag_tree_loop_distribute_patterns)
	{
	  /* With the following working list, we're asking
	     distribute_loop to separate from the rest of the loop the
	     stores of the form "A[i] = 0".  */
	  stores_zero_from_loop (loop, &work_list);

	  /* Do nothing if there are no patterns to be distributed.  */
	  if (VEC_length (gimple, work_list) > 0)
	    nb_generated_loops = distribute_loop (loop, work_list);
	}
      else if (flag_tree_loop_distribution)
	{
	  /* With the following working list, we're asking
	     distribute_loop to separate the stores of the loop: when
	     dependences allow, it will end on having one store per
	     loop.  */
	  stores_from_loop (loop, &work_list);

	  /* A simple heuristic for cache locality is to not split
	     stores to the same array.  Without this call, an unrolled
	     loop would be split into as many loops as unroll factor,
	     each loop storing in the same array.  */
	  remove_similar_memory_refs (&work_list);

	  nb_generated_loops = distribute_loop (loop, work_list);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (nb_generated_loops > 1)
	    fprintf (dump_file, "Loop %d distributed: split to %d loops.\n",
		     num, nb_generated_loops);
	  else
	    fprintf (dump_file, "Loop %d is the same.\n", num);
	}

      verify_loop_structure ();

      VEC_free (gimple, heap, work_list);
    }

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
