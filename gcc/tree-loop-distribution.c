/* Loop distribution.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "stor-layout.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "gimple-pretty-print.h"
#include "tree-vectorizer.h"


/* A Reduced Dependence Graph (RDG) vertex representing a statement.  */
typedef struct rdg_vertex
{
  /* The statement represented by this vertex.  */
  gimple stmt;

  /* Vector of data-references in this statement.  */
  vec<data_reference_p> datarefs;

  /* True when the statement contains a write to memory.  */
  bool has_mem_write;

  /* True when the statement contains a read from memory.  */
  bool has_mem_reads;
} *rdg_vertex_p;

#define RDGV_STMT(V)     ((struct rdg_vertex *) ((V)->data))->stmt
#define RDGV_DATAREFS(V) ((struct rdg_vertex *) ((V)->data))->datarefs
#define RDGV_HAS_MEM_WRITE(V) ((struct rdg_vertex *) ((V)->data))->has_mem_write
#define RDGV_HAS_MEM_READS(V) ((struct rdg_vertex *) ((V)->data))->has_mem_reads
#define RDG_STMT(RDG, I) RDGV_STMT (&(RDG->vertices[I]))
#define RDG_DATAREFS(RDG, I) RDGV_DATAREFS (&(RDG->vertices[I]))
#define RDG_MEM_WRITE_STMT(RDG, I) RDGV_HAS_MEM_WRITE (&(RDG->vertices[I]))
#define RDG_MEM_READS_STMT(RDG, I) RDGV_HAS_MEM_READS (&(RDG->vertices[I]))

/* Data dependence type.  */

enum rdg_dep_type
{
  /* Read After Write (RAW).  */
  flow_dd = 'f',

  /* Control dependence (execute conditional on).  */
  control_dd = 'c'
};

/* Dependence information attached to an edge of the RDG.  */

typedef struct rdg_edge
{
  /* Type of the dependence.  */
  enum rdg_dep_type type;
} *rdg_edge_p;

#define RDGE_TYPE(E)        ((struct rdg_edge *) ((E)->data))->type

/* Dump vertex I in RDG to FILE.  */

static void
dump_rdg_vertex (FILE *file, struct graph *rdg, int i)
{
  struct vertex *v = &(rdg->vertices[i]);
  struct graph_edge *e;

  fprintf (file, "(vertex %d: (%s%s) (in:", i,
	   RDG_MEM_WRITE_STMT (rdg, i) ? "w" : "",
	   RDG_MEM_READS_STMT (rdg, i) ? "r" : "");

  if (v->pred)
    for (e = v->pred; e; e = e->pred_next)
      fprintf (file, " %d", e->src);

  fprintf (file, ") (out:");

  if (v->succ)
    for (e = v->succ; e; e = e->succ_next)
      fprintf (file, " %d", e->dest);

  fprintf (file, ")\n");
  print_gimple_stmt (file, RDGV_STMT (v), 0, TDF_VOPS|TDF_MEMSYMS);
  fprintf (file, ")\n");
}

/* Call dump_rdg_vertex on stderr.  */

DEBUG_FUNCTION void
debug_rdg_vertex (struct graph *rdg, int i)
{
  dump_rdg_vertex (stderr, rdg, i);
}

/* Dump the reduced dependence graph RDG to FILE.  */

static void
dump_rdg (FILE *file, struct graph *rdg)
{
  fprintf (file, "(rdg\n");
  for (int i = 0; i < rdg->n_vertices; i++)
    dump_rdg_vertex (file, rdg, i);
  fprintf (file, ")\n");
}

/* Call dump_rdg on stderr.  */

DEBUG_FUNCTION void
debug_rdg (struct graph *rdg)
{
  dump_rdg (stderr, rdg);
}

static void
dot_rdg_1 (FILE *file, struct graph *rdg)
{
  int i;
  pretty_printer buffer;
  pp_needs_newline (&buffer) = false;
  buffer.buffer->stream = file;

  fprintf (file, "digraph RDG {\n");

  for (i = 0; i < rdg->n_vertices; i++)
    {
      struct vertex *v = &(rdg->vertices[i]);
      struct graph_edge *e;

      fprintf (file, "%d [label=\"[%d] ", i, i);
      pp_gimple_stmt_1 (&buffer, RDGV_STMT (v), 0, TDF_SLIM);
      pp_flush (&buffer);
      fprintf (file, "\"]\n");

      /* Highlight reads from memory.  */
      if (RDG_MEM_READS_STMT (rdg, i))
       fprintf (file, "%d [style=filled, fillcolor=green]\n", i);

      /* Highlight stores to memory.  */
      if (RDG_MEM_WRITE_STMT (rdg, i))
       fprintf (file, "%d [style=filled, fillcolor=red]\n", i);

      if (v->succ)
       for (e = v->succ; e; e = e->succ_next)
         switch (RDGE_TYPE (e))
           {
           case flow_dd:
             /* These are the most common dependences: don't print these. */
             fprintf (file, "%d -> %d \n", i, e->dest);
             break;

	   case control_dd:
             fprintf (file, "%d -> %d [label=control] \n", i, e->dest);
             break;

           default:
             gcc_unreachable ();
           }
    }

  fprintf (file, "}\n\n");
}

/* Display the Reduced Dependence Graph using dotty.  */

DEBUG_FUNCTION void
dot_rdg (struct graph *rdg)
{
  /* When debugging, you may want to enable the following code.  */
#if 1
  FILE *file = popen ("dot -Tx11", "w");
  if (!file)
    return;
  dot_rdg_1 (file, rdg);
  fflush (file);
  close (fileno (file));
  pclose (file);
#else
  dot_rdg_1 (stderr, rdg);
#endif
}

/* Returns the index of STMT in RDG.  */

static int
rdg_vertex_for_stmt (struct graph *rdg ATTRIBUTE_UNUSED, gimple stmt)
{
  int index = gimple_uid (stmt);
  gcc_checking_assert (index == -1 || RDG_STMT (rdg, index) == stmt);
  return index;
}

/* Creates dependence edges in RDG for all the uses of DEF.  IDEF is
   the index of DEF in RDG.  */

static void
create_rdg_edges_for_scalar (struct graph *rdg, tree def, int idef)
{
  use_operand_p imm_use_p;
  imm_use_iterator iterator;

  FOR_EACH_IMM_USE_FAST (imm_use_p, iterator, def)
    {
      struct graph_edge *e;
      int use = rdg_vertex_for_stmt (rdg, USE_STMT (imm_use_p));

      if (use < 0)
	continue;

      e = add_edge (rdg, idef, use);
      e->data = XNEW (struct rdg_edge);
      RDGE_TYPE (e) = flow_dd;
    }
}

/* Creates an edge for the control dependences of BB to the vertex V.  */

static void
create_edge_for_control_dependence (struct graph *rdg, basic_block bb,
				    int v, control_dependences *cd)
{
  bitmap_iterator bi;
  unsigned edge_n;
  EXECUTE_IF_SET_IN_BITMAP (cd->get_edges_dependent_on (bb->index),
			    0, edge_n, bi)
    {
      basic_block cond_bb = cd->get_edge (edge_n)->src;
      gimple stmt = last_stmt (cond_bb);
      if (stmt && is_ctrl_stmt (stmt))
	{
	  struct graph_edge *e;
	  int c = rdg_vertex_for_stmt (rdg, stmt);
	  if (c < 0)
	    continue;

	  e = add_edge (rdg, c, v);
	  e->data = XNEW (struct rdg_edge);
	  RDGE_TYPE (e) = control_dd;
	}
    }
}

/* Creates the edges of the reduced dependence graph RDG.  */

static void
create_rdg_flow_edges (struct graph *rdg)
{
  int i;
  def_operand_p def_p;
  ssa_op_iter iter;

  for (i = 0; i < rdg->n_vertices; i++)
    FOR_EACH_PHI_OR_STMT_DEF (def_p, RDG_STMT (rdg, i),
			      iter, SSA_OP_DEF)
      create_rdg_edges_for_scalar (rdg, DEF_FROM_PTR (def_p), i);
}

/* Creates the edges of the reduced dependence graph RDG.  */

static void
create_rdg_cd_edges (struct graph *rdg, control_dependences *cd)
{
  int i;

  for (i = 0; i < rdg->n_vertices; i++)
    {
      gimple stmt = RDG_STMT (rdg, i);
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->preds)
	      create_edge_for_control_dependence (rdg, e->src, i, cd);
	}
      else
	create_edge_for_control_dependence (rdg, gimple_bb (stmt), i, cd);
    }
}

/* Build the vertices of the reduced dependence graph RDG.  Return false
   if that failed.  */

static bool
create_rdg_vertices (struct graph *rdg, vec<gimple> stmts, loop_p loop,
		     vec<data_reference_p> *datarefs)
{
  int i;
  gimple stmt;

  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      struct vertex *v = &(rdg->vertices[i]);

      /* Record statement to vertex mapping.  */
      gimple_set_uid (stmt, i);

      v->data = XNEW (struct rdg_vertex);
      RDGV_STMT (v) = stmt;
      RDGV_DATAREFS (v).create (0);
      RDGV_HAS_MEM_WRITE (v) = false;
      RDGV_HAS_MEM_READS (v) = false;
      if (gimple_code (stmt) == GIMPLE_PHI)
	continue;

      unsigned drp = datarefs->length ();
      if (!find_data_references_in_stmt (loop, stmt, datarefs))
	return false;
      for (unsigned j = drp; j < datarefs->length (); ++j)
	{
	  data_reference_p dr = (*datarefs)[j];
	  if (DR_IS_READ (dr))
	    RDGV_HAS_MEM_READS (v) = true;
	  else
	    RDGV_HAS_MEM_WRITE (v) = true;
	  RDGV_DATAREFS (v).safe_push (dr);
	}
    }
  return true;
}

/* Initialize STMTS with all the statements of LOOP.  The order in
   which we discover statements is important as
   generate_loops_for_partition is using the same traversal for
   identifying statements in loop copies.  */

static void
stmts_from_loop (struct loop *loop, vec<gimple> *stmts)
{
  unsigned int i;
  basic_block *bbs = get_loop_body_in_dom_order (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator bsi;
      gimple stmt;

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	if (!virtual_operand_p (gimple_phi_result (gsi_stmt (bsi))))
	  stmts->safe_push (gsi_stmt (bsi));

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  stmt = gsi_stmt (bsi);
	  if (gimple_code (stmt) != GIMPLE_LABEL && !is_gimple_debug (stmt))
	    stmts->safe_push (stmt);
	}
    }

  free (bbs);
}

/* Free the reduced dependence graph RDG.  */

static void
free_rdg (struct graph *rdg)
{
  int i;

  for (i = 0; i < rdg->n_vertices; i++)
    {
      struct vertex *v = &(rdg->vertices[i]);
      struct graph_edge *e;

      for (e = v->succ; e; e = e->succ_next)
	free (e->data);

      if (v->data)
	{
	  gimple_set_uid (RDGV_STMT (v), -1);
	  free_data_refs (RDGV_DATAREFS (v));
	  free (v->data);
	}
    }

  free_graph (rdg);
}

/* Build the Reduced Dependence Graph (RDG) with one vertex per
   statement of the loop nest LOOP_NEST, and one edge per data dependence or
   scalar dependence.  */

static struct graph *
build_rdg (vec<loop_p> loop_nest, control_dependences *cd)
{
  struct graph *rdg;
  vec<data_reference_p> datarefs;

  /* Create the RDG vertices from the stmts of the loop nest.  */
  auto_vec<gimple, 10> stmts;
  stmts_from_loop (loop_nest[0], &stmts);
  rdg = new_graph (stmts.length ());
  datarefs.create (10);
  if (!create_rdg_vertices (rdg, stmts, loop_nest[0], &datarefs))
    {
      datarefs.release ();
      free_rdg (rdg);
      return NULL;
    }
  stmts.release ();

  create_rdg_flow_edges (rdg);
  if (cd)
    create_rdg_cd_edges (rdg, cd);

  datarefs.release ();

  return rdg;
}



enum partition_kind {
    PKIND_NORMAL, PKIND_MEMSET, PKIND_MEMCPY
};

typedef struct partition_s
{
  bitmap stmts;
  bitmap loops;
  bool reduction_p;
  enum partition_kind kind;
  /* data-references a kind != PKIND_NORMAL partition is about.  */
  data_reference_p main_dr;
  data_reference_p secondary_dr;
  tree niter;
  bool plus_one;
} *partition_t;


/* Allocate and initialize a partition from BITMAP.  */

static partition_t
partition_alloc (bitmap stmts, bitmap loops)
{
  partition_t partition = XCNEW (struct partition_s);
  partition->stmts = stmts ? stmts : BITMAP_ALLOC (NULL);
  partition->loops = loops ? loops : BITMAP_ALLOC (NULL);
  partition->reduction_p = false;
  partition->kind = PKIND_NORMAL;
  return partition;
}

/* Free PARTITION.  */

static void
partition_free (partition_t partition)
{
  BITMAP_FREE (partition->stmts);
  BITMAP_FREE (partition->loops);
  free (partition);
}

/* Returns true if the partition can be generated as a builtin.  */

static bool
partition_builtin_p (partition_t partition)
{
  return partition->kind != PKIND_NORMAL;
}

/* Returns true if the partition contains a reduction.  */

static bool
partition_reduction_p (partition_t partition)
{
  return partition->reduction_p;
}

/* Merge PARTITION into the partition DEST.  */

static void
partition_merge_into (partition_t dest, partition_t partition)
{
  dest->kind = PKIND_NORMAL;
  bitmap_ior_into (dest->stmts, partition->stmts);
  if (partition_reduction_p (partition))
    dest->reduction_p = true;
}


/* Returns true when DEF is an SSA_NAME defined in LOOP and used after
   the LOOP.  */

static bool
ssa_name_has_uses_outside_loop_p (tree def, loop_p loop)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, def)
    {
      gimple use_stmt = USE_STMT (use_p);
      if (!is_gimple_debug (use_stmt)
	  && loop != loop_containing_stmt (use_stmt))
	return true;
    }

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

/* Return a copy of LOOP placed before LOOP.  */

static struct loop *
copy_loop_before (struct loop *loop)
{
  struct loop *res;
  edge preheader = loop_preheader_edge (loop);

  initialize_original_copy_tables ();
  res = slpeel_tree_duplicate_loop_to_edge_cfg (loop, NULL, preheader);
  gcc_assert (res != NULL);
  free_original_copy_tables ();
  delete_update_ssa ();

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
  unsigned i;
  gimple_stmt_iterator bsi;
  basic_block *bbs;

  if (copy_p)
    {
      loop = copy_loop_before (loop);
      gcc_assert (loop != NULL);
      create_preheader (loop, CP_SIMPLE_PREHEADERS);
      create_bb_after_loop (loop);
    }

  /* Remove stmts not in the PARTITION bitmap.  */
  bbs = get_loop_body_in_dom_order (loop);

  if (MAY_HAVE_DEBUG_STMTS)
    for (i = 0; i < loop->num_nodes; i++)
      {
	basic_block bb = bbs[i];

	for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple phi = gsi_stmt (bsi);
	    if (!virtual_operand_p (gimple_phi_result (phi))
		&& !bitmap_bit_p (partition->stmts, gimple_uid (phi)))
	      reset_debug_uses (phi);
	  }

	for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple stmt = gsi_stmt (bsi);
	    if (gimple_code (stmt) != GIMPLE_LABEL
		&& !is_gimple_debug (stmt)
		&& !bitmap_bit_p (partition->stmts, gimple_uid (stmt)))
	      reset_debug_uses (stmt);
	  }
      }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi);)
	{
	  gimple phi = gsi_stmt (bsi);
	  if (!virtual_operand_p (gimple_phi_result (phi))
	      && !bitmap_bit_p (partition->stmts, gimple_uid (phi)))
	    remove_phi_node (&bsi, true);
	  else
	    gsi_next (&bsi);
	}

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi);)
	{
	  gimple stmt = gsi_stmt (bsi);
	  if (gimple_code (stmt) != GIMPLE_LABEL
	      && !is_gimple_debug (stmt)
	      && !bitmap_bit_p (partition->stmts, gimple_uid (stmt)))
	    {
	      /* Choose an arbitrary path through the empty CFG part
		 that this unnecessary control stmt controls.  */
	      if (gimple_code (stmt) == GIMPLE_COND)
		{
		  gimple_cond_make_false (stmt);
		  update_stmt (stmt);
		}
	      else if (gimple_code (stmt) == GIMPLE_SWITCH)
		{
		  gimple_switch_set_index
		      (stmt, CASE_LOW (gimple_switch_label (stmt, 1)));
		  update_stmt (stmt);
		}
	      else
		{
		  unlink_stmt_vdef (stmt);
		  gsi_remove (&bsi, true);
		  release_defs (stmt);
		  continue;
		}
	    }
	  gsi_next (&bsi);
	}
    }

  free (bbs);
}

/* Build the size argument for a memory operation call.  */

static tree
build_size_arg_loc (location_t loc, data_reference_p dr, tree nb_iter,
		    bool plus_one)
{
  tree size = fold_convert_loc (loc, sizetype, nb_iter);
  if (plus_one)
    size = size_binop (PLUS_EXPR, size, size_one_node);
  size = fold_build2_loc (loc, MULT_EXPR, sizetype, size,
			  TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr))));
  size = fold_convert_loc (loc, size_type_node, size);
  return size;
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

/* If VAL memory representation contains the same value in all bytes,
   return that value, otherwise return -1.
   E.g. for 0x24242424 return 0x24, for IEEE double
   747708026454360457216.0 return 0x44, etc.  */

static int
const_with_all_bytes_same (tree val)
{
  unsigned char buf[64];
  int i, len;

  if (integer_zerop (val)
      || real_zerop (val)
      || (TREE_CODE (val) == CONSTRUCTOR
          && !TREE_CLOBBER_P (val)
          && CONSTRUCTOR_NELTS (val) == 0))
    return 0;

  if (CHAR_BIT != 8 || BITS_PER_UNIT != 8)
    return -1;

  len = native_encode_expr (val, buf, sizeof (buf));
  if (len == 0)
    return -1;
  for (i = 1; i < len; i++)
    if (buf[i] != buf[0])
      return -1;
  return buf[0];
}

/* Generate a call to memset for PARTITION in LOOP.  */

static void
generate_memset_builtin (struct loop *loop, partition_t partition)
{
  gimple_stmt_iterator gsi;
  gimple stmt, fn_call;
  tree mem, fn, nb_bytes;
  location_t loc;
  tree val;

  stmt = DR_STMT (partition->main_dr);
  loc = gimple_location (stmt);

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = build_size_arg_loc (loc, partition->main_dr, partition->niter,
				 partition->plus_one);
  nb_bytes = force_gimple_operand_gsi (&gsi, nb_bytes, true, NULL_TREE,
				       false, GSI_CONTINUE_LINKING);
  mem = build_addr_arg_loc (loc, partition->main_dr, nb_bytes);
  mem = force_gimple_operand_gsi (&gsi, mem, true, NULL_TREE,
				  false, GSI_CONTINUE_LINKING);

  /* This exactly matches the pattern recognition in classify_partition.  */
  val = gimple_assign_rhs1 (stmt);
  /* Handle constants like 0x15151515 and similarly
     floating point constants etc. where all bytes are the same.  */
  int bytev = const_with_all_bytes_same (val);
  if (bytev != -1)
    val = build_int_cst (integer_type_node, bytev);
  else if (TREE_CODE (val) == INTEGER_CST)
    val = fold_convert (integer_type_node, val);
  else if (!useless_type_conversion_p (integer_type_node, TREE_TYPE (val)))
    {
      gimple cstmt;
      tree tem = make_ssa_name (integer_type_node, NULL);
      cstmt = gimple_build_assign_with_ops (NOP_EXPR, tem, val, NULL_TREE);
      gsi_insert_after (&gsi, cstmt, GSI_CONTINUE_LINKING);
      val = tem;
    }

  fn = build_fold_addr_expr (builtin_decl_implicit (BUILT_IN_MEMSET));
  fn_call = gimple_build_call (fn, 3, mem, val, nb_bytes);
  gsi_insert_after (&gsi, fn_call, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "generated memset");
      if (bytev == 0)
	fprintf (dump_file, " zero\n");
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
  tree dest, src, fn, nb_bytes;
  location_t loc;
  enum built_in_function kind;

  stmt = DR_STMT (partition->main_dr);
  loc = gimple_location (stmt);

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = build_size_arg_loc (loc, partition->main_dr, partition->niter,
				 partition->plus_one);
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
	  if (virtual_operand_p (gimple_phi_result (phi)))
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
    case PKIND_NORMAL:
      /* Reductions all have to be in the last partition.  */
      gcc_assert (!partition_reduction_p (partition)
		  || !copy_p);
      generate_loops_for_partition (loop, partition, copy_p);
      return;

    case PKIND_MEMSET:
      generate_memset_builtin (loop, partition);
      break;

    case PKIND_MEMCPY:
      generate_memcpy_builtin (loop, partition);
      break;

    default:
      gcc_unreachable ();
    }

  /* Common tail for partitions we turn into a call.  If this was the last
     partition for which we generate code, we have to destroy the loop.  */
  if (!copy_p)
    destroy_loop (loop);
}


/* Returns a partition with all the statements needed for computing
   the vertex V of the RDG, also including the loop exit conditions.  */

static partition_t
build_rdg_partition_for_vertex (struct graph *rdg, int v)
{
  partition_t partition = partition_alloc (NULL, NULL);
  auto_vec<int, 3> nodes;
  unsigned i;
  int x;

  graphds_dfs (rdg, &v, 1, &nodes, false, NULL);

  FOR_EACH_VEC_ELT (nodes, i, x)
    {
      bitmap_set_bit (partition->stmts, x);
      bitmap_set_bit (partition->loops,
		      loop_containing_stmt (RDG_STMT (rdg, x))->num);
    }

  return partition;
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
  bool volatiles_p = false;
  bool plus_one = false;

  partition->kind = PKIND_NORMAL;
  partition->main_dr = NULL;
  partition->secondary_dr = NULL;
  partition->niter = NULL_TREE;
  partition->plus_one = false;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple stmt = RDG_STMT (rdg, i);

      if (gimple_has_volatile_ops (stmt))
	volatiles_p = true;

      /* If the stmt has uses outside of the loop mark it as reduction.  */
      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
	{
	  partition->reduction_p = true;
	  return;
	}
    }

  /* Perform general partition disqualification for builtins.  */
  if (volatiles_p
      || !flag_tree_loop_distribute_patterns)
    return;

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
      for (j = 0; RDG_DATAREFS (rdg, i).iterate (j, &dr); ++j)
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

  if (!single_store)
    return;

  nb_iter = number_of_latch_executions (loop);
  if (!nb_iter || nb_iter == chrec_dont_know)
    return;
  if (dominated_by_p (CDI_DOMINATORS, single_exit (loop)->src,
		      gimple_bb (DR_STMT (single_store))))
    plus_one = true;

  if (single_store && !single_load)
    {
      gimple stmt = DR_STMT (single_store);
      tree rhs = gimple_assign_rhs1 (stmt);
      if (const_with_all_bytes_same (rhs) == -1
	  && (!INTEGRAL_TYPE_P (TREE_TYPE (rhs))
	      || (TYPE_MODE (TREE_TYPE (rhs))
		  != TYPE_MODE (unsigned_char_type_node))))
	return;
      if (TREE_CODE (rhs) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (rhs)
	  && flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (rhs))))
	return;
      if (!adjacent_dr_p (single_store)
	  || !dominated_by_p (CDI_DOMINATORS,
			      loop->latch, gimple_bb (stmt)))
	return;
      partition->kind = PKIND_MEMSET;
      partition->main_dr = single_store;
      partition->niter = nb_iter;
      partition->plus_one = plus_one;
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
			       DR_STEP (single_load), 0)
	  || !dominated_by_p (CDI_DOMINATORS,
			      loop->latch, gimple_bb (store)))
	return;
      /* Now check that if there is a dependence this dependence is
         of a suitable form for memmove.  */
      vec<loop_p> loops = vNULL;
      ddr_p ddr;
      loops.safe_push (loop);
      ddr = initialize_data_dependence_relation (single_load, single_store,
						 loops);
      compute_affine_dependence (ddr, loop);
      if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	{
	  free_dependence_relation (ddr);
	  loops.release ();
	  return;
	}
      if (DDR_ARE_DEPENDENT (ddr) != chrec_known)
	{
	  if (DDR_NUM_DIST_VECTS (ddr) == 0)
	    {
	      free_dependence_relation (ddr);
	      loops.release ();
	      return;
	    }
	  lambda_vector dist_v;
	  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
	    {
	      int dist = dist_v[index_in_loop_nest (loop->num,
						    DDR_LOOP_NEST (ddr))];
	      if (dist > 0 && !DDR_REVERSED_P (ddr))
		{
		  free_dependence_relation (ddr);
		  loops.release ();
		  return;
		}
	    }
	}
      free_dependence_relation (ddr);
      loops.release ();
      partition->kind = PKIND_MEMCPY;
      partition->main_dr = single_store;
      partition->secondary_dr = single_load;
      partition->niter = nb_iter;
      partition->plus_one = plus_one;
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
	    FOR_EACH_VEC_ELT (RDG_DATAREFS (rdg, i), k, ref1)
	      {
		tree base1 = ref_base_address (ref1);
		if (base1)
		  FOR_EACH_VEC_ELT (RDG_DATAREFS (rdg, j), l, ref2)
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
rdg_build_partitions (struct graph *rdg,
		      vec<gimple> starting_stmts,
		      vec<partition_t> *partitions)
{
  bitmap processed = BITMAP_ALLOC (NULL);
  int i;
  gimple stmt;

  FOR_EACH_VEC_ELT (starting_stmts, i, stmt)
    {
      int v = rdg_vertex_for_stmt (rdg, stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "ldist asked to generate code for vertex %d\n", v);

      /* If the vertex is already contained in another partition so
         is the partition rooted at it.  */
      if (bitmap_bit_p (processed, v))
	continue;

      partition_t partition = build_rdg_partition_for_vertex (rdg, v);
      bitmap_ior_into (processed, partition->stmts);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "ldist useful partition:\n");
	  dump_bitmap (dump_file, partition->stmts);
	}

      partitions->safe_push (partition);
    }

  /* All vertices should have been assigned to at least one partition now,
     other than vertices belonging to dead code.  */

  BITMAP_FREE (processed);
}

/* Dump to FILE the PARTITIONS.  */

static void
dump_rdg_partitions (FILE *file, vec<partition_t> partitions)
{
  int i;
  partition_t partition;

  FOR_EACH_VEC_ELT (partitions, i, partition)
    debug_bitmap_file (file, partition->stmts);
}

/* Debug PARTITIONS.  */
extern void debug_rdg_partitions (vec<partition_t> );

DEBUG_FUNCTION void
debug_rdg_partitions (vec<partition_t> partitions)
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
partition_contains_all_rw (struct graph *rdg,
			   vec<partition_t> partitions)
{
  int i;
  partition_t partition;
  int nrw = number_of_rw_in_rdg (rdg);

  FOR_EACH_VEC_ELT (partitions, i, partition)
    if (nrw == number_of_rw_in_partition (rdg, partition))
      return true;

  return false;
}

/* Compute partition dependence created by the data references in DRS1
   and DRS2 and modify and return DIR according to that.  */

static int
pg_add_dependence_edges (struct graph *rdg, vec<loop_p> loops, int dir,
			 vec<data_reference_p> drs1,
			 vec<data_reference_p> drs2)
{
  data_reference_p dr1, dr2;

  /* dependence direction - 0 is no dependence, -1 is back,
     1 is forth, 2 is both (we can stop then, merging will occur).  */
  for (int ii = 0; drs1.iterate (ii, &dr1); ++ii)
    for (int jj = 0; drs2.iterate (jj, &dr2); ++jj)
      {
	int this_dir = 1;
	ddr_p ddr;
	/* Re-shuffle data-refs to be in dominator order.  */
	if (rdg_vertex_for_stmt (rdg, DR_STMT (dr1))
	    > rdg_vertex_for_stmt (rdg, DR_STMT (dr2)))
	  {
	    data_reference_p tem = dr1;
	    dr1 = dr2;
	    dr2 = tem;
	    this_dir = -this_dir;
	  }
	ddr = initialize_data_dependence_relation (dr1, dr2, loops);
	compute_affine_dependence (ddr, loops[0]);
	if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	  this_dir = 2;
	else if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
	  {
	    if (DDR_REVERSED_P (ddr))
	      {
		data_reference_p tem = dr1;
		dr1 = dr2;
		dr2 = tem;
		this_dir = -this_dir;
	      }
	    /* Known dependences can still be unordered througout the
	       iteration space, see gcc.dg/tree-ssa/ldist-16.c.  */
	    if (DDR_NUM_DIST_VECTS (ddr) != 1)
	      this_dir = 2;
	    /* If the overlap is exact preserve stmt order.  */
	    else if (lambda_vector_zerop (DDR_DIST_VECT (ddr, 0), 1))
	      ;
	    else
	      {
		/* Else as the distance vector is lexicographic positive
		   swap the dependence direction.  */
		this_dir = -this_dir;
	      }
	  }
	else
	  this_dir = 0;
	free_dependence_relation (ddr);
	if (dir == 0)
	  dir = this_dir;
	else if (dir != this_dir)
	  return 2;
      }
  return dir;
}

/* Compare postorder number of the partition graph vertices V1 and V2.  */

static int
pgcmp (const void *v1_, const void *v2_)
{
  const vertex *v1 = (const vertex *)v1_;
  const vertex *v2 = (const vertex *)v2_;
  return v2->post - v1->post;
}

/* Distributes the code from LOOP in such a way that producer
   statements are placed before consumer statements.  Tries to separate
   only the statements from STMTS into separate loops.
   Returns the number of distributed loops.  */

static int
distribute_loop (struct loop *loop, vec<gimple> stmts,
		 control_dependences *cd, int *nb_calls)
{
  struct graph *rdg;
  partition_t partition;
  bool any_builtin;
  int i, nbp;
  graph *pg = NULL;
  int num_sccs = 1;

  *nb_calls = 0;
  auto_vec<loop_p, 3> loop_nest;
  if (!find_loop_nest (loop, &loop_nest))
    return 0;

  rdg = build_rdg (loop_nest, cd);
  if (!rdg)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %d not distributed: failed to build the RDG.\n",
		 loop->num);

      return 0;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg (dump_file, rdg);

  auto_vec<partition_t, 3> partitions;
  rdg_build_partitions (rdg, stmts, &partitions);

  any_builtin = false;
  FOR_EACH_VEC_ELT (partitions, i, partition)
    {
      classify_partition (loop, rdg, partition);
      any_builtin |= partition_builtin_p (partition);
    }

  /* If we are only distributing patterns but did not detect any,
     simply bail out.  */
  if (!flag_tree_loop_distribution
      && !any_builtin)
    {
      nbp = 0;
      goto ldist_done;
    }

  /* If we are only distributing patterns fuse all partitions that
     were not classified as builtins.  This also avoids chopping
     a loop into pieces, separated by builtin calls.  That is, we
     only want no or a single loop body remaining.  */
  partition_t into;
  if (!flag_tree_loop_distribution)
    {
      for (i = 0; partitions.iterate (i, &into); ++i)
	if (!partition_builtin_p (into))
	  break;
      for (++i; partitions.iterate (i, &partition); ++i)
	if (!partition_builtin_p (partition))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "fusing non-builtin partitions\n");
		dump_bitmap (dump_file, into->stmts);
		dump_bitmap (dump_file, partition->stmts);
	      }
	    partition_merge_into (into, partition);
	    partitions.unordered_remove (i);
	    partition_free (partition);
	    i--;
	  }
    }

  /* Due to limitations in the transform phase we have to fuse all
     reduction partitions into the last partition so the existing
     loop will contain all loop-closed PHI nodes.  */
  for (i = 0; partitions.iterate (i, &into); ++i)
    if (partition_reduction_p (into))
      break;
  for (i = i + 1; partitions.iterate (i, &partition); ++i)
    if (partition_reduction_p (partition))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "fusing partitions\n");
	    dump_bitmap (dump_file, into->stmts);
	    dump_bitmap (dump_file, partition->stmts);
	    fprintf (dump_file, "because they have reductions\n");
	  }
	partition_merge_into (into, partition);
	partitions.unordered_remove (i);
	partition_free (partition);
	i--;
      }

  /* Apply our simple cost model - fuse partitions with similar
     memory accesses.  */
  for (i = 0; partitions.iterate (i, &into); ++i)
    {
      if (partition_builtin_p (into))
	continue;
      for (int j = i + 1;
	   partitions.iterate (j, &partition); ++j)
	{
	  if (!partition_builtin_p (partition)
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
	      partition_merge_into (into, partition);
	      partitions.unordered_remove (j);
	      partition_free (partition);
	      j--;
	    }
	}
    }

  /* Build the partition dependency graph.  */
  if (partitions.length () > 1)
    {
      pg = new_graph (partitions.length ());
      struct pgdata {
	  partition_t partition;
	  vec<data_reference_p> writes;
	  vec<data_reference_p> reads;
      };
#define PGDATA(i) ((pgdata *)(pg->vertices[i].data))
      for (i = 0; partitions.iterate (i, &partition); ++i)
	{
	  vertex *v = &pg->vertices[i];
	  pgdata *data = new pgdata;
	  data_reference_p dr;
	  /* FIXME - leaks.  */
	  v->data = data;
	  bitmap_iterator bi;
	  unsigned j;
	  data->partition = partition;
	  data->reads = vNULL;
	  data->writes = vNULL;
	  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, j, bi)
	    for (int k = 0; RDG_DATAREFS (rdg, j).iterate (k, &dr); ++k)
	      if (DR_IS_READ (dr))
		data->reads.safe_push (dr);
	      else
		data->writes.safe_push (dr);
	}
      partition_t partition1, partition2;
      for (i = 0; partitions.iterate (i, &partition1); ++i)
	for (int j = i + 1; partitions.iterate (j, &partition2); ++j)
	  {
	    /* dependence direction - 0 is no dependence, -1 is back,
	       1 is forth, 2 is both (we can stop then, merging will occur).  */
	    int dir = 0;
	    dir = pg_add_dependence_edges (rdg, loop_nest, dir,
					   PGDATA(i)->writes,
					   PGDATA(j)->reads);
	    if (dir != 2)
	      dir = pg_add_dependence_edges (rdg, loop_nest, dir,
					     PGDATA(i)->reads,
					     PGDATA(j)->writes);
	    if (dir != 2)
	      dir = pg_add_dependence_edges (rdg, loop_nest, dir,
					     PGDATA(i)->writes,
					     PGDATA(j)->writes);
	    if (dir == 1 || dir == 2)
	      add_edge (pg, i, j);
	    if (dir == -1 || dir == 2)
	      add_edge (pg, j, i);
	  }

      /* Add edges to the reduction partition (if any) to force it last.  */
      unsigned j;
      for (j = 0; partitions.iterate (j, &partition); ++j)
	if (partition_reduction_p (partition))
	  break;
      if (j < partitions.length ())
	{
	  for (unsigned i = 0; partitions.iterate (i, &partition); ++i)
	    if (i != j)
	      add_edge (pg, i, j);
	}

      /* Compute partitions we cannot separate and fuse them.  */
      num_sccs = graphds_scc (pg, NULL);
      for (i = 0; i < num_sccs; ++i)
	{
	  partition_t first;
	  int j;
	  for (j = 0; partitions.iterate (j, &first); ++j)
	    if (pg->vertices[j].component == i)
	      break;
	  for (j = j + 1; partitions.iterate (j, &partition); ++j)
	    if (pg->vertices[j].component == i)
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  {
		    fprintf (dump_file, "fusing partitions\n");
		    dump_bitmap (dump_file, first->stmts);
		    dump_bitmap (dump_file, partition->stmts);
		    fprintf (dump_file, "because they are in the same "
			     "dependence SCC\n");
		  }
		partition_merge_into (first, partition);
		partitions[j] = NULL;
		partition_free (partition);
		PGDATA (j)->partition = NULL;
	      }
	}

      /* Now order the remaining nodes in postorder.  */
      qsort (pg->vertices, pg->n_vertices, sizeof (vertex), pgcmp);
      partitions.truncate (0);
      for (i = 0; i < pg->n_vertices; ++i)
	{
	  pgdata *data = PGDATA (i);
	  if (data->partition)
	    partitions.safe_push (data->partition);
	  data->reads.release ();
	  data->writes.release ();
	  delete data;
	}
      gcc_assert (partitions.length () == (unsigned)num_sccs);
      free_graph (pg);
    }

  nbp = partitions.length ();
  if (nbp == 0
      || (nbp == 1 && !partition_builtin_p (partitions[0]))
      || (nbp > 1 && partition_contains_all_rw (rdg, partitions)))
    {
      nbp = 0;
      goto ldist_done;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg_partitions (dump_file, partitions);

  FOR_EACH_VEC_ELT (partitions, i, partition)
    {
      if (partition_builtin_p (partition))
	(*nb_calls)++;
      generate_code_for_partition (loop, partition, i < nbp - 1);
    }

 ldist_done:

  FOR_EACH_VEC_ELT (partitions, i, partition)
    partition_free (partition);

  free_rdg (rdg);
  return nbp - *nb_calls;
}

/* Distribute all loops in the current function.  */

static unsigned int
tree_loop_distribution (void)
{
  struct loop *loop;
  bool changed = false;
  basic_block bb;
  control_dependences *cd = NULL;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_uid (gsi_stmt (gsi), -1);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_uid (gsi_stmt (gsi), -1);
    }

  /* We can at the moment only distribute non-nested loops, thus restrict
     walking to innermost loops.  */
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    {
      auto_vec<gimple> work_list;
      basic_block *bbs;
      int num = loop->num;
      unsigned int i;

      /* If the loop doesn't have a single exit we will fail anyway,
	 so do that early.  */
      if (!single_exit (loop))
	continue;

      /* Only optimize hot loops.  */
      if (!optimize_loop_for_speed_p (loop))
	continue;

      /* Initialize the worklist with stmts we seed the partitions with.  */
      bbs = get_loop_body_in_dom_order (loop);
      for (i = 0; i < loop->num_nodes; ++i)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_phis (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple phi = gsi_stmt (gsi);
	      if (virtual_operand_p (gimple_phi_result (phi)))
		continue;
	      /* Distribute stmts which have defs that are used outside of
	         the loop.  */
	      if (!stmt_has_scalar_dependences_outside_loop (loop, phi))
		continue;
	      work_list.safe_push (phi);
	    }
	  for (gsi = gsi_start_bb (bbs[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      /* If there is a stmt with side-effects bail out - we
	         cannot and should not distribute this loop.  */
	      if (gimple_has_side_effects (stmt))
		{
		  work_list.truncate (0);
		  goto out;
		}

	      /* Distribute stmts which have defs that are used outside of
	         the loop.  */
	      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
		;
	      /* Otherwise only distribute stores for now.  */
	      else if (!gimple_vdef (stmt))
		continue;

	      work_list.safe_push (stmt);
	    }
	}
out:
      free (bbs);

      int nb_generated_loops = 0;
      int nb_generated_calls = 0;
      location_t loc = find_loop_location (loop);
      if (work_list.length () > 0)
	{
	  if (!cd)
	    {
	      calculate_dominance_info (CDI_DOMINATORS);
	      calculate_dominance_info (CDI_POST_DOMINATORS);
	      cd = new control_dependences (create_edge_list ());
	      free_dominance_info (CDI_POST_DOMINATORS);
	    }
	  nb_generated_loops = distribute_loop (loop, work_list, cd,
						&nb_generated_calls);
	}

      if (nb_generated_loops + nb_generated_calls > 0)
	{
	  changed = true;
	  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS,
			   loc, "Loop %d distributed: split to %d loops "
			   "and %d library calls.\n",
			   num, nb_generated_loops, nb_generated_calls);
	}
      else if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Loop %d is the same.\n", num);
    }

  if (cd)
    delete cd;

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

namespace {

const pass_data pass_data_loop_distribution =
{
  GIMPLE_PASS, /* type */
  "ldist", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_DISTRIBUTION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_loop_distribution : public gimple_opt_pass
{
public:
  pass_loop_distribution (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_distribution, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_loop_distribution (); }
  unsigned int execute () { return tree_loop_distribution (); }

}; // class pass_loop_distribution

} // anon namespace

gimple_opt_pass *
make_pass_loop_distribution (gcc::context *ctxt)
{
  return new pass_loop_distribution (ctxt);
}
