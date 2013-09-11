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
#include "tree-flow.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "gimple-pretty-print.h"


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

  /* Write After Read (WAR).  */
  anti_dd = 'a',

  /* Write After Write (WAW).  */
  output_dd = 'o',

  /* Read After Read (RAR).  */
  input_dd = 'i'
};

/* Dependence information attached to an edge of the RDG.  */

typedef struct rdg_edge
{
  /* Type of the dependence.  */
  enum rdg_dep_type type;

  /* Levels of the dependence: the depth of the loops that carry the
     dependence.  */
  unsigned level;

  /* Dependence relation between data dependences, NULL when one of
     the vertices is a scalar.  */
  ddr_p relation;
} *rdg_edge_p;

#define RDGE_TYPE(E)        ((struct rdg_edge *) ((E)->data))->type
#define RDGE_LEVEL(E)       ((struct rdg_edge *) ((E)->data))->level
#define RDGE_RELATION(E)    ((struct rdg_edge *) ((E)->data))->relation

/* Strongly connected components of the reduced data dependence graph.  */

typedef struct rdg_component
{
  int num;
  vec<int> vertices;
} *rdgc;

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

/* Dump component C of RDG to FILE.  If DUMPED is non-null, set the
   dumped vertices to that bitmap.  */

static void
dump_rdg_component (FILE *file, struct graph *rdg, int c, bitmap dumped)
{
  int i;

  fprintf (file, "(%d\n", c);

  for (i = 0; i < rdg->n_vertices; i++)
    if (rdg->vertices[i].component == c)
      {
	if (dumped)
	  bitmap_set_bit (dumped, i);

	dump_rdg_vertex (file, rdg, i);
      }

  fprintf (file, ")\n");
}

/* Call dump_rdg_vertex on stderr.  */

DEBUG_FUNCTION void
debug_rdg_component (struct graph *rdg, int c)
{
  dump_rdg_component (stderr, rdg, c, NULL);
}

/* Dump the reduced dependence graph RDG to FILE.  */

static void
dump_rdg (FILE *file, struct graph *rdg)
{
  int i;
  bitmap dumped = BITMAP_ALLOC (NULL);

  fprintf (file, "(rdg\n");

  for (i = 0; i < rdg->n_vertices; i++)
    if (!bitmap_bit_p (dumped, i))
      dump_rdg_component (file, rdg, rdg->vertices[i].component, dumped);

  fprintf (file, ")\n");
  BITMAP_FREE (dumped);
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

  fprintf (file, "digraph RDG {\n");

  for (i = 0; i < rdg->n_vertices; i++)
    {
      struct vertex *v = &(rdg->vertices[i]);
      struct graph_edge *e;

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
           case input_dd:
             fprintf (file, "%d -> %d [label=input] \n", i, e->dest);
             break;

           case output_dd:
             fprintf (file, "%d -> %d [label=output] \n", i, e->dest);
             break;

           case flow_dd:
             /* These are the most common dependences: don't print these. */
             fprintf (file, "%d -> %d \n", i, e->dest);
             break;

           case anti_dd:
             fprintf (file, "%d -> %d [label=anti] \n", i, e->dest);
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
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *file = fopen ("/tmp/rdg.dot", "w");
  gcc_assert (file != NULL);

  dot_rdg_1 (file, rdg);
  fclose (file);

  system ("dotty /tmp/rdg.dot &");
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

/* Creates an edge in RDG for each distance vector from DDR.  The
   order that we keep track of in the RDG is the order in which
   statements have to be executed.  */

static void
create_rdg_edge_for_ddr (struct graph *rdg, ddr_p ddr)
{
  struct graph_edge *e;
  int va, vb;
  data_reference_p dra = DDR_A (ddr);
  data_reference_p drb = DDR_B (ddr);
  unsigned level = ddr_dependence_level (ddr);

  /* For non scalar dependences, when the dependence is REVERSED,
     statement B has to be executed before statement A.  */
  if (level > 0
      && !DDR_REVERSED_P (ddr))
    {
      data_reference_p tmp = dra;
      dra = drb;
      drb = tmp;
    }

  va = rdg_vertex_for_stmt (rdg, DR_STMT (dra));
  vb = rdg_vertex_for_stmt (rdg, DR_STMT (drb));

  if (va < 0 || vb < 0)
    return;

  e = add_edge (rdg, va, vb);
  e->data = XNEW (struct rdg_edge);

  RDGE_LEVEL (e) = level;
  RDGE_RELATION (e) = ddr;

  /* Determines the type of the data dependence.  */
  if (DR_IS_READ (dra) && DR_IS_READ (drb))
    RDGE_TYPE (e) = input_dd;
  else if (DR_IS_WRITE (dra) && DR_IS_WRITE (drb))
    RDGE_TYPE (e) = output_dd;
  else if (DR_IS_WRITE (dra) && DR_IS_READ (drb))
    RDGE_TYPE (e) = flow_dd;
  else if (DR_IS_READ (dra) && DR_IS_WRITE (drb))
    RDGE_TYPE (e) = anti_dd;
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
      RDGE_RELATION (e) = NULL;
    }
}

/* Creates the edges of the reduced dependence graph RDG.  */

static void
create_rdg_edges (struct graph *rdg, vec<ddr_p> ddrs)
{
  int i;
  struct data_dependence_relation *ddr;
  def_operand_p def_p;
  ssa_op_iter iter;

  FOR_EACH_VEC_ELT (ddrs, i, ddr)
    if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
      create_rdg_edge_for_ddr (rdg, ddr);

  for (i = 0; i < rdg->n_vertices; i++)
    FOR_EACH_PHI_OR_STMT_DEF (def_p, RDG_STMT (rdg, i),
			      iter, SSA_OP_DEF)
      create_rdg_edges_for_scalar (rdg, DEF_FROM_PTR (def_p), i);
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

/* Initialize STMTS with all the statements of LOOP.  When
   INCLUDE_PHIS is true, include also the PHI nodes.  The order in
   which we discover statements is important as
   generate_loops_for_partition is using the same traversal for
   identifying statements. */

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

/* Returns true when all the dependences are computable.  */

static bool
known_dependences_p (vec<ddr_p> dependence_relations)
{
  ddr_p ddr;
  unsigned int i;

  FOR_EACH_VEC_ELT (dependence_relations, i, ddr)
    if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
      return false;

  return true;
}

/* Build the Reduced Dependence Graph (RDG) with one vertex per
   statement of the loop nest, and one edge per data dependence or
   scalar dependence.  */

struct graph *
build_empty_rdg (int n_stmts)
{
  struct graph *rdg = new_graph (n_stmts);
  return rdg;
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
	{
	  free_dependence_relation (RDGE_RELATION (e));
	  free (e->data);
	}

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
   statement of the loop nest, and one edge per data dependence or
   scalar dependence.  */

static struct graph *
build_rdg (struct loop *loop)
{
  struct graph *rdg;
  vec<loop_p> loop_nest;
  vec<gimple> stmts;
  vec<data_reference_p> datarefs;
  vec<ddr_p> dependence_relations;

  loop_nest.create (3);
  if (!find_loop_nest (loop, &loop_nest))
    {
      loop_nest.release ();
      return NULL;
    }

  stmts.create (10);
  stmts_from_loop (loop, &stmts);
  rdg = build_empty_rdg (stmts.length ());
  datarefs.create (10);
  if (!create_rdg_vertices (rdg, stmts, loop, &datarefs))
    {
      stmts.release ();
      free_rdg (rdg);
      return NULL;
    }
  stmts.release ();
  dependence_relations.create (100);
  if (!compute_all_dependences (datarefs, &dependence_relations, loop_nest,
				false)
      || !known_dependences_p (dependence_relations))
    {
      loop_nest.release ();
      datarefs.release ();
      dependence_relations.release ();
      free_rdg (rdg);
      return NULL;
    }
  loop_nest.release ();
  create_rdg_edges (rdg, dependence_relations);
  dependence_relations.release ();

  return rdg;
}

/* Determines whether the statement from vertex V of the RDG has a
   definition used outside the loop that contains this statement.  */

static bool
rdg_defs_used_in_other_loops_p (struct graph *rdg, int v)
{
  gimple stmt = RDG_STMT (rdg, v);
  struct loop *loop = loop_containing_stmt (stmt);
  use_operand_p imm_use_p;
  imm_use_iterator iterator;
  ssa_op_iter it;
  def_operand_p def_p;

  if (!loop)
    return true;

  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, it, SSA_OP_DEF)
    {
      FOR_EACH_IMM_USE_FAST (imm_use_p, iterator, DEF_FROM_PTR (def_p))
	{
	  if (loop_containing_stmt (USE_STMT (imm_use_p)) != loop)
	    return true;
	}
    }

  return false;
}



enum partition_kind {
    PKIND_NORMAL, PKIND_REDUCTION, PKIND_MEMSET, PKIND_MEMCPY
};

typedef struct partition_s
{
  bitmap stmts;
  bitmap loops;
  bool has_writes;
  enum partition_kind kind;
  /* data-references a kind != PKIND_NORMAL partition is about.  */
  data_reference_p main_dr;
  data_reference_p secondary_dr;
} *partition_t;


/* Allocate and initialize a partition from BITMAP.  */

static partition_t
partition_alloc (bitmap stmts, bitmap loops)
{
  partition_t partition = XCNEW (struct partition_s);
  partition->stmts = stmts ? stmts : BITMAP_ALLOC (NULL);
  partition->loops = loops ? loops : BITMAP_ALLOC (NULL);
  partition->has_writes = false;
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
  return partition->kind > PKIND_REDUCTION;
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
  res = slpeel_tree_duplicate_loop_to_edge_cfg (loop, preheader);
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
	    if (virtual_operand_p (gimple_phi_result (phi)))
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

    case PKIND_REDUCTION:
      /* Reductions all have to be in the last partition.  */
      gcc_assert (!copy_p);
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

/* Returns NULL when there is no anti-dependence or output-dependence
   among the successors of vertex V, otherwise returns the edge with the
   dependency.  */

static struct graph_edge *
has_anti_or_output_dependence (struct vertex *v)
{
  struct graph_edge *e;

  if (v->succ)
    for (e = v->succ; e; e = e->succ_next)
      if (RDGE_TYPE (e) == anti_dd
	  || RDGE_TYPE (e) == output_dd)
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
	vec<int> nodes;
	nodes.create (3);

	graphds_dfs (rdg, &v, 1, &nodes, false, NULL);

	FOR_EACH_VEC_ELT (nodes, i, x)
	  {
	    if (!bitmap_set_bit (seen, x))
	      continue;

	    if (RDG_MEM_WRITE_STMT (rdg, x)
		|| predecessor_has_mem_write (rdg, &(rdg->vertices[x]))
		/* In anti dependences the read should occur before
		   the write, this is why both the read and the write
		   should be placed in the same partition.  In output
		   dependences the writes order need to be preserved.  */
		|| has_anti_or_output_dependence (&(rdg->vertices[x])))
	      bitmap_set_bit (upstream_mem_writes, x);
	  }

	nodes.release ();
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
					   bitmap);

/* Flag the uses of U stopping following the information from
   upstream_mem_writes.  */

static void
rdg_flag_uses (struct graph *rdg, int u, partition_t partition,
	       bitmap processed)
{
  struct vertex *x = &(rdg->vertices[u]);
  gimple stmt = RDGV_STMT (x);
  struct graph_edge *anti_dep = has_anti_or_output_dependence (x);

  /* Keep in the same partition the destination of an antidependence,
     because this is a store to the exact same location.  Putting this
     in another partition is bad for cache locality.  */
  if (anti_dep)
    {
      int v = anti_dep->dest;

      if (!already_processed_vertex_p (processed, v))
	rdg_flag_vertex_and_dependent (rdg, v, partition, processed);
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
		rdg_flag_vertex_and_dependent (rdg, v, partition, processed);
	    }
	}
    }
}

/* Flag V from RDG as part of PARTITION, and also flag its loop number
   in LOOPS.  */

static void
rdg_flag_vertex (struct graph *rdg, int v, partition_t partition)
{
  struct loop *loop;

  if (!bitmap_set_bit (partition->stmts, v))
    return;

  loop = loop_containing_stmt (RDG_STMT (rdg, v));
  bitmap_set_bit (partition->loops, loop->num);

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
			       bitmap processed)
{
  unsigned i;
  vec<int> nodes;
  nodes.create (3);
  int x;

  bitmap_set_bit (processed, v);
  rdg_flag_uses (rdg, v, partition, processed);
  graphds_dfs (rdg, &v, 1, &nodes, false, remaining_stmts);
  rdg_flag_vertex (rdg, v, partition);

  FOR_EACH_VEC_ELT (nodes, i, x)
    if (!already_processed_vertex_p (processed, x))
      rdg_flag_vertex_and_dependent (rdg, x, partition, processed);

  nodes.release ();
}

/* Initialize CONDS with all the condition statements from the basic
   blocks of LOOP.  */

static void
collect_condition_stmts (struct loop *loop, vec<gimple> *conds)
{
  unsigned i;
  edge e;
  vec<edge> exits = get_loop_exit_edges (loop);

  FOR_EACH_VEC_ELT (exits, i, e)
    {
      gimple cond = last_stmt (e->src);

      if (cond)
	conds->safe_push (cond);
    }

  exits.release ();
}

/* Add to PARTITION all the exit condition statements for LOOPS
   together with all their dependent statements determined from
   RDG.  */

static void
rdg_flag_loop_exits (struct graph *rdg, partition_t partition,
		     bitmap processed)
{
  unsigned i;
  bitmap_iterator bi;
  vec<gimple> conds;
  conds.create (3);

  EXECUTE_IF_SET_IN_BITMAP (partition->loops, 0, i, bi)
    collect_condition_stmts (get_loop (cfun, i), &conds);

  while (!conds.is_empty ())
    {
      gimple cond = conds.pop ();
      int v = rdg_vertex_for_stmt (rdg, cond);
      if (!already_processed_vertex_p (processed, v))
	{
	  bitmap saved_loops = BITMAP_ALLOC (NULL);
	  bitmap_copy (saved_loops, partition->loops);
	  rdg_flag_vertex_and_dependent (rdg, v, partition, processed);
	  EXECUTE_IF_AND_COMPL_IN_BITMAP (partition->loops, saved_loops,
					  0, i, bi)
	    collect_condition_stmts (get_loop (cfun, i), &conds);
	  BITMAP_FREE (saved_loops);
	}
    }

  conds.release ();
}

/* Returns a bitmap in which all the statements needed for computing
   the strongly connected component C of the RDG are flagged, also
   including the loop exit conditions.  */

static partition_t
build_rdg_partition_for_component (struct graph *rdg, rdgc c)
{
  int i, v;
  partition_t partition = partition_alloc (NULL, NULL);
  bitmap processed = BITMAP_ALLOC (NULL);

  FOR_EACH_VEC_ELT (c->vertices, i, v)
    if (!already_processed_vertex_p (processed, v))
      rdg_flag_vertex_and_dependent (rdg, v, partition, processed);

  rdg_flag_loop_exits (rdg, partition, processed);

  BITMAP_FREE (processed);
  return partition;
}

/* Free memory for COMPONENTS.  */

static void
free_rdg_components (vec<rdgc> components)
{
  int i;
  rdgc x;

  FOR_EACH_VEC_ELT (components, i, x)
    {
      x->vertices.release ();
      free (x);
    }

  components.release ();
}

/* Build the COMPONENTS vector with the strongly connected components
   of RDG in which the STARTING_VERTICES occur.  */

static void
rdg_build_components (struct graph *rdg, vec<int> starting_vertices,
		      vec<rdgc> *components)
{
  int i, v;
  bitmap saved_components = BITMAP_ALLOC (NULL);
  int n_components = graphds_scc (rdg, NULL);
  /* ??? Macros cannot process template types with more than one
     argument, so we need this typedef.  */
  typedef vec<int> vec_int_heap;
  vec<int> *all_components = XNEWVEC (vec_int_heap, n_components);

  for (i = 0; i < n_components; i++)
    all_components[i].create (3);

  for (i = 0; i < rdg->n_vertices; i++)
    all_components[rdg->vertices[i].component].safe_push (i);

  FOR_EACH_VEC_ELT (starting_vertices, i, v)
    {
      int c = rdg->vertices[v].component;

      if (bitmap_set_bit (saved_components, c))
	{
	  rdgc x = XCNEW (struct rdg_component);
	  x->num = c;
	  x->vertices = all_components[c];

	  components->safe_push (x);
	}
    }

  for (i = 0; i < n_components; i++)
    if (!bitmap_bit_p (saved_components, i))
      all_components[i].release ();

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
  bool volatiles_p = false;

  partition->kind = PKIND_NORMAL;
  partition->main_dr = NULL;
  partition->secondary_dr = NULL;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple stmt = RDG_STMT (rdg, i);

      if (gimple_has_volatile_ops (stmt))
	volatiles_p = true;

      /* If the stmt has uses outside of the loop fail.
	 ???  If the stmt is generated in another partition that
	 is not created as builtin we can ignore this.  */
      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "not generating builtin, partition has "
		     "scalar uses outside of the loop\n");
	  partition->kind = PKIND_REDUCTION;
	  return;
	}
    }

  /* Perform general partition disqualification for builtins.  */
  if (volatiles_p
      || !flag_tree_loop_distribute_patterns)
    return;

  nb_iter = number_of_exit_cond_executions (loop);
  if (!nb_iter || nb_iter == chrec_dont_know)
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
rdg_build_partitions (struct graph *rdg, vec<rdgc> components,
		      vec<int> *other_stores,
		      vec<partition_t> *partitions, bitmap processed)
{
  int i;
  rdgc x;
  partition_t partition = partition_alloc (NULL, NULL);

  FOR_EACH_VEC_ELT (components, i, x)
    {
      partition_t np;
      int v = x->vertices[0];

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

	  partitions->safe_push (partition);
	  partition = partition_alloc (NULL, NULL);
	}
    }

  /* Add the nodes from the RDG that were not marked as processed, and
     that are used outside the current loop.  These are scalar
     computations that are not yet part of previous partitions.  */
  for (i = 0; i < rdg->n_vertices; i++)
    if (!bitmap_bit_p (processed, i)
	&& rdg_defs_used_in_other_loops_p (rdg, i))
      other_stores->safe_push (i);

  /* If there are still statements left in the OTHER_STORES array,
     create other components and partitions with these stores and
     their dependences.  */
  if (other_stores->length () > 0)
    {
      vec<rdgc> comps;
      comps.create (3);
      vec<int> foo;
      foo.create (3);

      rdg_build_components (rdg, *other_stores, &comps);
      rdg_build_partitions (rdg, comps, &foo, partitions, processed);

      foo.release ();
      free_rdg_components (comps);
    }

  /* If there is something left in the last partition, save it.  */
  if (bitmap_count_bits (partition->stmts) > 0)
    partitions->safe_push (partition);
  else
    partition_free (partition);
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

/* Generate code from STARTING_VERTICES in RDG.  Returns the number of
   distributed loops.  */

static int
ldist_gen (struct loop *loop, struct graph *rdg,
	   vec<int> starting_vertices)
{
  int i, nbp;
  vec<rdgc> components;
  components.create (3);
  vec<partition_t> partitions;
  partitions.create (3);
  vec<int> other_stores;
  other_stores.create (3);
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

	  FOR_EACH_VEC_ELT (starting_vertices, j, v)
	    if (i == v)
	      {
		found = true;
		break;
	      }

	  if (!found)
	    other_stores.safe_push (i);
	}
    }

  mark_nodes_having_upstream_mem_writes (rdg);
  rdg_build_components (rdg, starting_vertices, &components);
  rdg_build_partitions (rdg, components, &other_stores, &partitions,
			processed);
  BITMAP_FREE (processed);

  any_builtin = false;
  FOR_EACH_VEC_ELT (partitions, i, partition)
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
	  for (; partitions.iterate (i, &into); ++i)
	    if (!partition_builtin_p (into))
	      break;
	  for (++i; partitions.iterate (i, &partition); ++i)
	    if (!partition_builtin_p (partition))
	      {
		bitmap_ior_into (into->stmts, partition->stmts);
		if (partition->kind == PKIND_REDUCTION)
		  into->kind = PKIND_REDUCTION;
		partitions.ordered_remove (i);
		partition_free (partition);
		i--;
	      }
	    else
	      break;
	}
      while ((unsigned) i < partitions.length ());
    }
  else
    {
      partition_t into;
      int j;
      for (i = 0; partitions.iterate (i, &into); ++i)
	{
	  if (partition_builtin_p (into))
	    continue;
	  for (j = i + 1;
	       partitions.iterate (j, &partition); ++j)
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
		  if (partition->kind == PKIND_REDUCTION)
		    into->kind = PKIND_REDUCTION;
		  partitions.ordered_remove (j);
		  partition_free (partition);
		  j--;
		}
	    }
	}
    }

  /* Fuse all reduction partitions into the last.  */
  if (partitions.length () > 1)
    {
      partition_t into = partitions.last ();
      for (i = partitions.length () - 2; i >= 0; --i)
	{
	  partition_t what = partitions[i];
	  if (what->kind == PKIND_REDUCTION)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "fusing partitions\n");
		  dump_bitmap (dump_file, into->stmts);
		  dump_bitmap (dump_file, what->stmts);
		  fprintf (dump_file, "because the latter has reductions\n");
		}
	      bitmap_ior_into (into->stmts, what->stmts);
	      into->kind = PKIND_REDUCTION;
	      partitions.ordered_remove (i);
	      partition_free (what);
	    }
	}
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
    generate_code_for_partition (loop, partition, i < nbp - 1);

 ldist_done:

  BITMAP_FREE (remaining_stmts);
  BITMAP_FREE (upstream_mem_writes);

  FOR_EACH_VEC_ELT (partitions, i, partition)
    partition_free (partition);

  other_stores.release ();
  partitions.release ();
  free_rdg_components (components);
  return nbp;
}

/* Distributes the code from LOOP in such a way that producer
   statements are placed before consumer statements.  When STMTS is
   NULL, performs the maximal distribution, if STMTS is not NULL,
   tries to separate only these statements from the LOOP's body.
   Returns the number of distributed loops.  */

static int
distribute_loop (struct loop *loop, vec<gimple> stmts)
{
  int res = 0;
  struct graph *rdg;
  gimple s;
  unsigned i;
  vec<int> vertices;

  rdg = build_rdg (loop);
  if (!rdg)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "FIXME: Loop %d not distributed: failed to build the RDG.\n",
		 loop->num);

      return res;
    }

  vertices.create (3);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg (dump_file, rdg);

  FOR_EACH_VEC_ELT (stmts, i, s)
    {
      int v = rdg_vertex_for_stmt (rdg, s);

      if (v >= 0)
	{
	  vertices.safe_push (v);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "ldist asked to generate code for vertex %d\n", v);
	}
    }

  res = ldist_gen (loop, rdg, vertices);
  vertices.release ();
  free_rdg (rdg);
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
      vec<gimple> work_list = vNULL;
      basic_block *bbs;
      int num = loop->num;
      int nb_generated_loops = 0;
      unsigned int i;

      /* If the loop doesn't have a single exit we will fail anyway,
	 so do that early.  */
      if (!single_exit (loop))
	continue;

      /* Only optimize hot loops.  */
      if (!optimize_loop_for_speed_p (loop))
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
	      /* Distribute stmts which have defs that are used outside of
	         the loop.  */
	      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
		;
	      /* Otherwise only distribute stores for now.  */
	      else if (!gimple_assign_single_p (stmt)
		       || is_gimple_reg (gimple_assign_lhs (stmt)))
		continue;

	      work_list.safe_push (stmt);
	    }
	}
      free (bbs);

      if (work_list.length () > 0)
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

      work_list.release ();
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
  pass_loop_distribution(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_loop_distribution, ctxt)
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
