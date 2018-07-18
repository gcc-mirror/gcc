/* Loop distribution.
   Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

   Loop distribution is the dual of loop fusion.  It separates statements
   of a loop (or loop nest) into multiple loops (or loop nests) with the
   same loop header.  The major goal is to separate statements which may
   be vectorized from those that can't.  This pass implements distribution
   in the following steps:

     1) Seed partitions with specific type statements.  For now we support
	two types seed statements: statement defining variable used outside
	of loop; statement storing to memory.
     2) Build reduced dependence graph (RDG) for loop to be distributed.
	The vertices (RDG:V) model all statements in the loop and the edges
	(RDG:E) model flow and control dependencies between statements.
     3) Apart from RDG, compute data dependencies between memory references.
     4) Starting from seed statement, build up partition by adding depended
	statements according to RDG's dependence information.  Partition is
	classified as parallel type if it can be executed paralleled; or as
	sequential type if it can't.  Parallel type partition is further
	classified as different builtin kinds if it can be implemented as
	builtin function calls.
     5) Build partition dependence graph (PG) based on data dependencies.
	The vertices (PG:V) model all partitions and the edges (PG:E) model
	all data dependencies between every partitions pair.  In general,
	data dependence is either compilation time known or unknown.  In C
	family languages, there exists quite amount compilation time unknown
	dependencies because of possible alias relation of data references.
	We categorize PG's edge to two types: "true" edge that represents
	compilation time known data dependencies; "alias" edge for all other
	data dependencies.
     6) Traverse subgraph of PG as if all "alias" edges don't exist.  Merge
	partitions in each strong connected component (SCC) correspondingly.
	Build new PG for merged partitions.
     7) Traverse PG again and this time with both "true" and "alias" edges
	included.  We try to break SCCs by removing some edges.  Because
	SCCs by "true" edges are all fused in step 6), we can break SCCs
	by removing some "alias" edges.  It's NP-hard to choose optimal
	edge set, fortunately simple approximation is good enough for us
	given the small problem scale.
     8) Collect all data dependencies of the removed "alias" edges.  Create
	runtime alias checks for collected data dependencies.
     9) Version loop under the condition of runtime alias checks.  Given
	loop distribution generally introduces additional overhead, it is
	only useful if vectorization is achieved in distributed loop.  We
	version loop with internal function call IFN_LOOP_DIST_ALIAS.  If
	no distributed loop can be vectorized, we simply remove distributed
	loops and recover to the original one.

   TODO:
     1) We only distribute innermost two-level loop nest now.  We should
	extend it for arbitrary loop nests in the future.
     2) We only fuse partitions in SCC now.  A better fusion algorithm is
	desired to minimize loop overhead, maximize parallelism and maximize
	data reuse.  */

#include "config.h"
#define INCLUDE_ALGORITHM /* stable_sort */
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "stor-layout.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-vectorizer.h"


#define MAX_DATAREFS_NUM \
	((unsigned) PARAM_VALUE (PARAM_LOOP_MAX_DATAREFS_FOR_DATADEPS))

/* Threshold controlling number of distributed partitions.  Given it may
   be unnecessary if a memory stream cost model is invented in the future,
   we define it as a temporary macro, rather than a parameter.  */
#define NUM_PARTITION_THRESHOLD (4)

/* Hashtable helpers.  */

struct ddr_hasher : nofree_ptr_hash <struct data_dependence_relation>
{
  static inline hashval_t hash (const data_dependence_relation *);
  static inline bool equal (const data_dependence_relation *,
			    const data_dependence_relation *);
};

/* Hash function for data dependence.  */

inline hashval_t
ddr_hasher::hash (const data_dependence_relation *ddr)
{
  inchash::hash h;
  h.add_ptr (DDR_A (ddr));
  h.add_ptr (DDR_B (ddr));
  return h.end ();
}

/* Hash table equality function for data dependence.  */

inline bool
ddr_hasher::equal (const data_dependence_relation *ddr1,
		   const data_dependence_relation *ddr2)
{
  return (DDR_A (ddr1) == DDR_A (ddr2) && DDR_B (ddr1) == DDR_B (ddr2));
}

/* The loop (nest) to be distributed.  */
static vec<loop_p> loop_nest;

/* Vector of data references in the loop to be distributed.  */
static vec<data_reference_p> datarefs_vec;

/* Store index of data reference in aux field.  */
#define DR_INDEX(dr)      ((uintptr_t) (dr)->aux)

/* Hash table for data dependence relation in the loop to be distributed.  */
static hash_table<ddr_hasher> *ddrs_table;

/* A Reduced Dependence Graph (RDG) vertex representing a statement.  */
struct rdg_vertex
{
  /* The statement represented by this vertex.  */
  gimple *stmt;

  /* Vector of data-references in this statement.  */
  vec<data_reference_p> datarefs;

  /* True when the statement contains a write to memory.  */
  bool has_mem_write;

  /* True when the statement contains a read from memory.  */
  bool has_mem_reads;
};

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

struct rdg_edge
{
  /* Type of the dependence.  */
  enum rdg_dep_type type;
};

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
#ifdef HAVE_POPEN
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
rdg_vertex_for_stmt (struct graph *rdg ATTRIBUTE_UNUSED, gimple *stmt)
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
      basic_block cond_bb = cd->get_edge_src (edge_n);
      gimple *stmt = last_stmt (cond_bb);
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
create_rdg_cd_edges (struct graph *rdg, control_dependences *cd, loop_p loop)
{
  int i;

  for (i = 0; i < rdg->n_vertices; i++)
    {
      gimple *stmt = RDG_STMT (rdg, i);
      if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->preds)
	    if (flow_bb_inside_loop_p (loop, e->src))
	      create_edge_for_control_dependence (rdg, e->src, i, cd);
	}
      else
	create_edge_for_control_dependence (rdg, gimple_bb (stmt), i, cd);
    }
}

/* Build the vertices of the reduced dependence graph RDG.  Return false
   if that failed.  */

static bool
create_rdg_vertices (struct graph *rdg, vec<gimple *> stmts, loop_p loop)
{
  int i;
  gimple *stmt;

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

      unsigned drp = datarefs_vec.length ();
      if (!find_data_references_in_stmt (loop, stmt, &datarefs_vec))
	return false;
      for (unsigned j = drp; j < datarefs_vec.length (); ++j)
	{
	  data_reference_p dr = datarefs_vec[j];
	  if (DR_IS_READ (dr))
	    RDGV_HAS_MEM_READS (v) = true;
	  else
	    RDGV_HAS_MEM_WRITE (v) = true;
	  RDGV_DATAREFS (v).safe_push (dr);
	}
    }
  return true;
}

/* Array mapping basic block's index to its topological order.  */
static int *bb_top_order_index;
/* And size of the array.  */
static int bb_top_order_index_size;

/* If X has a smaller topological sort number than Y, returns -1;
   if greater, returns 1.  */

static int
bb_top_order_cmp (const void *x, const void *y)
{
  basic_block bb1 = *(const basic_block *) x;
  basic_block bb2 = *(const basic_block *) y;

  gcc_assert (bb1->index < bb_top_order_index_size
	      && bb2->index < bb_top_order_index_size);
  gcc_assert (bb1 == bb2
	      || bb_top_order_index[bb1->index]
		 != bb_top_order_index[bb2->index]);

  return (bb_top_order_index[bb1->index] - bb_top_order_index[bb2->index]);
}

/* Initialize STMTS with all the statements of LOOP.  We use topological
   order to discover all statements.  The order is important because
   generate_loops_for_partition is using the same traversal for identifying
   statements in loop copies.  */

static void
stmts_from_loop (struct loop *loop, vec<gimple *> *stmts)
{
  unsigned int i;
  basic_block *bbs = get_loop_body_in_custom_order (loop, bb_top_order_cmp);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	if (!virtual_operand_p (gimple_phi_result (bsi.phi ())))
	  stmts->safe_push (bsi.phi ());

      for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
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
	  (RDGV_DATAREFS (v)).release ();
	  free (v->data);
	}
    }

  free_graph (rdg);
}

/* Build the Reduced Dependence Graph (RDG) with one vertex per statement of
   LOOP, and one edge per flow dependence or control dependence from control
   dependence CD.  During visiting each statement, data references are also
   collected and recorded in global data DATAREFS_VEC.  */

static struct graph *
build_rdg (struct loop *loop, control_dependences *cd)
{
  struct graph *rdg;

  /* Create the RDG vertices from the stmts of the loop nest.  */
  auto_vec<gimple *, 10> stmts;
  stmts_from_loop (loop, &stmts);
  rdg = new_graph (stmts.length ());
  if (!create_rdg_vertices (rdg, stmts, loop))
    {
      free_rdg (rdg);
      return NULL;
    }
  stmts.release ();

  create_rdg_flow_edges (rdg);
  if (cd)
    create_rdg_cd_edges (rdg, cd, loop);

  return rdg;
}


/* Kind of distributed loop.  */
enum partition_kind {
    PKIND_NORMAL,
    /* Partial memset stands for a paritition can be distributed into a loop
       of memset calls, rather than a single memset call.  It's handled just
       like a normal parition, i.e, distributed as separate loop, no memset
       call is generated.

       Note: This is a hacking fix trying to distribute ZERO-ing stmt in a
       loop nest as deep as possible.  As a result, parloop achieves better
       parallelization by parallelizing deeper loop nest.  This hack should
       be unnecessary and removed once distributed memset can be understood
       and analyzed in data reference analysis.  See PR82604 for more.  */
    PKIND_PARTIAL_MEMSET,
    PKIND_MEMSET, PKIND_MEMCPY, PKIND_MEMMOVE
};

/* Type of distributed loop.  */
enum partition_type {
    /* The distributed loop can be executed parallelly.  */
    PTYPE_PARALLEL = 0,
    /* The distributed loop has to be executed sequentially.  */
    PTYPE_SEQUENTIAL
};

/* Builtin info for loop distribution.  */
struct builtin_info
{
  /* data-references a kind != PKIND_NORMAL partition is about.  */
  data_reference_p dst_dr;
  data_reference_p src_dr;
  /* Base address and size of memory objects operated by the builtin.  Note
     both dest and source memory objects must have the same size.  */
  tree dst_base;
  tree src_base;
  tree size;
  /* Base and offset part of dst_base after stripping constant offset.  This
     is only used in memset builtin distribution for now.  */
  tree dst_base_base;
  unsigned HOST_WIDE_INT dst_base_offset;
};

/* Partition for loop distribution.  */
struct partition
{
  /* Statements of the partition.  */
  bitmap stmts;
  /* True if the partition defines variable which is used outside of loop.  */
  bool reduction_p;
  enum partition_kind kind;
  enum partition_type type;
  /* Data references in the partition.  */
  bitmap datarefs;
  /* Information of builtin parition.  */
  struct builtin_info *builtin;
};


/* Allocate and initialize a partition from BITMAP.  */

static partition *
partition_alloc (void)
{
  partition *partition = XCNEW (struct partition);
  partition->stmts = BITMAP_ALLOC (NULL);
  partition->reduction_p = false;
  partition->kind = PKIND_NORMAL;
  partition->datarefs = BITMAP_ALLOC (NULL);
  return partition;
}

/* Free PARTITION.  */

static void
partition_free (partition *partition)
{
  BITMAP_FREE (partition->stmts);
  BITMAP_FREE (partition->datarefs);
  if (partition->builtin)
    free (partition->builtin);

  free (partition);
}

/* Returns true if the partition can be generated as a builtin.  */

static bool
partition_builtin_p (partition *partition)
{
  return partition->kind > PKIND_PARTIAL_MEMSET;
}

/* Returns true if the partition contains a reduction.  */

static bool
partition_reduction_p (partition *partition)
{
  return partition->reduction_p;
}

/* Partitions are fused because of different reasons.  */
enum fuse_type
{
  FUSE_NON_BUILTIN = 0,
  FUSE_REDUCTION = 1,
  FUSE_SHARE_REF = 2,
  FUSE_SAME_SCC = 3,
  FUSE_FINALIZE = 4
};

/* Description on different fusing reason.  */
static const char *fuse_message[] = {
  "they are non-builtins",
  "they have reductions",
  "they have shared memory refs",
  "they are in the same dependence scc",
  "there is no point to distribute loop"};

static void
update_type_for_merge (struct graph *, partition *, partition *);

/* Merge PARTITION into the partition DEST.  RDG is the reduced dependence
   graph and we update type for result partition if it is non-NULL.  */

static void
partition_merge_into (struct graph *rdg, partition *dest,
		      partition *partition, enum fuse_type ft)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Fuse partitions because %s:\n", fuse_message[ft]);
      fprintf (dump_file, "  Part 1: ");
      dump_bitmap (dump_file, dest->stmts);
      fprintf (dump_file, "  Part 2: ");
      dump_bitmap (dump_file, partition->stmts);
    }

  dest->kind = PKIND_NORMAL;
  if (dest->type == PTYPE_PARALLEL)
    dest->type = partition->type;

  bitmap_ior_into (dest->stmts, partition->stmts);
  if (partition_reduction_p (partition))
    dest->reduction_p = true;

  /* Further check if any data dependence prevents us from executing the
     new partition parallelly.  */
  if (dest->type == PTYPE_PARALLEL && rdg != NULL)
    update_type_for_merge (rdg, dest, partition);

  bitmap_ior_into (dest->datarefs, partition->datarefs);
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
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;

      basic_block use_bb = gimple_bb (USE_STMT (use_p));
      if (!flow_bb_inside_loop_p (loop, use_bb))
	return true;
    }

  return false;
}

/* Returns true when STMT defines a scalar variable used after the
   loop LOOP.  */

static bool
stmt_has_scalar_dependences_outside_loop (loop_p loop, gimple *stmt)
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
generate_loops_for_partition (struct loop *loop, partition *partition,
			      bool copy_p)
{
  unsigned i;
  basic_block *bbs;

  if (copy_p)
    {
      int orig_loop_num = loop->orig_loop_num;
      loop = copy_loop_before (loop);
      gcc_assert (loop != NULL);
      loop->orig_loop_num = orig_loop_num;
      create_preheader (loop, CP_SIMPLE_PREHEADERS);
      create_bb_after_loop (loop);
    }
  else
    {
      /* Origin number is set to the new versioned loop's num.  */
      gcc_assert (loop->orig_loop_num != loop->num);
    }

  /* Remove stmts not in the PARTITION bitmap.  */
  bbs = get_loop_body_in_dom_order (loop);

  if (MAY_HAVE_DEBUG_BIND_STMTS)
    for (i = 0; i < loop->num_nodes; i++)
      {
	basic_block bb = bbs[i];

	for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	     gsi_next (&bsi))
	  {
	    gphi *phi = bsi.phi ();
	    if (!virtual_operand_p (gimple_phi_result (phi))
		&& !bitmap_bit_p (partition->stmts, gimple_uid (phi)))
	      reset_debug_uses (phi);
	  }

	for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple *stmt = gsi_stmt (bsi);
	    if (gimple_code (stmt) != GIMPLE_LABEL
		&& !is_gimple_debug (stmt)
		&& !bitmap_bit_p (partition->stmts, gimple_uid (stmt)))
	      reset_debug_uses (stmt);
	  }
      }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      edge inner_exit = NULL;

      if (loop != bb->loop_father)
	inner_exit = single_exit (bb->loop_father);

      for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);)
	{
	  gphi *phi = bsi.phi ();
	  if (!virtual_operand_p (gimple_phi_result (phi))
	      && !bitmap_bit_p (partition->stmts, gimple_uid (phi)))
	    remove_phi_node (&bsi, true);
	  else
	    gsi_next (&bsi);
	}

      for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);)
	{
	  gimple *stmt = gsi_stmt (bsi);
	  if (gimple_code (stmt) != GIMPLE_LABEL
	      && !is_gimple_debug (stmt)
	      && !bitmap_bit_p (partition->stmts, gimple_uid (stmt)))
	    {
	      /* In distribution of loop nest, if bb is inner loop's exit_bb,
		 we choose its exit edge/path in order to avoid generating
		 infinite loop.  For all other cases, we choose an arbitrary
		 path through the empty CFG part that this unnecessary
		 control stmt controls.  */
	      if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
		{
		  if (inner_exit && inner_exit->flags & EDGE_TRUE_VALUE)
		    gimple_cond_make_true (cond_stmt);
		  else
		    gimple_cond_make_false (cond_stmt);
		  update_stmt (stmt);
		}
	      else if (gimple_code (stmt) == GIMPLE_SWITCH)
		{
		  gswitch *switch_stmt = as_a <gswitch *> (stmt);
		  gimple_switch_set_index
		      (switch_stmt, CASE_LOW (gimple_switch_label (switch_stmt, 1)));
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
      || (TREE_CODE (val) == CONSTRUCTOR
          && !TREE_CLOBBER_P (val)
          && CONSTRUCTOR_NELTS (val) == 0))
    return 0;

  if (real_zerop (val))
    {
      /* Only return 0 for +0.0, not for -0.0, which doesn't have
	 an all bytes same memory representation.  Don't transform
	 -0.0 stores into +0.0 even for !HONOR_SIGNED_ZEROS.  */
      switch (TREE_CODE (val))
	{
	case REAL_CST:
	  if (!real_isneg (TREE_REAL_CST_PTR (val)))
	    return 0;
	  break;
	case COMPLEX_CST:
	  if (!const_with_all_bytes_same (TREE_REALPART (val))
	      && !const_with_all_bytes_same (TREE_IMAGPART (val)))
	    return 0;
	  break;
	case VECTOR_CST:
	  {
	    unsigned int count = vector_cst_encoded_nelts (val);
	    unsigned int j;
	    for (j = 0; j < count; ++j)
	      if (const_with_all_bytes_same (VECTOR_CST_ENCODED_ELT (val, j)))
		break;
	    if (j == count)
	      return 0;
	    break;
	  }
	default:
	  break;
	}
    }

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
generate_memset_builtin (struct loop *loop, partition *partition)
{
  gimple_stmt_iterator gsi;
  tree mem, fn, nb_bytes;
  tree val;
  struct builtin_info *builtin = partition->builtin;
  gimple *fn_call;

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = builtin->size;
  nb_bytes = force_gimple_operand_gsi (&gsi, nb_bytes, true, NULL_TREE,
				       false, GSI_CONTINUE_LINKING);
  mem = builtin->dst_base;
  mem = force_gimple_operand_gsi (&gsi, mem, true, NULL_TREE,
				  false, GSI_CONTINUE_LINKING);

  /* This exactly matches the pattern recognition in classify_partition.  */
  val = gimple_assign_rhs1 (DR_STMT (builtin->dst_dr));
  /* Handle constants like 0x15151515 and similarly
     floating point constants etc. where all bytes are the same.  */
  int bytev = const_with_all_bytes_same (val);
  if (bytev != -1)
    val = build_int_cst (integer_type_node, bytev);
  else if (TREE_CODE (val) == INTEGER_CST)
    val = fold_convert (integer_type_node, val);
  else if (!useless_type_conversion_p (integer_type_node, TREE_TYPE (val)))
    {
      tree tem = make_ssa_name (integer_type_node);
      gimple *cstmt = gimple_build_assign (tem, NOP_EXPR, val);
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
generate_memcpy_builtin (struct loop *loop, partition *partition)
{
  gimple_stmt_iterator gsi;
  gimple *fn_call;
  tree dest, src, fn, nb_bytes;
  enum built_in_function kind;
  struct builtin_info *builtin = partition->builtin;

  /* The new statements will be placed before LOOP.  */
  gsi = gsi_last_bb (loop_preheader_edge (loop)->src);

  nb_bytes = builtin->size;
  nb_bytes = force_gimple_operand_gsi (&gsi, nb_bytes, true, NULL_TREE,
				       false, GSI_CONTINUE_LINKING);
  dest = builtin->dst_base;
  src = builtin->src_base;
  if (partition->kind == PKIND_MEMCPY
      || ! ptr_derefs_may_alias_p (dest, src))
    kind = BUILT_IN_MEMCPY;
  else
    kind = BUILT_IN_MEMMOVE;

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

  i = nbbs;
  do
    {
      /* We have made sure to not leave any dangling uses of SSA
         names defined in the loop.  With the exception of virtuals.
	 Make sure we replace all uses of virtual defs that will remain
	 outside of the loop with the bare symbol as delete_basic_block
	 will release them.  */
      --i;
      for (gphi_iterator gsi = gsi_start_phis (bbs[i]); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    mark_virtual_phi_result_for_renaming (phi);
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree vdef = gimple_vdef (stmt);
	  if (vdef && TREE_CODE (vdef) == SSA_NAME)
	    mark_virtual_operand_for_renaming (vdef);
	}
      delete_basic_block (bbs[i]);
    }
  while (i != 0);

  free (bbs);

  set_immediate_dominator (CDI_DOMINATORS, dest,
			   recompute_dominator (CDI_DOMINATORS, dest));
}

/* Generates code for PARTITION.  Return whether LOOP needs to be destroyed.  */

static bool 
generate_code_for_partition (struct loop *loop,
			     partition *partition, bool copy_p)
{
  switch (partition->kind)
    {
    case PKIND_NORMAL:
    case PKIND_PARTIAL_MEMSET:
      /* Reductions all have to be in the last partition.  */
      gcc_assert (!partition_reduction_p (partition)
		  || !copy_p);
      generate_loops_for_partition (loop, partition, copy_p);
      return false;

    case PKIND_MEMSET:
      generate_memset_builtin (loop, partition);
      break;

    case PKIND_MEMCPY:
    case PKIND_MEMMOVE:
      generate_memcpy_builtin (loop, partition);
      break;

    default:
      gcc_unreachable ();
    }

  /* Common tail for partitions we turn into a call.  If this was the last
     partition for which we generate code, we have to destroy the loop.  */
  if (!copy_p)
    return true;
  return false;
}

/* Return data dependence relation for data references A and B.  The two
   data references must be in lexicographic order wrto reduced dependence
   graph RDG.  We firstly try to find ddr from global ddr hash table.  If
   it doesn't exist, compute the ddr and cache it.  */

static data_dependence_relation *
get_data_dependence (struct graph *rdg, data_reference_p a, data_reference_p b)
{
  struct data_dependence_relation ent, **slot;
  struct data_dependence_relation *ddr;

  gcc_assert (DR_IS_WRITE (a) || DR_IS_WRITE (b));
  gcc_assert (rdg_vertex_for_stmt (rdg, DR_STMT (a))
	      <= rdg_vertex_for_stmt (rdg, DR_STMT (b)));
  ent.a = a;
  ent.b = b;
  slot = ddrs_table->find_slot (&ent, INSERT);
  if (*slot == NULL)
    {
      ddr = initialize_data_dependence_relation (a, b, loop_nest);
      compute_affine_dependence (ddr, loop_nest[0]);
      *slot = ddr;
    }

  return *slot;
}

/* In reduced dependence graph RDG for loop distribution, return true if
   dependence between references DR1 and DR2 leads to a dependence cycle
   and such dependence cycle can't be resolved by runtime alias check.  */

static bool
data_dep_in_cycle_p (struct graph *rdg,
		     data_reference_p dr1, data_reference_p dr2)
{
  struct data_dependence_relation *ddr;

  /* Re-shuffle data-refs to be in topological order.  */
  if (rdg_vertex_for_stmt (rdg, DR_STMT (dr1))
      > rdg_vertex_for_stmt (rdg, DR_STMT (dr2)))
    std::swap (dr1, dr2);

  ddr = get_data_dependence (rdg, dr1, dr2);

  /* In case of no data dependence.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;
  /* For unknown data dependence or known data dependence which can't be
     expressed in classic distance vector, we check if it can be resolved
     by runtime alias check.  If yes, we still consider data dependence
     as won't introduce data dependence cycle.  */
  else if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
	   || DDR_NUM_DIST_VECTS (ddr) == 0)
    return !runtime_alias_check_p (ddr, NULL, true);
  else if (DDR_NUM_DIST_VECTS (ddr) > 1)
    return true;
  else if (DDR_REVERSED_P (ddr)
	   || lambda_vector_zerop (DDR_DIST_VECT (ddr, 0), 1))
    return false;

  return true;
}

/* Given reduced dependence graph RDG, PARTITION1 and PARTITION2, update
   PARTITION1's type after merging PARTITION2 into PARTITION1.  */

static void
update_type_for_merge (struct graph *rdg,
		       partition *partition1, partition *partition2)
{
  unsigned i, j;
  bitmap_iterator bi, bj;
  data_reference_p dr1, dr2;

  EXECUTE_IF_SET_IN_BITMAP (partition1->datarefs, 0, i, bi)
    {
      unsigned start = (partition1 == partition2) ? i + 1 : 0;

      dr1 = datarefs_vec[i];
      EXECUTE_IF_SET_IN_BITMAP (partition2->datarefs, start, j, bj)
	{
	  dr2 = datarefs_vec[j];
	  if (DR_IS_READ (dr1) && DR_IS_READ (dr2))
	    continue;

	  /* Partition can only be executed sequentially if there is any
	     data dependence cycle.  */
	  if (data_dep_in_cycle_p (rdg, dr1, dr2))
	    {
	      partition1->type = PTYPE_SEQUENTIAL;
	      return;
	    }
	}
    }
}

/* Returns a partition with all the statements needed for computing
   the vertex V of the RDG, also including the loop exit conditions.  */

static partition *
build_rdg_partition_for_vertex (struct graph *rdg, int v)
{
  partition *partition = partition_alloc ();
  auto_vec<int, 3> nodes;
  unsigned i, j;
  int x;
  data_reference_p dr;

  graphds_dfs (rdg, &v, 1, &nodes, false, NULL);

  FOR_EACH_VEC_ELT (nodes, i, x)
    {
      bitmap_set_bit (partition->stmts, x);

      for (j = 0; RDG_DATAREFS (rdg, x).iterate (j, &dr); ++j)
	{
	  unsigned idx = (unsigned) DR_INDEX (dr);
	  gcc_assert (idx < datarefs_vec.length ());

	  /* Partition can only be executed sequentially if there is any
	     unknown data reference.  */
	  if (!DR_BASE_ADDRESS (dr) || !DR_OFFSET (dr)
	      || !DR_INIT (dr) || !DR_STEP (dr))
	    partition->type = PTYPE_SEQUENTIAL;

	  bitmap_set_bit (partition->datarefs, idx);
	}
    }

  if (partition->type == PTYPE_SEQUENTIAL)
    return partition;

  /* Further check if any data dependence prevents us from executing the
     partition parallelly.  */
  update_type_for_merge (rdg, partition, partition);

  return partition;
}

/* Given PARTITION of LOOP and RDG, record single load/store data references
   for builtin partition in SRC_DR/DST_DR, return false if there is no such
   data references.  */

static bool
find_single_drs (struct loop *loop, struct graph *rdg, partition *partition,
		 data_reference_p *dst_dr, data_reference_p *src_dr)
{
  unsigned i;
  data_reference_p single_ld = NULL, single_st = NULL;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple *stmt = RDG_STMT (rdg, i);
      data_reference_p dr;

      if (gimple_code (stmt) == GIMPLE_PHI)
	continue;

      /* Any scalar stmts are ok.  */
      if (!gimple_vuse (stmt))
	continue;

      /* Otherwise just regular loads/stores.  */
      if (!gimple_assign_single_p (stmt))
	return false;

      /* But exactly one store and/or load.  */
      for (unsigned j = 0; RDG_DATAREFS (rdg, i).iterate (j, &dr); ++j)
	{
	  tree type = TREE_TYPE (DR_REF (dr));

	  /* The memset, memcpy and memmove library calls are only
	     able to deal with generic address space.  */
	  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (type)))
	    return false;

	  if (DR_IS_READ (dr))
	    {
	      if (single_ld != NULL)
		return false;
	      single_ld = dr;
	    }
	  else
	    {
	      if (single_st != NULL)
		return false;
	      single_st = dr;
	    }
	}
    }

  if (!single_st)
    return false;

  /* Bail out if this is a bitfield memory reference.  */
  if (TREE_CODE (DR_REF (single_st)) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (DR_REF (single_st), 1)))
    return false;

  /* Data reference must be executed exactly once per iteration of each
     loop in the loop nest.  We only need to check dominance information
     against the outermost one in a perfect loop nest because a bb can't
     dominate outermost loop's latch without dominating inner loop's.  */
  basic_block bb_st = gimple_bb (DR_STMT (single_st));
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb_st))
    return false;

  if (single_ld)
    {
      gimple *store = DR_STMT (single_st), *load = DR_STMT (single_ld);
      /* Direct aggregate copy or via an SSA name temporary.  */
      if (load != store
	  && gimple_assign_lhs (load) != gimple_assign_rhs1 (store))
	return false;

      /* Bail out if this is a bitfield memory reference.  */
      if (TREE_CODE (DR_REF (single_ld)) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (DR_REF (single_ld), 1)))
	return false;

      /* Load and store must be in the same loop nest.  */
      basic_block bb_ld = gimple_bb (DR_STMT (single_ld));
      if (bb_st->loop_father != bb_ld->loop_father)
	return false;

      /* Data reference must be executed exactly once per iteration.
	 Same as single_st, we only need to check against the outermost
	 loop.  */
      if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb_ld))
	return false;

      edge e = single_exit (bb_st->loop_father);
      bool dom_ld = dominated_by_p (CDI_DOMINATORS, e->src, bb_ld);
      bool dom_st = dominated_by_p (CDI_DOMINATORS, e->src, bb_st);
      if (dom_ld != dom_st)
	return false;
    }

  *src_dr = single_ld;
  *dst_dr = single_st;
  return true;
}

/* Given data reference DR in LOOP_NEST, this function checks the enclosing
   loops from inner to outer to see if loop's step equals to access size at
   each level of loop.  Return 2 if we can prove this at all level loops;
   record access base and size in BASE and SIZE; save loop's step at each
   level of loop in STEPS if it is not null.  For example:

     int arr[100][100][100];
     for (i = 0; i < 100; i++)       ;steps[2] = 40000
       for (j = 100; j > 0; j--)     ;steps[1] = -400
	 for (k = 0; k < 100; k++)   ;steps[0] = 4
	   arr[i][j - 1][k] = 0;     ;base = &arr, size = 4000000

   Return 1 if we can prove the equality at the innermost loop, but not all
   level loops.  In this case, no information is recorded.

   Return 0 if no equality can be proven at any level loops.  */

static int
compute_access_range (loop_p loop_nest, data_reference_p dr, tree *base,
		      tree *size, vec<tree> *steps = NULL)
{
  location_t loc = gimple_location (DR_STMT (dr));
  basic_block bb = gimple_bb (DR_STMT (dr));
  struct loop *loop = bb->loop_father;
  tree ref = DR_REF (dr);
  tree access_base = build_fold_addr_expr (ref);
  tree access_size = TYPE_SIZE_UNIT (TREE_TYPE (ref));
  int res = 0;

  do {
      tree scev_fn = analyze_scalar_evolution (loop, access_base);
      if (TREE_CODE (scev_fn) != POLYNOMIAL_CHREC)
	return res;

      access_base = CHREC_LEFT (scev_fn);
      if (tree_contains_chrecs (access_base, NULL))
	return res;

      tree scev_step = CHREC_RIGHT (scev_fn);
      /* Only support constant steps.  */
      if (TREE_CODE (scev_step) != INTEGER_CST)
	return res;

      enum ev_direction access_dir = scev_direction (scev_fn);
      if (access_dir == EV_DIR_UNKNOWN)
	return res;

      if (steps != NULL)
	steps->safe_push (scev_step);

      scev_step = fold_convert_loc (loc, sizetype, scev_step);
      /* Compute absolute value of scev step.  */
      if (access_dir == EV_DIR_DECREASES)
	scev_step = fold_build1_loc (loc, NEGATE_EXPR, sizetype, scev_step);

      /* At each level of loop, scev step must equal to access size.  In other
	 words, DR must access consecutive memory between loop iterations.  */
      if (!operand_equal_p (scev_step, access_size, 0))
	return res;

      /* Access stride can be computed for data reference at least for the
	 innermost loop.  */
      res = 1;

      /* Compute DR's execution times in loop.  */
      tree niters = number_of_latch_executions (loop);
      niters = fold_convert_loc (loc, sizetype, niters);
      if (dominated_by_p (CDI_DOMINATORS, single_exit (loop)->src, bb))
	niters = size_binop_loc (loc, PLUS_EXPR, niters, size_one_node);

      /* Compute DR's overall access size in loop.  */
      access_size = fold_build2_loc (loc, MULT_EXPR, sizetype,
				     niters, scev_step);
      /* Adjust base address in case of negative step.  */
      if (access_dir == EV_DIR_DECREASES)
	{
	  tree adj = fold_build2_loc (loc, MINUS_EXPR, sizetype,
				      scev_step, access_size);
	  access_base = fold_build_pointer_plus_loc (loc, access_base, adj);
	}
  } while (loop != loop_nest && (loop = loop_outer (loop)) != NULL);

  *base = access_base;
  *size = access_size;
  /* Access stride can be computed for data reference at each level loop.  */
  return 2;
}

/* Allocate and return builtin struct.  Record information like DST_DR,
   SRC_DR, DST_BASE, SRC_BASE and SIZE in the allocated struct.  */

static struct builtin_info *
alloc_builtin (data_reference_p dst_dr, data_reference_p src_dr,
	       tree dst_base, tree src_base, tree size)
{
  struct builtin_info *builtin = XNEW (struct builtin_info);
  builtin->dst_dr = dst_dr;
  builtin->src_dr = src_dr;
  builtin->dst_base = dst_base;
  builtin->src_base = src_base;
  builtin->size = size;
  return builtin;
}

/* Given data reference DR in loop nest LOOP, classify if it forms builtin
   memset call.  */

static void
classify_builtin_st (loop_p loop, partition *partition, data_reference_p dr)
{
  gimple *stmt = DR_STMT (dr);
  tree base, size, rhs = gimple_assign_rhs1 (stmt);

  if (const_with_all_bytes_same (rhs) == -1
      && (!INTEGRAL_TYPE_P (TREE_TYPE (rhs))
	  || (TYPE_MODE (TREE_TYPE (rhs))
	      != TYPE_MODE (unsigned_char_type_node))))
    return;

  if (TREE_CODE (rhs) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (rhs)
      && flow_bb_inside_loop_p (loop, gimple_bb (SSA_NAME_DEF_STMT (rhs))))
    return;

  int res = compute_access_range (loop, dr, &base, &size);
  if (res == 0)
    return;
  if (res == 1)
    {
      partition->kind = PKIND_PARTIAL_MEMSET;
      return;
    }

  poly_uint64 base_offset;
  unsigned HOST_WIDE_INT const_base_offset;
  tree base_base = strip_offset (base, &base_offset);
  if (!base_offset.is_constant (&const_base_offset))
    return;

  struct builtin_info *builtin;
  builtin = alloc_builtin (dr, NULL, base, NULL_TREE, size);
  builtin->dst_base_base = base_base;
  builtin->dst_base_offset = const_base_offset;
  partition->builtin = builtin;
  partition->kind = PKIND_MEMSET;
}

/* Given data references DST_DR and SRC_DR in loop nest LOOP and RDG, classify
   if it forms builtin memcpy or memmove call.  */

static void
classify_builtin_ldst (loop_p loop, struct graph *rdg, partition *partition,
		       data_reference_p dst_dr, data_reference_p src_dr)
{
  tree base, size, src_base, src_size;
  auto_vec<tree> dst_steps, src_steps;

  /* Compute access range of both load and store.  */
  int res = compute_access_range (loop, dst_dr, &base, &size, &dst_steps);
  if (res != 2)
    return;
  res = compute_access_range (loop, src_dr, &src_base, &src_size, &src_steps);
  if (res != 2)
    return;

  /* They much have the same access size.  */
  if (!operand_equal_p (size, src_size, 0))
    return;

  /* Load and store in loop nest must access memory in the same way, i.e,
     their must have the same steps in each loop of the nest.  */
  if (dst_steps.length () != src_steps.length ())
    return;
  for (unsigned i = 0; i < dst_steps.length (); ++i)
    if (!operand_equal_p (dst_steps[i], src_steps[i], 0))
      return;

  /* Now check that if there is a dependence.  */
  ddr_p ddr = get_data_dependence (rdg, src_dr, dst_dr);

  /* Classify as memcpy if no dependence between load and store.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    {
      partition->builtin = alloc_builtin (dst_dr, src_dr, base, src_base, size);
      partition->kind = PKIND_MEMCPY;
      return;
    }

  /* Can't do memmove in case of unknown dependence or dependence without
     classical distance vector.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
      || DDR_NUM_DIST_VECTS (ddr) == 0)
    return;

  unsigned i;
  lambda_vector dist_v;
  int num_lev = (DDR_LOOP_NEST (ddr)).length ();
  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
    {
      unsigned dep_lev = dependence_level (dist_v, num_lev);
      /* Can't do memmove if load depends on store.  */
      if (dep_lev > 0 && dist_v[dep_lev - 1] > 0 && !DDR_REVERSED_P (ddr))
	return;
    }

  partition->builtin = alloc_builtin (dst_dr, src_dr, base, src_base, size);
  partition->kind = PKIND_MEMMOVE;
  return;
}

/* Classifies the builtin kind we can generate for PARTITION of RDG and LOOP.
   For the moment we detect memset, memcpy and memmove patterns.  Bitmap
   STMT_IN_ALL_PARTITIONS contains statements belonging to all partitions.  */

static void
classify_partition (loop_p loop, struct graph *rdg, partition *partition,
		    bitmap stmt_in_all_partitions)
{
  bitmap_iterator bi;
  unsigned i;
  data_reference_p single_ld = NULL, single_st = NULL;
  bool volatiles_p = false, has_reduction = false;

  EXECUTE_IF_SET_IN_BITMAP (partition->stmts, 0, i, bi)
    {
      gimple *stmt = RDG_STMT (rdg, i);

      if (gimple_has_volatile_ops (stmt))
	volatiles_p = true;

      /* If the stmt is not included by all partitions and there is uses
	 outside of the loop, then mark the partition as reduction.  */
      if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
	{
	  /* Due to limitation in the transform phase we have to fuse all
	     reduction partitions.  As a result, this could cancel valid
	     loop distribution especially for loop that induction variable
	     is used outside of loop.  To workaround this issue, we skip
	     marking partition as reudction if the reduction stmt belongs
	     to all partitions.  In such case, reduction will be computed
	     correctly no matter how partitions are fused/distributed.  */
	  if (!bitmap_bit_p (stmt_in_all_partitions, i))
	    {
	      partition->reduction_p = true;
	      return;
	    }
	  has_reduction = true;
	}
    }

  /* Perform general partition disqualification for builtins.  */
  if (volatiles_p
      /* Simple workaround to prevent classifying the partition as builtin
	 if it contains any use outside of loop.  */
      || has_reduction
      || !flag_tree_loop_distribute_patterns)
    return;

  /* Find single load/store data references for builtin partition.  */
  if (!find_single_drs (loop, rdg, partition, &single_st, &single_ld))
    return;

  /* Classify the builtin kind.  */
  if (single_ld == NULL)
    classify_builtin_st (loop, partition, single_st);
  else
    classify_builtin_ldst (loop, rdg, partition, single_st, single_ld);
}

/* Returns true when PARTITION1 and PARTITION2 access the same memory
   object in RDG.  */

static bool
share_memory_accesses (struct graph *rdg,
		       partition *partition1, partition *partition2)
{
  unsigned i, j;
  bitmap_iterator bi, bj;
  data_reference_p dr1, dr2;

  /* First check whether in the intersection of the two partitions are
     any loads or stores.  Common loads are the situation that happens
     most often.  */
  EXECUTE_IF_AND_IN_BITMAP (partition1->stmts, partition2->stmts, 0, i, bi)
    if (RDG_MEM_WRITE_STMT (rdg, i)
	|| RDG_MEM_READS_STMT (rdg, i))
      return true;

  /* Then check whether the two partitions access the same memory object.  */
  EXECUTE_IF_SET_IN_BITMAP (partition1->datarefs, 0, i, bi)
    {
      dr1 = datarefs_vec[i];

      if (!DR_BASE_ADDRESS (dr1)
	  || !DR_OFFSET (dr1) || !DR_INIT (dr1) || !DR_STEP (dr1))
	continue;

      EXECUTE_IF_SET_IN_BITMAP (partition2->datarefs, 0, j, bj)
	{
	  dr2 = datarefs_vec[j];

	  if (!DR_BASE_ADDRESS (dr2)
	      || !DR_OFFSET (dr2) || !DR_INIT (dr2) || !DR_STEP (dr2))
	    continue;

	  if (operand_equal_p (DR_BASE_ADDRESS (dr1), DR_BASE_ADDRESS (dr2), 0)
	      && operand_equal_p (DR_OFFSET (dr1), DR_OFFSET (dr2), 0)
	      && operand_equal_p (DR_INIT (dr1), DR_INIT (dr2), 0)
	      && operand_equal_p (DR_STEP (dr1), DR_STEP (dr2), 0))
	    return true;
	}
    }

  return false;
}

/* For each seed statement in STARTING_STMTS, this function builds
   partition for it by adding depended statements according to RDG.
   All partitions are recorded in PARTITIONS.  */

static void
rdg_build_partitions (struct graph *rdg,
		      vec<gimple *> starting_stmts,
		      vec<partition *> *partitions)
{
  auto_bitmap processed;
  int i;
  gimple *stmt;

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

      partition *partition = build_rdg_partition_for_vertex (rdg, v);
      bitmap_ior_into (processed, partition->stmts);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "ldist creates useful %s partition:\n",
		   partition->type == PTYPE_PARALLEL ? "parallel" : "sequent");
	  bitmap_print (dump_file, partition->stmts, "  ", "\n");
	}

      partitions->safe_push (partition);
    }

  /* All vertices should have been assigned to at least one partition now,
     other than vertices belonging to dead code.  */
}

/* Dump to FILE the PARTITIONS.  */

static void
dump_rdg_partitions (FILE *file, vec<partition *> partitions)
{
  int i;
  partition *partition;

  FOR_EACH_VEC_ELT (partitions, i, partition)
    debug_bitmap_file (file, partition->stmts);
}

/* Debug PARTITIONS.  */
extern void debug_rdg_partitions (vec<partition *> );

DEBUG_FUNCTION void
debug_rdg_partitions (vec<partition *> partitions)
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
number_of_rw_in_partition (struct graph *rdg, partition *partition)
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
			   vec<partition *> partitions)
{
  int i;
  partition *partition;
  int nrw = number_of_rw_in_rdg (rdg);

  FOR_EACH_VEC_ELT (partitions, i, partition)
    if (nrw == number_of_rw_in_partition (rdg, partition))
      return true;

  return false;
}

/* Compute partition dependence created by the data references in DRS1
   and DRS2, modify and return DIR according to that.  IF ALIAS_DDR is
   not NULL, we record dependence introduced by possible alias between
   two data references in ALIAS_DDRS; otherwise, we simply ignore such
   dependence as if it doesn't exist at all.  */

static int
pg_add_dependence_edges (struct graph *rdg, int dir,
			 bitmap drs1, bitmap drs2, vec<ddr_p> *alias_ddrs)
{
  unsigned i, j;
  bitmap_iterator bi, bj;
  data_reference_p dr1, dr2, saved_dr1;

  /* dependence direction - 0 is no dependence, -1 is back,
     1 is forth, 2 is both (we can stop then, merging will occur).  */
  EXECUTE_IF_SET_IN_BITMAP (drs1, 0, i, bi)
    {
      dr1 = datarefs_vec[i];

      EXECUTE_IF_SET_IN_BITMAP (drs2, 0, j, bj)
	{
	  int res, this_dir = 1;
	  ddr_p ddr;

	  dr2 = datarefs_vec[j];

	  /* Skip all <read, read> data dependence.  */
	  if (DR_IS_READ (dr1) && DR_IS_READ (dr2))
	    continue;

	  saved_dr1 = dr1;
	  /* Re-shuffle data-refs to be in topological order.  */
	  if (rdg_vertex_for_stmt (rdg, DR_STMT (dr1))
	      > rdg_vertex_for_stmt (rdg, DR_STMT (dr2)))
	    {
	      std::swap (dr1, dr2);
	      this_dir = -this_dir;
	    }
	  ddr = get_data_dependence (rdg, dr1, dr2);
	  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
	    {
	      this_dir = 0;
	      res = data_ref_compare_tree (DR_BASE_ADDRESS (dr1),
					   DR_BASE_ADDRESS (dr2));
	      /* Be conservative.  If data references are not well analyzed,
		 or the two data references have the same base address and
		 offset, add dependence and consider it alias to each other.
		 In other words, the dependence can not be resolved by
		 runtime alias check.  */
	      if (!DR_BASE_ADDRESS (dr1) || !DR_BASE_ADDRESS (dr2)
		  || !DR_OFFSET (dr1) || !DR_OFFSET (dr2)
		  || !DR_INIT (dr1) || !DR_INIT (dr2)
		  || !DR_STEP (dr1) || !tree_fits_uhwi_p (DR_STEP (dr1))
		  || !DR_STEP (dr2) || !tree_fits_uhwi_p (DR_STEP (dr2))
		  || res == 0)
		this_dir = 2;
	      /* Data dependence could be resolved by runtime alias check,
		 record it in ALIAS_DDRS.  */
	      else if (alias_ddrs != NULL)
		alias_ddrs->safe_push (ddr);
	      /* Or simply ignore it.  */
	    }
	  else if (DDR_ARE_DEPENDENT (ddr) == NULL_TREE)
	    {
	      if (DDR_REVERSED_P (ddr))
		this_dir = -this_dir;

	      /* Known dependences can still be unordered througout the
		 iteration space, see gcc.dg/tree-ssa/ldist-16.c.  */
	      if (DDR_NUM_DIST_VECTS (ddr) != 1)
		this_dir = 2;
	      /* If the overlap is exact preserve stmt order.  */
	      else if (lambda_vector_zerop (DDR_DIST_VECT (ddr, 0), 1))
		;
	      /* Else as the distance vector is lexicographic positive swap
		 the dependence direction.  */
	      else
		this_dir = -this_dir;
	    }
	  else
	    this_dir = 0;
	  if (this_dir == 2)
	    return 2;
	  else if (dir == 0)
	    dir = this_dir;
	  else if (this_dir != 0 && dir != this_dir)
	    return 2;
	  /* Shuffle "back" dr1.  */
	  dr1 = saved_dr1;
	}
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

/* Data attached to vertices of partition dependence graph.  */
struct pg_vdata
{
  /* ID of the corresponding partition.  */
  int id;
  /* The partition.  */
  struct partition *partition;
};

/* Data attached to edges of partition dependence graph.  */
struct pg_edata
{
  /* If the dependence edge can be resolved by runtime alias check,
     this vector contains data dependence relations for runtime alias
     check.  On the other hand, if the dependence edge is introduced
     because of compilation time known data dependence, this vector
     contains nothing.  */
  vec<ddr_p> alias_ddrs;
};

/* Callback data for traversing edges in graph.  */
struct pg_edge_callback_data
{
  /* Bitmap contains strong connected components should be merged.  */
  bitmap sccs_to_merge;
  /* Array constains component information for all vertices.  */
  int *vertices_component;
  /* Vector to record all data dependence relations which are needed
     to break strong connected components by runtime alias checks.  */
  vec<ddr_p> *alias_ddrs;
};

/* Initialize vertice's data for partition dependence graph PG with
   PARTITIONS.  */

static void
init_partition_graph_vertices (struct graph *pg,
			       vec<struct partition *> *partitions)
{
  int i;
  partition *partition;
  struct pg_vdata *data;

  for (i = 0; partitions->iterate (i, &partition); ++i)
    {
      data = new pg_vdata;
      pg->vertices[i].data = data;
      data->id = i;
      data->partition = partition;
    }
}

/* Add edge <I, J> to partition dependence graph PG.  Attach vector of data
   dependence relations to the EDGE if DDRS isn't NULL.  */

static void
add_partition_graph_edge (struct graph *pg, int i, int j, vec<ddr_p> *ddrs)
{
  struct graph_edge *e = add_edge (pg, i, j);

  /* If the edge is attached with data dependence relations, it means this
     dependence edge can be resolved by runtime alias checks.  */
  if (ddrs != NULL)
    {
      struct pg_edata *data = new pg_edata;

      gcc_assert (ddrs->length () > 0);
      e->data = data;
      data->alias_ddrs = vNULL;
      data->alias_ddrs.safe_splice (*ddrs);
    }
}

/* Callback function for graph travesal algorithm.  It returns true
   if edge E should skipped when traversing the graph.  */

static bool
pg_skip_alias_edge (struct graph_edge *e)
{
  struct pg_edata *data = (struct pg_edata *)e->data;
  return (data != NULL && data->alias_ddrs.length () > 0);
}

/* Callback function freeing data attached to edge E of graph.  */

static void
free_partition_graph_edata_cb (struct graph *, struct graph_edge *e, void *)
{
  if (e->data != NULL)
    {
      struct pg_edata *data = (struct pg_edata *)e->data;
      data->alias_ddrs.release ();
      delete data;
    }
}

/* Free data attached to vertice of partition dependence graph PG.  */

static void
free_partition_graph_vdata (struct graph *pg)
{
  int i;
  struct pg_vdata *data;

  for (i = 0; i < pg->n_vertices; ++i)
    {
      data = (struct pg_vdata *)pg->vertices[i].data;
      delete data;
    }
}

/* Build and return partition dependence graph for PARTITIONS.  RDG is
   reduced dependence graph for the loop to be distributed.  If IGNORE_ALIAS_P
   is true, data dependence caused by possible alias between references
   is ignored, as if it doesn't exist at all; otherwise all depdendences
   are considered.  */

static struct graph *
build_partition_graph (struct graph *rdg,
		       vec<struct partition *> *partitions,
		       bool ignore_alias_p)
{
  int i, j;
  struct partition *partition1, *partition2;
  graph *pg = new_graph (partitions->length ());
  auto_vec<ddr_p> alias_ddrs, *alias_ddrs_p;

  alias_ddrs_p = ignore_alias_p ? NULL : &alias_ddrs;

  init_partition_graph_vertices (pg, partitions);

  for (i = 0; partitions->iterate (i, &partition1); ++i)
    {
      for (j = i + 1; partitions->iterate (j, &partition2); ++j)
	{
	  /* dependence direction - 0 is no dependence, -1 is back,
	     1 is forth, 2 is both (we can stop then, merging will occur).  */
	  int dir = 0;

	  /* If the first partition has reduction, add back edge; if the
	     second partition has reduction, add forth edge.  This makes
	     sure that reduction partition will be sorted as the last one.  */
	  if (partition_reduction_p (partition1))
	    dir = -1;
	  else if (partition_reduction_p (partition2))
	    dir = 1;

	  /* Cleanup the temporary vector.  */
	  alias_ddrs.truncate (0);

	  dir = pg_add_dependence_edges (rdg, dir, partition1->datarefs,
					 partition2->datarefs, alias_ddrs_p);

	  /* Add edge to partition graph if there exists dependence.  There
	     are two types of edges.  One type edge is caused by compilation
	     time known dependence, this type can not be resolved by runtime
	     alias check.  The other type can be resolved by runtime alias
	     check.  */
	  if (dir == 1 || dir == 2
	      || alias_ddrs.length () > 0)
	    {
	      /* Attach data dependence relations to edge that can be resolved
		 by runtime alias check.  */
	      bool alias_edge_p = (dir != 1 && dir != 2);
	      add_partition_graph_edge (pg, i, j,
					(alias_edge_p) ? &alias_ddrs : NULL);
	    }
	  if (dir == -1 || dir == 2
	      || alias_ddrs.length () > 0)
	    {
	      /* Attach data dependence relations to edge that can be resolved
		 by runtime alias check.  */
	      bool alias_edge_p = (dir != -1 && dir != 2);
	      add_partition_graph_edge (pg, j, i,
					(alias_edge_p) ? &alias_ddrs : NULL);
	    }
	}
    }
  return pg;
}

/* Sort partitions in PG in descending post order and store them in
   PARTITIONS.  */

static void
sort_partitions_by_post_order (struct graph *pg,
			       vec<struct partition *> *partitions)
{
  int i;
  struct pg_vdata *data;

  /* Now order the remaining nodes in descending postorder.  */
  qsort (pg->vertices, pg->n_vertices, sizeof (vertex), pgcmp);
  partitions->truncate (0);
  for (i = 0; i < pg->n_vertices; ++i)
    {
      data = (struct pg_vdata *)pg->vertices[i].data;
      if (data->partition)
	partitions->safe_push (data->partition);
    }
}

/* Given reduced dependence graph RDG merge strong connected components
   of PARTITIONS.  If IGNORE_ALIAS_P is true, data dependence caused by
   possible alias between references is ignored, as if it doesn't exist
   at all; otherwise all depdendences are considered.  */

static void
merge_dep_scc_partitions (struct graph *rdg,
			  vec<struct partition *> *partitions,
			  bool ignore_alias_p)
{
  struct partition *partition1, *partition2;
  struct pg_vdata *data;
  graph *pg = build_partition_graph (rdg, partitions, ignore_alias_p);
  int i, j, num_sccs = graphds_scc (pg, NULL);

  /* Strong connected compoenent means dependence cycle, we cannot distribute
     them.  So fuse them together.  */
  if ((unsigned) num_sccs < partitions->length ())
    {
      for (i = 0; i < num_sccs; ++i)
	{
	  for (j = 0; partitions->iterate (j, &partition1); ++j)
	    if (pg->vertices[j].component == i)
	      break;
	  for (j = j + 1; partitions->iterate (j, &partition2); ++j)
	    if (pg->vertices[j].component == i)
	      {
		partition_merge_into (NULL, partition1,
				      partition2, FUSE_SAME_SCC);
		partition1->type = PTYPE_SEQUENTIAL;
		(*partitions)[j] = NULL;
		partition_free (partition2);
		data = (struct pg_vdata *)pg->vertices[j].data;
		data->partition = NULL;
	      }
	}
    }

  sort_partitions_by_post_order (pg, partitions);
  gcc_assert (partitions->length () == (unsigned)num_sccs);
  free_partition_graph_vdata (pg);
  free_graph (pg);
}

/* Callback function for traversing edge E in graph G.  DATA is private
   callback data.  */

static void
pg_collect_alias_ddrs (struct graph *g, struct graph_edge *e, void *data)
{
  int i, j, component;
  struct pg_edge_callback_data *cbdata;
  struct pg_edata *edata = (struct pg_edata *) e->data;

  /* If the edge doesn't have attached data dependence, it represents
     compilation time known dependences.  This type dependence cannot
     be resolved by runtime alias check.  */
  if (edata == NULL || edata->alias_ddrs.length () == 0)
    return;

  cbdata = (struct pg_edge_callback_data *) data;
  i = e->src;
  j = e->dest;
  component = cbdata->vertices_component[i];
  /* Vertices are topologically sorted according to compilation time
     known dependences, so we can break strong connected components
     by removing edges of the opposite direction, i.e, edges pointing
     from vertice with smaller post number to vertice with bigger post
     number.  */
  if (g->vertices[i].post < g->vertices[j].post
      /* We only need to remove edges connecting vertices in the same
	 strong connected component to break it.  */
      && component == cbdata->vertices_component[j]
      /* Check if we want to break the strong connected component or not.  */
      && !bitmap_bit_p (cbdata->sccs_to_merge, component))
    cbdata->alias_ddrs->safe_splice (edata->alias_ddrs);
}

/* This is the main function breaking strong conected components in
   PARTITIONS giving reduced depdendence graph RDG.  Store data dependence
   relations for runtime alias check in ALIAS_DDRS.  */

static void
break_alias_scc_partitions (struct graph *rdg,
			    vec<struct partition *> *partitions,
			    vec<ddr_p> *alias_ddrs)
{
  int i, j, k, num_sccs, num_sccs_no_alias;
  /* Build partition dependence graph.  */
  graph *pg = build_partition_graph (rdg, partitions, false);

  alias_ddrs->truncate (0);
  /* Find strong connected components in the graph, with all dependence edges
     considered.  */
  num_sccs = graphds_scc (pg, NULL);
  /* All SCCs now can be broken by runtime alias checks because SCCs caused by
     compilation time known dependences are merged before this function.  */
  if ((unsigned) num_sccs < partitions->length ())
    {
      struct pg_edge_callback_data cbdata;
      auto_bitmap sccs_to_merge;
      auto_vec<enum partition_type> scc_types;
      struct partition *partition, *first;

      /* If all partitions in a SCC have the same type, we can simply merge the
	 SCC.  This loop finds out such SCCS and record them in bitmap.  */
      bitmap_set_range (sccs_to_merge, 0, (unsigned) num_sccs);
      for (i = 0; i < num_sccs; ++i)
	{
	  for (j = 0; partitions->iterate (j, &first); ++j)
	    if (pg->vertices[j].component == i)
	      break;

	  bool same_type = true, all_builtins = partition_builtin_p (first);
	  for (++j; partitions->iterate (j, &partition); ++j)
	    {
	      if (pg->vertices[j].component != i)
		continue;

	      if (first->type != partition->type)
		{
		  same_type = false;
		  break;
		}
	      all_builtins &= partition_builtin_p (partition);
	    }
	  /* Merge SCC if all partitions in SCC have the same type, though the
	     result partition is sequential, because vectorizer can do better
	     runtime alias check.  One expecption is all partitions in SCC are
	     builtins.  */
	  if (!same_type || all_builtins)
	    bitmap_clear_bit (sccs_to_merge, i);
	}

      /* Initialize callback data for traversing.  */
      cbdata.sccs_to_merge = sccs_to_merge;
      cbdata.alias_ddrs = alias_ddrs;
      cbdata.vertices_component = XNEWVEC (int, pg->n_vertices);
      /* Record the component information which will be corrupted by next
	 graph scc finding call.  */
      for (i = 0; i < pg->n_vertices; ++i)
	cbdata.vertices_component[i] = pg->vertices[i].component;

      /* Collect data dependences for runtime alias checks to break SCCs.  */
      if (bitmap_count_bits (sccs_to_merge) != (unsigned) num_sccs)
	{
	  /* Run SCC finding algorithm again, with alias dependence edges
	     skipped.  This is to topologically sort partitions according to
	     compilation time known dependence.  Note the topological order
	     is stored in the form of pg's post order number.  */
	  num_sccs_no_alias = graphds_scc (pg, NULL, pg_skip_alias_edge);
	  gcc_assert (partitions->length () == (unsigned) num_sccs_no_alias);
	  /* With topological order, we can construct two subgraphs L and R.
	     L contains edge <x, y> where x < y in terms of post order, while
	     R contains edge <x, y> where x > y.  Edges for compilation time
	     known dependence all fall in R, so we break SCCs by removing all
	     (alias) edges of in subgraph L.  */
	  for_each_edge (pg, pg_collect_alias_ddrs, &cbdata);
	}

      /* For SCC that doesn't need to be broken, merge it.  */
      for (i = 0; i < num_sccs; ++i)
	{
	  if (!bitmap_bit_p (sccs_to_merge, i))
	    continue;

	  for (j = 0; partitions->iterate (j, &first); ++j)
	    if (cbdata.vertices_component[j] == i)
	      break;
	  for (k = j + 1; partitions->iterate (k, &partition); ++k)
	    {
	      struct pg_vdata *data;

	      if (cbdata.vertices_component[k] != i)
		continue;

	      /* Update postorder number so that merged reduction partition is
		 sorted after other partitions.  */
	      if (!partition_reduction_p (first)
		  && partition_reduction_p (partition))
		{
		  gcc_assert (pg->vertices[k].post < pg->vertices[j].post);
		  pg->vertices[j].post = pg->vertices[k].post;
		}
	      partition_merge_into (NULL, first, partition, FUSE_SAME_SCC);
	      (*partitions)[k] = NULL;
	      partition_free (partition);
	      data = (struct pg_vdata *)pg->vertices[k].data;
	      gcc_assert (data->id == k);
	      data->partition = NULL;
	      /* The result partition of merged SCC must be sequential.  */
	      first->type = PTYPE_SEQUENTIAL;
	    }
	}
    }

  sort_partitions_by_post_order (pg, partitions);
  free_partition_graph_vdata (pg);
  for_each_edge (pg, free_partition_graph_edata_cb, NULL);
  free_graph (pg);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Possible alias data dependence to break:\n");
      dump_data_dependence_relations (dump_file, *alias_ddrs);
    }
}

/* Compute and return an expression whose value is the segment length which
   will be accessed by DR in NITERS iterations.  */

static tree
data_ref_segment_size (struct data_reference *dr, tree niters)
{
  niters = size_binop (MINUS_EXPR,
		       fold_convert (sizetype, niters),
		       size_one_node);
  return size_binop (MULT_EXPR,
		     fold_convert (sizetype, DR_STEP (dr)),
		     fold_convert (sizetype, niters));
}

/* Return true if LOOP's latch is dominated by statement for data reference
   DR.  */

static inline bool
latch_dominated_by_data_ref (struct loop *loop, data_reference *dr)
{
  return dominated_by_p (CDI_DOMINATORS, single_exit (loop)->src,
			 gimple_bb (DR_STMT (dr)));
}

/* Compute alias check pairs and store them in COMP_ALIAS_PAIRS for LOOP's
   data dependence relations ALIAS_DDRS.  */

static void
compute_alias_check_pairs (struct loop *loop, vec<ddr_p> *alias_ddrs,
			   vec<dr_with_seg_len_pair_t> *comp_alias_pairs)
{
  unsigned int i;
  unsigned HOST_WIDE_INT factor = 1;
  tree niters_plus_one, niters = number_of_latch_executions (loop);

  gcc_assert (niters != NULL_TREE && niters != chrec_dont_know);
  niters = fold_convert (sizetype, niters);
  niters_plus_one = size_binop (PLUS_EXPR, niters, size_one_node);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Creating alias check pairs:\n");

  /* Iterate all data dependence relations and compute alias check pairs.  */
  for (i = 0; i < alias_ddrs->length (); i++)
    {
      ddr_p ddr = (*alias_ddrs)[i];
      struct data_reference *dr_a = DDR_A (ddr);
      struct data_reference *dr_b = DDR_B (ddr);
      tree seg_length_a, seg_length_b;
      int comp_res = data_ref_compare_tree (DR_BASE_ADDRESS (dr_a),
					    DR_BASE_ADDRESS (dr_b));

      if (comp_res == 0)
	comp_res = data_ref_compare_tree (DR_OFFSET (dr_a), DR_OFFSET (dr_b));
      gcc_assert (comp_res != 0);

      if (latch_dominated_by_data_ref (loop, dr_a))
	seg_length_a = data_ref_segment_size (dr_a, niters_plus_one);
      else
	seg_length_a = data_ref_segment_size (dr_a, niters);

      if (latch_dominated_by_data_ref (loop, dr_b))
	seg_length_b = data_ref_segment_size (dr_b, niters_plus_one);
      else
	seg_length_b = data_ref_segment_size (dr_b, niters);

      unsigned HOST_WIDE_INT access_size_a
	= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr_a))));
      unsigned HOST_WIDE_INT access_size_b
	= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr_b))));
      unsigned int align_a = TYPE_ALIGN_UNIT (TREE_TYPE (DR_REF (dr_a)));
      unsigned int align_b = TYPE_ALIGN_UNIT (TREE_TYPE (DR_REF (dr_b)));

      dr_with_seg_len_pair_t dr_with_seg_len_pair
	(dr_with_seg_len (dr_a, seg_length_a, access_size_a, align_a),
	 dr_with_seg_len (dr_b, seg_length_b, access_size_b, align_b));

      /* Canonicalize pairs by sorting the two DR members.  */
      if (comp_res > 0)
	std::swap (dr_with_seg_len_pair.first, dr_with_seg_len_pair.second);

      comp_alias_pairs->safe_push (dr_with_seg_len_pair);
    }

  if (tree_fits_uhwi_p (niters))
    factor = tree_to_uhwi (niters);

  /* Prune alias check pairs.  */
  prune_runtime_alias_test_list (comp_alias_pairs, factor);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "Improved number of alias checks from %d to %d\n",
	     alias_ddrs->length (), comp_alias_pairs->length ());
}

/* Given data dependence relations in ALIAS_DDRS, generate runtime alias
   checks and version LOOP under condition of these runtime alias checks.  */

static void
version_loop_by_alias_check (vec<struct partition *> *partitions,
			     struct loop *loop, vec<ddr_p> *alias_ddrs)
{
  profile_probability prob;
  basic_block cond_bb;
  struct loop *nloop;
  tree lhs, arg0, cond_expr = NULL_TREE;
  gimple_seq cond_stmts = NULL;
  gimple *call_stmt = NULL;
  auto_vec<dr_with_seg_len_pair_t> comp_alias_pairs;

  /* Generate code for runtime alias checks if necessary.  */
  gcc_assert (alias_ddrs->length () > 0);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "Version loop <%d> with runtime alias check\n", loop->num);

  compute_alias_check_pairs (loop, alias_ddrs, &comp_alias_pairs);
  create_runtime_alias_checks (loop, &comp_alias_pairs, &cond_expr);
  cond_expr = force_gimple_operand_1 (cond_expr, &cond_stmts,
				      is_gimple_val, NULL_TREE);

  /* Depend on vectorizer to fold IFN_LOOP_DIST_ALIAS.  */
  bool cancelable_p = flag_tree_loop_vectorize;
  if (cancelable_p)
    {
      unsigned i = 0;
      struct partition *partition;
      for (; partitions->iterate (i, &partition); ++i)
	if (!partition_builtin_p (partition))
	  break;

     /* If all partitions are builtins, distributing it would be profitable and
	we don't want to cancel the runtime alias checks.  */
      if (i == partitions->length ())
	cancelable_p = false;
    }

  /* Generate internal function call for loop distribution alias check if the
     runtime alias check should be cancelable.  */
  if (cancelable_p)
    {
      call_stmt = gimple_build_call_internal (IFN_LOOP_DIST_ALIAS,
					      2, NULL_TREE, cond_expr);
      lhs = make_ssa_name (boolean_type_node);
      gimple_call_set_lhs (call_stmt, lhs);
    }
  else
    lhs = cond_expr;

  prob = profile_probability::guessed_always ().apply_scale (9, 10);
  initialize_original_copy_tables ();
  nloop = loop_version (loop, lhs, &cond_bb, prob, prob.invert (),
			prob, prob.invert (), true);
  free_original_copy_tables ();
  /* Record the original loop number in newly generated loops.  In case of
     distribution, the original loop will be distributed and the new loop
     is kept.  */
  loop->orig_loop_num = nloop->num;
  nloop->orig_loop_num = nloop->num;
  nloop->dont_vectorize = true;
  nloop->force_vectorize = false;

  if (call_stmt)
    {
      /* Record new loop's num in IFN_LOOP_DIST_ALIAS because the original
	 loop could be destroyed.  */
      arg0 = build_int_cst (integer_type_node, loop->orig_loop_num);
      gimple_call_set_arg (call_stmt, 0, arg0);
      gimple_seq_add_stmt_without_update (&cond_stmts, call_stmt);
    }

  if (cond_stmts)
    {
      gimple_stmt_iterator cond_gsi = gsi_last_bb (cond_bb);
      gsi_insert_seq_before (&cond_gsi, cond_stmts, GSI_SAME_STMT);
    }
  update_ssa (TODO_update_ssa);
}

/* Return true if loop versioning is needed to distrubute PARTITIONS.
   ALIAS_DDRS are data dependence relations for runtime alias check.  */

static inline bool
version_for_distribution_p (vec<struct partition *> *partitions,
			    vec<ddr_p> *alias_ddrs)
{
  /* No need to version loop if we have only one partition.  */
  if (partitions->length () == 1)
    return false;

  /* Need to version loop if runtime alias check is necessary.  */
  return (alias_ddrs->length () > 0);
}

/* Compare base offset of builtin mem* partitions P1 and P2.  */

static bool
offset_cmp (struct partition *p1, struct partition *p2)
{
  gcc_assert (p1 != NULL && p1->builtin != NULL);
  gcc_assert (p2 != NULL && p2->builtin != NULL);
  return p1->builtin->dst_base_offset < p2->builtin->dst_base_offset;
}

/* Fuse adjacent memset builtin PARTITIONS if possible.  This is a special
   case optimization transforming below code:

     __builtin_memset (&obj, 0, 100);
     _1 = &obj + 100;
     __builtin_memset (_1, 0, 200);
     _2 = &obj + 300;
     __builtin_memset (_2, 0, 100);

   into:

     __builtin_memset (&obj, 0, 400);

   Note we don't have dependence information between different partitions
   at this point, as a result, we can't handle nonadjacent memset builtin
   partitions since dependence might be broken.  */

static void
fuse_memset_builtins (vec<struct partition *> *partitions)
{
  unsigned i, j;
  struct partition *part1, *part2;
  tree rhs1, rhs2;

  for (i = 0; partitions->iterate (i, &part1);)
    {
      if (part1->kind != PKIND_MEMSET)
	{
	  i++;
	  continue;
	}

      /* Find sub-array of memset builtins of the same base.  Index range
	 of the sub-array is [i, j) with "j > i".  */
      for (j = i + 1; partitions->iterate (j, &part2); ++j)
	{
	  if (part2->kind != PKIND_MEMSET
	      || !operand_equal_p (part1->builtin->dst_base_base,
				   part2->builtin->dst_base_base, 0))
	    break;

	  /* Memset calls setting different values can't be merged.  */
	  rhs1 = gimple_assign_rhs1 (DR_STMT (part1->builtin->dst_dr));
	  rhs2 = gimple_assign_rhs1 (DR_STMT (part2->builtin->dst_dr));
	  if (!operand_equal_p (rhs1, rhs2, 0))
	    break;
	}

      /* Stable sort is required in order to avoid breaking dependence.  */
      std::stable_sort (&(*partitions)[i],
			&(*partitions)[i] + j - i, offset_cmp);
      /* Continue with next partition.  */
      i = j;
    }

  /* Merge all consecutive memset builtin partitions.  */
  for (i = 0; i < partitions->length () - 1;)
    {
      part1 = (*partitions)[i];
      if (part1->kind != PKIND_MEMSET)
	{
	  i++;
	  continue;
	}

      part2 = (*partitions)[i + 1];
      /* Only merge memset partitions of the same base and with constant
	 access sizes.  */
      if (part2->kind != PKIND_MEMSET
	  || TREE_CODE (part1->builtin->size) != INTEGER_CST
	  || TREE_CODE (part2->builtin->size) != INTEGER_CST
	  || !operand_equal_p (part1->builtin->dst_base_base,
			       part2->builtin->dst_base_base, 0))
	{
	  i++;
	  continue;
	}
      rhs1 = gimple_assign_rhs1 (DR_STMT (part1->builtin->dst_dr));
      rhs2 = gimple_assign_rhs1 (DR_STMT (part2->builtin->dst_dr));
      int bytev1 = const_with_all_bytes_same (rhs1);
      int bytev2 = const_with_all_bytes_same (rhs2);
      /* Only merge memset partitions of the same value.  */
      if (bytev1 != bytev2 || bytev1 == -1)
	{
	  i++;
	  continue;
	}
      wide_int end1 = wi::add (part1->builtin->dst_base_offset,
			       wi::to_wide (part1->builtin->size));
      /* Only merge adjacent memset partitions.  */
      if (wi::ne_p (end1, part2->builtin->dst_base_offset))
	{
	  i++;
	  continue;
	}
      /* Merge partitions[i] and partitions[i+1].  */
      part1->builtin->size = fold_build2 (PLUS_EXPR, sizetype,
					  part1->builtin->size,
					  part2->builtin->size);
      partition_free (part2);
      partitions->ordered_remove (i + 1);
    }
}

/* Fuse PARTITIONS of LOOP if necessary before finalizing distribution.
   ALIAS_DDRS contains ddrs which need runtime alias check.  */

static void
finalize_partitions (struct loop *loop, vec<struct partition *> *partitions,
		     vec<ddr_p> *alias_ddrs)
{
  unsigned i;
  struct partition *partition, *a;

  if (partitions->length () == 1
      || alias_ddrs->length () > 0)
    return;

  unsigned num_builtin = 0, num_normal = 0, num_partial_memset = 0;
  bool same_type_p = true;
  enum partition_type type = ((*partitions)[0])->type;
  for (i = 0; partitions->iterate (i, &partition); ++i)
    {
      same_type_p &= (type == partition->type);
      if (partition_builtin_p (partition))
	{
	  num_builtin++;
	  continue;
	}
      num_normal++;
      if (partition->kind == PKIND_PARTIAL_MEMSET)
	num_partial_memset++;
    }

  /* Don't distribute current loop into too many loops given we don't have
     memory stream cost model.  Be even more conservative in case of loop
     nest distribution.  */
  if ((same_type_p && num_builtin == 0
       && (loop->inner == NULL || num_normal != 2 || num_partial_memset != 1))
      || (loop->inner != NULL
	  && i >= NUM_PARTITION_THRESHOLD && num_normal > 1)
      || (loop->inner == NULL
	  && i >= NUM_PARTITION_THRESHOLD && num_normal > num_builtin))
    {
      a = (*partitions)[0];
      for (i = 1; partitions->iterate (i, &partition); ++i)
	{
	  partition_merge_into (NULL, a, partition, FUSE_FINALIZE);
	  partition_free (partition);
	}
      partitions->truncate (1);
    }

  /* Fuse memset builtins if possible.  */
  if (partitions->length () > 1)
    fuse_memset_builtins (partitions);
}

/* Distributes the code from LOOP in such a way that producer statements
   are placed before consumer statements.  Tries to separate only the
   statements from STMTS into separate loops.  Returns the number of
   distributed loops.  Set NB_CALLS to number of generated builtin calls.
   Set *DESTROY_P to whether LOOP needs to be destroyed.  */

static int
distribute_loop (struct loop *loop, vec<gimple *> stmts,
		 control_dependences *cd, int *nb_calls, bool *destroy_p)
{
  ddrs_table = new hash_table<ddr_hasher> (389);
  struct graph *rdg;
  partition *partition;
  bool any_builtin;
  int i, nbp;

  *destroy_p = false;
  *nb_calls = 0;
  loop_nest.create (0);
  if (!find_loop_nest (loop, &loop_nest))
    {
      loop_nest.release ();
      delete ddrs_table;
      return 0;
    }

  datarefs_vec.create (20);
  rdg = build_rdg (loop, cd);
  if (!rdg)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %d not distributed: failed to build the RDG.\n",
		 loop->num);

      loop_nest.release ();
      free_data_refs (datarefs_vec);
      delete ddrs_table;
      return 0;
    }

  if (datarefs_vec.length () > MAX_DATAREFS_NUM)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Loop %d not distributed: too many memory references.\n",
		 loop->num);

      free_rdg (rdg);
      loop_nest.release ();
      free_data_refs (datarefs_vec);
      delete ddrs_table;
      return 0;
    }

  data_reference_p dref;
  for (i = 0; datarefs_vec.iterate (i, &dref); ++i)
    dref->aux = (void *) (uintptr_t) i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_rdg (dump_file, rdg);

  auto_vec<struct partition *, 3> partitions;
  rdg_build_partitions (rdg, stmts, &partitions);

  auto_vec<ddr_p> alias_ddrs;

  auto_bitmap stmt_in_all_partitions;
  bitmap_copy (stmt_in_all_partitions, partitions[0]->stmts);
  for (i = 1; partitions.iterate (i, &partition); ++i)
    bitmap_and_into (stmt_in_all_partitions, partitions[i]->stmts);

  any_builtin = false;
  FOR_EACH_VEC_ELT (partitions, i, partition)
    {
      classify_partition (loop, rdg, partition, stmt_in_all_partitions);
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
  struct partition *into;
  if (!flag_tree_loop_distribution)
    {
      for (i = 0; partitions.iterate (i, &into); ++i)
	if (!partition_builtin_p (into))
	  break;
      for (++i; partitions.iterate (i, &partition); ++i)
	if (!partition_builtin_p (partition))
	  {
	    partition_merge_into (NULL, into, partition, FUSE_NON_BUILTIN);
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
	partition_merge_into (rdg, into, partition, FUSE_REDUCTION);
	partitions.unordered_remove (i);
	partition_free (partition);
	i--;
      }

  /* Apply our simple cost model - fuse partitions with similar
     memory accesses.  */
  for (i = 0; partitions.iterate (i, &into); ++i)
    {
      bool changed = false;
      if (partition_builtin_p (into) || into->kind == PKIND_PARTIAL_MEMSET)
	continue;
      for (int j = i + 1;
	   partitions.iterate (j, &partition); ++j)
	{
	  if (share_memory_accesses (rdg, into, partition))
	    {
	      partition_merge_into (rdg, into, partition, FUSE_SHARE_REF);
	      partitions.unordered_remove (j);
	      partition_free (partition);
	      j--;
	      changed = true;
	    }
	}
      /* If we fused 0 1 2 in step 1 to 0,2 1 as 0 and 2 have similar
         accesses when 1 and 2 have similar accesses but not 0 and 1
	 then in the next iteration we will fail to consider merging
	 1 into 0,2.  So try again if we did any merging into 0.  */
      if (changed)
	i--;
    }

  /* Build the partition dependency graph and fuse partitions in strong
     connected component.  */
  if (partitions.length () > 1)
    {
      /* Don't support loop nest distribution under runtime alias check
	 since it's not likely to enable many vectorization opportunities.  */
      if (loop->inner)
	merge_dep_scc_partitions (rdg, &partitions, false);
      else
	{
	  merge_dep_scc_partitions (rdg, &partitions, true);
	  if (partitions.length () > 1)
	    break_alias_scc_partitions (rdg, &partitions, &alias_ddrs);
	}
    }

  finalize_partitions (loop, &partitions, &alias_ddrs);

  nbp = partitions.length ();
  if (nbp == 0
      || (nbp == 1 && !partition_builtin_p (partitions[0]))
      || (nbp > 1 && partition_contains_all_rw (rdg, partitions)))
    {
      nbp = 0;
      goto ldist_done;
    }

  if (version_for_distribution_p (&partitions, &alias_ddrs))
    version_loop_by_alias_check (&partitions, loop, &alias_ddrs);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "distribute loop <%d> into partitions:\n", loop->num);
      dump_rdg_partitions (dump_file, partitions);
    }

  FOR_EACH_VEC_ELT (partitions, i, partition)
    {
      if (partition_builtin_p (partition))
	(*nb_calls)++;
      *destroy_p |= generate_code_for_partition (loop, partition, i < nbp - 1);
    }

 ldist_done:
  loop_nest.release ();
  free_data_refs (datarefs_vec);
  for (hash_table<ddr_hasher>::iterator iter = ddrs_table->begin ();
       iter != ddrs_table->end (); ++iter)
    {
      free_dependence_relation (*iter);
      *iter = NULL;
    }
  delete ddrs_table;

  FOR_EACH_VEC_ELT (partitions, i, partition)
    partition_free (partition);

  free_rdg (rdg);
  return nbp - *nb_calls;
}

/* Distribute all loops in the current function.  */

namespace {

const pass_data pass_data_loop_distribution =
{
  GIMPLE_PASS, /* type */
  "ldist", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP_DISTRIBUTION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_distribution : public gimple_opt_pass
{
public:
  pass_loop_distribution (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_distribution, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return flag_tree_loop_distribution
	|| flag_tree_loop_distribute_patterns;
    }

  virtual unsigned int execute (function *);

}; // class pass_loop_distribution


/* Given LOOP, this function records seed statements for distribution in
   WORK_LIST.  Return false if there is nothing for distribution.  */

static bool
find_seed_stmts_for_distribution (struct loop *loop, vec<gimple *> *work_list)
{
  basic_block *bbs = get_loop_body_in_dom_order (loop);

  /* Initialize the worklist with stmts we seed the partitions with.  */
  for (unsigned i = 0; i < loop->num_nodes; ++i)
    {
      for (gphi_iterator gsi = gsi_start_phis (bbs[i]);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;
	  /* Distribute stmts which have defs that are used outside of
	     the loop.  */
	  if (!stmt_has_scalar_dependences_outside_loop (loop, phi))
	    continue;
	  work_list->safe_push (phi);
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  /* If there is a stmt with side-effects bail out - we
	     cannot and should not distribute this loop.  */
	  if (gimple_has_side_effects (stmt))
	    {
	      free (bbs);
	      return false;
	    }

	  /* Distribute stmts which have defs that are used outside of
	     the loop.  */
	  if (stmt_has_scalar_dependences_outside_loop (loop, stmt))
	    ;
	  /* Otherwise only distribute stores for now.  */
	  else if (!gimple_vdef (stmt))
	    continue;

	  work_list->safe_push (stmt);
	}
    }
  free (bbs);
  return work_list->length () > 0;
}

/* Given innermost LOOP, return the outermost enclosing loop that forms a
   perfect loop nest.  */

static struct loop *
prepare_perfect_loop_nest (struct loop *loop)
{
  struct loop *outer = loop_outer (loop);
  tree niters = number_of_latch_executions (loop);

  /* TODO: We only support the innermost 3-level loop nest distribution
     because of compilation time issue for now.  This should be relaxed
     in the future.  Note we only allow 3-level loop nest distribution
     when parallelizing loops.  */
  while ((loop->inner == NULL
	  || (loop->inner->inner == NULL && flag_tree_parallelize_loops > 1))
	 && loop_outer (outer)
	 && outer->inner == loop && loop->next == NULL
	 && single_exit (outer)
	 && optimize_loop_for_speed_p (outer)
	 && !chrec_contains_symbols_defined_in_loop (niters, outer->num)
	 && (niters = number_of_latch_executions (outer)) != NULL_TREE
	 && niters != chrec_dont_know)
    {
      loop = outer;
      outer = loop_outer (loop);
    }

  return loop;
}

unsigned int
pass_loop_distribution::execute (function *fun)
{
  struct loop *loop;
  bool changed = false;
  basic_block bb;
  control_dependences *cd = NULL;
  auto_vec<loop_p> loops_to_be_destroyed;

  if (number_of_loops (fun) <= 1)
    return 0;

  /* Compute topological order for basic blocks.  Topological order is
     needed because data dependence is computed for data references in
     lexicographical order.  */
  if (bb_top_order_index == NULL)
    {
      int rpo_num;
      int *rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));

      bb_top_order_index = XNEWVEC (int, last_basic_block_for_fn (cfun));
      bb_top_order_index_size = last_basic_block_for_fn (cfun);
      rpo_num = pre_and_rev_post_order_compute_fn (cfun, NULL, rpo, true);
      for (int i = 0; i < rpo_num; i++)
	bb_top_order_index[rpo[i]] = i;

      free (rpo);
    }

  FOR_ALL_BB_FN (bb, fun)
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
      /* Don't distribute multiple exit edges loop, or cold loop.  */
      if (!single_exit (loop)
	  || !optimize_loop_for_speed_p (loop))
	continue;

      /* Don't distribute loop if niters is unknown.  */
      tree niters = number_of_latch_executions (loop);
      if (niters == NULL_TREE || niters == chrec_dont_know)
	continue;

      /* Get the perfect loop nest for distribution.  */
      loop = prepare_perfect_loop_nest (loop);
      for (; loop; loop = loop->inner)
	{
	  auto_vec<gimple *> work_list;
	  if (!find_seed_stmts_for_distribution (loop, &work_list))
	    break;

	  const char *str = loop->inner ? " nest" : "";
	  dump_user_location_t loc = find_loop_location (loop);
	  if (!cd)
	    {
	      calculate_dominance_info (CDI_DOMINATORS);
	      calculate_dominance_info (CDI_POST_DOMINATORS);
	      cd = new control_dependences ();
	      free_dominance_info (CDI_POST_DOMINATORS);
	    }

	  bool destroy_p;
	  int nb_generated_loops, nb_generated_calls;
	  nb_generated_loops = distribute_loop (loop, work_list, cd,
						&nb_generated_calls,
						&destroy_p);
	  if (destroy_p)
	    loops_to_be_destroyed.safe_push (loop);

	  if (nb_generated_loops + nb_generated_calls > 0)
	    {
	      changed = true;
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS,
			       loc, "Loop%s %d distributed: split to %d loops "
			       "and %d library calls.\n", str, loop->num,
			       nb_generated_loops, nb_generated_calls);

	      break;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Loop%s %d not distributed.\n", str, loop->num);
	}
    }

  if (cd)
    delete cd;

  if (bb_top_order_index != NULL)
    {
      free (bb_top_order_index);
      bb_top_order_index = NULL;
      bb_top_order_index_size = 0;
    }

  if (changed)
    {
      /* Destroy loop bodies that could not be reused.  Do this late as we
	 otherwise can end up refering to stale data in control dependences.  */
      unsigned i;
      FOR_EACH_VEC_ELT (loops_to_be_destroyed, i, loop)
	destroy_loop (loop);

      /* Cached scalar evolutions now may refer to wrong or non-existing
	 loops.  */
      scev_reset_htab ();
      mark_virtual_operands_for_renaming (fun);
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
    }

  checking_verify_loop_structure ();

  return changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_loop_distribution (gcc::context *ctxt)
{
  return new pass_loop_distribution (ctxt);
}
