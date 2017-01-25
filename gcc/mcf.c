/* Routines to implement minimum-cost maximal flow algorithm used to smooth
   basic block and edge frequency counts.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.
   Contributed by Paul Yuan (yingbo.com@gmail.com) and
                  Vinodha Ramasamy (vinodha@google.com).

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

/* References:
   [1] "Feedback-directed Optimizations in GCC with Estimated Edge Profiles
        from Hardware Event Sampling", Vinodha Ramasamy, Paul Yuan, Dehao Chen,
        and Robert Hundt; GCC Summit 2008.
   [2] "Complementing Missing and Inaccurate Profiling Using a Minimum Cost
        Circulation Algorithm", Roy Levin, Ilan Newman and Gadi Haber;
        HiPEAC '08.

   Algorithm to smooth basic block and edge counts:
   1. create_fixup_graph: Create fixup graph by translating function CFG into
      a graph that satisfies MCF algorithm requirements.
   2. find_max_flow: Find maximal flow.
   3. compute_residual_flow: Form residual network.
   4. Repeat:
      cancel_negative_cycle: While G contains a negative cost cycle C, reverse
      the flow on the found cycle by the minimum residual capacity in that
      cycle.
   5. Form the minimal cost flow
      f(u,v) = rf(v, u).
   6. adjust_cfg_counts: Update initial edge weights with corrected weights.
      delta(u.v) = f(u,v) -f(v,u).
      w*(u,v) = w(u,v) + delta(u,v).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "profile.h"
#include "dumpfile.h"

/* CAP_INFINITY: Constant to represent infinite capacity.  */
#define CAP_INFINITY INTTYPE_MAXIMUM (int64_t)

/* COST FUNCTION.  */
#define K_POS(b)        ((b))
#define K_NEG(b)        (50 * (b))
#define COST(k, w)      ((k) / mcf_ln ((w) + 2))
/* Limit the number of iterations for cancel_negative_cycles() to ensure
   reasonable compile time.  */
#define MAX_ITER(n, e)  10 + (1000000 / ((n) * (e)))
enum edge_type
{
  INVALID_EDGE,
  VERTEX_SPLIT_EDGE,	    /* Edge to represent vertex with w(e) = w(v).  */
  REDIRECT_EDGE,	    /* Edge after vertex transformation.  */
  REVERSE_EDGE,
  SOURCE_CONNECT_EDGE,	    /* Single edge connecting to single source.  */
  SINK_CONNECT_EDGE,	    /* Single edge connecting to single sink.  */
  BALANCE_EDGE,		    /* Edge connecting with source/sink: cp(e) = 0.  */
  REDIRECT_NORMALIZED_EDGE, /* Normalized edge for a redirect edge.  */
  REVERSE_NORMALIZED_EDGE   /* Normalized edge for a reverse edge.  */
};

/* Structure to represent an edge in the fixup graph.  */
struct fixup_edge_type
{
  int src;
  int dest;
  /* Flag denoting type of edge and attributes for the flow field.  */
  edge_type type;
  bool is_rflow_valid;
  /* Index to the normalization vertex added for this edge.  */
  int norm_vertex_index;
  /* Flow for this edge.  */
  gcov_type flow;
  /* Residual flow for this edge - used during negative cycle canceling.  */
  gcov_type rflow;
  gcov_type weight;
  gcov_type cost;
  gcov_type max_capacity;
};

typedef fixup_edge_type *fixup_edge_p;


/* Structure to represent a vertex in the fixup graph.  */
struct fixup_vertex_type
{
  vec<fixup_edge_p> succ_edges;
};

typedef fixup_vertex_type *fixup_vertex_p;

/* Fixup graph used in the MCF algorithm.  */
struct fixup_graph_type
{
  /* Current number of vertices for the graph.  */
  int num_vertices;
  /* Current number of edges for the graph.  */
  int num_edges;
  /* Index of new entry vertex.  */
  int new_entry_index;
  /* Index of new exit vertex.  */
  int new_exit_index;
  /* Fixup vertex list. Adjacency list for fixup graph.  */
  fixup_vertex_p vertex_list;
  /* Fixup edge list.  */
  fixup_edge_p edge_list;
};

struct queue_type
{
  int *queue;
  int head;
  int tail;
  int size;
};

/* Structure used in the maximal flow routines to find augmenting path.  */
struct augmenting_path_type
{
  /* Queue used to hold vertex indices.  */
  queue_type queue_list;
  /* Vector to hold chain of pred vertex indices in augmenting path.  */
  int *bb_pred;
  /* Vector that indicates if basic block i has been visited.  */
  int *is_visited;
};


/* Function definitions.  */

/* Dump routines to aid debugging.  */

/* Print basic block with index N for FIXUP_GRAPH in n' and n'' format.  */

static void
print_basic_block (FILE *file, fixup_graph_type *fixup_graph, int n)
{
  if (n == ENTRY_BLOCK)
    fputs ("ENTRY", file);
  else if (n == ENTRY_BLOCK + 1)
    fputs ("ENTRY''", file);
  else if (n == 2 * EXIT_BLOCK)
    fputs ("EXIT", file);
  else if (n == 2 * EXIT_BLOCK + 1)
    fputs ("EXIT''", file);
  else if (n == fixup_graph->new_exit_index)
    fputs ("NEW_EXIT", file);
  else if (n == fixup_graph->new_entry_index)
    fputs ("NEW_ENTRY", file);
  else
    {
      fprintf (file, "%d", n / 2);
      if (n % 2)
	fputs ("''", file);
      else
	fputs ("'", file);
    }
}


/* Print edge S->D for given fixup_graph with n' and n'' format.
   PARAMETERS:
   S is the index of the source vertex of the edge (input) and
   D is the index of the destination vertex of the edge (input) for the given
   fixup_graph (input).  */

static void
print_edge (FILE *file, fixup_graph_type *fixup_graph, int s, int d)
{
  print_basic_block (file, fixup_graph, s);
  fputs ("->", file);
  print_basic_block (file, fixup_graph, d);
}


/* Dump out the attributes of a given edge FEDGE in the fixup_graph to a
   file.  */
static void
dump_fixup_edge (FILE *file, fixup_graph_type *fixup_graph, fixup_edge_p fedge)
{
  if (!fedge)
    {
      fputs ("NULL fixup graph edge.\n", file);
      return;
    }

  print_edge (file, fixup_graph, fedge->src, fedge->dest);
  fputs (": ", file);

  if (fedge->type)
    {
      fprintf (file, "flow/capacity=%" PRId64 "/",
	       fedge->flow);
      if (fedge->max_capacity == CAP_INFINITY)
	fputs ("+oo,", file);
      else
	fprintf (file, "%" PRId64 ",", fedge->max_capacity);
    }

  if (fedge->is_rflow_valid)
    {
      if (fedge->rflow == CAP_INFINITY)
	fputs (" rflow=+oo.", file);
      else
	fprintf (file, " rflow=%" PRId64 ",", fedge->rflow);
    }

  fprintf (file, " cost=%" PRId64 ".", fedge->cost);

  fprintf (file, "\t(%d->%d)", fedge->src, fedge->dest);

  if (fedge->type)
    {
      switch (fedge->type)
	{
	case VERTEX_SPLIT_EDGE:
	  fputs (" @VERTEX_SPLIT_EDGE", file);
	  break;

	case REDIRECT_EDGE:
	  fputs (" @REDIRECT_EDGE", file);
	  break;

	case SOURCE_CONNECT_EDGE:
	  fputs (" @SOURCE_CONNECT_EDGE", file);
	  break;

	case SINK_CONNECT_EDGE:
	  fputs (" @SINK_CONNECT_EDGE", file);
	  break;

	case REVERSE_EDGE:
	  fputs (" @REVERSE_EDGE", file);
	  break;

	case BALANCE_EDGE:
	  fputs (" @BALANCE_EDGE", file);
	  break;

	case REDIRECT_NORMALIZED_EDGE:
	case REVERSE_NORMALIZED_EDGE:
	  fputs ("  @NORMALIZED_EDGE", file);
	  break;

	default:
	  fputs (" @INVALID_EDGE", file);
	  break;
	}
    }
  fputs ("\n", file);
}


/* Print out the edges and vertices of the given FIXUP_GRAPH, into the dump
   file. The input string MSG is printed out as a heading.  */

static void
dump_fixup_graph (FILE *file, fixup_graph_type *fixup_graph, const char *msg)
{
  int i, j;
  int fnum_vertices, fnum_edges;

  fixup_vertex_p fvertex_list, pfvertex;
  fixup_edge_p pfedge;

  gcc_assert (fixup_graph);
  fvertex_list = fixup_graph->vertex_list;
  fnum_vertices = fixup_graph->num_vertices;
  fnum_edges = fixup_graph->num_edges;

  fprintf (file, "\nDump fixup graph for %s(): %s.\n",
	   current_function_name (), msg);
  fprintf (file,
	   "There are %d vertices and %d edges. new_exit_index is %d.\n\n",
	   fnum_vertices, fnum_edges, fixup_graph->new_exit_index);

  for (i = 0; i < fnum_vertices; i++)
    {
      pfvertex = fvertex_list + i;
      fprintf (file, "vertex_list[%d]: %d succ fixup edges.\n",
	       i, pfvertex->succ_edges.length ());

      for (j = 0; pfvertex->succ_edges.iterate (j, &pfedge);
	   j++)
	{
	  /* Distinguish forward edges and backward edges in the residual flow
             network.  */
	  if (pfedge->type)
	    fputs ("(f) ", file);
	  else if (pfedge->is_rflow_valid)
	    fputs ("(b) ", file);
	  dump_fixup_edge (file, fixup_graph, pfedge);
	}
    }

  fputs ("\n", file);
}


/* Utility routines.  */
/* ln() implementation: approximate calculation. Returns ln of X.  */

static double
mcf_ln (double x)
{
#define E       2.71828
  int l = 1;
  double m = E;

  gcc_assert (x >= 0);

  while (m < x)
    {
      m *= E;
      l++;
    }

  return l;
}


/* sqrt() implementation: based on open source QUAKE3 code (magic sqrt
   implementation) by John Carmack.  Returns sqrt of X.  */

static double
mcf_sqrt (double x)
{
#define MAGIC_CONST1    0x1fbcf800
#define MAGIC_CONST2    0x5f3759df
  union {
    int intPart;
    float floatPart;
  } convertor, convertor2;

  gcc_assert (x >= 0);

  convertor.floatPart = x;
  convertor2.floatPart = x;
  convertor.intPart = MAGIC_CONST1 + (convertor.intPart >> 1);
  convertor2.intPart = MAGIC_CONST2 - (convertor2.intPart >> 1);

  return 0.5f * (convertor.floatPart + (x * convertor2.floatPart));
}


/* Common code shared between add_fixup_edge and add_rfixup_edge. Adds an edge
   (SRC->DEST) to the edge_list maintained in FIXUP_GRAPH with cost of the edge
   added set to COST.  */

static fixup_edge_p
add_edge (fixup_graph_type *fixup_graph, int src, int dest, gcov_type cost)
{
  fixup_vertex_p curr_vertex = fixup_graph->vertex_list + src;
  fixup_edge_p curr_edge = fixup_graph->edge_list + fixup_graph->num_edges;
  curr_edge->src = src;
  curr_edge->dest = dest;
  curr_edge->cost = cost;
  fixup_graph->num_edges++;
  if (dump_file)
    dump_fixup_edge (dump_file, fixup_graph, curr_edge);
  curr_vertex->succ_edges.safe_push (curr_edge);
  return curr_edge;
}


/* Add a fixup edge (src->dest) with attributes TYPE, WEIGHT, COST and
   MAX_CAPACITY to the edge_list in the fixup graph.  */

static void
add_fixup_edge (fixup_graph_type *fixup_graph, int src, int dest,
		edge_type type, gcov_type weight, gcov_type cost,
		gcov_type max_capacity)
{
  fixup_edge_p curr_edge = add_edge (fixup_graph, src, dest, cost);
  curr_edge->type = type;
  curr_edge->weight = weight;
  curr_edge->max_capacity = max_capacity;
}


/* Add a residual edge (SRC->DEST) with attributes RFLOW and COST
   to the fixup graph.  */

static void
add_rfixup_edge (fixup_graph_type *fixup_graph, int src, int dest,
		 gcov_type rflow, gcov_type cost)
{
  fixup_edge_p curr_edge = add_edge (fixup_graph, src, dest, cost);
  curr_edge->rflow = rflow;
  curr_edge->is_rflow_valid = true;
  /* This edge is not a valid edge - merely used to hold residual flow.  */
  curr_edge->type = INVALID_EDGE;
}


/* Return the pointer to fixup edge SRC->DEST or NULL if edge does not
   exist in the FIXUP_GRAPH.  */

static fixup_edge_p
find_fixup_edge (fixup_graph_type *fixup_graph, int src, int dest)
{
  int j;
  fixup_edge_p pfedge;
  fixup_vertex_p pfvertex;

  gcc_assert (src < fixup_graph->num_vertices);

  pfvertex = fixup_graph->vertex_list + src;

  for (j = 0; pfvertex->succ_edges.iterate (j, &pfedge);
       j++)
    if (pfedge->dest == dest)
      return pfedge;

  return NULL;
}


/* Cleanup routine to free structures in FIXUP_GRAPH.  */

static void
delete_fixup_graph (fixup_graph_type *fixup_graph)
{
  int i;
  int fnum_vertices = fixup_graph->num_vertices;
  fixup_vertex_p pfvertex = fixup_graph->vertex_list;

  for (i = 0; i < fnum_vertices; i++, pfvertex++)
    pfvertex->succ_edges.release ();

  free (fixup_graph->vertex_list);
  free (fixup_graph->edge_list);
}


/* Creates a fixup graph FIXUP_GRAPH from the function CFG.  */

static void
create_fixup_graph (fixup_graph_type *fixup_graph)
{
  double sqrt_avg_vertex_weight = 0;
  double total_vertex_weight = 0;
  double k_pos = 0;
  double k_neg = 0;
  /* Vector to hold D(v) = sum_out_edges(v) - sum_in_edges(v).  */
  gcov_type *diff_out_in = NULL;
  gcov_type supply_value = 1, demand_value = 0;
  gcov_type fcost = 0;
  int new_entry_index = 0, new_exit_index = 0;
  int i = 0, j = 0;
  int new_index = 0;
  basic_block bb;
  edge e;
  edge_iterator ei;
  fixup_edge_p pfedge, r_pfedge;
  fixup_edge_p fedge_list;
  int fnum_edges;

  /* Each basic_block will be split into 2 during vertex transformation.  */
  int fnum_vertices_after_transform =  2 * n_basic_blocks_for_fn (cfun);
  int fnum_edges_after_transform =
    n_edges_for_fn (cfun) + n_basic_blocks_for_fn (cfun);

  /* Count the new SOURCE and EXIT vertices to be added.  */
  int fmax_num_vertices =
    (fnum_vertices_after_transform + n_edges_for_fn (cfun)
     + n_basic_blocks_for_fn (cfun) + 2);

  /* In create_fixup_graph: Each basic block and edge can be split into 3
     edges. Number of balance edges = n_basic_blocks. So after
     create_fixup_graph:
     max_edges = 4 * n_basic_blocks + 3 * n_edges
     Accounting for residual flow edges
     max_edges = 2 * (4 * n_basic_blocks + 3 * n_edges)
     = 8 * n_basic_blocks + 6 * n_edges
     < 8 * n_basic_blocks + 8 * n_edges.  */
  int fmax_num_edges = 8 * (n_basic_blocks_for_fn (cfun) +
			    n_edges_for_fn (cfun));

  /* Initial num of vertices in the fixup graph.  */
  fixup_graph->num_vertices = n_basic_blocks_for_fn (cfun);

  /* Fixup graph vertex list.  */
  fixup_graph->vertex_list =
    (fixup_vertex_p) xcalloc (fmax_num_vertices, sizeof (fixup_vertex_type));

  /* Fixup graph edge list.  */
  fixup_graph->edge_list =
    (fixup_edge_p) xcalloc (fmax_num_edges, sizeof (fixup_edge_type));

  diff_out_in =
    (gcov_type *) xcalloc (1 + fnum_vertices_after_transform,
			   sizeof (gcov_type));

  /* Compute constants b, k_pos, k_neg used in the cost function calculation.
     b = sqrt(avg_vertex_weight(cfg)); k_pos = b; k_neg = 50b.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    total_vertex_weight += bb->count;

  sqrt_avg_vertex_weight = mcf_sqrt (total_vertex_weight /
				     n_basic_blocks_for_fn (cfun));

  k_pos = K_POS (sqrt_avg_vertex_weight);
  k_neg = K_NEG (sqrt_avg_vertex_weight);

  /* 1. Vertex Transformation: Split each vertex v into two vertices v' and v'',
     connected by an edge e from v' to v''. w(e) = w(v).  */

  if (dump_file)
    fprintf (dump_file, "\nVertex transformation:\n");

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
  {
    /* v'->v'': index1->(index1+1).  */
    i = 2 * bb->index;
    fcost = (gcov_type) COST (k_pos, bb->count);
    add_fixup_edge (fixup_graph, i, i + 1, VERTEX_SPLIT_EDGE, bb->count,
                    fcost, CAP_INFINITY);
    fixup_graph->num_vertices++;

    FOR_EACH_EDGE (e, ei, bb->succs)
    {
      /* Edges with ignore attribute set should be treated like they don't
         exist.  */
      if (EDGE_INFO (e) && EDGE_INFO (e)->ignore)
        continue;
      j = 2 * e->dest->index;
      fcost = (gcov_type) COST (k_pos, e->count);
      add_fixup_edge (fixup_graph, i + 1, j, REDIRECT_EDGE, e->count, fcost,
                      CAP_INFINITY);
    }
  }

  /* After vertex transformation.  */
  gcc_assert (fixup_graph->num_vertices == fnum_vertices_after_transform);
  /* Redirect edges are not added for edges with ignore attribute.  */
  gcc_assert (fixup_graph->num_edges <= fnum_edges_after_transform);

  fnum_edges_after_transform = fixup_graph->num_edges;

  /* 2. Initialize D(v).  */
  for (i = 0; i < fnum_edges_after_transform; i++)
    {
      pfedge = fixup_graph->edge_list + i;
      diff_out_in[pfedge->src] += pfedge->weight;
      diff_out_in[pfedge->dest] -= pfedge->weight;
    }

  /* Entry block - vertex indices 0, 1; EXIT block - vertex indices 2, 3.  */
  for (i = 0; i <= 3; i++)
    diff_out_in[i] = 0;

  /* 3. Add reverse edges: needed to decrease counts during smoothing.  */
  if (dump_file)
    fprintf (dump_file, "\nReverse edges:\n");
  for (i = 0; i < fnum_edges_after_transform; i++)
    {
      pfedge = fixup_graph->edge_list + i;
      if ((pfedge->src == 0) || (pfedge->src == 2))
        continue;
      r_pfedge = find_fixup_edge (fixup_graph, pfedge->dest, pfedge->src);
      if (!r_pfedge && pfedge->weight)
	{
	  /* Skip adding reverse edges for edges with w(e) = 0, as its maximum
	     capacity is 0.  */
	  fcost = (gcov_type) COST (k_neg, pfedge->weight);
	  add_fixup_edge (fixup_graph, pfedge->dest, pfedge->src,
			  REVERSE_EDGE, 0, fcost, pfedge->weight);
	}
    }

  /* 4. Create single source and sink. Connect new source vertex s' to function
     entry block. Connect sink vertex t' to function exit.  */
  if (dump_file)
    fprintf (dump_file, "\ns'->S, T->t':\n");

  new_entry_index = fixup_graph->new_entry_index = fixup_graph->num_vertices;
  fixup_graph->num_vertices++;
  /* Set supply_value to 1 to avoid zero count function ENTRY.  */
  add_fixup_edge (fixup_graph, new_entry_index, ENTRY_BLOCK, SOURCE_CONNECT_EDGE,
		  1 /* supply_value */, 0, 1 /* supply_value */);

  /* Create new exit with EXIT_BLOCK as single pred.  */
  new_exit_index = fixup_graph->new_exit_index = fixup_graph->num_vertices;
  fixup_graph->num_vertices++;
  add_fixup_edge (fixup_graph, 2 * EXIT_BLOCK + 1, new_exit_index,
                  SINK_CONNECT_EDGE,
                  0 /* demand_value */, 0, 0 /* demand_value */);

  /* Connect vertices with unbalanced D(v) to source/sink.  */
  if (dump_file)
    fprintf (dump_file, "\nD(v) balance:\n");
  /* Skip vertices for ENTRY (0, 1) and EXIT (2,3) blocks, so start with i = 4.
     diff_out_in[v''] will be 0, so skip v'' vertices, hence i += 2.  */
  for (i = 4; i < new_entry_index; i += 2)
    {
      if (diff_out_in[i] > 0)
	{
	  add_fixup_edge (fixup_graph, i, new_exit_index, BALANCE_EDGE, 0, 0,
			  diff_out_in[i]);
	  demand_value += diff_out_in[i];
	}
      else if (diff_out_in[i] < 0)
	{
	  add_fixup_edge (fixup_graph, new_entry_index, i, BALANCE_EDGE, 0, 0,
			  -diff_out_in[i]);
	  supply_value -= diff_out_in[i];
	}
    }

  /* Set supply = demand.  */
  if (dump_file)
    {
      fprintf (dump_file, "\nAdjust supply and demand:\n");
      fprintf (dump_file, "supply_value=%" PRId64 "\n",
	       supply_value);
      fprintf (dump_file, "demand_value=%" PRId64 "\n",
	       demand_value);
    }

  if (demand_value > supply_value)
    {
      pfedge = find_fixup_edge (fixup_graph, new_entry_index, ENTRY_BLOCK);
      pfedge->max_capacity += (demand_value - supply_value);
    }
  else
    {
      pfedge = find_fixup_edge (fixup_graph, 2 * EXIT_BLOCK + 1, new_exit_index);
      pfedge->max_capacity += (supply_value - demand_value);
    }

  /* 6. Normalize edges: remove anti-parallel edges. Anti-parallel edges are
     created by the vertex transformation step from self-edges in the original
     CFG and by the reverse edges added earlier.  */
  if (dump_file)
    fprintf (dump_file, "\nNormalize edges:\n");

  fnum_edges = fixup_graph->num_edges;
  fedge_list = fixup_graph->edge_list;

  for (i = 0; i < fnum_edges; i++)
    {
      pfedge = fedge_list + i;
      r_pfedge = find_fixup_edge (fixup_graph, pfedge->dest, pfedge->src);
      if (((pfedge->type == VERTEX_SPLIT_EDGE)
	   || (pfedge->type == REDIRECT_EDGE)) && r_pfedge)
	{
	  new_index = fixup_graph->num_vertices;
	  fixup_graph->num_vertices++;

	  if (dump_file)
	    {
	      fprintf (dump_file, "\nAnti-parallel edge:\n");
	      dump_fixup_edge (dump_file, fixup_graph, pfedge);
	      dump_fixup_edge (dump_file, fixup_graph, r_pfedge);
	      fprintf (dump_file, "New vertex is %d.\n", new_index);
	      fprintf (dump_file, "------------------\n");
	    }

	  pfedge->cost /= 2;
	  pfedge->norm_vertex_index = new_index;
	  if (dump_file)
	    {
	      fprintf (dump_file, "After normalization:\n");
	      dump_fixup_edge (dump_file, fixup_graph, pfedge);
	    }

	  /* Add a new fixup edge: new_index->src.  */
	  add_fixup_edge (fixup_graph, new_index, pfedge->src,
			  REVERSE_NORMALIZED_EDGE, 0, r_pfedge->cost,
			  r_pfedge->max_capacity);
	  gcc_assert (fixup_graph->num_vertices <= fmax_num_vertices);

	  /* Edge: r_pfedge->src -> r_pfedge->dest
             ==> r_pfedge->src -> new_index.  */
	  r_pfedge->dest = new_index;
	  r_pfedge->type = REVERSE_NORMALIZED_EDGE;
	  r_pfedge->cost = pfedge->cost;
	  r_pfedge->max_capacity = pfedge->max_capacity;
	  if (dump_file)
	    dump_fixup_edge (dump_file, fixup_graph, r_pfedge);
	}
    }

  if (dump_file)
    dump_fixup_graph (dump_file, fixup_graph, "After create_fixup_graph()");

  /* Cleanup.  */
  free (diff_out_in);
}


/* Allocates space for the structures in AUGMENTING_PATH.  The space needed is
   proportional to the number of nodes in the graph, which is given by
   GRAPH_SIZE.  */

static void
init_augmenting_path (augmenting_path_type *augmenting_path, int graph_size)
{
  augmenting_path->queue_list.queue = (int *)
    xcalloc (graph_size + 2, sizeof (int));
  augmenting_path->queue_list.size = graph_size + 2;
  augmenting_path->bb_pred = (int *) xcalloc (graph_size, sizeof (int));
  augmenting_path->is_visited = (int *) xcalloc (graph_size, sizeof (int));
}

/* Free the structures in AUGMENTING_PATH.  */
static void
free_augmenting_path (augmenting_path_type *augmenting_path)
{
  free (augmenting_path->queue_list.queue);
  free (augmenting_path->bb_pred);
  free (augmenting_path->is_visited);
}


/* Queue routines. Assumes queue will never overflow.  */

static void
init_queue (queue_type *queue_list)
{
  gcc_assert (queue_list);
  queue_list->head = 0;
  queue_list->tail = 0;
}

/* Return true if QUEUE_LIST is empty.  */
static bool
is_empty (queue_type *queue_list)
{
  return (queue_list->head == queue_list->tail);
}

/* Insert element X into QUEUE_LIST.  */
static void
enqueue (queue_type *queue_list, int x)
{
  gcc_assert (queue_list->tail < queue_list->size);
  queue_list->queue[queue_list->tail] = x;
  (queue_list->tail)++;
}

/* Return the first element in QUEUE_LIST.  */
static int
dequeue (queue_type *queue_list)
{
  int x;
  gcc_assert (queue_list->head >= 0);
  x = queue_list->queue[queue_list->head];
  (queue_list->head)++;
  return x;
}


/* Finds a negative cycle in the residual network using
   the Bellman-Ford algorithm. The flow on the found cycle is reversed by the
   minimum residual capacity of that cycle. ENTRY and EXIT vertices are not
   considered.

Parameters:
   FIXUP_GRAPH - Residual graph  (input/output)
   The following are allocated/freed by the caller:
   PI - Vector to hold predecessors in path  (pi = pred index)
   D - D[I] holds minimum cost of path from i to sink
   CYCLE - Vector to hold the minimum cost cycle

Return:
   true if a negative cycle was found, false otherwise.  */

static bool
cancel_negative_cycle (fixup_graph_type *fixup_graph,
		       int *pi, gcov_type *d, int *cycle)
{
  int i, j, k;
  int fnum_vertices, fnum_edges;
  fixup_edge_p fedge_list, pfedge, r_pfedge;
  bool found_cycle = false;
  int cycle_start = 0, cycle_end = 0;
  gcov_type sum_cost = 0, cycle_flow = 0;
  int new_entry_index;
  bool propagated = false;

  gcc_assert (fixup_graph);
  fnum_vertices = fixup_graph->num_vertices;
  fnum_edges = fixup_graph->num_edges;
  fedge_list = fixup_graph->edge_list;
  new_entry_index = fixup_graph->new_entry_index;

  /* Initialize.  */
  /* Skip ENTRY.  */
  for (i = 1; i < fnum_vertices; i++)
    {
      d[i] = CAP_INFINITY;
      pi[i] = -1;
      cycle[i] = -1;
    }
  d[ENTRY_BLOCK] = 0;

  /* Relax.  */
  for (k = 1; k < fnum_vertices; k++)
  {
    propagated = false;
    for (i = 0; i < fnum_edges; i++)
      {
	pfedge = fedge_list + i;
	if (pfedge->src == new_entry_index)
	  continue;
	if (pfedge->is_rflow_valid && pfedge->rflow
            && d[pfedge->src] != CAP_INFINITY
	    && (d[pfedge->dest] > d[pfedge->src] + pfedge->cost))
	  {
	    d[pfedge->dest] = d[pfedge->src] + pfedge->cost;
	    pi[pfedge->dest] = pfedge->src;
            propagated = true;
	  }
      }
    if (!propagated)
      break;
  }

  if (!propagated)
  /* No negative cycles exist.  */
    return 0;

  /* Detect.  */
  for (i = 0; i < fnum_edges; i++)
    {
      pfedge = fedge_list + i;
      if (pfedge->src == new_entry_index)
	continue;
      if (pfedge->is_rflow_valid && pfedge->rflow
          && d[pfedge->src] != CAP_INFINITY
	  && (d[pfedge->dest] > d[pfedge->src] + pfedge->cost))
	{
	  found_cycle = true;
	  break;
	}
    }

  if (!found_cycle)
    return 0;

  /* Augment the cycle with the cycle's minimum residual capacity.  */
  found_cycle = false;
  cycle[0] = pfedge->dest;
  j = pfedge->dest;

  for (i = 1; i < fnum_vertices; i++)
    {
      j = pi[j];
      cycle[i] = j;
      for (k = 0; k < i; k++)
	{
	  if (cycle[k] == j)
	    {
	      /* cycle[k] -> ... -> cycle[i].  */
	      cycle_start = k;
	      cycle_end = i;
	      found_cycle = true;
	      break;
	    }
	}
      if (found_cycle)
	break;
    }

  gcc_assert (cycle[cycle_start] == cycle[cycle_end]);
  if (dump_file)
    fprintf (dump_file, "\nNegative cycle length is %d:\n",
	     cycle_end - cycle_start);

  sum_cost = 0;
  cycle_flow = CAP_INFINITY;
  for (k = cycle_start; k < cycle_end; k++)
    {
      pfedge = find_fixup_edge (fixup_graph, cycle[k + 1], cycle[k]);
      cycle_flow = MIN (cycle_flow, pfedge->rflow);
      sum_cost += pfedge->cost;
      if (dump_file)
	fprintf (dump_file, "%d ", cycle[k]);
    }

  if (dump_file)
    {
      fprintf (dump_file, "%d", cycle[k]);
      fprintf (dump_file,
	       ": (%" PRId64 ", %" PRId64
	       ")\n", sum_cost, cycle_flow);
      fprintf (dump_file,
	       "Augment cycle with %" PRId64 "\n",
	       cycle_flow);
    }

  for (k = cycle_start; k < cycle_end; k++)
    {
      pfedge = find_fixup_edge (fixup_graph, cycle[k + 1], cycle[k]);
      r_pfedge = find_fixup_edge (fixup_graph, cycle[k], cycle[k + 1]);
      pfedge->rflow -= cycle_flow;
      if (pfedge->type)
	pfedge->flow += cycle_flow;
      r_pfedge->rflow += cycle_flow;
      if (r_pfedge->type)
	r_pfedge->flow -= cycle_flow;
    }

  return true;
}


/* Computes the residual flow for FIXUP_GRAPH by setting the rflow field of
   the edges. ENTRY and EXIT vertices should not be considered.  */

static void
compute_residual_flow (fixup_graph_type *fixup_graph)
{
  int i;
  int fnum_edges;
  fixup_edge_p fedge_list, pfedge;

  gcc_assert (fixup_graph);

  if (dump_file)
    fputs ("\ncompute_residual_flow():\n", dump_file);

  fnum_edges = fixup_graph->num_edges;
  fedge_list = fixup_graph->edge_list;

  for (i = 0; i < fnum_edges; i++)
    {
      pfedge = fedge_list + i;
      pfedge->rflow = pfedge->max_capacity - pfedge->flow;
      pfedge->is_rflow_valid = true;
      add_rfixup_edge (fixup_graph, pfedge->dest, pfedge->src, pfedge->flow,
		       -pfedge->cost);
    }
}


/* Uses Edmonds-Karp algorithm - BFS to find augmenting path from SOURCE to
   SINK. The fields in the edge vector in the FIXUP_GRAPH are not modified by
   this routine. The vector bb_pred in the AUGMENTING_PATH structure is updated
   to reflect the path found.
   Returns: 0 if no augmenting path is found, 1 otherwise.  */

static int
find_augmenting_path (fixup_graph_type *fixup_graph,
		      augmenting_path_type *augmenting_path, int source,
		      int sink)
{
  int u = 0;
  int i;
  fixup_vertex_p fvertex_list, pfvertex;
  fixup_edge_p pfedge;
  int *bb_pred, *is_visited;
  queue_type *queue_list;

  gcc_assert (augmenting_path);
  bb_pred = augmenting_path->bb_pred;
  gcc_assert (bb_pred);
  is_visited = augmenting_path->is_visited;
  gcc_assert (is_visited);
  queue_list = &(augmenting_path->queue_list);

  gcc_assert (fixup_graph);

  fvertex_list = fixup_graph->vertex_list;

  for (u = 0; u < fixup_graph->num_vertices; u++)
    is_visited[u] = 0;

  init_queue (queue_list);
  enqueue (queue_list, source);
  bb_pred[source] = -1;

  while (!is_empty (queue_list))
    {
      u = dequeue (queue_list);
      is_visited[u] = 1;
      pfvertex = fvertex_list + u;
      for (i = 0; pfvertex->succ_edges.iterate (i, &pfedge);
	   i++)
	{
	  int dest = pfedge->dest;
	  if ((pfedge->rflow > 0) && (is_visited[dest] == 0))
	    {
	      enqueue (queue_list, dest);
	      bb_pred[dest] = u;
	      is_visited[dest] = 1;
	      if (dest == sink)
		return 1;
	    }
	}
    }

  return 0;
}


/* Routine to find the maximal flow:
   Algorithm:
   1. Initialize flow to 0
   2. Find an augmenting path form source to sink.
   3. Send flow equal to the path's residual capacity along the edges of this path.
   4. Repeat steps 2 and 3 until no new augmenting path is found.

Parameters:
SOURCE: index of source vertex (input)
SINK: index of sink vertex    (input)
FIXUP_GRAPH: adjacency matrix representing the graph. The flow of the edges will be
             set to have a valid maximal flow by this routine. (input)
Return: Maximum flow possible.  */

static gcov_type
find_max_flow (fixup_graph_type *fixup_graph, int source, int sink)
{
  int fnum_edges;
  augmenting_path_type augmenting_path;
  int *bb_pred;
  gcov_type max_flow = 0;
  int i, u;
  fixup_edge_p fedge_list, pfedge, r_pfedge;

  gcc_assert (fixup_graph);

  fnum_edges = fixup_graph->num_edges;
  fedge_list = fixup_graph->edge_list;

  /* Initialize flow to 0.  */
  for (i = 0; i < fnum_edges; i++)
    {
      pfedge = fedge_list + i;
      pfedge->flow = 0;
    }

  compute_residual_flow (fixup_graph);

  init_augmenting_path (&augmenting_path, fixup_graph->num_vertices);

  bb_pred = augmenting_path.bb_pred;
  while (find_augmenting_path (fixup_graph, &augmenting_path, source, sink))
    {
      /* Determine the amount by which we can increment the flow.  */
      gcov_type increment = CAP_INFINITY;
      for (u = sink; u != source; u = bb_pred[u])
	{
	  pfedge = find_fixup_edge (fixup_graph, bb_pred[u], u);
	  increment = MIN (increment, pfedge->rflow);
	}
      max_flow += increment;

      /* Now increment the flow. EXIT vertex index is 1.  */
      for (u = sink; u != source; u = bb_pred[u])
	{
	  pfedge = find_fixup_edge (fixup_graph, bb_pred[u], u);
	  r_pfedge = find_fixup_edge (fixup_graph, u, bb_pred[u]);
	  if (pfedge->type)
	    {
	      /* forward edge.  */
	      pfedge->flow += increment;
	      pfedge->rflow -= increment;
	      r_pfedge->rflow += increment;
	    }
	  else
	    {
	      /* backward edge.  */
	      gcc_assert (r_pfedge->type);
	      r_pfedge->rflow += increment;
	      r_pfedge->flow -= increment;
	      pfedge->rflow -= increment;
	    }
	}

      if (dump_file)
	{
	  fprintf (dump_file, "\nDump augmenting path:\n");
	  for (u = sink; u != source; u = bb_pred[u])
	    {
	      print_basic_block (dump_file, fixup_graph, u);
	      fprintf (dump_file, "<-");
	    }
	  fprintf (dump_file,
		   "ENTRY  (path_capacity=%" PRId64 ")\n",
		   increment);
	  fprintf (dump_file,
		   "Network flow is %" PRId64 ".\n",
		   max_flow);
	}
    }

  free_augmenting_path (&augmenting_path);
  if (dump_file)
    dump_fixup_graph (dump_file, fixup_graph, "After find_max_flow()");
  return max_flow;
}


/* Computes the corrected edge and basic block weights using FIXUP_GRAPH
   after applying the find_minimum_cost_flow() routine.  */

static void
adjust_cfg_counts (fixup_graph_type *fixup_graph)
{
  basic_block bb;
  edge e;
  edge_iterator ei;
  int i, j;
  fixup_edge_p pfedge, pfedge_n;

  gcc_assert (fixup_graph);

  if (dump_file)
    fprintf (dump_file, "\nadjust_cfg_counts():\n");

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      i = 2 * bb->index;

      /* Fixup BB.  */
      if (dump_file)
        fprintf (dump_file,
                 "BB%d: %" PRId64 "", bb->index, bb->count);

      pfedge = find_fixup_edge (fixup_graph, i, i + 1);
      if (pfedge->flow)
        {
          bb->count += pfedge->flow;
	  if (dump_file)
	    {
	      fprintf (dump_file, " + %" PRId64 "(",
	               pfedge->flow);
	      print_edge (dump_file, fixup_graph, i, i + 1);
	      fprintf (dump_file, ")");
	    }
        }

      pfedge_n =
        find_fixup_edge (fixup_graph, i + 1, pfedge->norm_vertex_index);
      /* Deduct flow from normalized reverse edge.  */
      if (pfedge->norm_vertex_index && pfedge_n->flow)
        {
          bb->count -= pfedge_n->flow;
	  if (dump_file)
	    {
	      fprintf (dump_file, " - %" PRId64 "(",
		       pfedge_n->flow);
	      print_edge (dump_file, fixup_graph, i + 1,
			  pfedge->norm_vertex_index);
	      fprintf (dump_file, ")");
	    }
        }
      if (dump_file)
        fprintf (dump_file, " = %" PRId64 "\n", bb->count);

      /* Fixup edge.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
          /* Treat edges with ignore attribute set as if they don't exist.  */
          if (EDGE_INFO (e) && EDGE_INFO (e)->ignore)
	    continue;

          j = 2 * e->dest->index;
          if (dump_file)
	    fprintf (dump_file, "%d->%d: %" PRId64 "",
		     bb->index, e->dest->index, e->count);

          pfedge = find_fixup_edge (fixup_graph, i + 1, j);

          if (bb->index != e->dest->index)
	    {
	      /* Non-self edge.  */
	      if (pfedge->flow)
	        {
	          e->count += pfedge->flow;
	          if (dump_file)
		    {
		      fprintf (dump_file, " + %" PRId64 "(",
			       pfedge->flow);
		      print_edge (dump_file, fixup_graph, i + 1, j);
		      fprintf (dump_file, ")");
		    }
	        }

	      pfedge_n =
	        find_fixup_edge (fixup_graph, j, pfedge->norm_vertex_index);
	      /* Deduct flow from normalized reverse edge.  */
	      if (pfedge->norm_vertex_index && pfedge_n->flow)
	        {
	          e->count -= pfedge_n->flow;
	          if (dump_file)
		    {
		      fprintf (dump_file, " - %" PRId64 "(",
			       pfedge_n->flow);
		      print_edge (dump_file, fixup_graph, j,
			          pfedge->norm_vertex_index);
		      fprintf (dump_file, ")");
		    }
	        }
	    }
          else
	    {
	      /* Handle self edges. Self edge is split with a normalization
                 vertex. Here i=j.  */
	      pfedge = find_fixup_edge (fixup_graph, j, i + 1);
	      pfedge_n =
	        find_fixup_edge (fixup_graph, i + 1, pfedge->norm_vertex_index);
	      e->count += pfedge_n->flow;
	      bb->count += pfedge_n->flow;
	      if (dump_file)
	        {
	          fprintf (dump_file, "(self edge)");
	          fprintf (dump_file, " + %" PRId64 "(",
		           pfedge_n->flow);
	          print_edge (dump_file, fixup_graph, i + 1,
			      pfedge->norm_vertex_index);
	          fprintf (dump_file, ")");
	        }
	    }

          if (bb->count)
	    e->probability = REG_BR_PROB_BASE * e->count / bb->count;
          if (dump_file)
	    fprintf (dump_file, " = %" PRId64 "\t(%.1f%%)\n",
		     e->count, e->probability * 100.0 / REG_BR_PROB_BASE);
        }
    }

  ENTRY_BLOCK_PTR_FOR_FN (cfun)->count =
		     sum_edge_counts (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs);
  EXIT_BLOCK_PTR_FOR_FN (cfun)->count =
		     sum_edge_counts (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds);

  /* Compute edge probabilities.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      if (bb->count)
        {
          FOR_EACH_EDGE (e, ei, bb->succs)
            e->probability = REG_BR_PROB_BASE * e->count / bb->count;
        }
      else
        {
          int total = 0;
          FOR_EACH_EDGE (e, ei, bb->succs)
            if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
              total++;
          if (total)
            {
              FOR_EACH_EDGE (e, ei, bb->succs)
                {
                  if (!(e->flags & (EDGE_COMPLEX | EDGE_FAKE)))
                    e->probability = REG_BR_PROB_BASE / total;
                  else
                    e->probability = 0;
                }
            }
          else
            {
              total += EDGE_COUNT (bb->succs);
              FOR_EACH_EDGE (e, ei, bb->succs)
                  e->probability = REG_BR_PROB_BASE / total;
            }
        }
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nCheck %s() CFG flow conservation:\n",
	       current_function_name ());
      FOR_EACH_BB_FN (bb, cfun)
        {
          if ((bb->count != sum_edge_counts (bb->preds))
               || (bb->count != sum_edge_counts (bb->succs)))
            {
              fprintf (dump_file,
                       "BB%d(%" PRId64 ")  **INVALID**: ",
                       bb->index, bb->count);
              fprintf (stderr,
                       "******** BB%d(%" PRId64
                       ")  **INVALID**: \n", bb->index, bb->count);
              fprintf (dump_file, "in_edges=%" PRId64 " ",
                       sum_edge_counts (bb->preds));
              fprintf (dump_file, "out_edges=%" PRId64 "\n",
                       sum_edge_counts (bb->succs));
            }
         }
    }
}


/* Implements the negative cycle canceling algorithm to compute a minimum cost
   flow.
Algorithm:
1. Find maximal flow.
2. Form residual network
3. Repeat:
  While G contains a negative cost cycle C, reverse the flow on the found cycle
  by the minimum residual capacity in that cycle.
4. Form the minimal cost flow
  f(u,v) = rf(v, u)
Input:
  FIXUP_GRAPH - Initial fixup graph.
  The flow field is modified to represent the minimum cost flow.  */

static void
find_minimum_cost_flow (fixup_graph_type *fixup_graph)
{
  /* Holds the index of predecessor in path.  */
  int *pred;
  /* Used to hold the minimum cost cycle.  */
  int *cycle;
  /* Used to record the number of iterations of cancel_negative_cycle.  */
  int iteration;
  /* Vector d[i] holds the minimum cost of path from i to sink.  */
  gcov_type *d;
  int fnum_vertices;
  int new_exit_index;
  int new_entry_index;

  gcc_assert (fixup_graph);
  fnum_vertices = fixup_graph->num_vertices;
  new_exit_index = fixup_graph->new_exit_index;
  new_entry_index = fixup_graph->new_entry_index;

  find_max_flow (fixup_graph, new_entry_index, new_exit_index);

  /* Initialize the structures for find_negative_cycle().  */
  pred = (int *) xcalloc (fnum_vertices, sizeof (int));
  d = (gcov_type *) xcalloc (fnum_vertices, sizeof (gcov_type));
  cycle = (int *) xcalloc (fnum_vertices, sizeof (int));

  /* Repeatedly find and cancel negative cost cycles, until
     no more negative cycles exist. This also updates the flow field
     to represent the minimum cost flow so far.  */
  iteration = 0;
  while (cancel_negative_cycle (fixup_graph, pred, d, cycle))
    {
      iteration++;
      if (iteration > MAX_ITER (fixup_graph->num_vertices,
                                fixup_graph->num_edges))
        break;
    }

  if (dump_file)
    dump_fixup_graph (dump_file, fixup_graph,
		      "After find_minimum_cost_flow()");

  /* Cleanup structures.  */
  free (pred);
  free (d);
  free (cycle);
}


/* Compute the sum of the edge counts in TO_EDGES.  */

gcov_type
sum_edge_counts (vec<edge, va_gc> *to_edges)
{
  gcov_type sum = 0;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, to_edges)
    {
      if (EDGE_INFO (e) && EDGE_INFO (e)->ignore)
        continue;
      sum += e->count;
    }
  return sum;
}


/* Main routine. Smoothes the initial assigned basic block and edge counts using
   a minimum cost flow algorithm, to ensure that the flow consistency rule is
   obeyed: sum of outgoing edges = sum of incoming edges for each basic
   block.  */

void
mcf_smooth_cfg (void)
{
  fixup_graph_type fixup_graph;
  memset (&fixup_graph, 0, sizeof (fixup_graph));
  create_fixup_graph (&fixup_graph);
  find_minimum_cost_flow (&fixup_graph);
  adjust_cfg_counts (&fixup_graph);
  delete_fixup_graph (&fixup_graph);
}
