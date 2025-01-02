/* Graph representation and manipulation functions.
   Copyright (C) 2007-2025 Free Software Foundation, Inc.

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
#include "bitmap.h"
#include "graphds.h"

/* Dumps graph G into F.  */

void
dump_graph (FILE *f, struct graph *g)
{
  int i;
  struct graph_edge *e;

  fprintf (f, "digraph {\n");
  for (i = 0; i < g->n_vertices; i++)
    {
      fprintf (f, "\"%d\" [label=\"%d (%d): %p\"];\n",
	       i, i, g->vertices[i].component, g->vertices[i].data);
      for (e = g->vertices[i].pred; e; e = e->pred_next)
	fprintf (f, "\"%d\" -> \"%d\" [label=\"%p\"];\n", e->src, e->dest, e->data);
      for (e = g->vertices[i].succ; e; e = e->succ_next)
	fprintf (f, "\"%d\" -> \"%d\";\n", e->src, e->dest);
    }
  fprintf (f, "}\n");
}

/* Creates a new graph with N_VERTICES vertices.  */

struct graph *
new_graph (int n_vertices)
{
  struct graph *g = XNEW (struct graph);

  gcc_obstack_init (&g->ob);
  g->n_vertices = n_vertices;
  g->vertices = XOBNEWVEC (&g->ob, struct vertex, n_vertices);
  memset (g->vertices, 0, sizeof (struct vertex) * n_vertices);

  return g;
}

/* Adds an edge from F to T to graph G.  The new edge is returned.  */

struct graph_edge *
add_edge (struct graph *g, int f, int t)
{
  struct graph_edge *e = XOBNEW (&g->ob, struct graph_edge);
  struct vertex *vf = &g->vertices[f], *vt = &g->vertices[t];

  e->src = f;
  e->dest = t;

  e->pred_next = vt->pred;
  vt->pred = e;

  e->succ_next = vf->succ;
  vf->succ = e;

  e->data = NULL;
  return e;
}

/* Moves all the edges incident with U to V.  */

void
identify_vertices (struct graph *g, int v, int u)
{
  struct vertex *vv = &g->vertices[v];
  struct vertex *uu = &g->vertices[u];
  struct graph_edge *e, *next;

  for (e = uu->succ; e; e = next)
    {
      next = e->succ_next;

      e->src = v;
      e->succ_next = vv->succ;
      vv->succ = e;
    }
  uu->succ = NULL;

  for (e = uu->pred; e; e = next)
    {
      next = e->pred_next;

      e->dest = v;
      e->pred_next = vv->pred;
      vv->pred = e;
    }
  uu->pred = NULL;
}

/* Helper function for graphds_dfs.  Returns the source vertex of E, in the
   direction given by FORWARD.  */

static inline int
dfs_edge_src (struct graph_edge *e, bool forward)
{
  return forward ? e->src : e->dest;
}

/* Helper function for graphds_dfs.  Returns the destination vertex of E, in
   the direction given by FORWARD.  */

static inline int
dfs_edge_dest (struct graph_edge *e, bool forward)
{
  return forward ? e->dest : e->src;
}

/* Helper function for graphds_dfs.  Returns the first edge after E (including
   E), in the graph direction given by FORWARD, that belongs to SUBGRAPH.  If
   SKIP_EDGE_P is not NULL, it points to a callback function.  Edge E will be
   skipped if callback function returns true.  */

static inline struct graph_edge *
foll_in_subgraph (struct graph_edge *e, bool forward, bitmap subgraph,
		  skip_edge_callback skip_edge_p)
{
  int d;

  if (!e)
    return e;

  if (!subgraph && (!skip_edge_p || !skip_edge_p (e)))
    return e;

  while (e)
    {
      d = dfs_edge_dest (e, forward);
      /* Return edge if it belongs to subgraph and shouldn't be skipped.  */
      if ((!subgraph || bitmap_bit_p (subgraph, d))
	  && (!skip_edge_p || !skip_edge_p (e)))
	return e;

      e = forward ? e->succ_next : e->pred_next;
    }

  return e;
}

/* Helper function for graphds_dfs.  Select the first edge from V in G, in the
   direction given by FORWARD, that belongs to SUBGRAPH.  If SKIP_EDGE_P is not
   NULL, it points to a callback function.  Edge E will be skipped if callback
   function returns true.  */

static inline struct graph_edge *
dfs_fst_edge (struct graph *g, int v, bool forward, bitmap subgraph,
	      skip_edge_callback skip_edge_p)
{
  struct graph_edge *e;

  e = (forward ? g->vertices[v].succ : g->vertices[v].pred);
  return foll_in_subgraph (e, forward, subgraph, skip_edge_p);
}

/* Helper function for graphds_dfs.  Returns the next edge after E, in the
   graph direction given by FORWARD, that belongs to SUBGRAPH.  If SKIP_EDGE_P
   is not NULL, it points to a callback function.  Edge E will be skipped if
   callback function returns true.  */

static inline struct graph_edge *
dfs_next_edge (struct graph_edge *e, bool forward, bitmap subgraph,
	       skip_edge_callback skip_edge_p)
{
  return foll_in_subgraph (forward ? e->succ_next : e->pred_next,
			   forward, subgraph, skip_edge_p);
}

/* Runs dfs search over vertices of G, from NQ vertices in queue QS.
   The vertices in postorder are stored into QT.  If FORWARD is false,
   backward dfs is run.  If SUBGRAPH is not NULL, it specifies the
   subgraph of G to run DFS on.  Returns the number of the components
   of the graph (number of the restarts of DFS).  If SKIP_EDGE_P is not
   NULL, it points to a callback function.  Edge E will be skipped if
   callback function returns true.  */

int
graphds_dfs (struct graph *g, int *qs, int nq, vec<int> *qt,
	     bool forward, bitmap subgraph,
	     skip_edge_callback skip_edge_p)
{
  int i, tick = 0, v, comp = 0, top;
  struct graph_edge *e;
  struct graph_edge **stack = XNEWVEC (struct graph_edge *, g->n_vertices);
  bitmap_iterator bi;
  unsigned av;

  if (subgraph)
    {
      EXECUTE_IF_SET_IN_BITMAP (subgraph, 0, av, bi)
	{
	  g->vertices[av].component = -1;
	  g->vertices[av].post = -1;
	}
    }
  else
    {
      for (i = 0; i < g->n_vertices; i++)
	{
	  g->vertices[i].component = -1;
	  g->vertices[i].post = -1;
	}
    }

  for (i = 0; i < nq; i++)
    {
      v = qs[i];
      if (g->vertices[v].post != -1)
	continue;

      g->vertices[v].component = comp++;
      e = dfs_fst_edge (g, v, forward, subgraph, skip_edge_p);
      top = 0;

      while (1)
	{
	  while (e)
	    {
	      if (g->vertices[dfs_edge_dest (e, forward)].component
		  == -1)
		break;
	      e = dfs_next_edge (e, forward, subgraph, skip_edge_p);
	    }

	  if (!e)
	    {
	      if (qt)
		qt->safe_push (v);
	      g->vertices[v].post = tick++;

	      if (!top)
		break;

	      e = stack[--top];
	      v = dfs_edge_src (e, forward);
	      e = dfs_next_edge (e, forward, subgraph, skip_edge_p);
	      continue;
	    }

	  stack[top++] = e;
	  v = dfs_edge_dest (e, forward);
	  e = dfs_fst_edge (g, v, forward, subgraph, skip_edge_p);
	  g->vertices[v].component = comp - 1;
	}
    }

  free (stack);

  return comp;
}

/* Determines the strongly connected components of G, using the algorithm of
   Kosaraju -- first determine the postorder dfs numbering in reversed graph,
   then run the dfs on the original graph in the order given by decreasing
   numbers assigned by the previous pass.  If SUBGRAPH is not NULL, it
   specifies the subgraph of G whose strongly connected components we want
   to determine.  If SKIP_EDGE_P is not NULL, it points to a callback function.
   Edge E will be skipped if callback function returns true.  If SCC_GROUPING
   is not null, the nodes will be added to it in the following order:

   - If SCC A is a direct or indirect predecessor of SCC B in the SCC dag,
     A's nodes come before B's nodes.

   - All of an SCC's nodes are listed consecutively, although the order
     of the nodes within an SCC is not really meaningful.

   After running this function, v->component is the number of the strongly
   connected component for each vertex of G.  Returns the number of the
   sccs of G.  */

int
graphds_scc (struct graph *g, bitmap subgraph,
	     skip_edge_callback skip_edge_p, vec<int> *scc_grouping)
{
  int *queue = XNEWVEC (int, g->n_vertices);
  vec<int> postorder = vNULL;
  int nq, i, comp;
  unsigned v;
  bitmap_iterator bi;

  if (subgraph)
    {
      nq = 0;
      EXECUTE_IF_SET_IN_BITMAP (subgraph, 0, v, bi)
	{
	  queue[nq++] = v;
	}
    }
  else
    {
      for (i = 0; i < g->n_vertices; i++)
	queue[i] = i;
      nq = g->n_vertices;
    }

  graphds_dfs (g, queue, nq, &postorder, false, subgraph, skip_edge_p);
  gcc_assert (postorder.length () == (unsigned) nq);

  for (i = 0; i < nq; i++)
    queue[i] = postorder[nq - i - 1];
  comp = graphds_dfs (g, queue, nq, scc_grouping, true, subgraph, skip_edge_p);

  free (queue);
  postorder.release ();

  return comp;
}

/* Runs CALLBACK for all edges in G.  DATA is private data for CALLBACK.  */

void
for_each_edge (struct graph *g, graphds_edge_callback callback, void *data)
{
  struct graph_edge *e;
  int i;

  for (i = 0; i < g->n_vertices; i++)
    for (e = g->vertices[i].succ; e; e = e->succ_next)
      callback (g, e, data);
}

/* Releases the memory occupied by G.  */

void
free_graph (struct graph *g)
{
  obstack_free (&g->ob, NULL);
  free (g);
}

/* Returns the nearest common ancestor of X and Y in tree whose parent
   links are given by PARENT.  MARKS is the array used to mark the
   vertices of the tree, and MARK is the number currently used as a mark.  */

static int
tree_nca (int x, int y, int *parent, int *marks, int mark)
{
  if (x == -1 || x == y)
    return y;

  /* We climb with X and Y up the tree, marking the visited nodes.  When
     we first arrive to a marked node, it is the common ancestor.  */
  marks[x] = mark;
  marks[y] = mark;

  while (1)
    {
      x = parent[x];
      if (x == -1)
	break;
      if (marks[x] == mark)
	return x;
      marks[x] = mark;

      y = parent[y];
      if (y == -1)
	break;
      if (marks[y] == mark)
	return y;
      marks[y] = mark;
    }

  /* If we reached the root with one of the vertices, continue
     with the other one till we reach the marked part of the
     tree.  */
  if (x == -1)
    {
      for (y = parent[y]; marks[y] != mark; y = parent[y])
	continue;

      return y;
    }
  else
    {
      for (x = parent[x]; marks[x] != mark; x = parent[x])
	continue;

      return x;
    }
}

/* Determines the dominance tree of G (stored in the PARENT, SON and BROTHER
   arrays), where the entry node is ENTRY.  */

void
graphds_domtree (struct graph *g, int entry,
		 int *parent, int *son, int *brother)
{
  vec<int> postorder = vNULL;
  int *marks = XCNEWVEC (int, g->n_vertices);
  int mark = 1, i, v, idom;
  bool changed = true;
  struct graph_edge *e;

  /* We use a slight modification of the standard iterative algorithm, as
     described in

     K. D. Cooper, T. J. Harvey and K. Kennedy: A Simple, Fast Dominance
	Algorithm

     sort vertices in reverse postorder
     foreach v
       dom(v) = everything
     dom(entry) = entry;

     while (anything changes)
       foreach v
         dom(v) = {v} union (intersection of dom(p) over all predecessors of v)

     The sets dom(v) are represented by the parent links in the current version
     of the dominance tree.  */

  for (i = 0; i < g->n_vertices; i++)
    {
      parent[i] = -1;
      son[i] = -1;
      brother[i] = -1;
    }
  graphds_dfs (g, &entry, 1, &postorder, true, NULL);
  gcc_assert (postorder.length () == (unsigned) g->n_vertices);
  gcc_assert (postorder[g->n_vertices - 1] == entry);

  while (changed)
    {
      changed = false;

      for (i = g->n_vertices - 2; i >= 0; i--)
	{
	  v = postorder[i];
	  idom = -1;
	  for (e = g->vertices[v].pred; e; e = e->pred_next)
	    {
	      if (e->src != entry
		  && parent[e->src] == -1)
		continue;

	      idom = tree_nca (idom, e->src, parent, marks, mark++);
	    }

	  if (idom != parent[v])
	    {
	      parent[v] = idom;
	      changed = true;
	    }
	}
    }

  free (marks);
  postorder.release ();

  for (i = 0; i < g->n_vertices; i++)
    if (parent[i] != -1)
      {
	brother[i] = son[parent[i]];
	son[parent[i]] = i;
      }
}
