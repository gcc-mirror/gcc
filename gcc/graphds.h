/* Graph representation.
   Copyright (C) 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Structure representing edge of a graph.  */

struct edge
{
  int src, dest;	/* Source and destination.  */
  struct edge *pred_next, *succ_next;
			/* Next edge in predecessor and successor lists.  */
  void *data;		/* Data attached to the edge.  */
};

/* Structure representing vertex of a graph.  */

struct vertex
{
  struct edge *pred, *succ;
			/* Lists of predecessors and successors.  */
  int component;	/* Number of dfs restarts before reaching the
			   vertex.  */
  int post;		/* Postorder number.  */
  void *data;		/* Data attached to the vertex.  */
};

/* Structure representing a graph.  */

struct graph
{
  int n_vertices;	/* Number of vertices.  */
  struct vertex *vertices;
			/* The vertices.  */
};

struct graph *new_graph (int);
void dump_graph (FILE *, struct graph *);
struct edge *add_edge (struct graph *, int, int);
void identify_vertices (struct graph *, int, int);
int graphds_dfs (struct graph *, int *, int,
		 VEC (int, heap) **, bool, bitmap);
int graphds_scc (struct graph *, bitmap);
void graphds_domtree (struct graph *, int, int *, int *, int *);
typedef void (*graphds_edge_callback) (struct graph *, struct edge *);
void for_each_edge (struct graph *, graphds_edge_callback);
void free_graph (struct graph *g);
