/* Template class for Dijkstra's algorithm on directed graphs.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_SHORTEST_PATHS_H
#define GCC_SHORTEST_PATHS_H

#include "timevar.h"

/* A record of the shortest path to each node in an graph
   from the origin node.
   The constructor runs Dijkstra's algorithm, and the results are
   stored in this class.  */

template <typename GraphTraits, typename Path_t>
class shortest_paths
{
public:
  typedef typename GraphTraits::graph_t graph_t;
  typedef typename GraphTraits::node_t node_t;
  typedef typename GraphTraits::edge_t edge_t;
  typedef Path_t path_t;

  shortest_paths (const graph_t &graph, const node_t *origin);

  path_t get_shortest_path (const node_t *to) const;

private:
  const graph_t &m_graph;

  /* For each node (by index), the minimal distance to that node from the
     origin.  */
  auto_vec<int> m_dist;

  /* For each exploded_node (by index), the previous edge in the shortest
     path from the origin.  */
  auto_vec<const edge_t *> m_prev;
};

/* shortest_paths's constructor.

   Use Dijkstra's algorithm relative to ORIGIN to populate m_dist and
   m_prev with enough information to be able to generate Path_t instances
   to give the shortest path to any node in GRAPH from ORIGIN.  */

template <typename GraphTraits, typename Path_t>
inline
shortest_paths<GraphTraits, Path_t>::shortest_paths (const graph_t &graph,
						     const node_t *origin)
: m_graph (graph),
  m_dist (graph.m_nodes.length ()),
  m_prev (graph.m_nodes.length ())
{
  auto_timevar tv (TV_ANALYZER_SHORTEST_PATHS);

  auto_vec<int> queue (graph.m_nodes.length ());

  for (unsigned i = 0; i < graph.m_nodes.length (); i++)
    {
      m_dist.quick_push (INT_MAX);
      m_prev.quick_push (NULL);
      queue.quick_push (i);
    }
  m_dist[origin->m_index] = 0;

  while (queue.length () > 0)
    {
      /* Get minimal distance in queue.
	 FIXME: this is O(N^2); replace with a priority queue.  */
      int idx_with_min_dist = -1;
      int idx_in_queue_with_min_dist = -1;
      int min_dist = INT_MAX;
      for (unsigned i = 0; i < queue.length (); i++)
	{
	  int idx = queue[i];
	  if (m_dist[queue[i]] < min_dist)
	    {
	      min_dist = m_dist[idx];
	      idx_with_min_dist = idx;
	      idx_in_queue_with_min_dist = i;
	    }
	}
      gcc_assert (idx_with_min_dist != -1);
      gcc_assert (idx_in_queue_with_min_dist != -1);

      // FIXME: this is confusing: there are two indices here

      queue.unordered_remove (idx_in_queue_with_min_dist);

      node_t *n
	= static_cast <node_t *> (m_graph.m_nodes[idx_with_min_dist]);

      int i;
      edge_t *succ;
      FOR_EACH_VEC_ELT (n->m_succs, i, succ)
	{
	  // TODO: only for dest still in queue
	  node_t *dest = succ->m_dest;
	  int alt = m_dist[n->m_index] + 1;
	  if (alt < m_dist[dest->m_index])
	    {
	      m_dist[dest->m_index] = alt;
	      m_prev[dest->m_index] = succ;
	    }
	}
   }
}

/* Generate an Path_t instance giving the shortest path to the node
   TO from the origin node.  */

template <typename GraphTraits, typename Path_t>
inline Path_t
shortest_paths<GraphTraits, Path_t>::get_shortest_path (const node_t *to) const
{
  Path_t result;

  while (m_prev[to->m_index])
    {
      result.m_edges.safe_push (m_prev[to->m_index]);
      to = m_prev[to->m_index]->m_src;
    }

  result.m_edges.reverse ();

  return result;
}

#endif /* GCC_SHORTEST_PATHS_H */
