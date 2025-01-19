/* Trimming an exploded graph to a subset of nodes and edges.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_TRIMMED_GRAPH_H
#define GCC_ANALYZER_TRIMMED_GRAPH_H

namespace ana {

/* Forward decls.  */

class trimmed_node;
class trimmed_edge;
class trimmed_graph;
class trimmed_cluster;

/* A traits class for trimming a digraph to a subset of nodes and edges.  */

struct tg_traits
{
  typedef trimmed_node node_t;
  typedef trimmed_edge edge_t;
  typedef trimmed_graph graph_t;
  struct dump_args_t
  {
    typedef typename eg_traits::dump_args_t inner_args_t;

    dump_args_t (const inner_args_t &inner_args)
    : m_inner_args (inner_args)
    {
    }

    const inner_args_t &m_inner_args;
  };
  typedef trimmed_cluster cluster_t;
};

/* A node within the trimmed_graph, corresponding to an "inner node"
   within the original exploded_graph.  */

class trimmed_node : public dnode<tg_traits>
{
public:
  trimmed_node (const exploded_node *inner_node)
  : m_inner_node (inner_node) {}

  void dump_dot (graphviz_out *gv,
		 const dump_args_t &args) const final override;

private:
  const exploded_node *m_inner_node;
};

/* An edge within the trimmed_graph, corresponding to an "inner edge"
   within the original exploded_graph.  */

class trimmed_edge : public dedge<tg_traits>
{
 public:
  trimmed_edge (trimmed_node *src, trimmed_node *dest,
		const exploded_edge *inner_edge);

  void dump_dot (graphviz_out *gv,
		 const dump_args_t &args) const final override;

 private:
  const exploded_edge *m_inner_edge;
};

/* A digraph for trimming an exploded_graph to the subset of nodes and edges
   from which paths reach INNER_DST_NODE (along with a precanned way to print
   these in .dot form).  */

class trimmed_graph : public digraph <tg_traits>
{
 public:
  trimmed_graph (const exploded_graph &inner_graph,
		 const exploded_node *inner_dst_node);

  bool contains_p (const exploded_edge *eedge) const
  {
    hash_set <const exploded_edge *> & mut
      = const_cast <hash_set <const exploded_edge *> &> (m_eedges);
    return mut.contains (eedge);
  }

  void log_stats (logger *logger) const;

 private:
  /* The subset of nodes in the inner graph that are in the
     trimmed graph.  */
  hash_set <const exploded_node *> m_enodes;
  /* Likewise for edges.  */
  hash_set <const exploded_edge *> m_eedges;

  typedef hash_map<const exploded_node *, trimmed_node *> map_t;
  map_t m_map_from_enode_to_tnode;
};

class trimmed_cluster : public cluster <tg_traits>
{
};

} // namespace ana

#endif /* GCC_ANALYZER_TRIMMED_GRAPH_H */
