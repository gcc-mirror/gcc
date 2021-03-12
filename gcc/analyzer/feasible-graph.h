/* A graph for exploring trees of feasible paths through the egraph.
   Copyright (C) 2021 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_FEASIBLE_GRAPH_H
#define GCC_ANALYZER_FEASIBLE_GRAPH_H

namespace ana {

/* Forward decls.  */

class base_feasible_node;
  class feasible_node;
  class infeasible_node;
class base_feasible_edge;
  class feasible_edge;
  class infeasible_edge;
class feasible_graph;
class feasible_cluster;

/* A traits class for feasible_graph.  */

struct fg_traits
{
  typedef base_feasible_node node_t;
  typedef base_feasible_edge edge_t;
  typedef feasible_graph graph_t;
  struct dump_args_t
  {
    typedef typename eg_traits::dump_args_t inner_args_t;

    dump_args_t (const inner_args_t &inner_args)
    : m_inner_args (inner_args)
    {
    }

    const inner_args_t &m_inner_args;
  };
  typedef feasible_cluster cluster_t;
};

/* Base class of node within a feasible_graph.
   There can be 0 or more base_feasible_nodes per exploded_node.  */

class base_feasible_node : public dnode<fg_traits>
{
 public:
  void dump_dot_id (pretty_printer *pp) const;

  const exploded_node *get_inner_node () const { return m_inner_node; }
  unsigned get_index () const { return m_index; }

 protected:
  base_feasible_node (const exploded_node *inner_node, unsigned index)
  : m_inner_node (inner_node), m_index (index)
  {}

  const exploded_node *m_inner_node;
  unsigned m_index;
};

/* Subclass of base_feasible_node for a node that is reachable via a
   feasible path, with a particular state.  */

class feasible_node : public base_feasible_node
{
public:
  feasible_node (const exploded_node *inner_node, unsigned index,
		 const feasibility_state &state,
		 unsigned path_length)
  : base_feasible_node (inner_node, index),
    m_state (state),
    m_path_length (path_length)
  {
  }

  void dump_dot (graphviz_out *gv,
		 const dump_args_t &args) const FINAL OVERRIDE;

  const feasibility_state &get_state () const { return m_state; }
  const region_model &get_model () const { return m_state.get_model (); }
  const auto_sbitmap &get_snodes_visited () const
  {
    return m_state.get_snodes_visited ();
  }

  unsigned get_path_length () const { return m_path_length; }

private:
  feasibility_state m_state;
  unsigned m_path_length;
};

/* Subclass of base_feasible_node for a node that requires following
   an infeasible edge to reach (and thus terminating this part of the
   exploration).  */

class infeasible_node : public base_feasible_node
{
public:
  infeasible_node (const exploded_node *inner_node, unsigned index,
		   const rejected_constraint &rc)
  : base_feasible_node (inner_node, index),
    m_rc (rc)
  {
  }

  void dump_dot (graphviz_out *gv,
		 const dump_args_t &args) const FINAL OVERRIDE;

private:
  rejected_constraint m_rc;
};

/* Base class of edge within a feasible_graph.  */

class base_feasible_edge : public dedge<fg_traits>
{
 public:
  void dump_dot (graphviz_out *gv,
		 const dump_args_t &args) const FINAL OVERRIDE;

  const exploded_edge *get_inner_edge () const { return m_inner_edge; }

 protected:
  base_feasible_edge (base_feasible_node *src, base_feasible_node *dest,
		      const exploded_edge *inner_edge)
  : dedge<fg_traits> (src, dest), m_inner_edge (inner_edge)
  {
  }

  const exploded_edge *m_inner_edge;
};

/* Subclass of base_feasible_edge for connecting two feasible_nodes.  */

class feasible_edge : public base_feasible_edge
{
 public:
  feasible_edge (feasible_node *src, feasible_node *dest,
		 const exploded_edge *inner_edge)
  : base_feasible_edge (src, dest, inner_edge)
  {
  }
};

/* Subclass of base_feasible_edge for connecting a feasible_node
   to an infeasible_node (and thus terminating this part of the
   exploration).  */

class infeasible_edge : public base_feasible_edge
{
 public:
  infeasible_edge (feasible_node *src, infeasible_node *dest,
		   const exploded_edge *inner_edge)
  : base_feasible_edge (src, dest, inner_edge)
  {
  }
};

/* A digraph subclass for exploring trees of feasible paths through
   the egraph.  This is actually a tree.

   The paths within the graph of feasible_nodes express feasible paths
   through the graph, and it also captures known infeasible edges,
   which is invaluable for debugging.  */

class feasible_graph : public digraph <fg_traits>
{
 public:
  feasible_graph ();

  feasible_node *add_node (const exploded_node *enode,
			   const feasibility_state &state,
			   unsigned path_length);

  void add_feasibility_problem (feasible_node *src_fnode,
				const exploded_edge *eedge,
				const rejected_constraint &rc);

  exploded_path *make_epath (feasible_node *fnode) const;

  unsigned get_num_infeasible () const { return m_num_infeasible; }

  void log_stats (logger *logger) const;

private:
  unsigned m_num_infeasible;
};

class feasible_cluster : public cluster <fg_traits>
{
};

} // namespace ana

#endif /* GCC_ANALYZER_FEASIBLE_GRAPH_H */
