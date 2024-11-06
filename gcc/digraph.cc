/* Template classes for directed graphs.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "graphviz.h"
#include "digraph.h"
#include "shortest-paths.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* A family of digraph classes for writing selftests.  */

struct test_node;
struct test_edge;
struct test_graph;
struct test_dump_args_t {};
struct test_cluster;

struct test_graph_traits
{
  typedef test_node node_t;
  typedef test_edge edge_t;
  typedef test_graph graph_t;
  typedef test_dump_args_t dump_args_t;
  typedef test_cluster cluster_t;
};

struct test_node : public dnode<test_graph_traits>
{
  test_node (const char *name, int index) : m_name (name), m_index (index) {}
  void dump_dot (graphviz_out *, const dump_args_t &) const override
  {
  }

  const char *m_name;
  int m_index;
};

struct test_edge : public dedge<test_graph_traits>
{
  test_edge (node_t *src, node_t *dest)
  : dedge<test_graph_traits> (src, dest)
  {}

  void dump_dot (graphviz_out *gv, const dump_args_t &) const override
  {
    gv->println ("%s %s %s%c", m_src->m_name, "->", m_dest->m_name, ';');
  }
};

struct test_graph : public digraph<test_graph_traits>
{
  test_node *add_test_node (const char *name)
  {
    test_node *result = new test_node (name, m_nodes.length ());
    add_node (result);
    return result;
  }

  test_edge *add_test_edge (test_node *src, test_node *dst)
  {
    test_edge *result = new test_edge (src, dst);
    add_edge (result);
    return result;
  }
};

struct test_cluster : public cluster<test_graph_traits>
{
};

struct test_path
{
  auto_vec<const test_edge *> m_edges;
};

/* Smoketest of digraph dumping.  */

static void
test_dump_to_dot ()
{
  test_graph g;
  test_node *a = g.add_test_node ("a");
  test_node *b = g.add_test_node ("b");
  g.add_test_edge (a, b);

  pretty_printer pp;
  pp.set_output_stream (nullptr);
  test_dump_args_t dump_args;
  g.dump_dot_to_pp (&pp, NULL, dump_args);

  ASSERT_STR_CONTAINS (pp_formatted_text (&pp),
		       "a -> b;\n");
}

/* Test shortest paths from A in this digraph,
   where edges run top-to-bottom if not otherwise labeled:

      A
     / \
    B   C-->D
    |   |
    E   |
     \ /
      F.  */

static void
test_shortest_paths ()
{
  test_graph g;
  test_node *a = g.add_test_node ("a");
  test_node *b = g.add_test_node ("b");
  test_node *c = g.add_test_node ("d");
  test_node *d = g.add_test_node ("d");
  test_node *e = g.add_test_node ("e");
  test_node *f = g.add_test_node ("f");

  test_edge *ab = g.add_test_edge (a, b);
  test_edge *ac = g.add_test_edge (a, c);
  test_edge *cd = g.add_test_edge (c, d);
  test_edge *be = g.add_test_edge (b, e);
  test_edge *ef = g.add_test_edge (e, f);
  test_edge *cf = g.add_test_edge (c, f);

  /* Use "A" as the origin; all nodes should be reachable.  */
  {
    shortest_paths<test_graph_traits, test_path> sp (g, a,
						     SPS_FROM_GIVEN_ORIGIN);

    test_path path_to_a = sp.get_shortest_path (a);
    ASSERT_EQ (path_to_a.m_edges.length (), 0); /* Trivial path.  */

    test_path path_to_b = sp.get_shortest_path (b);
    ASSERT_EQ (path_to_b.m_edges.length (), 1);
    ASSERT_EQ (path_to_b.m_edges[0], ab);

    test_path path_to_c = sp.get_shortest_path (c);
    ASSERT_EQ (path_to_c.m_edges.length (), 1);
    ASSERT_EQ (path_to_c.m_edges[0], ac);

    test_path path_to_d = sp.get_shortest_path (d);
    ASSERT_EQ (path_to_d.m_edges.length (), 2);
    ASSERT_EQ (path_to_d.m_edges[0], ac);
    ASSERT_EQ (path_to_d.m_edges[1], cd);

    test_path path_to_e = sp.get_shortest_path (e);
    ASSERT_EQ (path_to_e.m_edges.length (), 2);
    ASSERT_EQ (path_to_e.m_edges[0], ab);
    ASSERT_EQ (path_to_e.m_edges[1], be);

    test_path path_to_f = sp.get_shortest_path (f);
    ASSERT_EQ (path_to_f.m_edges.length (), 2);
    ASSERT_EQ (path_to_f.m_edges[0], ac);
    ASSERT_EQ (path_to_f.m_edges[1], cf);
  }

  /* Verify that we gracefully handle an origin from which some nodes
     aren't reachable.  */

  /* Use "B" as the origin, so only E and F are reachable.  */
  {
    shortest_paths<test_graph_traits, test_path> sp (g, b,
						     SPS_FROM_GIVEN_ORIGIN);

    test_path path_to_a = sp.get_shortest_path (a);
    ASSERT_EQ (path_to_a.m_edges.length (), 0); /* No path.  */

    test_path path_to_b = sp.get_shortest_path (b);
    ASSERT_EQ (path_to_b.m_edges.length (), 0); /* Trivial path.  */

    test_path path_to_c = sp.get_shortest_path (c);
    ASSERT_EQ (path_to_c.m_edges.length (), 0); /* No path.  */

    test_path path_to_d = sp.get_shortest_path (d);
    ASSERT_EQ (path_to_d.m_edges.length (), 0); /* No path.  */

    test_path path_to_e = sp.get_shortest_path (e);
    ASSERT_EQ (path_to_e.m_edges.length (), 1);
    ASSERT_EQ (path_to_e.m_edges[0], be);

    test_path path_to_f = sp.get_shortest_path (f);
    ASSERT_EQ (path_to_f.m_edges.length (), 2);
    ASSERT_EQ (path_to_f.m_edges[0], be);
    ASSERT_EQ (path_to_f.m_edges[1], ef);
  }

  /* Use "C" as the origin, so only D and F are reachable.  */
  {
    shortest_paths<test_graph_traits, test_path> sp (g, c,
						     SPS_FROM_GIVEN_ORIGIN);

    test_path path_to_a = sp.get_shortest_path (a);
    ASSERT_EQ (path_to_a.m_edges.length (), 0); /* No path.  */

    test_path path_to_b = sp.get_shortest_path (b);
    ASSERT_EQ (path_to_b.m_edges.length (), 0); /* No path.  */

    test_path path_to_c = sp.get_shortest_path (c);
    ASSERT_EQ (path_to_c.m_edges.length (), 0); /* Trivial path.  */

    test_path path_to_d = sp.get_shortest_path (d);
    ASSERT_EQ (path_to_d.m_edges.length (), 1);
    ASSERT_EQ (path_to_d.m_edges[0], cd);

    test_path path_to_e = sp.get_shortest_path (e);
    ASSERT_EQ (path_to_e.m_edges.length (), 0); /* No path.  */

    test_path path_to_f = sp.get_shortest_path (f);
    ASSERT_EQ (path_to_f.m_edges.length (), 1);
    ASSERT_EQ (path_to_f.m_edges[0], cf);
  }

  /* Test of SPS_TO_GIVEN_TARGET.  Use "F" as the target.  */
  {
    shortest_paths<test_graph_traits, test_path> sp (g, f,
						     SPS_TO_GIVEN_TARGET);

    test_path path_to_a = sp.get_shortest_path (a);
    ASSERT_EQ (path_to_a.m_edges.length (), 2);
    ASSERT_EQ (path_to_a.m_edges[0], ac);
    ASSERT_EQ (path_to_a.m_edges[1], cf);

    test_path path_to_b = sp.get_shortest_path (b);
    ASSERT_EQ (path_to_b.m_edges.length (), 2);
    ASSERT_EQ (path_to_b.m_edges[0], be);
    ASSERT_EQ (path_to_b.m_edges[1], ef);

    test_path path_to_c = sp.get_shortest_path (c);
    ASSERT_EQ (path_to_c.m_edges.length (), 1);
    ASSERT_EQ (path_to_c.m_edges[0], cf);

    test_path path_to_d = sp.get_shortest_path (d);
    ASSERT_EQ (path_to_d.m_edges.length (), 0); /* No path.  */

    test_path path_to_e = sp.get_shortest_path (e);
    ASSERT_EQ (path_to_e.m_edges.length (), 1);
    ASSERT_EQ (path_to_e.m_edges[0], ef);

    test_path path_to_f = sp.get_shortest_path (f);
    ASSERT_EQ (path_to_f.m_edges.length (), 0);
  }
}

/* Run all of the selftests within this file.  */

void
digraph_cc_tests ()
{
  test_dump_to_dot ();
  test_shortest_paths ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
