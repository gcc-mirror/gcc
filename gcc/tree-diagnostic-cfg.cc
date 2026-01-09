/* Generating diagnostics graphs from GCC CFGs.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#define INCLUDE_LIST
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "tree.h"
#include "lazily-created.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/dumping.h"
#include "diagnostic.h"
#include "tree-pass.h"
#include "custom-sarif-properties/cfg.h"
#include "tree-diagnostic-sink-extensions.h"
#include "tree-logical-location.h"
#include "tree-pass.h"
#include "function.h"
#include "topics/pass-events.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/sink.h"
#include "context.h"
#include "channels.h"
#include "bitmap.h"
#include "sbitmap.h"
#include "cfghooks.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "graph.h"
#include "basic-block.h"
#include "cfg.h"


namespace {
  namespace graph_properties = custom_sarif_properties::cfg::graph;
  namespace node_properties = custom_sarif_properties::cfg::node;
  namespace edge_properties = custom_sarif_properties::cfg::edge;
}

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

class cfg_diagnostic_digraph
  : public lazily_created<diagnostics::digraphs::digraph>
{
public:
  cfg_diagnostic_digraph (function *fun,
			  opt_pass *pass)
  : m_fun (fun), m_pass (pass)
  {}

  std::unique_ptr<diagnostics::digraphs::digraph>
  create_object () const final override
  {
    auto g = std::make_unique<diagnostics::digraphs::digraph> ();
    g->set_graph_kind ("cfg");

    pretty_printer pp;
    pp_printf (&pp, "%s: %s", function_name (m_fun), m_pass->name);
    g->set_description (pp_formatted_text (&pp));

    g->set_property (graph_properties::pass_name, m_pass->name);
    g->set_property (graph_properties::pass_number, m_pass->static_pass_number);

    add_cluster_for_function (*g, m_fun);
    return g;
  }

  void
  add_cluster_for_function (diagnostics::digraphs::digraph &g,
			    function *fun) const
  {
    const char *funcname = function_name (fun);
    auto cluster = std::make_unique<diagnostics::digraphs::node> (g, funcname);
    cluster->set_property (node_properties::kind, "function");

    bb_to_node_map node_map;
    add_cfg_nodes (g, node_map, *cluster, fun);
    add_cfg_edges (g, node_map, fun);
    g.add_node (std::move (cluster));
  }

private:
  typedef std::map<basic_block, diagnostics::digraphs::node *> bb_to_node_map;

  /* Add a basic block BB belonging to the function with FUNCDEF_NO
     as its unique number.  */
  void
  add_cfg_node (diagnostics::digraphs::digraph &g,
		bb_to_node_map &node_map,
		diagnostics::digraphs::node &parent_node,
		int funcdef_no,
		basic_block bb) const
  {
    pretty_printer pp;
    pp_printf (&pp, "fn_%d_basic_block_%d",
	       funcdef_no, bb->index);
    std::string name (pp_formatted_text (&pp));
    auto bb_node = std::make_unique<diagnostics::digraphs::node> (g, name);
    node_map.insert ({bb, bb_node.get ()});

    bb_node->set_property (node_properties::kind, "basic_block");

    dump_bb_as_sarif_properties (nullptr,
				 bb_node->ensure_property_bag (),
				 bb);

    parent_node.add_child (std::move (bb_node));
  }

  /* Add all successor edges of a basic block BB belonging to the function
     with FUNCDEF_NO as its unique number.  */
  void
  add_cfg_node_succ_edges (diagnostics::digraphs::digraph &g,
			   bb_to_node_map &node_map,
			   int /*funcdef_no*/,
			   basic_block bb) const
  {
    edge e;
    edge_iterator ei;
    FOR_EACH_EDGE (e, ei, bb->succs)
      {
	auto src_node = node_map.find (e->src);
	gcc_assert (src_node != node_map.end ());
	auto dst_node = node_map.find (e->dest);
	gcc_assert (dst_node != node_map.end ());

	auto diag_edge
	  = std::make_unique<diagnostics::digraphs::edge> (g, nullptr,
							   *(src_node->second),
							   *(dst_node->second));
	auto flag_arr = std::make_unique<json::array> ();
#define DEF_EDGE_FLAG(NAME,IDX) \
	{ handle_edge_flag (*flag_arr, #NAME, (e->flags & EDGE_##NAME)); }
#include "cfg-flags.def"
#undef DEF_EDGE_FLAG

	auto &bag = diag_edge->ensure_property_bag ();
	bag.set<json::array> (edge_properties::flags.m_key.get (),
			      std::move (flag_arr));

	if (e->probability.initialized_p ())
	  diag_edge->set_property (edge_properties::probability_pc,
				   (e->probability.to_reg_br_prob_base ()
				    * 100 / REG_BR_PROB_BASE));

	g.add_edge (std::move (diag_edge));
      }
  }

  void
  handle_edge_flag (json::array &flag_arr,
		    const char *flag_name,
		    bool value) const
  {
    if (value)
      flag_arr.append_string (flag_name);
  }

  /* Add all the basic blocks in the CFG in case loops are not available.
     First compute a topological order of the blocks to get a good ranking of
     the nodes.  Then, if any nodes are not reachable from ENTRY, add them at
     the end.  */
  void
  add_cfg_nodes_no_loops (diagnostics::digraphs::digraph &g,
			  bb_to_node_map &node_map,
			  diagnostics::digraphs::node &parent_node,
			  function *fun) const
  {
    int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fun));
    int i, n;

    auto_sbitmap visited (last_basic_block_for_fn (fun));
    bitmap_clear (visited);

    n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, true);
    for (i = n_basic_blocks_for_fn (fun) - n;
	 i < n_basic_blocks_for_fn (fun); i++)
      {
	basic_block bb = BASIC_BLOCK_FOR_FN (fun, rpo[i]);
	add_cfg_node (g, node_map, parent_node, fun->funcdef_no, bb);
	bitmap_set_bit (visited, bb->index);
      }
    free (rpo);

    if (n != n_basic_blocks_for_fn (fun))
      {
	/* Some blocks are unreachable.  We still want to dump them.  */
	basic_block bb;
	FOR_ALL_BB_FN (bb, fun)
	  if (! bitmap_bit_p (visited, bb->index))
	    add_cfg_node (g, node_map, parent_node, fun->funcdef_no, bb);
      }
  }

/* Add all the basic blocks in LOOP.  Add the blocks in breath-first
   order to get a good ranking of the nodes.  This function is recursive:
   It first adds inner loops, then the body of LOOP itself.  */

  void
  add_cfg_nodes_for_loop (diagnostics::digraphs::digraph &g,
			  bb_to_node_map &node_map,
			  diagnostics::digraphs::node *parent_node,
			  int funcdef_no,
			  class loop *loop) const
  {
    namespace loop_properties = custom_sarif_properties::cfg::loop;

    gcc_assert (parent_node);
    diagnostics::digraphs::node &orig_parent_node = *parent_node;

    unsigned int i;
    std::unique_ptr<diagnostics::digraphs::node> loop_node;

    if (loop->header != NULL
	&& loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun))
      {
	pretty_printer pp;
	pp_printf (&pp, "fun_%d_loop_%d", funcdef_no, loop->num);
	std::string name (pp_formatted_text (&pp));
	loop_node
	  = std::make_unique<diagnostics::digraphs::node> (g, name);
	parent_node = loop_node.get ();
	loop_node->set_property (node_properties::kind, "loop");
	loop_node->set_property (loop_properties::num, loop->num);
	loop_node->set_property (loop_properties::depth, loop_depth (loop));
      }

    for (class loop *inner = loop->inner; inner; inner = inner->next)
      add_cfg_nodes_for_loop (g, node_map, parent_node, funcdef_no, inner);

    if (loop->header == NULL)
      return;

    basic_block *body;
    if (loop->latch == EXIT_BLOCK_PTR_FOR_FN (cfun))
      body = get_loop_body (loop);
    else
      body = get_loop_body_in_bfs_order (loop);

    for (i = 0; i < loop->num_nodes; i++)
      {
	basic_block bb = body[i];
	if (bb->loop_father == loop)
	  add_cfg_node (g, node_map, *parent_node, funcdef_no, bb);
      }

    free (body);

    if (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun))
      {
	gcc_assert (loop_node);
	orig_parent_node.add_child (std::move (loop_node));
      }
  }

  void
  add_cfg_nodes (diagnostics::digraphs::digraph &g,
		 bb_to_node_map &node_map,
		 diagnostics::digraphs::node &parent_node,
		 function *fun) const
  {
    /* ???  The loop and dominance APIs are dependent on fun == cfun.  */
    if (fun == cfun && loops_for_fn (fun))
      add_cfg_nodes_for_loop (g, node_map, &parent_node, fun->funcdef_no,
			      get_loop (fun, 0));
    else
      add_cfg_nodes_no_loops (g, node_map, parent_node, fun);
  }

  void
  add_cfg_edges (diagnostics::digraphs::digraph &g,
		 bb_to_node_map &node_map,
		 function *fun) const
  {
    basic_block bb;

    /* Save EDGE_DFS_BACK flag to dfs_back.  */
    auto_bitmap dfs_back;
    edge e;
    edge_iterator ei;
    unsigned int idx = 0;
    FOR_EACH_BB_FN (bb, fun)
      FOR_EACH_EDGE (e, ei, bb->succs)
      {
	if (e->flags & EDGE_DFS_BACK)
	  bitmap_set_bit (dfs_back, idx);
	idx++;
      }

    mark_dfs_back_edges (fun);
    FOR_ALL_BB_FN (bb, fun)
      add_cfg_node_succ_edges (g, node_map, fun->funcdef_no, bb);

    /* Restore EDGE_DFS_BACK flag from dfs_back.  */
    idx = 0;
    FOR_EACH_BB_FN (bb, fun)
      FOR_EACH_EDGE (e, ei, bb->succs)
      {
	if (bitmap_bit_p (dfs_back, idx))
	  e->flags |= EDGE_DFS_BACK;
	else
	  e->flags &= ~EDGE_DFS_BACK;
	idx++;
      }
  }

  function *m_fun;
  opt_pass *m_pass;
};

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

namespace pass_events = gcc::topics::pass_events;

/* A diagnostics::sink::extension which subscribes to pass_events
   and responds to "after_pass" events by adding a diagnostics digraph
   for the CFG for the relevant function.  */

class compiler_capture_cfgs : public diagnostics::sink::extension
{
public:
  compiler_capture_cfgs (diagnostics::sink &sink)
  : extension (sink),
    m_event_subscriber (sink)
  {
    g->get_channels ().pass_events_channel.add_subscriber (m_event_subscriber);
  }

  void
  dump (FILE *out, int indent) const
  {
    diagnostics::dumping::emit_heading (out, indent, "compiler_capture_cfgs");
  }

private:
  class event_subscriber : public pass_events::subscriber
  {
  public:
    event_subscriber (diagnostics::sink &sink) : m_sink (sink) {}
    void on_message (const pass_events::before_pass &) final override
    {
    }
    void on_message (const pass_events::after_pass &m) final override
    {
      if (m.fun
	  && m.fun->cfg
	  && m.pass->static_pass_number > 0)
	m_sink.report_digraph_for_logical_location
	  (cfg_diagnostic_digraph (m.fun, m.pass),
	   tree_logical_location_manager::key_from_tree (m.fun->decl));
    }

  private:
    diagnostics::sink &m_sink;
  } m_event_subscriber;
};

std::unique_ptr<diagnostics::sink::extension>
compiler_extension_factory::make_cfg_extension (diagnostics::sink &sink) const
{
  return std::make_unique<compiler_capture_cfgs> (sink);
}
