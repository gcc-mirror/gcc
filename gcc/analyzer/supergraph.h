/* "Supergraph" classes that combine CFGs and callgraph into one digraph.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_SUPERGRAPH_H
#define GCC_ANALYZER_SUPERGRAPH_H

#include "ordered-hash-map.h"
#include "cfg.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"
#include "except.h"

#include "analyzer/ops.h"

using namespace ana;

namespace ana {

/* Forward decls, using indentation to show inheritance.  */

class supergraph;
class supernode;
class superedge;
class supercluster;
class dot_annotator;

class logger;

/* Flags for controlling the appearance of .dot dumps.  */

enum supergraph_dot_flags
{
  SUPERGRAPH_DOT_SHOW_BBS = (1 << 0)
};

/* A traits struct describing the family of node, edge and digraph
   classes for supergraphs.  */

struct supergraph_traits
{
  typedef supernode node_t;
  typedef superedge edge_t;
  typedef supergraph graph_t;
  struct dump_args_t
  {
    dump_args_t (enum supergraph_dot_flags flags,
		 const dot_annotator *node_annotator,
		 const exploded_graph *eg)
    : m_flags (flags),
      m_node_annotator (node_annotator),
      m_eg (eg)
    {}

    enum supergraph_dot_flags m_flags;
    const dot_annotator *m_node_annotator;
    const exploded_graph *m_eg;
  };
  typedef supercluster cluster_t;
};

/* A class to manage the setting and restoring of statement uids.  */

class saved_uids
{
public:
  void make_uid_unique (gimple *stmt);
  void restore_uids () const;

private:
  auto_vec<std::pair<gimple *, unsigned> > m_old_stmt_uids;
};

/* A directed graph class representing the users's code,
   with nodes representing locations within functions, and
   edges representing transitions between them.

   For historical reasons we call this the "supergraph", although
   this is now a misnomer as we no longer add callgraph edges to this
   graph: the edges within the supergraph are purely intraprocedural:
   either linking consecutive stmts in a basic block, or linking
   basic blocks (corresponding to CFG edges).  However, all functions
   are within the same graph.  */

class supergraph : public digraph<supergraph_traits>
{
public:
  supergraph (region_model_manager &mgr, logger *logger);
  ~supergraph ();

  supernode *get_node_for_function_entry (const function &fun) const
  {
    return get_initial_node_for_block (ENTRY_BLOCK_PTR_FOR_FN (&fun));
  }

  supernode *get_node_for_function_exit (const function &fun) const
  {
    return get_final_node_for_block (EXIT_BLOCK_PTR_FOR_FN (&fun));
  }

  supernode *get_initial_node_for_block (basic_block bb) const
  {
    return *const_cast <bb_to_node_t &> (m_bb_to_initial_node).get (bb);
  }

  supernode *get_final_node_for_block (basic_block bb) const
  {
    return *const_cast <bb_to_node_t &> (m_bb_to_final_node).get (bb);
  }

  supernode *get_supernode_for_stmt (const gimple *stmt) const
  {
    auto iter = m_node_for_stmt.find (stmt);
    gcc_assert (iter != m_node_for_stmt.end ());
    return iter->second;
  }

  superedge *get_superedge_for_phis (::edge cfg_edge) const
  {
    auto iter = m_edges_for_phis.find (cfg_edge);
    if (iter != m_edges_for_phis.end ())
      return iter->second;
    return nullptr;
  }

  void dump_dot_to_pp (pretty_printer *pp, const dump_args_t &) const;
  void dump_dot_to_file (FILE *fp, const dump_args_t &) const;
  void dump_dot (const char *path, const dump_args_t &) const;

  std::unique_ptr<json::object> to_json () const;

  int num_nodes () const { return m_nodes.length (); }
  int num_edges () const { return m_edges.length (); }

  unsigned get_num_snodes (const function *fun) const
  {
    function_to_num_snodes_t &map
      = const_cast <function_to_num_snodes_t &>(m_function_to_num_snodes);
    return *map.get (fun);
  }

  void log_stats (logger *logger) const;

  void delete_nodes (const std::set<supernode *> &snodes);

  /* Implemented in supergraph-fixup-locations.cc.  */
  void fixup_locations (logger *);

  /* Implemented in supergraph-simplify.cc.  */
  void simplify (logger *);

  /* Implemented in supergraph-sorting.cc.  */
  void sort_nodes (logger *logger);

  supernode *add_node (function *fun, basic_block bb, logger *logger);

private:
  gimple *
  populate_for_basic_block (basic_block bb,
			    function *fun,
			    logger *logger);

  void
  add_sedges_for_cfg_edge (supernode *src,
			   supernode *dest,
			   ::edge e,
			   gimple *control_stmt,
			   region_model_manager &mgr,
			   logger *logger);

  void dump_dot_to_gv_for_loop (graphviz_out &gv, const dump_args_t &,
				class loop *, function *) const;
  void dump_dot_to_gv_for_bb (graphviz_out &gv, const dump_args_t &,
			      basic_block, function *) const;

  /* Implemented in supergraph-sorting.cc.  */
  void
  reorder_nodes_and_ids (const std::vector<supernode *> &ordering,
			 logger *logger);

  /* Data.  */

  typedef ordered_hash_map<basic_block, supernode *> bb_to_node_t;
  bb_to_node_t m_bb_to_initial_node;
  bb_to_node_t m_bb_to_final_node;

  std::map<const gimple *, supernode *> m_node_for_stmt;
  std::map<::edge, superedge *> m_edges_for_phis;

  typedef hash_map<const function *, unsigned> function_to_num_snodes_t;
  function_to_num_snodes_t m_function_to_num_snodes;

  saved_uids m_stmt_uids;

  /* Hand out unique IDs to supernodes, to make it easier
     to track them when deleting/splitting etc (they're easier to
     think about when debugging than pointer values).  */
  int m_next_snode_id;
  std::vector<supernode *> m_snode_by_id;
};

/* A node within a supergraph.  */

class supernode : public dnode<supergraph_traits>
{
 public:
  supernode (function *fun, basic_block bb, int id)
  : m_fun (fun), m_bb (bb),
    m_loc (UNKNOWN_LOCATION),
    m_stmt_loc (UNKNOWN_LOCATION),
    m_id (id),
    m_original_id (id),
    m_label (NULL_TREE),
    m_preserve_p (false),
    m_state_merger_node (false)
  {}

  function *get_function () const { return m_fun; }

  bool entry_p () const
  {
    return m_bb == ENTRY_BLOCK_PTR_FOR_FN (m_fun);
  }
  bool exit_p () const
  {
    return m_bb == EXIT_BLOCK_PTR_FOR_FN (m_fun);
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const override;
  void dump_dot_id (pretty_printer *pp) const;

  void print (pretty_printer *pp) const
  {
    pp_printf (pp, "SN %i", m_id);
  }

  std::unique_ptr<json::object> to_json () const;

  location_t get_location () const { return m_loc; }

  tree get_label () const { return m_label; }

  bool preserve_p () const { return m_preserve_p; }

  function * const m_fun;
  const basic_block m_bb;
  location_t m_loc;
  location_t m_stmt_loc; // for debugging
  int m_id; /* unique within the supergraph as a whole.  */
  const int m_original_id;
  tree m_label;
  bool m_preserve_p;
  bool m_state_merger_node;
};

/* An edge within the supergraph, with an optional operation.
   Edges can be CFG edges or edges between statements, or persist
   in order to give more opportunities for state-merging when
   building the exploded graph.  */

class superedge : public dedge<supergraph_traits>
{
 public:
  superedge (supernode *src, supernode *dest,
	     std::unique_ptr<operation> op,
	     ::edge cfg_edge)
  : dedge<supergraph_traits> (src, dest),
    m_op (std::move (op)),
    m_cfg_edge (cfg_edge)
  {
    /* All edges are intraprocedural.  */
    gcc_assert (m_src->get_function ()
		== m_dest->get_function ());
  }

  virtual ~superedge () {}

  void dump (pretty_printer *pp) const;
  void dump () const;
  void dump_dot (graphviz_out *gv, const dump_args_t &args)
    const final override;

  const operation *get_op () const { return m_op.get (); }
  void set_op (std::unique_ptr<operation> op) { m_op = std::move (op); }

  void dump_label_to_pp (pretty_printer *pp,
				 bool user_facing) const;

  std::unique_ptr<json::object> to_json () const;

  const supernode *get_dest_snode () const { return m_dest; }

  ::edge get_any_cfg_edge () const { return m_cfg_edge; }

  bool preserve_p () const;

  label_text get_description (bool user_facing) const;

  bool
  supports_bulk_merge_p () const;

private:
  std::unique_ptr<operation> m_op;
  ::edge m_cfg_edge;
};

/* An ID representing an expression at a callsite:
   either a parameter index, or the return value (or unknown).  */

class callsite_expr
{
 public:
  callsite_expr () : m_val (-1) {}

  static callsite_expr from_zero_based_param (int idx)
  {
    return callsite_expr (idx + 1);
  }

  static callsite_expr from_return_value ()
  {
    return callsite_expr (0);
  }

  bool param_p () const
  {
    return m_val > 0;
  }

  bool return_value_p () const
  {
    return m_val == 0;
  }

 private:
  callsite_expr (int val) : m_val (val) {}

  int m_val; /* 1-based parm, 0 for return value, or -1 for "unknown".  */
};

/* Base class for adding additional content to the .dot output
   for a supergraph.  */

class dot_annotator
{
 public:
  virtual ~dot_annotator () = default;

  virtual void
  add_node_annotations (graphviz_out *gv ATTRIBUTE_UNUSED,
			const supernode &n ATTRIBUTE_UNUSED) const
  {
    // no-op
  }

  virtual void
  add_extra_objects (graphviz_out *gv ATTRIBUTE_UNUSED) const
  {
    // no-op
  }
};

} // namespace ana

#endif /* GCC_ANALYZER_SUPERGRAPH_H */
