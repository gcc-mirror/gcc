/* "Supergraph" classes that combine CFGs and callgraph into one digraph.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"

using namespace ana;

namespace ana {

/* Forward decls, using indentation to show inheritance.  */

class supergraph;
class supernode;
class superedge;
  class callgraph_superedge;
    class call_superedge;
    class return_superedge;
  class cfg_superedge;
    class switch_cfg_superedge;
class supercluster;
class dot_annotator;

class logger;

/* An enum for discriminating between superedge subclasses.  */

enum edge_kind
{
  SUPEREDGE_CFG_EDGE,
  SUPEREDGE_CALL,
  SUPEREDGE_RETURN,
  SUPEREDGE_INTRAPROCEDURAL_CALL
};

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
		 const dot_annotator *node_annotator)
    : m_flags (flags),
      m_node_annotator (node_annotator)
    {}

    enum supergraph_dot_flags m_flags;
    const dot_annotator *m_node_annotator;
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

/* A "supergraph" is a directed graph formed by joining together all CFGs,
   linking them via interprocedural call and return edges.

   Basic blocks are split at callsites, so that a call statement occurs
   twice: once at the end of a supernode, and a second instance at the
   start of the next supernode (to handle the return).  */

class supergraph : public digraph<supergraph_traits>
{
public:
  supergraph (logger *logger);
  ~supergraph ();

  supernode *get_node_for_function_entry (function *fun) const
  {
    return get_node_for_block (ENTRY_BLOCK_PTR_FOR_FN (fun));
  }

  supernode *get_node_for_function_exit (function *fun) const
  {
    return get_node_for_block (EXIT_BLOCK_PTR_FOR_FN (fun));
  }

  supernode *get_node_for_block (basic_block bb) const
  {
    return *const_cast <bb_to_node_t &> (m_bb_to_initial_node).get (bb);
  }

  /* Get the supernode containing the second half of the gcall *
     at an interprocedural call, within the caller.  */
  supernode *get_caller_next_node (cgraph_edge *edge) const
  {
    return (*const_cast <cgraph_edge_to_node_t &>
	      (m_cgraph_edge_to_caller_next_node).get (edge));
  }

  call_superedge *get_edge_for_call (cgraph_edge *edge) const
  {
    return (*const_cast <cgraph_edge_to_call_superedge_t &>
	      (m_cgraph_edge_to_call_superedge).get (edge));
  }

  return_superedge *get_edge_for_return (cgraph_edge *edge) const
  {
    return (*const_cast <cgraph_edge_to_return_superedge_t &>
	      (m_cgraph_edge_to_return_superedge).get (edge));
  }

  superedge *get_intraprocedural_edge_for_call (cgraph_edge *edge) const
  {
    return (*const_cast <cgraph_edge_to_intraproc_superedge_t &>
	      (m_cgraph_edge_to_intraproc_superedge).get (edge));
  }

  cfg_superedge *get_edge_for_cfg_edge (edge e) const
  {
    return (*const_cast <cfg_edge_to_cfg_superedge_t &>
	      (m_cfg_edge_to_cfg_superedge).get (e));
  }

  supernode *get_supernode_for_stmt (const gimple *stmt) const
  {
    return (*const_cast <stmt_to_node_t &>(m_stmt_to_node_t).get
	    (const_cast <gimple *> (stmt)));
  }

  void dump_dot_to_pp (pretty_printer *pp, const dump_args_t &) const;
  void dump_dot_to_file (FILE *fp, const dump_args_t &) const;
  void dump_dot (const char *path, const dump_args_t &) const;

  json::object *to_json () const;

  int num_nodes () const { return m_nodes.length (); }
  int num_edges () const { return m_edges.length (); }

  supernode *get_node_by_index (int idx) const
  {
    return m_nodes[idx];
  }

  unsigned get_num_snodes (const function *fun) const
  {
    function_to_num_snodes_t &map
      = const_cast <function_to_num_snodes_t &>(m_function_to_num_snodes);
    return *map.get (fun);
  }

private:
  supernode *add_node (function *fun, basic_block bb, gcall *returning_call,
		       gimple_seq phi_nodes);
  cfg_superedge *add_cfg_edge (supernode *src, supernode *dest, ::edge e);
  call_superedge *add_call_superedge (supernode *src, supernode *dest,
				      cgraph_edge *cedge);
  return_superedge *add_return_superedge (supernode *src, supernode *dest,
					  cgraph_edge *cedge);

  /* Data.  */

  typedef ordered_hash_map<basic_block, supernode *> bb_to_node_t;
  bb_to_node_t m_bb_to_initial_node;
  bb_to_node_t m_bb_to_final_node;

  typedef ordered_hash_map<cgraph_edge *, supernode *> cgraph_edge_to_node_t;
  cgraph_edge_to_node_t m_cgraph_edge_to_caller_prev_node;
  cgraph_edge_to_node_t m_cgraph_edge_to_caller_next_node;

  typedef ordered_hash_map< ::edge, cfg_superedge *>
    cfg_edge_to_cfg_superedge_t;
  cfg_edge_to_cfg_superedge_t m_cfg_edge_to_cfg_superedge;

  typedef ordered_hash_map<cgraph_edge *, call_superedge *>
    cgraph_edge_to_call_superedge_t;
  cgraph_edge_to_call_superedge_t m_cgraph_edge_to_call_superedge;

  typedef ordered_hash_map<cgraph_edge *, return_superedge *>
    cgraph_edge_to_return_superedge_t;
  cgraph_edge_to_return_superedge_t m_cgraph_edge_to_return_superedge;

  typedef ordered_hash_map<cgraph_edge *, superedge *>
    cgraph_edge_to_intraproc_superedge_t;
  cgraph_edge_to_intraproc_superedge_t m_cgraph_edge_to_intraproc_superedge;

  typedef ordered_hash_map<gimple *, supernode *> stmt_to_node_t;
  stmt_to_node_t m_stmt_to_node_t;

  typedef hash_map<const function *, unsigned> function_to_num_snodes_t;
  function_to_num_snodes_t m_function_to_num_snodes;

  saved_uids m_stmt_uids;
};

/* A node within a supergraph.  */

class supernode : public dnode<supergraph_traits>
{
 public:
  supernode (function *fun, basic_block bb, gcall *returning_call,
	     gimple_seq phi_nodes, int index)
  : m_fun (fun), m_bb (bb), m_returning_call (returning_call),
    m_phi_nodes (phi_nodes), m_index (index)
  {}

  function *get_function () const { return m_fun; }

  bool entry_p () const
  {
    return m_bb == ENTRY_BLOCK_PTR_FOR_FN (m_fun);
  }

  bool return_p () const
  {
    return m_bb == EXIT_BLOCK_PTR_FOR_FN (m_fun);
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const override;
  void dump_dot_id (pretty_printer *pp) const;

  json::object *to_json () const;

  location_t get_start_location () const;
  location_t get_end_location () const;

  /* Returns iterator at the start of the list of phi nodes, if any.  */
  gphi_iterator start_phis ()
  {
    gimple_seq *pseq = &m_phi_nodes;

    /* Adapted from gsi_start_1. */
    gphi_iterator i;

    i.ptr = gimple_seq_first (*pseq);
    i.seq = pseq;
    i.bb = i.ptr ? gimple_bb (i.ptr) : NULL;

    return i;
  }

  gcall *get_returning_call () const
  {
    return m_returning_call;
  }

  gimple *get_last_stmt () const
  {
    if (m_stmts.length () == 0)
      return NULL;
    return m_stmts[m_stmts.length () - 1];
  }

  gcall *get_final_call () const
  {
    gimple *stmt = get_last_stmt ();
    if (stmt == NULL)
      return NULL;
    return dyn_cast<gcall *> (stmt);
  }

  unsigned int get_stmt_index (const gimple *stmt) const;

  function * const m_fun; // alternatively could be stored as runs of indices within the supergraph
  const basic_block m_bb;
  gcall * const m_returning_call; // for handling the result of a returned call
  gimple_seq m_phi_nodes; // ptr to that of the underlying BB, for the first supernode for the BB
  auto_vec<gimple *> m_stmts;
  const int m_index; /* unique within the supergraph as a whole.  */
};

/* An abstract base class encapsulating an edge within a supergraph.
   Edges can be CFG edges, or calls/returns for callgraph edges.  */

class superedge : public dedge<supergraph_traits>
{
 public:
  virtual ~superedge () {}

  void dump (pretty_printer *pp) const;
  void dump () const;
  void dump_dot (graphviz_out *gv, const dump_args_t &args)
    const final override;

  virtual void dump_label_to_pp (pretty_printer *pp,
				 bool user_facing) const = 0;

  json::object *to_json () const;

  enum edge_kind get_kind () const { return m_kind; }

  virtual cfg_superedge *dyn_cast_cfg_superedge () { return NULL; }
  virtual const cfg_superedge *dyn_cast_cfg_superedge () const { return NULL; }
  virtual const switch_cfg_superedge *dyn_cast_switch_cfg_superedge () const { return NULL; }
  virtual callgraph_superedge *dyn_cast_callgraph_superedge () { return NULL; }
  virtual const callgraph_superedge *dyn_cast_callgraph_superedge () const { return NULL; }
  virtual call_superedge *dyn_cast_call_superedge () { return NULL; }
  virtual const call_superedge *dyn_cast_call_superedge () const { return NULL; }
  virtual return_superedge *dyn_cast_return_superedge () { return NULL; }
  virtual const return_superedge *dyn_cast_return_superedge () const { return NULL; }

  ::edge get_any_cfg_edge () const;
  cgraph_edge *get_any_callgraph_edge () const;

  label_text get_description (bool user_facing) const;

 protected:
  superedge (supernode *src, supernode *dest, enum edge_kind kind)
  : dedge<supergraph_traits> (src, dest),
    m_kind (kind)
  {}

 public:
  const enum edge_kind m_kind;
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

/* A subclass of superedge with an associated callgraph edge (either a
   call or a return).  */

class callgraph_superedge : public superedge
{
 public:
  callgraph_superedge (supernode *src, supernode *dst, enum edge_kind kind,
		       cgraph_edge *cedge)
  : superedge (src, dst, kind),
    m_cedge (cedge)
  {}

  void dump_label_to_pp (pretty_printer *pp, bool user_facing) const
    final override;

  callgraph_superedge *dyn_cast_callgraph_superedge () final override
  {
    return this;
  }
  const callgraph_superedge *dyn_cast_callgraph_superedge () const
    final override
  {
    return this;
  }

  function *get_callee_function () const;
  function *get_caller_function () const;
  tree get_callee_decl () const;
  tree get_caller_decl () const;
  gcall *get_call_stmt () const;
  tree get_arg_for_parm (tree parm, callsite_expr *out) const;
  tree get_parm_for_arg (tree arg, callsite_expr *out) const;
  tree map_expr_from_caller_to_callee (tree caller_expr,
				       callsite_expr *out) const;
  tree map_expr_from_callee_to_caller (tree callee_expr,
				       callsite_expr *out) const;

  cgraph_edge *const m_cedge;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const callgraph_superedge *>::test (const superedge *sedge)
{
  return (sedge->get_kind () == SUPEREDGE_INTRAPROCEDURAL_CALL
	  || sedge->get_kind () == SUPEREDGE_CALL
	  || sedge->get_kind () == SUPEREDGE_RETURN);
}

namespace ana {

/* A subclass of superedge representing an interprocedural call.  */

class call_superedge : public callgraph_superedge
{
 public:
  call_superedge (supernode *src, supernode *dst, cgraph_edge *cedge)
  : callgraph_superedge (src, dst, SUPEREDGE_CALL, cedge)
  {}

  call_superedge *dyn_cast_call_superedge () final override
  {
    return this;
  }
  const call_superedge *dyn_cast_call_superedge () const final override
  {
    return this;
  }

  return_superedge *get_edge_for_return (const supergraph &sg) const
  {
    return sg.get_edge_for_return (m_cedge);
  }
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const call_superedge *>::test (const superedge *sedge)
{
  return sedge->get_kind () == SUPEREDGE_CALL;
}

namespace ana {

/* A subclass of superedge represesnting an interprocedural return.  */

class return_superedge : public callgraph_superedge
{
 public:
  return_superedge (supernode *src, supernode *dst, cgraph_edge *cedge)
  : callgraph_superedge (src, dst, SUPEREDGE_RETURN, cedge)
  {}

  return_superedge *dyn_cast_return_superedge () final override { return this; }
  const return_superedge *dyn_cast_return_superedge () const final override
  {
    return this;
  }

  call_superedge *get_edge_for_call (const supergraph &sg) const
  {
    return sg.get_edge_for_call (m_cedge);
  }
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const return_superedge *>::test (const superedge *sedge)
{
  return sedge->get_kind () == SUPEREDGE_RETURN;
}

namespace ana {

/* A subclass of superedge that corresponds to a CFG edge.  */

class cfg_superedge : public superedge
{
 public:
  cfg_superedge (supernode *src, supernode *dst, ::edge e)
  : superedge (src, dst, SUPEREDGE_CFG_EDGE),
    m_cfg_edge (e)
  {}

  void dump_label_to_pp (pretty_printer *pp, bool user_facing) const override;
  cfg_superedge *dyn_cast_cfg_superedge () final override { return this; }
  const cfg_superedge *dyn_cast_cfg_superedge () const final override { return this; }

  ::edge get_cfg_edge () const { return m_cfg_edge; }
  int get_flags () const { return m_cfg_edge->flags; }
  int true_value_p () const { return get_flags () & EDGE_TRUE_VALUE; }
  int false_value_p () const { return get_flags () & EDGE_FALSE_VALUE; }
  int back_edge_p () const { return get_flags () & EDGE_DFS_BACK; }

  size_t get_phi_arg_idx () const;
  tree get_phi_arg (const gphi *phi) const;

 private:
  const ::edge m_cfg_edge;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const cfg_superedge *>::test (const superedge *sedge)
{
  return sedge->get_kind () == SUPEREDGE_CFG_EDGE;
}

namespace ana {

/* A subclass for edges from switch statements, retaining enough
   information to identify the pertinent cases, and for adding labels
   when rendering via graphviz.  */

class switch_cfg_superedge : public cfg_superedge {
 public:
  switch_cfg_superedge (supernode *src, supernode *dst, ::edge e);

  const switch_cfg_superedge *dyn_cast_switch_cfg_superedge () const
    final override
  {
    return this;
  }

  void dump_label_to_pp (pretty_printer *pp, bool user_facing) const
    final override;

  gswitch *get_switch_stmt () const
  {
    return as_a <gswitch *> (m_src->get_last_stmt ());
  }

  const vec<tree> &get_case_labels () const { return m_case_labels; }

  bool implicitly_created_default_p () const;

private:
  auto_vec<tree> m_case_labels;
};

} // namespace ana

template <>
template <>
inline bool
is_a_helper <const switch_cfg_superedge *>::test (const superedge *sedge)
{
  return sedge->dyn_cast_switch_cfg_superedge () != NULL;
}

namespace ana {

/* Base class for adding additional content to the .dot output
   for a supergraph.  */

class dot_annotator
{
 public:
  virtual ~dot_annotator () {}
  virtual bool add_node_annotations (graphviz_out *gv ATTRIBUTE_UNUSED,
				     const supernode &n ATTRIBUTE_UNUSED,
				     bool within_table ATTRIBUTE_UNUSED)
    const
  {
    return false;
  }
  virtual void add_stmt_annotations (graphviz_out *gv ATTRIBUTE_UNUSED,
				     const gimple *stmt ATTRIBUTE_UNUSED,
				     bool within_row ATTRIBUTE_UNUSED)
    const {}
  virtual bool add_after_node_annotations (graphviz_out *gv ATTRIBUTE_UNUSED,
					   const supernode &n ATTRIBUTE_UNUSED)
    const
  {
    return false;
  }
};

extern cgraph_edge *supergraph_call_edge (function *fun, const gimple *stmt);
extern function *get_ultimate_function_for_cgraph_edge (cgraph_edge *edge);

} // namespace ana

#endif /* GCC_ANALYZER_SUPERGRAPH_H */
