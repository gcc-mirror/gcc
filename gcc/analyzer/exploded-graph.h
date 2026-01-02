/* Classes for managing a directed graph of <point, state> pairs.
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

#ifndef GCC_ANALYZER_EXPLODED_GRAPH_H
#define GCC_ANALYZER_EXPLODED_GRAPH_H

#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "supergraph.h"
#include "sbitmap.h"
#include "shortest-paths.h"
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/region-model.h"

namespace ana {

/* Concrete implementation of region_model_context, wiring it up to the
   rest of the analysis engine.  */

class impl_region_model_context : public region_model_context
{
 public:
  impl_region_model_context (exploded_graph &eg,
			     exploded_node *enode_for_diag,

			     /* TODO: should we be getting the ECs from the
				old state, rather than the new?  */
			     const program_state *old_state,
			     program_state *new_state,
			     uncertainty_t *uncertainty,
			     path_context *path_ctxt,
			     const gimple *stmt = nullptr,
			     bool *out_could_have_done_work = nullptr);

  impl_region_model_context (program_state *state,
			     const extrinsic_state &ext_state,
			     uncertainty_t *uncertainty,
			     logger *logger = nullptr);

  bool
  warn_at (std::unique_ptr<pending_diagnostic> d,
	   pending_location &&ploc) final override;
  void add_note (std::unique_ptr<pending_note> pn) final override;
  void add_event (std::unique_ptr<checker_event> event) final override;
  void on_svalue_leak (const svalue *) override;
  void on_liveness_change (const svalue_set &live_svalues,
			   const region_model *model) final override;
  logger *get_logger () final override
  {
    return m_logger.get_logger ();
  }

  void on_state_leak (const state_machine &sm,
		      const svalue *sval,
		      state_machine::state_t state);

  void on_condition (const svalue *lhs,
		     enum tree_code op,
		     const svalue *rhs) final override;

  void on_bounded_ranges (const svalue &sval,
			  const bounded_ranges &ranges) final override;

  void on_pop_frame (const frame_region *frame_reg) final override;

  void on_unknown_change (const svalue *sval, bool is_mutable) final override;

  void on_phi (const gphi *phi, tree rhs) final override;

  void on_unexpected_tree_code (tree t,
				const dump_location_t &loc) final override;

  void on_escaped_function (tree fndecl) final override;

  uncertainty_t *get_uncertainty () final override;

  void purge_state_involving (const svalue *sval) final override;

  void bifurcate (std::unique_ptr<custom_edge_info> info) final override;
  void terminate_path () final override;
  const extrinsic_state *get_ext_state () const final override
  {
    return &m_ext_state;
  }
  bool
  get_state_map_by_name (const char *name,
			 sm_state_map **out_smap,
			 const state_machine **out_sm,
			 unsigned *out_sm_idx,
			 std::unique_ptr<sm_context> *out_sm_context) override;

  const gimple *get_stmt () const override { return m_stmt; }
  const exploded_graph *get_eg () const override { return m_eg; }

  const program_state *get_state () const override { return m_new_state; }

  void maybe_did_work () override;
  bool checking_for_infinite_loop_p () const override { return false; }
  void on_unusable_in_infinite_loop () override {}

  pending_location
  get_pending_location_for_diag () const override;

  exploded_graph *m_eg;
  log_user m_logger;
  exploded_node *m_enode_for_diag;
  const program_state *m_old_state;
  program_state *m_new_state;
  const gimple *m_stmt;
  const extrinsic_state &m_ext_state;
  uncertainty_t *m_uncertainty;
  path_context *m_path_ctxt;
  bool *m_out_could_have_done_work;
};

/* A <program_point, program_state> pair, used internally by
   exploded_node as its immutable data, and as a key for identifying
   exploded_nodes we've already seen in the graph.  */

class point_and_state
{
public:
  point_and_state (const program_point &point,
		   const program_state &state)
  : m_point (point),
    m_state (state),
    m_hash (m_point.hash () ^ m_state.hash ())
  {
    /* We shouldn't be building point_and_states and thus exploded_nodes
       for states that aren't valid.  */
    gcc_assert (state.m_valid);
  }

  hashval_t hash () const
  {
    return m_hash;
  }
  bool operator== (const point_and_state &other) const
  {
    return m_point == other.m_point && m_state == other.m_state;
  }

  const program_point &get_point () const { return m_point; }
  const program_state &get_state () const { return m_state; }

  void set_state (const program_state &state)
  {
    m_state = state;
    m_hash = m_point.hash () ^ m_state.hash ();
  }

  void validate (const extrinsic_state &ext_state) const;

private:
  program_point m_point;
  program_state m_state;
  hashval_t m_hash;
};

/* A traits class for exploded graphs and their nodes and edges.  */

struct eg_traits
{
  typedef exploded_node node_t;
  typedef exploded_edge edge_t;
  typedef exploded_graph graph_t;
  struct dump_args_t
  {
    dump_args_t (const exploded_graph &eg) : m_eg (eg) {}

    bool show_enode_details_p (const exploded_node &enode) const;

    virtual void
    dump_extra_info (const exploded_node *, pretty_printer *) const {}

    const exploded_graph &m_eg;
  };
  typedef exploded_cluster cluster_t;
};

/* An exploded_node is a unique, immutable <point, state> pair within the
   exploded_graph.
   Each exploded_node has a unique index within the graph
   (for ease of debugging).  */

class exploded_node : public dnode<eg_traits>
{
 public:
  /* Has this enode had exploded_graph::process_node called on it?
     This allows us to distinguish enodes that were merged during
     worklist-handling, and thus never had process_node called on them
     (in favor of processing the merged node).  */
  enum class status
  {
    /* Node is in the worklist.  */
    worklist,

    /* Node has had exploded_graph::process_node called on it.  */
    processed,

    /* Node was excluded from worklist on creation.
       e.g. for handling exception-unwinding.  */
    special,

    /* Node was left unprocessed due to merger; it won't have had
       exploded_graph::process_node called on it.  */
    merger,

    /* Node was processed by maybe_process_run_of_enodes.  */
    bulk_merged
  };
  static const char * status_to_str (enum status s);

  exploded_node (const point_and_state &ps, int index);

  hashval_t hash () const { return m_ps.hash (); }

  const char * get_dot_fillcolor () const;
  void dump_dot (graphviz_out *gv, const dump_args_t &args)
    const final override;
  void dump_dot_id (pretty_printer *pp) const;

  void dump_to_pp (pretty_printer *pp, const extrinsic_state &ext_state) const;
  void dump (FILE *fp, const extrinsic_state &ext_state) const;
  void dump (const extrinsic_state &ext_state) const;

  void dump_processed_stmts (pretty_printer *pp) const;
  void dump_saved_diagnostics (pretty_printer *pp) const;

  std::unique_ptr<json::object> to_json (const extrinsic_state &ext_state) const;

  void on_longjmp (exploded_graph &eg,
		   const gcall &call,
		   program_state *new_state,
		   region_model_context *ctxt);
  void on_throw (exploded_graph &eg,
		 const gcall &call,
		 const program_point &after_throw_point,
		 program_state *new_state,
		 bool is_rethrow,
		 region_model_context *ctxt);

  void detect_leaks (exploded_graph &eg);

  const program_point &get_point () const { return m_ps.get_point (); }
  const supernode *get_supernode () const
  {
    return get_point ().get_supernode ();
  }
  location_t get_location () const
  {
    return get_point ().get_location ();
  }
  function *get_function () const
  {
    return get_point ().get_function ();
  }
  int get_stack_depth () const
  {
    return get_point ().get_stack_depth ();
  }

  const program_state &get_state () const { return m_ps.get_state (); }

  const point_and_state *get_ps_key () const { return &m_ps; }
  const program_point *get_point_key () const { return &m_ps.get_point (); }

  void dump_succs_and_preds (FILE *outf) const;

  enum status get_status () const { return m_status; }
  void set_status (enum status s)
  {
    gcc_assert (m_status == status::worklist);
    m_status = s;
  }

  void add_diagnostic (const saved_diagnostic *sd)
  {
    m_saved_diagnostics.safe_push (sd);
  }
  unsigned get_num_diagnostics () const
  {
    return m_saved_diagnostics.length ();
  }
  const saved_diagnostic *get_saved_diagnostic (unsigned idx) const
  {
    return m_saved_diagnostics[idx];
  }

private:
  DISABLE_COPY_AND_ASSIGN (exploded_node);

  /* The <program_point, program_state> pair.  This is const, as it
     is immutable once the exploded_node has been created.  */
  const point_and_state m_ps;

  enum status m_status;

  /* The saved_diagnostics at this enode, borrowed from the
     diagnostic_manager.  */
  auto_vec <const saved_diagnostic *> m_saved_diagnostics;

public:
  /* The index of this exploded_node.  */
  const int m_index;

  /* The number of stmts that were processed when process_node was
     called on this enode.  */
  unsigned m_num_processed_stmts;
};

/* An edge within the exploded graph.
   Some exploded_edges have an underlying superedge; others don't.  */

class exploded_edge : public dedge<eg_traits>
{
 public:
  exploded_edge (exploded_node *src, exploded_node *dest,
		 const superedge *sedge, bool could_do_work,
		 std::unique_ptr<custom_edge_info> custom_info);
  void dump_dot (graphviz_out *gv, const dump_args_t &args)
    const final override;
  void dump_dot_label (pretty_printer *pp) const;

  std::unique_ptr<json::object> to_json () const;

  //private:
  const superedge *const m_sedge;

  /* nullptr for most edges; will be non-NULL for special cases
     such as an unwind from a longjmp to a setjmp, or when
     a signal is delivered to a signal-handler.  */
  std::unique_ptr<custom_edge_info> m_custom_info;

  bool could_do_work_p () const { return m_could_do_work_p; }

  const gimple *maybe_get_stmt () const;
  const operation *maybe_get_op () const;

private:
  DISABLE_COPY_AND_ASSIGN (exploded_edge);

  /* For -Wanalyzer-infinite-loop.
     Set to true during processing if any of the activity along
     this edge is "externally-visible work" (and thus ruling this
     out as being part of an infinite loop.

     For example, given:

     while (1)
       do_something ();

     although it is an infinite loop, presumably the point of the
     program is the loop body, and thus reporting this as an infinite
     loop is likely to be unhelpful to the user.  */
  bool m_could_do_work_p;
};

/* Extra data for an exploded_edge that represents an interprocedural
   call.  */

class interprocedural_call : public custom_edge_info
{
public:
  interprocedural_call (const gcall &call_stmt,
			function &callee_fun)
  : m_call_stmt (call_stmt),
    m_callee_fun (callee_fun)
  {}

  void print (pretty_printer *pp) const final override;

  void get_dot_attrs (const char *&out_style,
		      const char *&out_color) const final override;

  bool update_state (program_state *state,
		     const exploded_edge *eedge,
		     region_model_context *ctxt) const final override;

  bool update_model (region_model *model,
		     const exploded_edge *eedge,
		     region_model_context *ctxt) const final override;

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &eedge,
			   pending_diagnostic &pd) const final override;

private:
  const gcall &m_call_stmt;
  function &m_callee_fun;
};

/* Extra data for an exploded_edge that represents an interprocedural
   return.  */

class interprocedural_return : public custom_edge_info
{
public:
  interprocedural_return (const gcall &call_stmt)
  : m_call_stmt (call_stmt)
  {}

  void print (pretty_printer *pp) const final override;

  void get_dot_attrs (const char *&out_style,
		      const char *&out_color) const final override;

  bool update_state (program_state *state,
		     const exploded_edge *eedge,
		     region_model_context *ctxt) const final override;

  bool update_model (region_model *model,
		     const exploded_edge *eedge,
		     region_model_context *ctxt) const final override;

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &eedge,
			   pending_diagnostic &pd) const final override;

private:
  const gcall &m_call_stmt;
};

/* Extra data for an exploded_edge that represents a rewind from a
   longjmp to a setjmp (or from a siglongjmp to a sigsetjmp).  */

class rewind_info_t : public custom_edge_info
{
public:
  rewind_info_t (const setjmp_record &setjmp_record,
		 const gcall &longjmp_call)
  : m_setjmp_record (setjmp_record),
    m_longjmp_call (longjmp_call)
  {}

  void print (pretty_printer *pp) const final override
  {
    pp_string (pp, "rewind");
  }

  bool update_model (region_model *model,
		     const exploded_edge *eedge,
		     region_model_context *ctxt) const final override;

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &eedge,
			   pending_diagnostic &pd) const final override;

  program_point
  get_point_before_setjmp () const
  {
    return get_enode_origin ()->get_point ();
  }

  program_point
  get_point_after_setjmp () const
  {
    const program_point &origin_point = get_enode_origin ()->get_point ();
    return program_point (m_setjmp_record.m_sedge->m_dest,
			  origin_point.get_call_string ());
  }

  const gcall &get_setjmp_call () const
  {
    return *m_setjmp_record.m_setjmp_call;
  }

  const gcall &get_longjmp_call () const
  {
    return m_longjmp_call;
  }

  const exploded_node *get_enode_origin () const
  {
    return m_setjmp_record.m_enode;
  }

private:
  setjmp_record m_setjmp_record;
  const gcall &m_longjmp_call;
};

/* Statistics about aspects of an exploded_graph.  */

struct stats
{
  stats (int num_supernodes);
  void log (logger *logger) const;
  void dump (FILE *out) const;

  int get_total_enodes () const;

  int m_num_nodes;
  int m_node_reuse_count;
  int m_node_reuse_after_merge_count;
  int m_num_supernodes;
};

/* Traits class for ensuring uniqueness of point_and_state data within
   an exploded_graph.  */

struct eg_hash_map_traits
{
  typedef const point_and_state *key_type;
  typedef exploded_node *value_type;
  typedef exploded_node *compare_type;

  static inline hashval_t hash (const key_type &k)
  {
    gcc_assert (k != nullptr);
    gcc_assert (k != reinterpret_cast<key_type> (1));
    return k->hash ();
  }
  static inline bool equal_keys (const key_type &k1, const key_type &k2)
  {
    gcc_assert (k1 != nullptr);
    gcc_assert (k2 != nullptr);
    gcc_assert (k1 != reinterpret_cast<key_type> (1));
    gcc_assert (k2 != reinterpret_cast<key_type> (1));
    if (k1 && k2)
      return *k1 == *k2;
    else
      /* Otherwise they must both be non-NULL.  */
      return k1 == k2;
  }
  template <typename T>
  static inline void remove (T &)
  {
    /* empty; the nodes are handled elsewhere.  */
  }
  template <typename T>
  static inline void mark_deleted (T &entry)
  {
    entry.m_key = reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline void mark_empty (T &entry)
  {
    entry.m_key = nullptr;
  }
  template <typename T>
  static inline bool is_deleted (const T &entry)
  {
    return entry.m_key == reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline bool is_empty (const T &entry)
  {
    return entry.m_key == nullptr;
  }
  static const bool empty_zero_p = false;
};

/* Per-program_point data for an exploded_graph.  */

struct per_program_point_data
{
  per_program_point_data (const program_point &key)
  : m_key (key), m_excess_enodes (0)
  {}

  const program_point m_key;
  auto_vec<exploded_node *> m_enodes;
  /* The number of attempts to create an enode for this point
     after exceeding --param=analyzer-max-enodes-per-program-point.  */
  int m_excess_enodes;
};

/* Traits class for storing per-program_point data within
   an exploded_graph.  */

struct eg_point_hash_map_traits
{
  typedef const program_point *key_type;
  typedef per_program_point_data *value_type;
  typedef per_program_point_data *compare_type;

  static inline hashval_t hash (const key_type &k)
  {
    gcc_assert (k != nullptr);
    gcc_assert (k != reinterpret_cast<key_type> (1));
    return k->hash ();
  }
  static inline bool equal_keys (const key_type &k1, const key_type &k2)
  {
    gcc_assert (k1 != nullptr);
    gcc_assert (k2 != nullptr);
    gcc_assert (k1 != reinterpret_cast<key_type> (1));
    gcc_assert (k2 != reinterpret_cast<key_type> (1));
    if (k1 && k2)
      return *k1 == *k2;
    else
      /* Otherwise they must both be non-NULL.  */
      return k1 == k2;
  }
  template <typename T>
  static inline void remove (T &)
  {
    /* empty; the nodes are handled elsewhere.  */
  }
  template <typename T>
  static inline void mark_deleted (T &entry)
  {
    entry.m_key = reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline void mark_empty (T &entry)
  {
    entry.m_key = nullptr;
  }
  template <typename T>
  static inline bool is_deleted (const T &entry)
  {
    return entry.m_key == reinterpret_cast<key_type> (1);
  }
  template <typename T>
  static inline bool is_empty (const T &entry)
  {
    return entry.m_key == nullptr;
  }
  static const bool empty_zero_p = false;
};

/* Data about a particular call_string within an exploded_graph.  */

struct per_call_string_data
{
  per_call_string_data (const call_string &key, int num_supernodes)
  : m_key (key), m_stats (num_supernodes)
  {}

  const call_string &m_key;
  stats m_stats;
};

/* Data about a particular function within an exploded_graph.  */

struct per_function_data
{
  per_function_data () {}
  ~per_function_data ();

  void add_call_summary (exploded_node *node);

  auto_vec<call_summary *> m_summaries;
};

/* The strongly connected components of a supergraph.
   In particular, this allows us to compute a partial ordering
   of supernodes.  */

class strongly_connected_components
{
public:
  strongly_connected_components (const supergraph &sg, logger *logger);

  int get_scc_id (int node_index) const
  {
    return m_per_node[node_index].m_lowlink;
  }

  void dump () const;

  std::unique_ptr<json::array> to_json () const;

private:
  struct per_node_data
  {
    per_node_data ()
    : m_id (-1), m_lowlink (-1), m_on_stack (false)
    {}

    int m_id;
    int m_lowlink;
    bool m_on_stack;
  };

  void strong_connect (unsigned index, logger *logger);

  const supergraph &m_sg;
  auto_vec<unsigned> m_stack;
  auto_vec<per_node_data> m_per_node;
};

/* The worklist of exploded_node instances that have been added to
   an exploded_graph, but that haven't yet been processed to find
   their successors (or warnings).

   The enodes are stored in a priority queue, ordered by a topological
   sort of the SCCs in the supergraph, so that enodes for the same
   program_point should appear at the front of the queue together.
   This allows for state-merging at CFG join-points, so that
   sufficiently-similar enodes can be merged into one.  */

class worklist
{
public:
  worklist (const exploded_graph &eg, const analysis_plan &plan);
  unsigned length () const;
  exploded_node *take_next ();
  exploded_node *peek_next ();
  void add_node (exploded_node *enode);
  int get_scc_id (const supernode &snode) const
  {
    return m_scc.get_scc_id (snode.m_id);
  }

  std::unique_ptr<json::object> to_json () const;

private:
  class key_t
  {
  public:
    key_t (const worklist &w, exploded_node *enode)
    : m_worklist (w), m_enode (enode)
    {}

    bool operator< (const key_t &other) const
    {
      return cmp (*this, other) < 0;
    }

    bool operator== (const key_t &other) const
    {
      return cmp (*this, other) == 0;
    }

    bool operator> (const key_t &other) const
    {
      return !(*this == other || *this < other);
    }

  private:
    static int cmp (const key_t &ka, const key_t &kb);

    int get_scc_id (const exploded_node *enode) const
    {
      const supernode *snode = enode->get_supernode ();
      if (snode == nullptr)
	return 0;
      return m_worklist.m_scc.get_scc_id (snode->m_id);
    }

    const worklist &m_worklist;
    exploded_node *m_enode;
  };

  /* The order is important here: m_scc needs to stick around
     until after m_queue has finished being cleaned up (the dtor
     calls the ordering fns).  */
  strongly_connected_components m_scc;
  const analysis_plan &m_plan;

  /* Priority queue, backed by a fibonacci_heap.  */
  typedef fibonacci_heap<key_t, exploded_node> queue_t;
  queue_t m_queue;
};

/* An exploded_graph is a directed graph of unique <point, state> pairs.
   It also has a worklist of nodes that are waiting for their successors
   to be added to the graph.  */

class exploded_graph : public digraph<eg_traits>
{
public:
  typedef hash_map <const call_string *,
		    per_call_string_data *> call_string_data_map_t;

  exploded_graph (const supergraph &sg, logger *logger,
		  const extrinsic_state &ext_state,
		  const state_purge_map *purge_map,
		  const analysis_plan &plan,
		  int verbosity);
  ~exploded_graph ();

  logger *get_logger () const { return m_logger.get_logger (); }

  const supergraph &get_supergraph () const { return m_sg; }
  const extrinsic_state &get_ext_state () const { return m_ext_state; }
  engine *get_engine () const { return m_ext_state.get_engine (); }
  const state_purge_map *get_purge_map () const { return m_purge_map; }
  const analysis_plan &get_analysis_plan () const { return m_plan; }

  exploded_node *get_origin () const { return m_origin; }

  exploded_node *add_function_entry (const function &fun);

  void build_initial_worklist ();
  void process_worklist ();
  bool maybe_process_run_of_enodes (exploded_node *node);
  void process_node (exploded_node *node);

  exploded_node *get_or_create_node (const program_point &point,
				     const program_state &state,
				     exploded_node *enode_for_diag,
				     bool add_to_worklist = true);
  exploded_edge *add_edge (exploded_node *src, exploded_node *dest,
			   const superedge *sedge, bool could_do_work,
			   std::unique_ptr<custom_edge_info> custom = nullptr);

  per_program_point_data *
  get_or_create_per_program_point_data (const program_point &);
  per_program_point_data *
  get_per_program_point_data (const program_point &) const;

  per_call_string_data *
  get_or_create_per_call_string_data (const call_string &);

  per_function_data *
  get_or_create_per_function_data (function *);
  per_function_data *get_per_function_data (function *) const;

  void save_diagnostic (const state_machine &sm,
			const exploded_node *enode,
			const supernode *node, const gimple *stmt,
			tree var, state_machine::state_t state,
			pending_diagnostic *d);

  diagnostic_manager &get_diagnostic_manager ()
  {
    return m_diagnostic_manager;
  }
  const diagnostic_manager &get_diagnostic_manager () const
  {
    return m_diagnostic_manager;
  }

  stats *get_global_stats () { return &m_global_stats; }
  stats *get_or_create_function_stats (function *fn);
  void log_stats () const;
  void dump_stats (FILE *) const;
  void dump_exploded_nodes () const;

  std::unique_ptr<json::object> to_json () const;

  exploded_node *get_node_by_index (int idx) const;

  const call_string_data_map_t *get_per_call_string_data () const
  { return &m_per_call_string_data; }

  int get_scc_id (const supernode &node) const
  {
    return m_worklist.get_scc_id (node);
  }

  void on_escaped_function (tree fndecl);

  void unwind_from_exception (exploded_node &enode,
			      const gimple *stmt,
			      region_model_context *ctxt);

  /* In infinite-loop.cc */
  void detect_infinite_loops ();

  /* In infinite-recursion.cc */
  void detect_infinite_recursion (exploded_node *enode);
  exploded_node *find_previous_entry_to (function *top_of_stack_fun,
					 exploded_node *enode) const;

private:
  void print_bar_charts (pretty_printer *pp) const;

  DISABLE_COPY_AND_ASSIGN (exploded_graph);

  const supergraph &m_sg;

  log_user m_logger;

  /* Map from point/state to exploded node.
     To avoid duplication we store point_and_state
     *pointers* as keys, rather than point_and_state, using the
     instance from within the exploded_node, with a custom hasher.  */
  typedef hash_map <const point_and_state *, exploded_node *,
		    eg_hash_map_traits> map_t;
  map_t m_point_and_state_to_node;

  /* Map from program_point to per-program_point data.  */
  typedef hash_map <const program_point *, per_program_point_data *,
		    eg_point_hash_map_traits> point_map_t;
  point_map_t m_per_point_data;

  worklist m_worklist;

  exploded_node *m_origin;

  const extrinsic_state &m_ext_state;

  const state_purge_map *const m_purge_map;

  const analysis_plan &m_plan;

  typedef hash_map<function *, per_function_data *> per_function_data_t;
  per_function_data_t m_per_function_data;

  diagnostic_manager m_diagnostic_manager;

  /* Stats.  */
  stats m_global_stats;
  typedef ordered_hash_map<function *, stats *> function_stat_map_t;
  function_stat_map_t m_per_function_stats;
  stats m_functionless_stats;

  call_string_data_map_t m_per_call_string_data;

  /* Functions with a top-level enode, to make add_function_entry
     be idempotent, for use in handling callbacks.  */
  hash_set<function *> m_functions_with_enodes;
};

/* A path within an exploded_graph: a sequence of edges.  */

class exploded_path
{
public:
  exploded_path () : m_edges () {}
  exploded_path (const exploded_path &other);

  unsigned length () const { return m_edges.length (); }

  bool find_stmt_backwards (const gimple *search_stmt,
			    int *out_idx) const;

  exploded_node *get_final_enode () const;

  void dump_to_pp (pretty_printer *pp,
		   const extrinsic_state *ext_state) const;
  void dump (FILE *fp, const extrinsic_state *ext_state) const;
  void dump (const extrinsic_state *ext_state = nullptr) const;
  void dump_to_file (const char *filename,
		     const extrinsic_state &ext_state) const;

  bool feasible_p (logger *logger, std::unique_ptr<feasibility_problem> *out,
		    engine *eng, const exploded_graph *eg) const;

  auto_vec<const exploded_edge *> m_edges;
};

/* A reason why a particular exploded_path is infeasible.  */

class feasibility_problem
{
public:
  feasibility_problem (unsigned eedge_idx,
		       const exploded_edge &eedge,
		       std::unique_ptr<rejected_constraint> rc)
  : m_eedge_idx (eedge_idx), m_eedge (eedge),
    m_rc (std::move (rc))
  {}

  void dump_to_pp (pretty_printer *pp) const;

  unsigned m_eedge_idx;
  const exploded_edge &m_eedge;
  std::unique_ptr<rejected_constraint> m_rc;
};

/* A class for capturing the state of a node when checking a path
   through the exploded_graph for feasibility.  */

class feasibility_state
{
public:
  feasibility_state (region_model_manager *manager,
		     const supergraph &sg);
  feasibility_state (const region_model &model,
		     const supergraph &sg);
  feasibility_state (const feasibility_state &other);

  feasibility_state &operator= (const feasibility_state &other);

  bool maybe_update_for_edge (logger *logger,
			      const exploded_edge *eedge,
			      region_model_context *ctxt,
			      std::unique_ptr<rejected_constraint> *out_rc);

  region_model &get_model () { return m_model; }
  const region_model &get_model () const { return m_model; }
  auto_sbitmap &get_snodes_visited () { return m_snodes_visited; }
  const auto_sbitmap &get_snodes_visited () const { return m_snodes_visited; }

  void dump_to_pp (pretty_printer *pp, bool simple, bool multiline) const;

private:
  region_model m_model;
  auto_sbitmap m_snodes_visited;
};

/* Finding the shortest exploded_path within an exploded_graph.  */

typedef shortest_paths<eg_traits, exploded_path> shortest_exploded_paths;

// TODO: split the above up?

} // namespace ana

#endif /* GCC_ANALYZER_EXPLODED_GRAPH_H */
