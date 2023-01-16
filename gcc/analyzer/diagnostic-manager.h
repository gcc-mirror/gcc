/* Classes for saving, deduplicating, and emitting analyzer diagnostics.
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

#ifndef GCC_ANALYZER_DIAGNOSTIC_MANAGER_H
#define GCC_ANALYZER_DIAGNOSTIC_MANAGER_H

namespace ana {

class epath_finder;

/* A to-be-emitted diagnostic stored within diagnostic_manager.  */

class saved_diagnostic
{
public:
  saved_diagnostic (const state_machine *sm,
		    const exploded_node *enode,
		    const supernode *snode, const gimple *stmt,
		    const stmt_finder *stmt_finder,
		    tree var, const svalue *sval,
		    state_machine::state_t state,
		    std::unique_ptr<pending_diagnostic> d,
		    unsigned idx);

  bool operator== (const saved_diagnostic &other) const;

  void add_note (std::unique_ptr<pending_note> pn);

  json::object *to_json () const;

  void dump_dot_id (pretty_printer *pp) const;
  void dump_as_dot_node (pretty_printer *pp) const;

  const feasibility_problem *get_feasibility_problem () const
  {
    return m_problem.get ();
  }

  bool calc_best_epath (epath_finder *pf);
  const exploded_path *get_best_epath () const { return m_best_epath.get (); }
  unsigned get_epath_length () const;

  void add_duplicate (saved_diagnostic *other);
  unsigned get_num_dupes () const { return m_duplicates.length (); }

  unsigned get_index () const { return m_idx; }

  bool supercedes_p (const saved_diagnostic &other) const;

  void emit_any_notes () const;

  //private:
  const state_machine *m_sm;
  const exploded_node *m_enode;
  const supernode *m_snode;
  const gimple *m_stmt;
  std::unique_ptr<stmt_finder> m_stmt_finder;
  tree m_var;
  const svalue *m_sval;
  state_machine::state_t m_state;
  std::unique_ptr<pending_diagnostic> m_d;
  const exploded_edge *m_trailing_eedge;

private:
  DISABLE_COPY_AND_ASSIGN (saved_diagnostic);

  unsigned m_idx;
  std::unique_ptr<exploded_path> m_best_epath;
  std::unique_ptr<feasibility_problem> m_problem;

  auto_vec<const saved_diagnostic *> m_duplicates;
  auto_delete_vec <pending_note> m_notes;
};

class path_builder;

/* A class with responsibility for saving pending diagnostics, so that
   they can be emitted after the exploded_graph is complete.
   This lets us de-duplicate diagnostics, and find the shortest path
   for each similar diagnostic, potentially using edges that might
   not have been found when each diagnostic was first saved.

   This also lets us compute shortest_paths once, rather than
   per-diagnostic.  */

class diagnostic_manager : public log_user
{
public:
  diagnostic_manager (logger *logger, engine *eng, int verbosity);

  engine *get_engine () const { return m_eng; }

  json::object *to_json () const;

  bool add_diagnostic (const state_machine *sm,
		       exploded_node *enode,
		       const supernode *snode, const gimple *stmt,
		       const stmt_finder *finder,
		       tree var,
		       const svalue *sval,
		       state_machine::state_t state,
		       std::unique_ptr<pending_diagnostic> d);

  bool add_diagnostic (exploded_node *enode,
		       const supernode *snode, const gimple *stmt,
		       const stmt_finder *finder,
		       std::unique_ptr<pending_diagnostic> d);

  void add_note (std::unique_ptr<pending_note> pn);

  void emit_saved_diagnostics (const exploded_graph &eg);

  void emit_saved_diagnostic (const exploded_graph &eg,
			      const saved_diagnostic &sd);

  unsigned get_num_diagnostics () const
  {
    return m_saved_diagnostics.length ();
  }
  saved_diagnostic *get_saved_diagnostic (unsigned idx)
  {
    return m_saved_diagnostics[idx];
  }
  const saved_diagnostic *get_saved_diagnostic (unsigned idx) const
  {
    return m_saved_diagnostics[idx];
  }

private:
  void build_emission_path (const path_builder &pb,
			    const exploded_path &epath,
			    checker_path *emission_path) const;

  void add_event_on_final_node (const path_builder &pb,
				const exploded_node *final_enode,
				checker_path *emission_path,
				interesting_t *interest) const;

  void add_events_for_eedge (const path_builder &pb,
			     const exploded_edge &eedge,
			     checker_path *emission_path,
			     interesting_t *interest) const;

  bool significant_edge_p (const path_builder &pb,
			   const exploded_edge &eedge) const;

  void add_events_for_superedge (const path_builder &pb,
				 const exploded_edge &eedge,
				 checker_path *emission_path) const;

  void prune_path (checker_path *path,
		   const state_machine *sm,
		   const svalue *sval,
		   state_machine::state_t state) const;

  void prune_for_sm_diagnostic (checker_path *path,
				const state_machine *sm,
				tree var,
				state_machine::state_t state) const;
  void prune_for_sm_diagnostic (checker_path *path,
				const state_machine *sm,
				const svalue *sval,
				state_machine::state_t state) const;
  void update_for_unsuitable_sm_exprs (tree *expr) const;
  void prune_interproc_events (checker_path *path) const;
  void consolidate_conditions (checker_path *path) const;
  void finish_pruning (checker_path *path) const;

  engine *m_eng;
  auto_delete_vec<saved_diagnostic> m_saved_diagnostics;
  const int m_verbosity;
  int m_num_disabled_diagnostics;
};

} // namespace ana

#endif /* GCC_ANALYZER_DIAGNOSTIC_MANAGER_H */
