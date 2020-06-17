/* Classes for saving, deduplicating, and emitting analyzer diagnostics.
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

#ifndef GCC_ANALYZER_DIAGNOSTIC_MANAGER_H
#define GCC_ANALYZER_DIAGNOSTIC_MANAGER_H

namespace ana {

/* A to-be-emitted diagnostic stored within diagnostic_manager.  */

class saved_diagnostic
{
public:
  enum status
  {
   STATUS_NEW,
   STATUS_INFEASIBLE_PATH,
   STATUS_FEASIBLE_PATH
  };

  saved_diagnostic (const state_machine *sm,
		    const exploded_node *enode,
		    const supernode *snode, const gimple *stmt,
		    stmt_finder *stmt_finder,
		    tree var, state_machine::state_t state,
		    pending_diagnostic *d);
  ~saved_diagnostic ();

  bool operator== (const saved_diagnostic &other) const;

  void set_feasible ()
  {
    gcc_assert (m_status == STATUS_NEW);
    m_status = STATUS_FEASIBLE_PATH;
  }
  void set_infeasible (feasibility_problem *p)
  {
    gcc_assert (m_status == STATUS_NEW);
    m_status = STATUS_INFEASIBLE_PATH;
    m_problem = p; // take ownership
  }
  const feasibility_problem *get_feasibility_problem () const
  {
    return m_problem;
  }

  enum status get_status () const { return m_status; }

  void set_epath_length (unsigned length) { m_epath_length = length; }
  unsigned get_epath_length () const { return m_epath_length; }

  //private:
  const state_machine *m_sm;
  const exploded_node *m_enode;
  const supernode *m_snode;
  const gimple *m_stmt;
  stmt_finder *m_stmt_finder;
  tree m_var;
  state_machine::state_t m_state;
  pending_diagnostic *m_d;
  exploded_edge *m_trailing_eedge;

private:
  DISABLE_COPY_AND_ASSIGN (saved_diagnostic);

  enum status m_status;
  unsigned m_epath_length;
  feasibility_problem *m_problem;
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
  diagnostic_manager (logger *logger, int verbosity);

  void add_diagnostic (const state_machine *sm,
		       const exploded_node *enode,
		       const supernode *snode, const gimple *stmt,
		       stmt_finder *finder,
		       tree var, state_machine::state_t state,
		       pending_diagnostic *d);

  void add_diagnostic (const exploded_node *enode,
		       const supernode *snode, const gimple *stmt,
		       stmt_finder *finder,
		       pending_diagnostic *d);

  void emit_saved_diagnostics (const exploded_graph &eg);

  void emit_saved_diagnostic (const exploded_graph &eg,
			      const saved_diagnostic &sd,
			      const exploded_path &epath,
			      const gimple *stmt,
			      int num_dupes);

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

  void add_events_for_eedge (const path_builder &pb,
			     const exploded_edge &eedge,
			     checker_path *emission_path) const;

  bool significant_edge_p (const path_builder &pb,
			   const exploded_edge &eedge) const;

  void add_events_for_superedge (const path_builder &pb,
				 const exploded_edge &eedge,
				 checker_path *emission_path) const;

  void prune_path (checker_path *path,
		   const state_machine *sm,
		   tree var, state_machine::state_t state) const;

  void prune_for_sm_diagnostic (checker_path *path,
				const state_machine *sm,
				tree var,
				state_machine::state_t state) const;
  void update_for_unsuitable_sm_exprs (tree *expr) const;
  void prune_interproc_events (checker_path *path) const;
  void finish_pruning (checker_path *path) const;

  auto_delete_vec<saved_diagnostic> m_saved_diagnostics;
  const int m_verbosity;
};

} // namespace ana

#endif /* GCC_ANALYZER_DIAGNOSTIC_MANAGER_H */
