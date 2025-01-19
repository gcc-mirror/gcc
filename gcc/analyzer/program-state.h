/* Classes for representing the state of interest at a given path of analysis.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_PROGRAM_STATE_H
#define GCC_ANALYZER_PROGRAM_STATE_H

#include "text-art/widget.h"

namespace ana {

/* Data shared by all program_state instances.  */

class extrinsic_state
{
public:
  extrinsic_state (auto_delete_vec <state_machine> &checkers,
		   engine *eng,
		   logger *logger = NULL)
  : m_checkers (checkers), m_logger (logger), m_engine (eng)
  {
  }

  const state_machine &get_sm (int idx) const
  {
    return *m_checkers[idx];
  }

  const char *get_name (int idx) const
  {
    return m_checkers[idx]->get_name ();
  }

  unsigned get_num_checkers () const { return m_checkers.length (); }

  logger *get_logger () const { return m_logger; }

  void dump_to_pp (pretty_printer *pp) const;
  void dump_to_file (FILE *outf) const;
  void dump () const;

  std::unique_ptr<json::object> to_json () const;

  engine *get_engine () const { return m_engine; }
  region_model_manager *get_model_manager () const;

  bool get_sm_idx_by_name (const char *name, unsigned *out) const;

private:
  /* The state machines.  */
  auto_delete_vec <state_machine> &m_checkers;

  logger *m_logger;
  engine *m_engine;
};

/* Map from svalue * to state machine state, also capturing the origin of
   each state.  */

class sm_state_map
{
public:
  /* An entry in the hash_map.  */
  struct entry_t
  {
    /* Default ctor needed by hash_map::empty.  */
    entry_t ()
    : m_state (0), m_origin (NULL)
    {
    }

    entry_t (state_machine::state_t state,
	     const svalue *origin)
    : m_state (state), m_origin (origin)
    {}

    bool operator== (const entry_t &other) const
    {
      return (m_state == other.m_state
	      && m_origin == other.m_origin);
    }
    bool operator!= (const entry_t &other) const
    {
      return !(*this == other);
    }

    static int cmp (const entry_t &entry_a, const entry_t &entry_b);

    state_machine::state_t m_state;
    const svalue *m_origin;
  };
  typedef hash_map <const svalue *, entry_t> map_t;
  typedef map_t::iterator iterator_t;

  sm_state_map (const state_machine &sm);

  sm_state_map *clone () const;

  void print (const region_model *model,
	      bool simple, bool multiline,
	      pretty_printer *pp) const;
  void dump (bool simple) const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi,
		    const region_model *model) const;

  bool is_empty_p () const;

  hashval_t hash () const;

  bool operator== (const sm_state_map &other) const;
  bool operator!= (const sm_state_map &other) const
  {
    return !(*this == other);
  }

  state_machine::state_t get_state (const svalue *sval,
				    const extrinsic_state &ext_state) const;
  const svalue *get_origin (const svalue *sval,
			    const extrinsic_state &ext_state) const;

  void set_state (region_model *model,
		  const svalue *sval,
		  state_machine::state_t state,
		  const svalue *origin,
		  const extrinsic_state &ext_state);
  bool set_state (const equiv_class &ec,
		  state_machine::state_t state,
		  const svalue *origin,
		  const extrinsic_state &ext_state);
  bool impl_set_state (const svalue *sval,
		       state_machine::state_t state,
		       const svalue *origin,
		       const extrinsic_state &ext_state);
  void clear_any_state (const svalue *sval);
  void clear_all_per_svalue_state ();

  void set_global_state (state_machine::state_t state);
  state_machine::state_t get_global_state () const;

  void on_svalue_leak (const svalue *sval,
		       impl_region_model_context *ctxt);
  void on_liveness_change (const svalue_set &live_svalues,
			   const region_model *model,
			   const extrinsic_state &ext_state,
			   impl_region_model_context *ctxt);

  void on_unknown_change (const svalue *sval,
			  bool is_mutable,
			  const extrinsic_state &ext_state);

  void purge_state_involving (const svalue *sval,
			      const extrinsic_state &ext_state);

  iterator_t begin () const { return m_map.begin (); }
  iterator_t end () const { return m_map.end (); }
  size_t elements () const { return m_map.elements (); }

  static int cmp (const sm_state_map &smap_a, const sm_state_map &smap_b);

  static const svalue *
  canonicalize_svalue (const svalue *sval, const extrinsic_state &ext_state);

  bool replay_call_summary (call_summary_replay &r,
			    const sm_state_map &summary);

  bool can_merge_with_p (const sm_state_map &other,
			 const state_machine &sm,
			 const extrinsic_state &ext_state,
			 sm_state_map **out) const;

private:
  const state_machine &m_sm;
  map_t m_map;
  state_machine::state_t m_global_state;
};

/* A class for representing the state of interest at a given path of
   analysis.

   Currently this is a combination of:
   (a) a region_model, giving:
      (a.1) a hierarchy of memory regions
      (a.2) values for the regions
      (a.3) inequalities between values
   (b) sm_state_maps per state machine, giving a sparse mapping of
       values to states.  */

class program_state
{
public:
  program_state (const extrinsic_state &ext_state);
  program_state (const program_state &other);
  program_state& operator= (const program_state &other);
  program_state (program_state &&other);
  ~program_state ();

  hashval_t hash () const;
  bool operator== (const program_state &other) const;
  bool operator!= (const program_state &other) const
  {
    return !(*this == other);
  }

  void print (const extrinsic_state &ext_state,
	      pretty_printer *pp) const;

  void dump_to_pp (const extrinsic_state &ext_state, bool simple,
		   bool multiline, pretty_printer *pp) const;
  void dump_to_file (const extrinsic_state &ext_state, bool simple,
		     bool multiline, FILE *outf) const;
  void dump (const extrinsic_state &ext_state, bool simple) const;
  void dump () const;

  std::unique_ptr<json::object>
  to_json (const extrinsic_state &ext_state) const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  void push_frame (const extrinsic_state &ext_state, const function &fun);
  const function * get_current_function () const;

  void push_call (exploded_graph &eg,
		  exploded_node *enode,
		  const gcall *call_stmt,
		  uncertainty_t *uncertainty);

  void returning_call (exploded_graph &eg,
		       exploded_node *enode,
		       const gcall *call_stmt,
		       uncertainty_t *uncertainty);


  bool on_edge (exploded_graph &eg,
		exploded_node *enode,
		const superedge *succ,
		uncertainty_t *uncertainty);

  program_state prune_for_point (exploded_graph &eg,
				 const program_point &point,
				 exploded_node *enode_for_diag,
				 uncertainty_t *uncertainty) const;

  tree get_representative_tree (const svalue *sval) const;

  bool can_purge_p (const extrinsic_state &ext_state,
		    const svalue *sval) const
  {
    /* Don't purge vars that have non-purgeable sm state, to avoid
       generating false "leak" complaints.  */
    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (m_checker_states, i, smap)
      {
	const state_machine &sm = ext_state.get_sm (i);
	if (!sm.can_purge_p (smap->get_state (sval, ext_state)))
	  return false;
      }
    return true;
  }

  bool can_purge_base_region_p (const extrinsic_state &ext_state,
				const region *base_reg) const;

  bool can_merge_with_p (const program_state &other,
			 const extrinsic_state &ext_state,
			 const program_point &point,
			 program_state *out) const;

  void validate (const extrinsic_state &ext_state) const;

  static void detect_leaks (const program_state &src_state,
			    const program_state &dest_state,
			    const svalue *extra_sval,
			    const extrinsic_state &ext_state,
			    region_model_context *ctxt);

  bool replay_call_summary (call_summary_replay &r,
			    const program_state &summary);

  void impl_call_analyzer_dump_state (const gcall *call,
				      const extrinsic_state &ext_state,
				      region_model_context *ctxt);

  /* TODO: lose the pointer here (const-correctness issues?).  */
  region_model *m_region_model;
  auto_delete_vec<sm_state_map> m_checker_states;

  /* If false, then don't attempt to explore further states along this path.
     For use in "handling" lvalues for tree codes we haven't yet
     implemented.  */
  bool m_valid;
};

/* An abstract base class for use with for_each_state_change.  */

class state_change_visitor
{
public:
  virtual ~state_change_visitor () {}

  /* Return true for early exit, false to keep iterating.  */
  virtual bool on_global_state_change (const state_machine &sm,
				       state_machine::state_t src_sm_val,
				       state_machine::state_t dst_sm_val) = 0;

  /* Return true for early exit, false to keep iterating.  */
  virtual bool on_state_change (const state_machine &sm,
				state_machine::state_t src_sm_val,
				state_machine::state_t dst_sm_val,
				const svalue *dst_sval,
				const svalue *dst_origin_sval) = 0;
};

extern bool for_each_state_change (const program_state &src_state,
				    const program_state &dst_state,
				    const extrinsic_state &ext_state,
				    state_change_visitor *visitor);

} // namespace ana

#endif /* GCC_ANALYZER_PROGRAM_STATE_H */
