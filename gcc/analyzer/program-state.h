/* Classes for representing the state of interest at a given path of analysis.
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

#ifndef GCC_ANALYZER_PROGRAM_STATE_H
#define GCC_ANALYZER_PROGRAM_STATE_H

namespace ana {

/* Data shared by all program_state instances.  */

class extrinsic_state
{
public:
  extrinsic_state (auto_delete_vec <state_machine> &checkers)
  : m_checkers (checkers)
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

  void dump_to_pp (pretty_printer *pp) const;
  void dump_to_file (FILE *outf) const;
  void dump () const;

private:
  /* The state machines.  */
  auto_delete_vec <state_machine> &m_checkers;
};

} // namespace ana

template <> struct default_hash_traits<svalue_id>
: public pod_hash_traits<svalue_id>
{
  static const bool empty_zero_p = false;
};

template <>
inline hashval_t
pod_hash_traits<svalue_id>::hash (value_type v)
{
  return v.as_int ();
}

template <>
inline bool
pod_hash_traits<svalue_id>::equal (const value_type &existing,
				   const value_type &candidate)
{
  return existing == candidate;
}
template <>
inline void
pod_hash_traits<svalue_id>::mark_deleted (value_type &v)
{
  v = svalue_id::from_int (-2);
}
template <>
inline void
pod_hash_traits<svalue_id>::mark_empty (value_type &v)
{
  v = svalue_id::null ();
}
template <>
inline bool
pod_hash_traits<svalue_id>::is_deleted (value_type v)
{
  return v.as_int () == -2;
}
template <>
inline bool
pod_hash_traits<svalue_id>::is_empty (value_type v)
{
  return v.null_p ();
}

namespace ana {

/* Map from svalue_id to state machine state, also capturing the origin of
   each state.  */

class sm_state_map
{
public:
  /* An entry in the hash_map.  */
  struct entry_t
  {
    /* Default ctor needed by hash_map::empty.  */
    entry_t ()
    : m_state (0), m_origin (svalue_id::null ())
    {
    }

    entry_t (state_machine::state_t state,
	     svalue_id origin)
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

    state_machine::state_t m_state;
    svalue_id m_origin;
  };
  typedef hash_map <svalue_id, entry_t> map_t;
  typedef map_t::iterator iterator_t;

  sm_state_map ();

  sm_state_map *clone () const;

  sm_state_map *
  clone_with_remapping (const one_way_svalue_id_map &id_map) const;

  void print (const state_machine &sm, const region_model *model,
	      pretty_printer *pp) const;
  void dump (const state_machine &sm) const;

  bool is_empty_p () const;

  hashval_t hash () const;

  bool operator== (const sm_state_map &other) const;
  bool operator!= (const sm_state_map &other) const
  {
    return !(*this == other);
  }

  state_machine::state_t get_state (svalue_id sid) const;
  svalue_id get_origin (svalue_id sid) const;

  void set_state (region_model *model,
		  svalue_id sid,
		  state_machine::state_t state,
		  svalue_id origin);
  bool set_state (const equiv_class &ec,
		  state_machine::state_t state,
		  svalue_id origin);
  bool impl_set_state (svalue_id sid,
		       state_machine::state_t state,
		       svalue_id origin);

  void set_global_state (state_machine::state_t state);
  state_machine::state_t get_global_state () const;

  void purge_for_unknown_fncall (const exploded_graph &eg,
				 const state_machine &sm,
				 const gcall *call, tree fndecl,
				 region_model *new_model,
				 region_model_context *ctxt);

  void remap_svalue_ids (const svalue_id_map &map);

  int on_svalue_purge (const state_machine &sm,
		       int sm_idx,
		       svalue_id first_unused_sid,
		       const svalue_id_map &map,
		       impl_region_model_context *ctxt);

  void on_inherited_svalue (svalue_id parent_sid,
			    svalue_id child_sid);

  void on_cast (svalue_id src_sid,
		svalue_id dst_sid);

  void on_unknown_change (svalue_id sid);

  void validate (const state_machine &sm, int num_svalues) const;

  iterator_t begin () const { return m_map.begin (); }
  iterator_t end () const { return m_map.end (); }

private:
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

#if __cplusplus >= 201103
  program_state (program_state &&other);
  program_state& operator= (program_state &&other); // doesn't seem to be used
#endif

  ~program_state ();

  hashval_t hash () const;
  bool operator== (const program_state &other) const;
  bool operator!= (const program_state &other) const
  {
    return !(*this == other);
  }

  void print (const extrinsic_state &ext_state,
	      pretty_printer *pp) const;

  void dump_to_pp (const extrinsic_state &ext_state, bool summarize,
		   pretty_printer *pp) const;
  void dump_to_file (const extrinsic_state &ext_state, bool summarize,
		     FILE *outf) const;
  void dump (const extrinsic_state &ext_state, bool summarize) const;

  bool on_edge (exploded_graph &eg,
		const exploded_node &enode,
		const superedge *succ,
		state_change *change);

  program_state prune_for_point (exploded_graph &eg,
				 const program_point &point,
				 state_change *change) const;

  void remap_svalue_ids (const svalue_id_map &map);

  tree get_representative_tree (svalue_id sid) const;

  bool can_purge_p (const extrinsic_state &ext_state,
		    svalue_id sid)
  {
    /* Don't purge vars that have non-purgeable sm state, to avoid
       generating false "leak" complaints.  */
    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (m_checker_states, i, smap)
      {
	const state_machine &sm = ext_state.get_sm (i);
	if (!sm.can_purge_p (smap->get_state (sid)))
	  return false;
      }
    return true;
  }

  bool can_merge_with_p (const program_state &other,
			 const extrinsic_state &ext_state,
			 program_state *out) const;

  void validate (const extrinsic_state &ext_state) const;

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
				tree dst_rep,
				svalue_id dst_origin_sid) = 0;
};

extern bool for_each_state_change (const program_state &src_state,
				   const program_state &dst_state,
				   const extrinsic_state &ext_state,
				   state_change_visitor *visitor);

/* A class for recording "interesting" state changes.
   This is used for annotating edges in the GraphViz output of the
   exploded_graph, and for recording sm-state-changes, so that
   values that change aren't purged (to make it easier to generate
   state_change_event instances in the diagnostic_path).  */

class state_change
{
 public:
  struct sm_change
  {
    sm_change (int sm_idx,
	       svalue_id new_sid,
	       state_machine::state_t old_state,
	       state_machine::state_t new_state)
    : m_sm_idx (sm_idx),
      m_new_sid (new_sid),
      m_old_state (old_state), m_new_state (new_state)
    {}

    const state_machine &get_sm (const extrinsic_state &ext_state) const
    {
      return ext_state.get_sm (m_sm_idx);
    }

    void dump (pretty_printer *pp, const extrinsic_state &ext_state) const;

    void remap_svalue_ids (const svalue_id_map &map);
    int on_svalue_purge (svalue_id first_unused_sid);

    void validate (const program_state &new_state,
		   const extrinsic_state &ext_state) const;

    int m_sm_idx;
    svalue_id m_new_sid;
    state_machine::state_t m_old_state;
    state_machine::state_t m_new_state;
  };

  state_change ();
  state_change (const state_change &other);

  void add_sm_change (int sm_idx,
		      svalue_id new_sid,
		      state_machine::state_t old_state,
		      state_machine::state_t new_state);

  bool affects_p (svalue_id sid) const;

  void dump (pretty_printer *pp, const extrinsic_state &ext_state) const;
  void dump (const extrinsic_state &ext_state) const;

  void remap_svalue_ids (const svalue_id_map &map);
  int on_svalue_purge (svalue_id first_unused_sid);

  void validate (const program_state &new_state,
		 const extrinsic_state &ext_state) const;

 private:
  auto_vec<sm_change> m_sm_changes;
};

} // namespace ana

#endif /* GCC_ANALYZER_PROGRAM_STATE_H */
