/* Classes for purging state at function_points.
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

#ifndef GCC_ANALYZER_STATE_PURGE_H
#define GCC_ANALYZER_STATE_PURGE_H

/* Hash traits for function_point.  */

template <> struct default_hash_traits<function_point>
: public pod_hash_traits<function_point>
{
  static const bool empty_zero_p = false;
};

template <>
inline hashval_t
pod_hash_traits<function_point>::hash (value_type v)
{
  return v.hash ();
}

template <>
inline bool
pod_hash_traits<function_point>::equal (const value_type &existing,
                                 const value_type &candidate)
{
  return existing == candidate;
}
template <>
inline void
pod_hash_traits<function_point>::mark_deleted (value_type &v)
{
  v = function_point::deleted ();
}
template <>
inline void
pod_hash_traits<function_point>::mark_empty (value_type &v)
{
  v = function_point::empty ();
}
template <>
inline bool
pod_hash_traits<function_point>::is_deleted (value_type v)
{
  return v.get_kind () == PK_DELETED;
}
template <>
inline bool
pod_hash_traits<function_point>::is_empty (value_type v)
{
  return v.get_kind () == PK_EMPTY;
}

namespace ana {

/* The result of analyzing which SSA names can be purged from state at
   different points in the program, so that we can simplify program_state
   objects, in the hope of reducing state-blowup.  */

class state_purge_map : public log_user
{
public:
  typedef ordered_hash_map<tree, state_purge_per_ssa_name *> map_t;
  typedef map_t::iterator iterator;

  state_purge_map (const supergraph &sg, logger *logger);
  ~state_purge_map ();

  const state_purge_per_ssa_name &get_data_for_ssa_name (tree name) const
  {
    gcc_assert (TREE_CODE (name) == SSA_NAME);
    if (tree var = SSA_NAME_VAR (name))
      if (TREE_CODE (var) == VAR_DECL)
	gcc_assert (!VAR_DECL_IS_VIRTUAL_OPERAND (var));

    state_purge_per_ssa_name **slot
      = const_cast <map_t&> (m_map).get (name);
    return **slot;
  }

  const supergraph &get_sg () const { return m_sg; }

  iterator begin () const { return m_map.begin (); }
  iterator end () const { return m_map.end (); }

private:
  DISABLE_COPY_AND_ASSIGN (state_purge_map);

  const supergraph &m_sg;
  map_t m_map;
};

/* The part of a state_purge_map relating to a specific SSA name.

   The result of analyzing a given SSA name, recording which
   function_points need to retain state information about it to handle
   their successor states, so that we can simplify program_state objects,
   in the hope of reducing state-blowup.  */

class state_purge_per_ssa_name
{
public:
  state_purge_per_ssa_name (const state_purge_map &map,
			    tree name,
			    function *fun);

  bool needed_at_point_p (const function_point &point) const;

  function *get_function () const { return m_fun; }

private:
  static function_point before_use_stmt (const state_purge_map &map,
					 const gimple *use_stmt);

  void add_to_worklist (const function_point &point,
			auto_vec<function_point> *worklist,
			logger *logger);

  void process_point (const function_point &point,
		      auto_vec<function_point> *worklist,
		      const state_purge_map &map);

  typedef hash_set<function_point> point_set_t;
  point_set_t m_points_needing_name;
  tree m_name;
  function *m_fun;
};

/* Subclass of dot_annotator for use by -fdump-analyzer-state-purge.
   Annotate the .dot output with state-purge information.  */

class state_purge_annotator : public dot_annotator
{
public:
  state_purge_annotator (const state_purge_map *map) : m_map (map) {}

  bool add_node_annotations (graphviz_out *gv, const supernode &n, bool)
    const FINAL OVERRIDE;

  void add_stmt_annotations (graphviz_out *gv, const gimple *stmt,
			     bool within_row)
    const FINAL OVERRIDE;

private:
  const state_purge_map *m_map;
};

} // namespace ana

#endif /* GCC_ANALYZER_STATE_PURGE_H */
