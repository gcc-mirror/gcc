/* Classes for purging state at function_points.
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

/* The result of analyzing which decls and SSA names can be purged from state at
   different points in the program, so that we can simplify program_state
   objects, in the hope of reducing state-blowup.  */

class state_purge_map : public log_user
{
public:
  typedef ordered_hash_map<tree, state_purge_per_ssa_name *> ssa_map_t;
  typedef ssa_map_t::iterator ssa_iterator;

  typedef ordered_hash_map<tree, state_purge_per_decl *> decl_map_t;
  typedef decl_map_t::iterator decl_iterator;

  state_purge_map (const supergraph &sg,
		   region_model_manager *mgr,
		   logger *logger);
  ~state_purge_map ();

  const state_purge_per_ssa_name &get_data_for_ssa_name (tree name) const
  {
    gcc_assert (TREE_CODE (name) == SSA_NAME);
    if (tree var = SSA_NAME_VAR (name))
      if (TREE_CODE (var) == VAR_DECL)
	gcc_assert (!VAR_DECL_IS_VIRTUAL_OPERAND (var));

    state_purge_per_ssa_name **slot
      = const_cast <ssa_map_t&> (m_ssa_map).get (name);
    return **slot;
  }

  const state_purge_per_decl *get_any_data_for_decl (tree decl) const
  {
    gcc_assert (TREE_CODE (decl) == VAR_DECL
		|| TREE_CODE (decl) == PARM_DECL
		|| TREE_CODE (decl) == RESULT_DECL);
    if (state_purge_per_decl **slot
	= const_cast <decl_map_t&> (m_decl_map).get (decl))
      return *slot;
    else
      return NULL;
  }

  state_purge_per_decl &get_or_create_data_for_decl (function *fun, tree decl);

  const supergraph &get_sg () const { return m_sg; }

  ssa_iterator begin_ssas () const { return m_ssa_map.begin (); }
  ssa_iterator end_ssas () const { return m_ssa_map.end (); }

  decl_iterator begin_decls () const { return m_decl_map.begin (); }
  decl_iterator end_decls () const { return m_decl_map.end (); }

private:
  DISABLE_COPY_AND_ASSIGN (state_purge_map);

  const supergraph &m_sg;
  ssa_map_t m_ssa_map;
  decl_map_t m_decl_map;
};

  /* Base class for state_purge_per_ssa_name and state_purge_per_decl.  */

class state_purge_per_tree
{
public:
  function *get_function () const { return m_fun; }
  tree get_fndecl () const { return m_fun->decl; }

protected:
  typedef hash_set<function_point> point_set_t;

  state_purge_per_tree (function *fun)
  : m_fun (fun)
  {
  }

private:
  function *m_fun;
};

/* The part of a state_purge_map relating to a specific SSA name.

   The result of analyzing a given SSA name, recording which
   function_points need to retain state information about it to handle
   their successor states, so that we can simplify program_state objects,
   in the hope of reducing state-blowup.  */

class state_purge_per_ssa_name : public state_purge_per_tree
{
public:
  state_purge_per_ssa_name (const state_purge_map &map,
			    tree name,
			    function *fun);

  bool needed_at_point_p (const function_point &point) const;

private:
  static function_point before_use_stmt (const state_purge_map &map,
					 const gimple *use_stmt);

  void add_to_worklist (const function_point &point,
			auto_vec<function_point> *worklist,
			logger *logger);

  void process_point (const function_point &point,
		      auto_vec<function_point> *worklist,
		      const state_purge_map &map);

  point_set_t m_points_needing_name;
  tree m_name;
};

/* The part of a state_purge_map relating to a specific decl.

   Analogous to state_purge_per_ssa_name, but for local decls.

   This is more involved than the SSA name case, because we also need
   to handle pointers and components.  */

class state_purge_per_decl : public state_purge_per_tree
{
public:
  state_purge_per_decl (const state_purge_map &map,
			tree decl,
			function *fun);

  bool needed_at_point_p (const function_point &point) const;

  void add_needed_at (const function_point &point);
  void add_pointed_to_at (const function_point &point);
  void process_worklists (const state_purge_map &map,
			  region_model_manager *mgr);

private:
  static function_point before_use_stmt (const state_purge_map &map,
					 const gimple *use_stmt);

  void add_to_worklist (const function_point &point,
			auto_vec<function_point> *worklist,
			point_set_t *seen,
			logger *logger);

  void process_point_backwards (const function_point &point,
				auto_vec<function_point> *worklist,
				point_set_t *seen,
				const state_purge_map &map,
				const region_model &model);
  void process_point_forwards (const function_point &point,
			       auto_vec<function_point> *worklist,
			       point_set_t *seen,
			       const state_purge_map &map);

  point_set_t m_points_needing_decl;
  point_set_t m_points_taking_address;
  tree m_decl;
};

/* Subclass of dot_annotator for use by -fdump-analyzer-state-purge.
   Annotate the .dot output with state-purge information.  */

class state_purge_annotator : public dot_annotator
{
public:
  state_purge_annotator (const state_purge_map *map) : m_map (map) {}

  bool add_node_annotations (graphviz_out *gv, const supernode &n, bool)
    const final override;

  void add_stmt_annotations (graphviz_out *gv, const gimple *stmt,
			     bool within_row)
    const final override;

private:
  void print_needed (graphviz_out *gv,
		     const function_point &point,
		     bool within_table) const;

  const state_purge_map *m_map;
};

} // namespace ana

#endif /* GCC_ANALYZER_STATE_PURGE_H */
