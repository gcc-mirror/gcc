/* Utility functions for the analyzer.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_ANALYZER_H
#define GCC_ANALYZER_ANALYZER_H

class graphviz_out;

namespace ana {

/* Forward decls of common types, with indentation to show inheritance.  */

class supergraph;
class supernode;
class superedge;
  class cfg_superedge;
    class switch_cfg_superedge;
  class callgraph_superedge;
    class call_superedge;
    class return_superedge;

class svalue;
  class region_svalue;
  class constant_svalue;
  class unknown_svalue;
  class poisoned_svalue;
  class setjmp_svalue;
  class initial_svalue;
  class unaryop_svalue;
  class binop_svalue;
  class sub_svalue;
  class unmergeable_svalue;
  class placeholder_svalue;
  class widening_svalue;
  class compound_svalue;
  class conjured_svalue;
typedef hash_set<const svalue *> svalue_set;
class region;
  class frame_region;
  class function_region;
  class label_region;
  class decl_region;
  class symbolic_region;
  class element_region;
  class offset_region;
  class cast_region;
  class field_region;
  class string_region;
class region_model_manager;
struct model_merger;
class store_manager;
class store;
class region_model;
class region_model_context;
  class impl_region_model_context;
class call_details;
struct rejected_constraint;
class constraint_manager;
class equiv_class;

class pending_diagnostic;
class state_change_event;
class checker_path;
class extrinsic_state;
class sm_state_map;
class stmt_finder;
class program_point;
class function_point;
class program_state;
class exploded_graph;
class exploded_node;
class exploded_edge;
class feasibility_problem;
class exploded_cluster;
class exploded_path;
class analysis_plan;
class state_purge_map;
class state_purge_per_ssa_name;
class state_change;
class rewind_info_t;

class engine;
class state_machine;
class logger;
class visitor;

/* Forward decls of functions.  */

extern void dump_tree (pretty_printer *pp, tree t);
extern void dump_quoted_tree (pretty_printer *pp, tree t);
extern void print_quoted_type (pretty_printer *pp, tree t);
extern int readability_comparator (const void *p1, const void *p2);
extern int tree_cmp (const void *p1, const void *p2);
extern tree fixup_tree_for_diagnostic (tree);

/* A tree, extended with stack frame information for locals, so that
   we can distinguish between different values of locals within a potentially
   recursive callstack.  */

class path_var
{
public:
  path_var (tree t, int stack_depth)
  : m_tree (t), m_stack_depth (stack_depth)
  {
    // TODO: ignore stack depth for globals and constants
  }

  bool operator== (const path_var &other) const
  {
    return (m_tree == other.m_tree
	    && m_stack_depth == other.m_stack_depth);
  }

  operator bool () const
  {
    return m_tree != NULL_TREE;
  }

  void dump (pretty_printer *pp) const;

  tree m_tree;
  int m_stack_depth; // or -1 for globals?
};

typedef offset_int bit_offset_t;
typedef offset_int bit_size_t;
typedef offset_int byte_size_t;

/* The location of a region expressesd as an offset relative to a
   base region.  */

class region_offset
{
public:
  static region_offset make_concrete (const region *base_region,
				      bit_offset_t offset)
  {
    return region_offset (base_region, offset, false);
  }
  static region_offset make_symbolic (const region *base_region)
  {
    return region_offset (base_region, 0, true);
  }

  const region *get_base_region () const { return m_base_region; }

  bool symbolic_p () const { return m_is_symbolic; }

  bit_offset_t get_bit_offset () const
  {
    gcc_assert (!symbolic_p ());
    return m_offset;
  }

  bool operator== (const region_offset &other) const
  {
    return (m_base_region == other.m_base_region
	    && m_offset == other.m_offset
	    && m_is_symbolic == other.m_is_symbolic);
  }

private:
  region_offset (const region *base_region, bit_offset_t offset,
		 bool is_symbolic)
  : m_base_region (base_region), m_offset (offset), m_is_symbolic (is_symbolic)
  {}

  const region *m_base_region;
  bit_offset_t m_offset;
  bool m_is_symbolic;
};

extern location_t get_stmt_location (const gimple *stmt, function *fun);

/* Passed by pointer to PLUGIN_ANALYZER_INIT callbacks.  */

class plugin_analyzer_init_iface
{
public:
  virtual void register_state_machine (state_machine *) = 0;
  virtual logger *get_logger () const = 0;
};

} // namespace ana

extern bool is_special_named_call_p (const gcall *call, const char *funcname,
				     unsigned int num_args);
extern bool is_named_call_p (tree fndecl, const char *funcname);
extern bool is_named_call_p (tree fndecl, const char *funcname,
			     const gcall *call, unsigned int num_args);
extern bool is_std_named_call_p (tree fndecl, const char *funcname);
extern bool is_std_named_call_p (tree fndecl, const char *funcname,
				 const gcall *call, unsigned int num_args);
extern bool is_setjmp_call_p (const gcall *call);
extern bool is_longjmp_call_p (const gcall *call);

extern const char *get_user_facing_name (const gcall *call);

extern void register_analyzer_pass ();

extern label_text make_label_text (bool can_colorize, const char *fmt, ...);

extern bool fndecl_has_gimple_body_p (tree fndecl);

/* An RAII-style class for pushing/popping cfun within a scope.
   Doing so ensures we get "In function " announcements
   from the diagnostics subsystem.  */

class auto_cfun
{
public:
  auto_cfun (function *fun) { push_cfun (fun); }
  ~auto_cfun () { pop_cfun (); }
};

/* A template for creating hash traits for a POD type.  */

template <typename Type>
struct pod_hash_traits : typed_noop_remove<Type>
{
  typedef Type value_type;
  typedef Type compare_type;
  static inline hashval_t hash (value_type);
  static inline bool equal (const value_type &existing,
			    const value_type &candidate);
  static inline void mark_deleted (Type &);
  static inline void mark_empty (Type &);
  static inline bool is_deleted (Type);
  static inline bool is_empty (Type);
};

/* A hash traits class that uses member functions to implement
   the various required ops.  */

template <typename Type>
struct member_function_hash_traits : public typed_noop_remove<Type>
{
  typedef Type value_type;
  typedef Type compare_type;
  static inline hashval_t hash (value_type v) { return v.hash (); }
  static inline bool equal (const value_type &existing,
			    const value_type &candidate)
  {
    return existing == candidate;
  }
  static inline void mark_deleted (Type &t) { t.mark_deleted (); }
  static inline void mark_empty (Type &t) { t.mark_empty (); }
  static inline bool is_deleted (Type t) { return t.is_deleted (); }
  static inline bool is_empty (Type t) { return t.is_empty (); }
};

/* A map from T::key_t to T* for use in consolidating instances of T.
   Owns all instances of T.
   T::key_t should have operator== and be hashable.  */

template <typename T>
class consolidation_map
{
public:
  typedef typename T::key_t key_t;
  typedef T instance_t;
  typedef hash_map<key_t, instance_t *> inner_map_t;
  typedef typename inner_map_t::iterator iterator;

  /* Delete all instances of T.  */

  ~consolidation_map ()
  {
    for (typename inner_map_t::iterator iter = m_inner_map.begin ();
	 iter != m_inner_map.end (); ++iter)
      delete (*iter).second;
  }

  /* Get the instance of T for K if one exists, or NULL.  */

  T *get (const key_t &k) const
  {
    if (instance_t **slot = const_cast<inner_map_t &> (m_inner_map).get (k))
      return *slot;
    return NULL;
  }

  /* Take ownership of INSTANCE.  */

  void put (const key_t &k, T *instance)
  {
    m_inner_map.put (k, instance);
  }

  size_t elements () const { return m_inner_map.elements (); }

  iterator begin () const { return m_inner_map.begin (); }
  iterator end () const { return m_inner_map.end (); }

private:
  inner_map_t m_inner_map;
};

/* Disable -Wformat-diag; we want to be able to use pp_printf
   for logging/dumping without complying with the rules for diagnostics.  */
#if __GNUC__ >= 10
#pragma GCC diagnostic ignored "-Wformat-diag"
#endif

#if !ENABLE_ANALYZER
extern void sorry_no_analyzer ();
#endif /* #if !ENABLE_ANALYZER */

#endif /* GCC_ANALYZER_ANALYZER_H */
