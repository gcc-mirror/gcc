/* Utility functions for the analyzer.
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

#ifndef GCC_ANALYZER_ANALYZER_H
#define GCC_ANALYZER_ANALYZER_H

#include "rich-location.h"
#include "function.h"
#include "json.h"
#include "tristate.h"

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
  class repeated_svalue;
  class bits_within_svalue;
  class unmergeable_svalue;
  class placeholder_svalue;
  class widening_svalue;
  class compound_svalue;
  class conjured_svalue;
  class asm_output_svalue;
  class const_fn_result_svalue;
typedef hash_set<const svalue *> svalue_set;
class region;
  class frame_region;
  class function_region;
  class label_region;
  class decl_region;
  class symbolic_region;
  class element_region;
  class offset_region;
  class sized_region;
  class cast_region;
  class field_region;
  class string_region;
  class bit_range_region;
  class var_arg_region;
class region_model_manager;
class conjured_purge;
struct model_merger;
class store_manager;
class store;
class region_model;
class region_model_context;
  class impl_region_model_context;
class call_details;
class rejected_constraint;
class constraint_manager;
class equiv_class;
class reachable_regions;
class bounded_ranges;
class bounded_ranges_manager;

struct pending_location;
class pending_diagnostic;
class pending_note;
class saved_diagnostic;
struct event_loc_info;
class checker_event;
  class state_change_event;
  class warning_event;
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
class state_purge_per_decl;
class state_change;
class rewind_info_t;

class engine;
class state_machine;
class logger;
class visitor;
class known_function_manager;
class call_summary;
class call_summary_replay;
struct per_function_data;
struct interesting_t;

class feasible_node;

class known_function;
  class builtin_known_function;
  class internal_known_function;

/* Forward decls of functions.  */

extern void dump_tree (pretty_printer *pp, tree t);
extern void dump_quoted_tree (pretty_printer *pp, tree t);
extern void print_quoted_type (pretty_printer *pp, tree t);
extern int readability_comparator (const void *p1, const void *p2);
extern int tree_cmp (const void *p1, const void *p2);
extern tree fixup_tree_for_diagnostic (tree);
extern tree get_diagnostic_tree_for_gassign (const gassign *);

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
typedef offset_int byte_offset_t;
typedef offset_int byte_size_t;

extern bool int_size_in_bits (const_tree type, bit_size_t *out);

extern tree get_field_at_bit_offset (tree record_type, bit_offset_t bit_offset);

/* The location of a region expressesd as an offset relative to a
   base region.  */

class region_offset
{
public:
  region_offset ()
  : m_base_region (NULL), m_offset (0), m_sym_offset (NULL)
  {
  }

  static region_offset make_concrete (const region *base_region,
				      bit_offset_t offset)
  {
    return region_offset (base_region, offset, NULL);
  }
  static region_offset make_symbolic (const region *base_region,
				      const svalue *sym_offset)
  {
    return region_offset (base_region, 0, sym_offset);
  }
  static region_offset make_byte_offset (const region *base_region,
					 const svalue *num_bytes_sval);

  const region *get_base_region () const { return m_base_region; }

  bool concrete_p () const { return m_sym_offset == NULL; }
  bool symbolic_p () const { return m_sym_offset != NULL; }

  bit_offset_t get_bit_offset () const
  {
    gcc_assert (!symbolic_p ());
    return m_offset;
  }

  bool get_concrete_byte_offset (byte_offset_t *out) const
  {
    gcc_assert (!symbolic_p ());
    if (m_offset % BITS_PER_UNIT == 0)
      {
	*out = m_offset / BITS_PER_UNIT;
	return true;
      }
    return false;
  }

  const svalue *get_symbolic_byte_offset () const
  {
    gcc_assert (symbolic_p ());
    return m_sym_offset;
  }

  tree calc_symbolic_bit_offset (const region_model &model) const;
  const svalue *calc_symbolic_byte_offset (region_model_manager *mgr) const;

  bool operator== (const region_offset &other) const
  {
    return (m_base_region == other.m_base_region
	    && m_offset == other.m_offset
	    && m_sym_offset == other.m_sym_offset);
  }

  void dump_to_pp (pretty_printer *pp, bool) const;
  void dump (bool) const;

private:
  region_offset (const region *base_region, bit_offset_t offset,
		 const svalue *sym_offset)
  : m_base_region (base_region), m_offset (offset), m_sym_offset (sym_offset)
  {}

  const region *m_base_region;
  bit_offset_t m_offset;
  const svalue *m_sym_offset;
};

extern bool operator< (const region_offset &, const region_offset &);
extern bool operator<= (const region_offset &, const region_offset &);
extern bool operator> (const region_offset &, const region_offset &);
extern bool operator>= (const region_offset &, const region_offset &);

extern location_t get_stmt_location (const gimple *stmt, function *fun);

extern bool compat_types_p (tree src_type, tree dst_type);

/* Abstract base class for simulating the behavior of known functions,
   supplied by the core of the analyzer, or by plugins.
   The former are typically implemented in the various kf*.cc  */

class known_function
{
public:
  virtual ~known_function () {}
  virtual bool matches_call_types_p (const call_details &cd) const = 0;
  virtual void impl_call_pre (const call_details &) const
  {
    return;
  }
  virtual void impl_call_post (const call_details &) const
  {
    return;
  }

  virtual const builtin_known_function *
  dyn_cast_builtin_kf () const { return NULL; }
};

/* Subclass of known_function for builtin functions.  */

class builtin_known_function : public known_function
{
public:
  virtual enum built_in_function builtin_code () const = 0;
  tree builtin_decl () const {
    gcc_assert (builtin_code () < END_BUILTINS);
    return builtin_info[builtin_code ()].decl;
  }

  const builtin_known_function *
  dyn_cast_builtin_kf () const final override { return this; }
};

/* Subclass of known_function for IFN_* functions.  */

class internal_known_function : public known_function
{
public:
  bool matches_call_types_p (const call_details &) const final override
  {
    /* Types are assumed to be correct.  */
    return true;
  }
};

/* Abstract subclass of known_function that merely sets the return
   value of the function (based on function attributes), and assumes
   it has no side-effects.  */

class pure_known_function_with_default_return : public known_function
{
public:
  void impl_call_pre (const call_details &cd) const override;
};

extern void register_known_functions (known_function_manager &kfm,
				      region_model_manager &rmm);
extern void register_known_analyzer_functions (known_function_manager &kfm);
extern void register_known_fd_functions (known_function_manager &kfm);
extern void register_known_file_functions (known_function_manager &kfm);
extern void register_known_functions_lang_cp (known_function_manager &kfm);
extern void register_varargs_builtins (known_function_manager &kfm);

/* Passed by pointer to PLUGIN_ANALYZER_INIT callbacks.  */

class plugin_analyzer_init_iface
{
public:
  virtual void register_state_machine (std::unique_ptr<state_machine>) = 0;
  virtual void register_known_function (const char *name,
					std::unique_ptr<known_function>) = 0;
  virtual logger *get_logger () const = 0;
};

/* An enum for describing the direction of an access to memory.  */

enum access_direction
{
  DIR_READ,
  DIR_WRITE
};

/* Abstract base class for associating custom data with an
   exploded_edge, for handling non-standard edges such as
   rewinding from a longjmp, signal handlers, etc.
   Also used when "bifurcating" state: splitting the execution
   path in non-standard ways (e.g. for simulating the various
   outcomes of "realloc").  */

class custom_edge_info
{
public:
  virtual ~custom_edge_info () {}

  /* Hook for making .dot label more readable.  */
  virtual void print (pretty_printer *pp) const = 0;

  /* Hook for updating STATE when handling bifurcation.  */
  virtual bool update_state (program_state *state,
			     const exploded_edge *eedge,
			     region_model_context *ctxt) const;

  /* Hook for updating MODEL within exploded_path::feasible_p
     and when handling bifurcation.  */
  virtual bool update_model (region_model *model,
			     const exploded_edge *eedge,
			     region_model_context *ctxt) const = 0;

  virtual void add_events_to_path (checker_path *emission_path,
				   const exploded_edge &eedge) const = 0;
};

/* Abstract base class for splitting state.

   Most of the state-management code in the analyzer involves
   modifying state objects in-place, which assumes a single outcome.

   This class provides an escape hatch to allow for multiple outcomes
   for such updates e.g. for modelling multiple outcomes from function
   calls, such as the various outcomes of "realloc".  */

class path_context
{
public:
  virtual ~path_context () {}

  /* Hook for clients to split state with a non-standard path.  */
  virtual void bifurcate (std::unique_ptr<custom_edge_info> info) = 0;

  /* Hook for clients to terminate the standard path.  */
  virtual void terminate_path () = 0;

  /* Hook for clients to determine if the standard path has been
     terminated.  */
  virtual bool terminate_path_p () const = 0;
};

extern tree get_stashed_constant_by_name (const char *name);
extern void log_stashed_constants (logger *logger);

extern FILE *get_or_create_any_logfile ();

extern json::value *
tree_to_json (tree node);

extern json::value *
diagnostic_event_id_to_json (const diagnostic_event_id_t &);

extern json::value *
bit_offset_to_json (const bit_offset_t &offset);

extern json::value *
byte_offset_to_json (const byte_offset_t &offset);

} // namespace ana

extern bool is_special_named_call_p (const gcall *call, const char *funcname,
				     unsigned int num_args);
extern bool is_named_call_p (const_tree fndecl, const char *funcname);
extern bool is_named_call_p (const_tree fndecl, const char *funcname,
			     const gcall *call, unsigned int num_args);
extern bool is_std_named_call_p (const_tree fndecl, const char *funcname);
extern bool is_std_named_call_p (const_tree fndecl, const char *funcname,
				 const gcall *call, unsigned int num_args);
extern bool is_setjmp_call_p (const gcall *call);
extern bool is_longjmp_call_p (const gcall *call);
extern bool is_placement_new_p (const gcall *call);

extern const char *get_user_facing_name (const gcall *call);

extern void register_analyzer_pass ();

extern label_text make_label_text (bool can_colorize, const char *fmt, ...);
extern label_text make_label_text_n (bool can_colorize,
				     unsigned HOST_WIDE_INT n,
				     const char *singular_fmt,
				     const char *plural_fmt, ...);

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
