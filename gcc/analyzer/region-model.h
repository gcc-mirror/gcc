/* Classes for modeling the state of memory.
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

#ifndef GCC_ANALYZER_REGION_MODEL_H
#define GCC_ANALYZER_REGION_MODEL_H

/* Implementation of the region-based ternary model described in:
     "A Memory Model for Static Analysis of C Programs"
      (Zhongxing Xu, Ted Kremenek, and Jian Zhang)
     http://lcs.ios.ac.cn/~xuzb/canalyze/memmodel.pdf  */

#include "analyzer/svalue.h"
#include "analyzer/region.h"

using namespace ana;

namespace inchash
{
  extern void add_path_var (path_var pv, hash &hstate);
} // namespace inchash

namespace ana {

template <typename T>
class one_way_id_map
{
 public:
  one_way_id_map (int num_ids);
  void put (T src, T dst);
  T get_dst_for_src (T src) const;
  void dump_to_pp (pretty_printer *pp) const;
  void dump () const;
  void update (T *) const;

 private:
  auto_vec<T> m_src_to_dst;
 };

/* class one_way_id_map.  */

/* one_way_id_map's ctor, which populates the map with dummy null values.  */

template <typename T>
inline one_way_id_map<T>::one_way_id_map (int num_svalues)
: m_src_to_dst (num_svalues)
{
  for (int i = 0; i < num_svalues; i++)
    m_src_to_dst.quick_push (T::null ());
}

/* Record that SRC is to be mapped to DST.  */

template <typename T>
inline void
one_way_id_map<T>::put (T src, T dst)
{
  m_src_to_dst[src.as_int ()] = dst;
}

/* Get the new value for SRC within the map.  */

template <typename T>
inline T
one_way_id_map<T>::get_dst_for_src (T src) const
{
  if (src.null_p ())
    return src;
  return m_src_to_dst[src.as_int ()];
}

/* Dump this map to PP.  */

template <typename T>
inline void
one_way_id_map<T>::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, "src to dst: {");
  unsigned i;
  T *dst;
  FOR_EACH_VEC_ELT (m_src_to_dst, i, dst)
    {
      if (i > 0)
	pp_string (pp, ", ");
      T src (T::from_int (i));
      src.print (pp);
      pp_string (pp, " -> ");
      dst->print (pp);
    }
  pp_string (pp, "}");
  pp_newline (pp);
}

/* Dump this map to stderr.  */

template <typename T>
DEBUG_FUNCTION inline void
one_way_id_map<T>::dump () const
{
  pretty_printer pp;
  pp.buffer->stream = stderr;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Update *ID from the old value to its new value in this map.  */

template <typename T>
inline void
one_way_id_map<T>::update (T *id) const
{
  *id = get_dst_for_src (*id);
}

/* Various operations delete information from a region_model.

   This struct tracks how many of each kind of entity were purged (e.g.
   for selftests, and for debugging).  */

struct purge_stats
{
  purge_stats ()
  : m_num_svalues (0),
    m_num_regions (0),
    m_num_equiv_classes (0),
    m_num_constraints (0),
    m_num_client_items (0)
  {}

  int m_num_svalues;
  int m_num_regions;
  int m_num_equiv_classes;
  int m_num_constraints;
  int m_num_client_items;
};

/* A base class for visiting regions and svalues, with do-nothing
   base implementations of the per-subclass vfuncs.  */

class visitor
{
public:
  virtual void visit_region_svalue (const region_svalue *) {}
  virtual void visit_constant_svalue (const constant_svalue *) {}
  virtual void visit_unknown_svalue (const unknown_svalue *) {}
  virtual void visit_poisoned_svalue (const poisoned_svalue *) {}
  virtual void visit_setjmp_svalue (const setjmp_svalue *) {}
  virtual void visit_initial_svalue (const initial_svalue *) {}
  virtual void visit_unaryop_svalue (const unaryop_svalue *) {}
  virtual void visit_binop_svalue (const binop_svalue *) {}
  virtual void visit_sub_svalue (const sub_svalue *) {}
  virtual void visit_unmergeable_svalue (const unmergeable_svalue *) {}
  virtual void visit_placeholder_svalue (const placeholder_svalue *) {}
  virtual void visit_widening_svalue (const widening_svalue *) {}
  virtual void visit_compound_svalue (const compound_svalue *) {}
  virtual void visit_conjured_svalue (const conjured_svalue *) {}

  virtual void visit_region (const region *) {}
};

} // namespace ana

namespace ana {

/* A class responsible for owning and consolidating region and svalue
   instances.
   region and svalue instances are immutable as far as clients are
   concerned, so they are provided as "const" ptrs.  */

class region_model_manager
{
public:
  region_model_manager ();
  ~region_model_manager ();

  /* svalue consolidation.  */
  const svalue *get_or_create_constant_svalue (tree cst_expr);
  const svalue *get_or_create_unknown_svalue (tree type);
  const svalue *get_or_create_setjmp_svalue (const setjmp_record &r,
					     tree type);
  const svalue *get_or_create_poisoned_svalue (enum poison_kind kind,
					       tree type);
  const svalue *get_or_create_initial_value (const region *reg);
  const svalue *get_ptr_svalue (tree ptr_type, const region *pointee);
  const svalue *get_or_create_unaryop (tree type, enum tree_code op,
				       const svalue *arg);
  const svalue *get_or_create_cast (tree type, const svalue *arg);
  const svalue *get_or_create_binop (tree type,
				     enum tree_code op,
				     const svalue *arg0, const svalue *arg1);
  const svalue *get_or_create_sub_svalue (tree type,
					  const svalue *parent_svalue,
					  const region *subregion);
  const svalue *get_or_create_unmergeable (const svalue *arg);
  const svalue *get_or_create_widening_svalue (tree type,
					       const program_point &point,
					       const svalue *base_svalue,
					       const svalue *iter_svalue);
  const svalue *get_or_create_compound_svalue (tree type,
					       const binding_map &map);
  const svalue *get_or_create_conjured_svalue (tree type, const gimple *stmt,
					       const region *id_reg);

  const svalue *maybe_get_char_from_string_cst (tree string_cst,
						tree byte_offset_cst);

  /* region consolidation.  */
  const stack_region * get_stack_region () const { return &m_stack_region; }
  const heap_region *get_heap_region () const { return &m_heap_region; }
  const code_region *get_code_region () const { return &m_code_region; }
  const globals_region *get_globals_region () const
  {
    return &m_globals_region;
  }
  const function_region *get_region_for_fndecl (tree fndecl);
  const label_region *get_region_for_label (tree label);
  const decl_region *get_region_for_global (tree expr);
  const region *get_field_region (const region *parent, tree field);
  const region *get_element_region (const region *parent,
				    tree element_type,
				    const svalue *index);
  const region *get_offset_region (const region *parent,
				   tree type,
				   const svalue *byte_offset);
  const region *get_cast_region (const region *original_region,
				 tree type);
  const frame_region *get_frame_region (const frame_region *calling_frame,
					function *fun);
  const region *get_symbolic_region (const svalue *sval);
  const string_region *get_region_for_string (tree string_cst);

  const region *
  get_region_for_unexpected_tree_code (region_model_context *ctxt,
				       tree t,
				       const dump_location_t &loc);

  unsigned alloc_region_id () { return m_next_region_id++; }

  store_manager *get_store_manager () { return &m_store_mgr; }

  /* Dynamically-allocated region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  const region *create_region_for_heap_alloc ();
  const region *create_region_for_alloca (const frame_region *frame);

  void log_stats (logger *logger, bool show_objs) const;

private:
  bool too_complex_p (const complexity &c) const;
  bool reject_if_too_complex (svalue *sval);

  const svalue *maybe_fold_unaryop (tree type, enum tree_code op,
				    const svalue *arg);
  const svalue *maybe_fold_binop (tree type, enum tree_code op,
				  const svalue *arg0, const svalue *arg1);
  const svalue *maybe_fold_sub_svalue (tree type,
				       const svalue *parent_svalue,
				       const region *subregion);

  unsigned m_next_region_id;
  root_region m_root_region;
  stack_region m_stack_region;
  heap_region m_heap_region;

  /* svalue consolidation.  */
  typedef hash_map<tree, constant_svalue *> constants_map_t;
  constants_map_t m_constants_map;

  typedef hash_map<tree, unknown_svalue *> unknowns_map_t;
  unknowns_map_t m_unknowns_map;
  const unknown_svalue *m_unknown_NULL;

  typedef hash_map<poisoned_svalue::key_t,
		   poisoned_svalue *> poisoned_values_map_t;
  poisoned_values_map_t m_poisoned_values_map;

  typedef hash_map<setjmp_svalue::key_t,
		   setjmp_svalue *> setjmp_values_map_t;
  setjmp_values_map_t m_setjmp_values_map;

  typedef hash_map<const region *, initial_svalue *> initial_values_map_t;
  initial_values_map_t m_initial_values_map;

  typedef hash_map<region_svalue::key_t, region_svalue *> pointer_values_map_t;
  pointer_values_map_t m_pointer_values_map;

  typedef hash_map<unaryop_svalue::key_t,
		   unaryop_svalue *> unaryop_values_map_t;
  unaryop_values_map_t m_unaryop_values_map;

  typedef hash_map<binop_svalue::key_t, binop_svalue *> binop_values_map_t;
  binop_values_map_t m_binop_values_map;

  typedef hash_map<sub_svalue::key_t, sub_svalue *> sub_values_map_t;
  sub_values_map_t m_sub_values_map;

  typedef hash_map<const svalue *,
		   unmergeable_svalue *> unmergeable_values_map_t;
  unmergeable_values_map_t m_unmergeable_values_map;

  typedef hash_map<widening_svalue::key_t,
		   widening_svalue */*,
		   widening_svalue::key_t::hash_map_traits*/>
    widening_values_map_t;
  widening_values_map_t m_widening_values_map;

  typedef hash_map<compound_svalue::key_t,
		   compound_svalue *> compound_values_map_t;
  compound_values_map_t m_compound_values_map;

  typedef hash_map<conjured_svalue::key_t,
		   conjured_svalue *> conjured_values_map_t;
  conjured_values_map_t m_conjured_values_map;

  /* Maximum complexity of svalues that weren't rejected.  */
  complexity m_max_complexity;

  /* region consolidation.  */

  code_region m_code_region;
  typedef hash_map<tree, function_region *> fndecls_map_t;
  typedef fndecls_map_t::iterator fndecls_iterator_t;
  fndecls_map_t m_fndecls_map;

  typedef hash_map<tree, label_region *> labels_map_t;
  typedef labels_map_t::iterator labels_iterator_t;
  labels_map_t m_labels_map;

  globals_region m_globals_region;
  typedef hash_map<tree, decl_region *> globals_map_t;
  typedef globals_map_t::iterator globals_iterator_t;
  globals_map_t m_globals_map;

  consolidation_map<field_region> m_field_regions;
  consolidation_map<element_region> m_element_regions;
  consolidation_map<offset_region> m_offset_regions;
  consolidation_map<cast_region> m_cast_regions;
  consolidation_map<frame_region> m_frame_regions;
  consolidation_map<symbolic_region> m_symbolic_regions;

  typedef hash_map<tree, string_region *> string_map_t;
  string_map_t m_string_map;

  store_manager m_store_mgr;

  /* "Dynamically-allocated" region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  auto_delete_vec<region> m_managed_dynamic_regions;
};

struct append_ssa_names_cb_data;

/* Helper class for handling calls to functions with known behavior.
   Implemented in region-model-impl-calls.c.  */

class call_details
{
public:
  call_details (const gcall *call, region_model *model,
		region_model_context *ctxt);

  region_model_context *get_ctxt () const { return m_ctxt; }
  uncertainty_t *get_uncertainty () const;
  tree get_lhs_type () const { return m_lhs_type; }
  const region *get_lhs_region () const { return m_lhs_region; }

  bool maybe_set_lhs (const svalue *result) const;

  unsigned num_args () const;

  tree get_arg_tree (unsigned idx) const;
  tree get_arg_type (unsigned idx) const;
  const svalue *get_arg_svalue (unsigned idx) const;

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (bool simple) const;

private:
  const gcall *m_call;
  region_model *m_model;
  region_model_context *m_ctxt;
  tree m_lhs_type;
  const region *m_lhs_region;
};

/* A region_model encapsulates a representation of the state of memory, with
   a tree of regions, along with their associated values.
   The representation is graph-like because values can be pointers to
   regions.
   It also stores a constraint_manager, capturing relationships between
   the values.  */

class region_model
{
 public:
  region_model (region_model_manager *mgr);
  region_model (const region_model &other);
  ~region_model ();
  region_model &operator= (const region_model &other);

  bool operator== (const region_model &other) const;
  bool operator!= (const region_model &other) const
  {
    return !(*this == other);
  }

  hashval_t hash () const;

  void print (pretty_printer *pp) const;

  void dump_to_pp (pretty_printer *pp, bool simple, bool multiline) const;
  void dump (FILE *fp, bool simple, bool multiline) const;
  void dump (bool simple) const;

  void debug () const;

  void validate () const;

  void canonicalize ();
  bool canonicalized_p () const;

  void on_assignment (const gassign *stmt, region_model_context *ctxt);
  const svalue *get_gassign_result (const gassign *assign,
				    region_model_context *ctxt);
  bool on_call_pre (const gcall *stmt, region_model_context *ctxt,
		    bool *out_terminate_path);
  void on_call_post (const gcall *stmt,
		     bool unknown_side_effects,
		     region_model_context *ctxt);

  /* Specific handling for on_call_pre.  */
  bool impl_call_alloca (const call_details &cd);
  void impl_call_analyzer_describe (const gcall *call,
				    region_model_context *ctxt);
  void impl_call_analyzer_eval (const gcall *call,
				region_model_context *ctxt);
  bool impl_call_builtin_expect (const call_details &cd);
  bool impl_call_calloc (const call_details &cd);
  bool impl_call_error (const call_details &cd, unsigned min_args,
			bool *out_terminate_path);
  void impl_call_free (const call_details &cd);
  bool impl_call_malloc (const call_details &cd);
  void impl_call_memcpy (const call_details &cd);
  bool impl_call_memset (const call_details &cd);
  void impl_call_realloc (const call_details &cd);
  void impl_call_strcpy (const call_details &cd);
  bool impl_call_strlen (const call_details &cd);
  bool impl_call_operator_new (const call_details &cd);
  bool impl_call_operator_delete (const call_details &cd);
  void impl_deallocation_call (const call_details &cd);

  void handle_unrecognized_call (const gcall *call,
				 region_model_context *ctxt);
  void get_reachable_svalues (svalue_set *out,
			      const svalue *extra_sval,
			      const uncertainty_t *uncertainty);

  void on_return (const greturn *stmt, region_model_context *ctxt);
  void on_setjmp (const gcall *stmt, const exploded_node *enode,
		  region_model_context *ctxt);
  void on_longjmp (const gcall *longjmp_call, const gcall *setjmp_call,
		   int setjmp_stack_depth, region_model_context *ctxt);

  void update_for_phis (const supernode *snode,
			const cfg_superedge *last_cfg_superedge,
			region_model_context *ctxt);

  void handle_phi (const gphi *phi, tree lhs, tree rhs,
		   region_model_context *ctxt);

  bool maybe_update_for_edge (const superedge &edge,
			      const gimple *last_stmt,
			      region_model_context *ctxt,
			      rejected_constraint **out);

  const region *push_frame (function *fun, const vec<const svalue *> *arg_sids,
			    region_model_context *ctxt);
  const frame_region *get_current_frame () const { return m_current_frame; }
  function * get_current_function () const;
  void pop_frame (const region *result_dst,
		  const svalue **out_result,
		  region_model_context *ctxt);
  int get_stack_depth () const;
  const frame_region *get_frame_at_index (int index) const;

  const region *get_lvalue (path_var pv, region_model_context *ctxt);
  const region *get_lvalue (tree expr, region_model_context *ctxt);
  const svalue *get_rvalue (path_var pv, region_model_context *ctxt);
  const svalue *get_rvalue (tree expr, region_model_context *ctxt);

  const region *deref_rvalue (const svalue *ptr_sval, tree ptr_tree,
			       region_model_context *ctxt);

  void set_value (const region *lhs_reg, const svalue *rhs_sval,
		  region_model_context *ctxt);
  void set_value (tree lhs, tree rhs, region_model_context *ctxt);
  void clobber_region (const region *reg);
  void purge_region (const region *reg);
  void zero_fill_region (const region *reg);
  void mark_region_as_unknown (const region *reg, uncertainty_t *uncertainty);

  void copy_region (const region *dst_reg, const region *src_reg,
		    region_model_context *ctxt);
  tristate eval_condition (const svalue *lhs,
			   enum tree_code op,
			   const svalue *rhs) const;
  tristate eval_condition_without_cm (const svalue *lhs,
				      enum tree_code op,
				      const svalue *rhs) const;
  tristate compare_initial_and_pointer (const initial_svalue *init,
					const region_svalue *ptr) const;
  tristate eval_condition (tree lhs,
			   enum tree_code op,
			   tree rhs,
			   region_model_context *ctxt);
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt);
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt,
		       rejected_constraint **out);

  const region *create_region_for_heap_alloc (const svalue *size_in_bytes);
  const region *create_region_for_alloca (const svalue *size_in_bytes);

  tree get_representative_tree (const svalue *sval) const;
  path_var
  get_representative_path_var (const svalue *sval,
			       svalue_set *visited) const;
  path_var
  get_representative_path_var (const region *reg,
			       svalue_set *visited) const;

  /* For selftests.  */
  constraint_manager *get_constraints ()
  {
    return m_constraints;
  }

  store *get_store () { return &m_store; }
  const store *get_store () const { return &m_store; }

  region_model_manager *get_manager () const { return m_mgr; }

  void unbind_region_and_descendents (const region *reg,
				      enum poison_kind pkind);

  bool can_merge_with_p (const region_model &other_model,
			 const program_point &point,
			 region_model *out_model) const;

  tree get_fndecl_for_call (const gcall *call,
			    region_model_context *ctxt);

  void get_ssa_name_regions_for_current_frame
    (auto_vec<const decl_region *> *out) const;
  static void append_ssa_names_cb (const region *base_reg,
				   struct append_ssa_names_cb_data *data);

  const svalue *get_store_value (const region *reg) const;

  bool region_exists_p (const region *reg) const;

  void loop_replay_fixup (const region_model *dst_state);

 private:
  const region *get_lvalue_1 (path_var pv, region_model_context *ctxt);
  const svalue *get_rvalue_1 (path_var pv, region_model_context *ctxt);

  path_var
  get_representative_path_var_1 (const svalue *sval,
				 svalue_set *visited) const;
  path_var
  get_representative_path_var_1 (const region *reg,
				 svalue_set *visited) const;

  void add_any_constraints_from_ssa_def_stmt (tree lhs,
					      enum tree_code op,
					      tree rhs,
					      region_model_context *ctxt);
  void add_any_constraints_from_gassign (enum tree_code op,
					 tree rhs,
					 const gassign *assign,
					 region_model_context *ctxt);
  void add_any_constraints_from_gcall (enum tree_code op,
				       tree rhs,
				       const gcall *call,
				       region_model_context *ctxt);

  void update_for_call_superedge (const call_superedge &call_edge,
				  region_model_context *ctxt);
  void update_for_return_superedge (const return_superedge &return_edge,
				    region_model_context *ctxt);
  void update_for_call_summary (const callgraph_superedge &cg_sedge,
				region_model_context *ctxt);
  bool apply_constraints_for_gcond (const cfg_superedge &edge,
				    const gcond *cond_stmt,
				    region_model_context *ctxt,
				    rejected_constraint **out);
  bool apply_constraints_for_gswitch (const switch_cfg_superedge &edge,
				      const gswitch *switch_stmt,
				      region_model_context *ctxt,
				      rejected_constraint **out);
  bool apply_constraints_for_exception (const gimple *last_stmt,
					region_model_context *ctxt,
					rejected_constraint **out);

  int poison_any_pointers_to_descendents (const region *reg,
					  enum poison_kind pkind);

  void on_top_level_param (tree param, region_model_context *ctxt);

  void record_dynamic_extents (const region *reg,
			       const svalue *size_in_bytes);

  bool called_from_main_p () const;
  const svalue *get_initial_value_for_global (const region *reg) const;

  void check_for_writable_region (const region* dest_reg,
				  region_model_context *ctxt) const;

  /* Storing this here to avoid passing it around everywhere.  */
  region_model_manager *const m_mgr;

  store m_store;

  constraint_manager *m_constraints; // TODO: embed, rather than dynalloc?

  const frame_region *m_current_frame;
};

/* Some region_model activity could lead to warnings (e.g. attempts to use an
   uninitialized value).  This abstract base class encapsulates an interface
   for the region model to use when emitting such warnings.

   Having this as an abstract base class allows us to support the various
   operations needed by program_state in the analyzer within region_model,
   whilst keeping them somewhat modularized.  */

class region_model_context
{
 public:
  virtual void warn (pending_diagnostic *d) = 0;

  /* Hook for clients to be notified when an SVAL that was reachable
     in a previous state is no longer live, so that clients can emit warnings
     about leaks.  */
  virtual void on_svalue_leak (const svalue *sval) = 0;

  /* Hook for clients to be notified when the set of explicitly live
     svalues changes, so that they can purge state relating to dead
     svalues.  */
  virtual void on_liveness_change (const svalue_set &live_svalues,
				   const region_model *model) = 0;

  virtual logger *get_logger () = 0;

  /* Hook for clients to be notified when the condition
     "LHS OP RHS" is added to the region model.
     This exists so that state machines can detect tests on edges,
     and use them to trigger sm-state transitions (e.g. transitions due
     to ptrs becoming known to be NULL or non-NULL, rather than just
     "unchecked") */
  virtual void on_condition (tree lhs, enum tree_code op, tree rhs) = 0;

  /* Hooks for clients to be notified when an unknown change happens
     to SVAL (in response to a call to an unknown function).  */
  virtual void on_unknown_change (const svalue *sval, bool is_mutable) = 0;

  /* Hooks for clients to be notified when a phi node is handled,
     where RHS is the pertinent argument.  */
  virtual void on_phi (const gphi *phi, tree rhs) = 0;

  /* Hooks for clients to be notified when the region model doesn't
     know how to handle the tree code of T at LOC.  */
  virtual void on_unexpected_tree_code (tree t,
					const dump_location_t &loc) = 0;

  /* Hook for clients to be notified when a function_decl escapes.  */
  virtual void on_escaped_function (tree fndecl) = 0;

  virtual uncertainty_t *get_uncertainty () = 0;
};

/* A "do nothing" subclass of region_model_context.  */

class noop_region_model_context : public region_model_context
{
public:
  void warn (pending_diagnostic *) OVERRIDE {}
  void on_svalue_leak (const svalue *) OVERRIDE {}
  void on_liveness_change (const svalue_set &,
			   const region_model *) OVERRIDE {}
  logger *get_logger () OVERRIDE { return NULL; }
  void on_condition (tree lhs ATTRIBUTE_UNUSED,
		     enum tree_code op ATTRIBUTE_UNUSED,
		     tree rhs ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_unknown_change (const svalue *sval ATTRIBUTE_UNUSED,
			  bool is_mutable ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_phi (const gphi *phi ATTRIBUTE_UNUSED,
	       tree rhs ATTRIBUTE_UNUSED) OVERRIDE
  {
  }
  void on_unexpected_tree_code (tree, const dump_location_t &) OVERRIDE {}

  void on_escaped_function (tree) OVERRIDE {}

  uncertainty_t *get_uncertainty () OVERRIDE { return NULL; }
};

/* A subclass of region_model_context for determining if operations fail
   e.g. "can we generate a region for the lvalue of EXPR?".  */

class tentative_region_model_context : public noop_region_model_context
{
public:
  tentative_region_model_context () : m_num_unexpected_codes (0) {}

  void on_unexpected_tree_code (tree, const dump_location_t &)
    FINAL OVERRIDE
  {
    m_num_unexpected_codes++;
  }

  bool had_errors_p () const { return m_num_unexpected_codes > 0; }

private:
  int m_num_unexpected_codes;
};

/* A bundle of data for use when attempting to merge two region_model
   instances to make a third.  */

struct model_merger
{
  model_merger (const region_model *model_a,
		const region_model *model_b,
		const program_point &point,
		region_model *merged_model)
  : m_model_a (model_a), m_model_b (model_b),
    m_point (point),
    m_merged_model (merged_model)
  {
  }

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (FILE *fp, bool simple) const;
  void dump (bool simple) const;

  region_model_manager *get_manager () const
  {
    return m_model_a->get_manager ();
  }

  const region_model *m_model_a;
  const region_model *m_model_b;
  const program_point &m_point;
  region_model *m_merged_model;
};

/* A record that can (optionally) be written out when
   region_model::add_constraint fails.  */

struct rejected_constraint
{
  rejected_constraint (const region_model &model,
		     tree lhs, enum tree_code op, tree rhs)
  : m_model (model), m_lhs (lhs), m_op (op), m_rhs (rhs)
  {}

  void dump_to_pp (pretty_printer *pp) const;

  region_model m_model;
  tree m_lhs;
  enum tree_code m_op;
  tree m_rhs;
};

/* A bundle of state.  */

class engine
{
public:
  region_model_manager *get_model_manager () { return &m_mgr; }

  void log_stats (logger *logger) const;

private:
  region_model_manager m_mgr;

};

} // namespace ana

extern void debug (const region_model &rmodel);

namespace ana {

#if CHECKING_P

namespace selftest {

using namespace ::selftest;

/* An implementation of region_model_context for use in selftests, which
   stores any pending_diagnostic instances passed to it.  */

class test_region_model_context : public noop_region_model_context
{
public:
  void warn (pending_diagnostic *d) FINAL OVERRIDE
  {
    m_diagnostics.safe_push (d);
  }

  unsigned get_num_diagnostics () const { return m_diagnostics.length (); }

  void on_unexpected_tree_code (tree t, const dump_location_t &)
    FINAL OVERRIDE
  {
    internal_error ("unhandled tree code: %qs",
		    get_tree_code_name (TREE_CODE (t)));
  }

private:
  /* Implicitly delete any diagnostics in the dtor.  */
  auto_delete_vec<pending_diagnostic> m_diagnostics;
};

/* Attempt to add the constraint (LHS OP RHS) to MODEL.
   Verify that MODEL remains satisfiable.  */

#define ADD_SAT_CONSTRAINT(MODEL, LHS, OP, RHS)	\
  SELFTEST_BEGIN_STMT					\
    bool sat = (MODEL).add_constraint (LHS, OP, RHS, NULL);	\
    ASSERT_TRUE (sat);					\
  SELFTEST_END_STMT

/* Attempt to add the constraint (LHS OP RHS) to MODEL.
   Verify that the result is not satisfiable.  */

#define ADD_UNSAT_CONSTRAINT(MODEL, LHS, OP, RHS)	\
  SELFTEST_BEGIN_STMT					\
    bool sat = (MODEL).add_constraint (LHS, OP, RHS, NULL);	\
    ASSERT_FALSE (sat);				\
  SELFTEST_END_STMT

/* Implementation detail of the ASSERT_CONDITION_* macros.  */

void assert_condition (const location &loc,
		       region_model &model,
		       const svalue *lhs, tree_code op, const svalue *rhs,
		       tristate expected);

void assert_condition (const location &loc,
		       region_model &model,
		       tree lhs, tree_code op, tree rhs,
		       tristate expected);

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "true".  */

#define ASSERT_CONDITION_TRUE(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_TRUE));		\
  SELFTEST_END_STMT

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "false".  */

#define ASSERT_CONDITION_FALSE(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_FALSE));		\
  SELFTEST_END_STMT

/* Assert that REGION_MODEL evaluates the condition "LHS OP RHS"
   as "unknown".  */

#define ASSERT_CONDITION_UNKNOWN(REGION_MODEL, LHS, OP, RHS) \
  SELFTEST_BEGIN_STMT							\
  assert_condition (SELFTEST_LOCATION, REGION_MODEL, LHS, OP, RHS,	\
		    tristate (tristate::TS_UNKNOWN));		\
  SELFTEST_END_STMT

} /* end of namespace selftest.  */

#endif /* #if CHECKING_P */

} // namespace ana

#endif /* GCC_ANALYZER_REGION_MODEL_H */
