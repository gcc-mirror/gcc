/* Classes for modeling the state of memory.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#include "bitmap.h"
#include "stringpool.h"
#include "attribs.h" // for rdwr_map
#include "selftest.h"
#include "analyzer/svalue.h"
#include "analyzer/region.h"
#include "analyzer/known-function-manager.h"
#include "analyzer/region-model-manager.h"
#include "analyzer/pending-diagnostic.h"
#include "text-art/widget.h"
#include "text-art/dump.h"

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
  pp.set_output_stream (stderr);
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

/* A mapping from region to svalue for use when tracking state.  */

class region_to_value_map
{
public:
  typedef hash_map<const region *, const svalue *> hash_map_t;
  typedef hash_map_t::iterator iterator;

  region_to_value_map () : m_hash_map () {}
  region_to_value_map (const region_to_value_map &other)
  : m_hash_map (other.m_hash_map) {}
  region_to_value_map &operator= (const region_to_value_map &other);

  bool operator== (const region_to_value_map &other) const;
  bool operator!= (const region_to_value_map &other) const
  {
    return !(*this == other);
  }

  iterator begin () const { return m_hash_map.begin (); }
  iterator end () const { return m_hash_map.end (); }

  const svalue * const *get (const region *reg) const
  {
    return const_cast <hash_map_t &> (m_hash_map).get (reg);
  }
  void put (const region *reg, const svalue *sval)
  {
    m_hash_map.put (reg, sval);
  }
  void remove (const region *reg)
  {
    m_hash_map.remove (reg);
  }

  bool is_empty () const { return m_hash_map.is_empty (); }

  void dump_to_pp (pretty_printer *pp, bool simple, bool multiline) const;
  void dump (bool simple) const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  bool can_merge_with_p (const region_to_value_map &other,
			 region_to_value_map *out) const;

  void purge_state_involving (const svalue *sval);

private:
  hash_map_t m_hash_map;
};

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
    m_num_bounded_ranges_constraints (0),
    m_num_client_items (0)
  {}

  int m_num_svalues;
  int m_num_regions;
  int m_num_equiv_classes;
  int m_num_constraints;
  int m_num_bounded_ranges_constraints;
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
  virtual void visit_repeated_svalue (const repeated_svalue *) {}
  virtual void visit_bits_within_svalue (const bits_within_svalue *) {}
  virtual void visit_unmergeable_svalue (const unmergeable_svalue *) {}
  virtual void visit_placeholder_svalue (const placeholder_svalue *) {}
  virtual void visit_widening_svalue (const widening_svalue *) {}
  virtual void visit_compound_svalue (const compound_svalue *) {}
  virtual void visit_conjured_svalue (const conjured_svalue *) {}
  virtual void visit_asm_output_svalue (const asm_output_svalue *) {}
  virtual void visit_const_fn_result_svalue (const const_fn_result_svalue *) {}

  virtual void visit_region (const region *) {}
};

struct append_regions_cb_data;

typedef void (*pop_frame_callback) (const region_model *model,
				    const region_model *prev_model,
				    const svalue *retval,
				    region_model_context *ctxt);

/* A region_model encapsulates a representation of the state of memory, with
   a tree of regions, along with their associated values.
   The representation is graph-like because values can be pointers to
   regions.
   It also stores:
   - a constraint_manager, capturing relationships between the values, and
   - dynamic extents, mapping dynamically-allocated regions to svalues (their
   capacities).  */

class region_model
{
 public:
  typedef region_to_value_map dynamic_extents_t;

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
  void dump () const;

  void debug () const;

  std::unique_ptr<json::object> to_json () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (const text_art::dump_widget_info &dwi) const;

  void validate () const;

  void canonicalize ();
  bool canonicalized_p () const;

  void
  on_stmt_pre (const gimple *stmt,
	       bool *out_unknown_side_effects,
	       region_model_context *ctxt);

  void on_assignment (const gassign *stmt, region_model_context *ctxt);
  const svalue *get_gassign_result (const gassign *assign,
				    region_model_context *ctxt);
  void on_asm_stmt (const gasm *asm_stmt, region_model_context *ctxt);
  bool on_call_pre (const gcall *stmt, region_model_context *ctxt);
  void on_call_post (const gcall *stmt,
		     bool unknown_side_effects,
		     region_model_context *ctxt);

  void purge_state_involving (const svalue *sval, region_model_context *ctxt);

  void impl_deallocation_call (const call_details &cd);

  const svalue *maybe_get_copy_bounds (const region *src_reg,
				       const svalue *num_bytes_sval);
  void update_for_int_cst_return (const call_details &cd,
				  int retval,
				  bool unmergeable);
  void update_for_zero_return (const call_details &cd,
			       bool unmergeable);
  void update_for_nonzero_return (const call_details &cd);

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
		   const region_model &old_state,
		   hash_set<const svalue *> &svals_changing_meaning,
		   region_model_context *ctxt);

  bool maybe_update_for_edge (const superedge &edge,
			      const gimple *last_stmt,
			      region_model_context *ctxt,
			      std::unique_ptr<rejected_constraint> *out);

  void update_for_gcall (const gcall *call_stmt,
                         region_model_context *ctxt,
                         function *callee = NULL);

  void update_for_return_gcall (const gcall *call_stmt,
                                region_model_context *ctxt);

  const region *push_frame (const function &fun, const vec<const svalue *> *arg_sids,
			    region_model_context *ctxt);
  const frame_region *get_current_frame () const { return m_current_frame; }
  const function *get_current_function () const;
  void pop_frame (tree result_lvalue,
		  const svalue **out_result,
		  region_model_context *ctxt,
		  const gcall *call_stmt,
		  bool eval_return_svalue = true);
  int get_stack_depth () const;
  const frame_region *get_frame_at_index (int index) const;

  const region *get_lvalue (path_var pv, region_model_context *ctxt) const;
  const region *get_lvalue (tree expr, region_model_context *ctxt) const;
  const svalue *get_rvalue (path_var pv, region_model_context *ctxt) const;
  const svalue *get_rvalue (tree expr, region_model_context *ctxt) const;

  const region *deref_rvalue (const svalue *ptr_sval, tree ptr_tree,
			      region_model_context *ctxt,
			      bool add_nonnull_constraint = true) const;

  const svalue *get_rvalue_for_bits (tree type,
				     const region *reg,
				     const bit_range &bits,
				     region_model_context *ctxt) const;

  void set_value (const region *lhs_reg, const svalue *rhs_sval,
		  region_model_context *ctxt);
  void set_value (tree lhs, tree rhs, region_model_context *ctxt);
  void clobber_region (const region *reg);
  void purge_region (const region *reg);
  void fill_region (const region *reg,
		    const svalue *sval,
		    region_model_context *ctxt);
  void zero_fill_region (const region *reg,
			 region_model_context *ctxt);
  void write_bytes (const region *dest_reg,
		    const svalue *num_bytes_sval,
		    const svalue *sval,
		    region_model_context *ctxt);
  const svalue *read_bytes (const region *src_reg,
			    tree src_ptr_expr,
			    const svalue *num_bytes_sval,
			    region_model_context *ctxt) const;
  void copy_bytes (const region *dest_reg,
		   const region *src_reg,
		   tree src_ptr_expr,
		   const svalue *num_bytes_sval,
		   region_model_context *ctxt);
  void mark_region_as_unknown (const region *reg, uncertainty_t *uncertainty);

  tristate eval_condition (const svalue *lhs,
			   enum tree_code op,
			   const svalue *rhs) const;
  tristate compare_initial_and_pointer (const initial_svalue *init,
					const region_svalue *ptr) const;
  tristate symbolic_greater_than (const binop_svalue *a,
				  const svalue *b) const;
  tristate structural_equality (const svalue *a, const svalue *b) const;
  tristate eval_condition (tree lhs,
			   enum tree_code op,
			   tree rhs,
			   region_model_context *ctxt) const;
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt);
  bool add_constraint (tree lhs, enum tree_code op, tree rhs,
		       region_model_context *ctxt,
		       std::unique_ptr<rejected_constraint> *out);

	const region *
	get_or_create_region_for_heap_alloc (const svalue *size_in_bytes,
				region_model_context *ctxt,
				bool update_state_machine = false,
				const call_details *cd = nullptr);

  const region *create_region_for_alloca (const svalue *size_in_bytes,
					  region_model_context *ctxt);
  void get_referenced_base_regions (auto_bitmap &out_ids) const;

  tree get_representative_tree (const svalue *sval,
				logger *logger = nullptr) const;
  tree get_representative_tree (const region *reg,
				logger *logger = nullptr) const;
  path_var
  get_representative_path_var (const svalue *sval,
			       svalue_set *visited,
			       logger *logger) const;
  path_var
  get_representative_path_var (const region *reg,
			       svalue_set *visited,
			       logger *logger) const;

  /* For selftests.  */
  constraint_manager *get_constraints ()
  {
    return m_constraints;
  }

  store *get_store () { return &m_store; }
  const store *get_store () const { return &m_store; }

  const dynamic_extents_t &
  get_dynamic_extents () const
  {
    return m_dynamic_extents;
  }
  const svalue *get_dynamic_extents (const region *reg) const;
  void set_dynamic_extents (const region *reg,
			    const svalue *size_in_bytes,
			    region_model_context *ctxt);
  void unset_dynamic_extents (const region *reg);

  region_model_manager *get_manager () const { return m_mgr; }
  bounded_ranges_manager *get_range_manager () const
  {
    return m_mgr->get_range_manager ();
  }

  void unbind_region_and_descendents (const region *reg,
				      enum poison_kind pkind);

  bool can_merge_with_p (const region_model &other_model,
			 const program_point &point,
			 region_model *out_model,
			 const extrinsic_state *ext_state = NULL,
			 const program_state *state_a = NULL,
			 const program_state *state_b = NULL) const;

  tree get_fndecl_for_call (const gcall *call,
			    region_model_context *ctxt);

  void get_regions_for_current_frame (auto_vec<const decl_region *> *out) const;
  static void append_regions_cb (const region *base_reg,
				 struct append_regions_cb_data *data);

  const svalue *get_store_value (const region *reg,
				 region_model_context *ctxt) const;
  const svalue *get_store_bytes (const region *base_reg,
				 const byte_range &bytes,
				 region_model_context *ctxt) const;
  const svalue *scan_for_null_terminator (const region *reg,
					  tree expr,
					  const svalue **out_sval,
					  region_model_context *ctxt) const;
  const svalue *scan_for_null_terminator_1 (const region *reg,
					    tree expr,
					    const svalue **out_sval,
					    region_model_context *ctxt) const;

  bool region_exists_p (const region *reg) const;

  void loop_replay_fixup (const region_model *dst_state);

  const svalue *get_capacity (const region *reg) const;

  bool replay_call_summary (call_summary_replay &r,
			    const region_model &summary);

  void maybe_complain_about_infoleak (const region *dst_reg,
				      const svalue *copied_sval,
				      const region *src_reg,
				      region_model_context *ctxt);

  void set_errno (const call_details &cd);

  /* Implemented in sm-fd.cc  */
  void mark_as_valid_fd (const svalue *sval, region_model_context *ctxt);

  /* Implemented in sm-malloc.cc  */
  void on_realloc_with_move (const call_details &cd,
			     const svalue *old_ptr_sval,
			     const svalue *new_ptr_sval);

  /* Implemented in sm-malloc.cc.  */
  void
  transition_ptr_sval_non_null (region_model_context *ctxt,
      const svalue *new_ptr_sval);

  /* Implemented in sm-taint.cc.  */
  void mark_as_tainted (const svalue *sval,
			region_model_context *ctxt);

  bool add_constraint (const svalue *lhs,
		       enum tree_code op,
		       const svalue *rhs,
		       region_model_context *ctxt);

  const svalue *check_for_poison (const svalue *sval,
				  tree expr,
				  const region *src_region,
				  region_model_context *ctxt) const;

  void check_region_for_write (const region *dest_reg,
			       const svalue *sval_hint,
			       region_model_context *ctxt) const;

  const svalue *
  check_for_null_terminated_string_arg (const call_details &cd,
					unsigned idx) const;
  const svalue *
  check_for_null_terminated_string_arg (const call_details &cd,
					unsigned idx,
					bool include_terminator,
					const svalue **out_sval) const;

  const builtin_known_function *
  get_builtin_kf (const gcall *call,
		  region_model_context *ctxt = NULL) const;

  static void
  register_pop_frame_callback (const pop_frame_callback &callback)
  {
    pop_frame_callbacks.safe_push (callback);
  }

  static void
  notify_on_pop_frame (const region_model *model,
		       const region_model *prev_model,
		       const svalue *retval,
		       region_model_context *ctxt)
  {
    for (auto &callback : pop_frame_callbacks)
	callback (model, prev_model, retval, ctxt);
  }

  bool called_from_main_p () const;

private:
  const region *get_lvalue_1 (path_var pv, region_model_context *ctxt) const;
  const svalue *get_rvalue_1 (path_var pv, region_model_context *ctxt) const;

  path_var
  get_representative_path_var_1 (const svalue *sval,
				 svalue_set *visited,
				 logger *logger) const;
  path_var
  get_representative_path_var_1 (const region *reg,
				 svalue_set *visited,
				 logger *logger) const;

  const known_function *get_known_function (tree fndecl,
					    const call_details &cd) const;
  const known_function *get_known_function (enum internal_fn) const;

  bool add_constraints_from_binop (const svalue *outer_lhs,
				   enum tree_code outer_op,
				   const svalue *outer_rhs,
				   bool *out,
				   region_model_context *ctxt);

  void update_for_call_superedge (const call_superedge &call_edge,
				  region_model_context *ctxt);
  void update_for_return_superedge (const return_superedge &return_edge,
				    region_model_context *ctxt);
  bool apply_constraints_for_gcond (const cfg_superedge &edge,
				    const gcond *cond_stmt,
				    region_model_context *ctxt,
				    std::unique_ptr<rejected_constraint> *out);
  bool apply_constraints_for_gswitch (const switch_cfg_superedge &edge,
				      const gswitch *switch_stmt,
				      region_model_context *ctxt,
				      std::unique_ptr<rejected_constraint> *out);
  bool apply_constraints_for_ggoto (const cfg_superedge &edge,
				    const ggoto *goto_stmt,
				    region_model_context *ctxt);
  bool apply_constraints_for_exception (const gimple *last_stmt,
					region_model_context *ctxt,
					std::unique_ptr<rejected_constraint> *out);

  int poison_any_pointers_to_descendents (const region *reg,
					  enum poison_kind pkind);

  void on_top_level_param (tree param,
			   bool nonnull,
			   region_model_context *ctxt);

  const svalue *get_initial_value_for_global (const region *reg) const;

  const region * get_region_for_poisoned_expr (tree expr) const;

  void check_dynamic_size_for_taint (enum memory_space mem_space,
				     const svalue *size_in_bytes,
				     region_model_context *ctxt) const;
  void check_dynamic_size_for_floats (const svalue *size_in_bytes,
				      region_model_context *ctxt) const;

  void check_region_for_taint (const region *reg,
			       enum access_direction dir,
			       region_model_context *ctxt) const;

  void check_for_writable_region (const region* dest_reg,
				  region_model_context *ctxt) const;
  bool check_region_access (const region *reg,
			    enum access_direction dir,
			    const svalue *sval_hint,
			    region_model_context *ctxt) const;
  bool check_region_for_read (const region *src_reg,
			      region_model_context *ctxt) const;
  void check_region_size (const region *lhs_reg, const svalue *rhs_sval,
			  region_model_context *ctxt) const;

  /* Implemented in bounds-checking.cc  */
  bool check_symbolic_bounds (const region *base_reg,
			      const svalue *sym_byte_offset,
			      const svalue *num_bytes_sval,
			      const svalue *capacity,
			      enum access_direction dir,
			      const svalue *sval_hint,
			      region_model_context *ctxt) const;
  bool check_region_bounds (const region *reg, enum access_direction dir,
			    const svalue *sval_hint,
			    region_model_context *ctxt) const;

  void check_call_args (const call_details &cd) const;
  void check_call_format_attr (const call_details &cd,
			       tree format_attr) const;
  void check_function_attr_access (const gcall *call,
				   tree callee_fndecl,
				   region_model_context *ctxt,
				   rdwr_map &rdwr_idx) const;
  void check_function_attr_null_terminated_string_arg (const gcall *call,
						       tree callee_fndecl,
						       region_model_context *ctxt,
						       rdwr_map &rdwr_idx);
  void check_one_function_attr_null_terminated_string_arg (const gcall *call,
							   tree callee_fndecl,
							   region_model_context *ctxt,
							   rdwr_map &rdwr_idx,
							   tree attr);
  void check_function_attrs (const gcall *call,
			     tree callee_fndecl,
			     region_model_context *ctxt);

  static auto_vec<pop_frame_callback> pop_frame_callbacks;
  /* Storing this here to avoid passing it around everywhere.  */
  region_model_manager *const m_mgr;

  store m_store;

  constraint_manager *m_constraints; // TODO: embed, rather than dynalloc?

  const frame_region *m_current_frame;

  /* Map from base region to size in bytes, for tracking the sizes of
     dynamically-allocated regions.
     This is part of the region_model rather than the region to allow for
     memory regions to be resized (e.g. by realloc).  */
  dynamic_extents_t m_dynamic_extents;
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
  /* Hook for clients to store pending diagnostics.
     Return true if the diagnostic was stored, or false if it was deleted.
     Optionally provide a custom stmt_finder.  */
  virtual bool warn (std::unique_ptr<pending_diagnostic> d,
		     const stmt_finder *custom_finder = NULL) = 0;

  /* Hook for clients to add a note to the last previously stored
     pending diagnostic.  */
  virtual void add_note (std::unique_ptr<pending_note> pn) = 0;

  /* Hook for clients to add an event to the last previously stored
     pending diagnostic.  */
  virtual void add_event (std::unique_ptr<checker_event> event) = 0;

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
  virtual void on_condition (const svalue *lhs,
			     enum tree_code op,
			     const svalue *rhs) = 0;

  /* Hook for clients to be notified when the condition that
     SVAL is within RANGES is added to the region model.
     Similar to on_condition, but for use when handling switch statements.
     RANGES is non-empty.  */
  virtual void on_bounded_ranges (const svalue &sval,
				  const bounded_ranges &ranges) = 0;

  /* Hook for clients to be notified when a frame is popped from the stack.  */
  virtual void on_pop_frame (const frame_region *) = 0;

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

  /* Hook for clients to purge state involving SVAL.  */
  virtual void purge_state_involving (const svalue *sval) = 0;

  /* Hook for clients to split state with a non-standard path.  */
  virtual void bifurcate (std::unique_ptr<custom_edge_info> info) = 0;

  /* Hook for clients to terminate the standard path.  */
  virtual void terminate_path () = 0;

  virtual const extrinsic_state *get_ext_state () const = 0;

  /* Hook for clients to access the a specific state machine in
     any underlying program_state.  */
  virtual bool
  get_state_map_by_name (const char *name,
			 sm_state_map **out_smap,
			 const state_machine **out_sm,
			 unsigned *out_sm_idx,
			 std::unique_ptr<sm_context> *out_sm_context) = 0;

  /* Precanned ways for clients to access specific state machines.  */
  bool get_fd_map (sm_state_map **out_smap,
		   const state_machine **out_sm,
		   unsigned *out_sm_idx,
		   std::unique_ptr<sm_context> *out_sm_context)
  {
    return get_state_map_by_name ("file-descriptor", out_smap, out_sm,
				  out_sm_idx, out_sm_context);
  }
  bool get_malloc_map (sm_state_map **out_smap,
		       const state_machine **out_sm,
		       unsigned *out_sm_idx)
  {
    return get_state_map_by_name ("malloc", out_smap, out_sm, out_sm_idx, NULL);
  }
  bool get_taint_map (sm_state_map **out_smap,
		      const state_machine **out_sm,
		      unsigned *out_sm_idx)
  {
    return get_state_map_by_name ("taint", out_smap, out_sm, out_sm_idx, NULL);
  }

  bool possibly_tainted_p (const svalue *sval);

  /* Get the current statement, if any.  */
  virtual const gimple *get_stmt () const = 0;

  virtual const exploded_graph *get_eg () const = 0;

  /* Hooks for detecting infinite loops.  */
  virtual void maybe_did_work () = 0;
  virtual bool checking_for_infinite_loop_p () const = 0;
  virtual void on_unusable_in_infinite_loop () = 0;
};

/* A "do nothing" subclass of region_model_context.  */

class noop_region_model_context : public region_model_context
{
public:
  bool warn (std::unique_ptr<pending_diagnostic>,
	     const stmt_finder *) override { return false; }
  void add_note (std::unique_ptr<pending_note>) override;
  void add_event (std::unique_ptr<checker_event>) override;
  void on_svalue_leak (const svalue *) override {}
  void on_liveness_change (const svalue_set &,
			   const region_model *) override {}
  logger *get_logger () override { return NULL; }
  void on_condition (const svalue *lhs ATTRIBUTE_UNUSED,
		     enum tree_code op ATTRIBUTE_UNUSED,
		     const svalue *rhs ATTRIBUTE_UNUSED) override
  {
  }
  void on_bounded_ranges (const svalue &,
			  const bounded_ranges &) override
  {
  }
  void on_pop_frame (const frame_region *) override {}
  void on_unknown_change (const svalue *sval ATTRIBUTE_UNUSED,
			  bool is_mutable ATTRIBUTE_UNUSED) override
  {
  }
  void on_phi (const gphi *phi ATTRIBUTE_UNUSED,
	       tree rhs ATTRIBUTE_UNUSED) override
  {
  }
  void on_unexpected_tree_code (tree, const dump_location_t &) override {}

  void on_escaped_function (tree) override {}

  uncertainty_t *get_uncertainty () override { return NULL; }

  void purge_state_involving (const svalue *sval ATTRIBUTE_UNUSED) override {}

  void bifurcate (std::unique_ptr<custom_edge_info> info) override;
  void terminate_path () override;

  const extrinsic_state *get_ext_state () const override { return NULL; }

  bool get_state_map_by_name (const char *,
			      sm_state_map **,
			      const state_machine **,
			      unsigned *,
			      std::unique_ptr<sm_context> *) override
  {
    return false;
  }

  const gimple *get_stmt () const override { return NULL; }
  const exploded_graph *get_eg () const override { return NULL; }
  void maybe_did_work () override {}
  bool checking_for_infinite_loop_p () const override { return false; }
  void on_unusable_in_infinite_loop () override {}
};

/* A subclass of region_model_context for determining if operations fail
   e.g. "can we generate a region for the lvalue of EXPR?".  */

class tentative_region_model_context : public noop_region_model_context
{
public:
  tentative_region_model_context () : m_num_unexpected_codes (0) {}

  void on_unexpected_tree_code (tree, const dump_location_t &)
    final override
  {
    m_num_unexpected_codes++;
  }

  bool had_errors_p () const { return m_num_unexpected_codes > 0; }

private:
  int m_num_unexpected_codes;
};

/* Subclass of region_model_context that wraps another context, allowing
   for extra code to be added to the various hooks.  */

class region_model_context_decorator : public region_model_context
{
 public:
  bool warn (std::unique_ptr<pending_diagnostic> d,
	     const stmt_finder *custom_finder) override
  {
    if (m_inner)
      return m_inner->warn (std::move (d), custom_finder);
    else
      return false;
  }

  void add_note (std::unique_ptr<pending_note> pn) override
  {
    if (m_inner)
      m_inner->add_note (std::move (pn));
  }
  void add_event (std::unique_ptr<checker_event> event) override;

  void on_svalue_leak (const svalue *sval) override
  {
    if (m_inner)
      m_inner->on_svalue_leak (sval);
  }

  void on_liveness_change (const svalue_set &live_svalues,
			   const region_model *model) override
  {
    if (m_inner)
      m_inner->on_liveness_change (live_svalues, model);
  }

  logger *get_logger () override
  {
    if (m_inner)
      return m_inner->get_logger ();
    else
      return nullptr;
  }

  void on_condition (const svalue *lhs,
		     enum tree_code op,
		     const svalue *rhs) override
  {
    if (m_inner)
      m_inner->on_condition (lhs, op, rhs);
  }

  void on_bounded_ranges (const svalue &sval,
			  const bounded_ranges &ranges) override
  {
    if (m_inner)
      m_inner->on_bounded_ranges (sval, ranges);
  }

  void on_pop_frame (const frame_region *frame_reg) override
  {
    if (m_inner)
      m_inner->on_pop_frame (frame_reg);
  }

  void on_unknown_change (const svalue *sval, bool is_mutable) override
  {
    if (m_inner)
      m_inner->on_unknown_change (sval, is_mutable);
  }

  void on_phi (const gphi *phi, tree rhs) override
  {
    if (m_inner)
      m_inner->on_phi (phi, rhs);
  }

  void on_unexpected_tree_code (tree t,
				const dump_location_t &loc) override
  {
    if (m_inner)
      m_inner->on_unexpected_tree_code (t, loc);
  }

  void on_escaped_function (tree fndecl) override
  {
    if (m_inner)
      m_inner->on_escaped_function (fndecl);
  }

  uncertainty_t *get_uncertainty () override
  {
    if (m_inner)
      return m_inner->get_uncertainty ();
    else
      return nullptr;
  }

  void purge_state_involving (const svalue *sval) override
  {
    if (m_inner)
      m_inner->purge_state_involving (sval);
  }

  void bifurcate (std::unique_ptr<custom_edge_info> info) override
  {
    if (m_inner)
      m_inner->bifurcate (std::move (info));
  }

  void terminate_path () override
  {
    if (m_inner)
      m_inner->terminate_path ();
  }

  const extrinsic_state *get_ext_state () const override
  {
    if (m_inner)
      return m_inner->get_ext_state ();
    else
      return nullptr;
  }

  bool get_state_map_by_name (const char *name,
			      sm_state_map **out_smap,
			      const state_machine **out_sm,
			      unsigned *out_sm_idx,
			      std::unique_ptr<sm_context> *out_sm_context)
    override
  {
    if (m_inner)
      return m_inner->get_state_map_by_name (name, out_smap, out_sm, out_sm_idx,
					     out_sm_context);
    else
      return false;
  }

  const gimple *get_stmt () const override
  {
    if (m_inner)
      return m_inner->get_stmt ();
    else
      return nullptr;
  }

  const exploded_graph *get_eg () const override
  {
    if (m_inner)
	return m_inner->get_eg ();
    else
	return nullptr;
  }

  void maybe_did_work () override
  {
    if (m_inner)
      m_inner->maybe_did_work ();
  }

  bool checking_for_infinite_loop_p () const override
  {
    if (m_inner)
      return m_inner->checking_for_infinite_loop_p ();
    return false;
  }
  void on_unusable_in_infinite_loop () override
  {
    if (m_inner)
      m_inner->on_unusable_in_infinite_loop ();
  }

protected:
  region_model_context_decorator (region_model_context *inner)
  : m_inner (inner)
  {
  }

  region_model_context *m_inner;
};

/* Subclass of region_model_context_decorator with a hook for adding
   notes/events when saving diagnostics.  */

class annotating_context : public region_model_context_decorator
{
public:
  bool warn (std::unique_ptr<pending_diagnostic> d,
	     const stmt_finder *custom_finder) override
  {
    if (m_inner)
      if (m_inner->warn (std::move (d), custom_finder))
	{
	  add_annotations ();
	  return true;
	}
    return false;
  }

  /* Hook to add new event(s)/note(s)  */
  virtual void add_annotations () = 0;

protected:
  annotating_context (region_model_context *inner)
  : region_model_context_decorator (inner)
  {
  }
};

/* A bundle of data for use when attempting to merge two region_model
   instances to make a third.  */

struct model_merger
{
  model_merger (const region_model *model_a,
		const region_model *model_b,
		const program_point &point,
		region_model *merged_model,
		const extrinsic_state *ext_state,
		const program_state *state_a,
		const program_state *state_b)
  : m_model_a (model_a), m_model_b (model_b),
    m_point (point),
    m_merged_model (merged_model),
    m_ext_state (ext_state),
    m_state_a (state_a), m_state_b (state_b)
  {
  }

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (FILE *fp, bool simple) const;
  void dump (bool simple) const;

  region_model_manager *get_manager () const
  {
    return m_model_a->get_manager ();
  }

  bool mergeable_svalue_p (const svalue *) const;
  const function_point &get_function_point () const
  {
    return m_point.get_function_point ();
  }

  void on_widening_reuse (const widening_svalue *widening_sval);

  const region_model *m_model_a;
  const region_model *m_model_b;
  const program_point &m_point;
  region_model *m_merged_model;

  const extrinsic_state *m_ext_state;
  const program_state *m_state_a;
  const program_state *m_state_b;

  hash_set<const svalue *> m_svals_changing_meaning;
};

/* A record that can (optionally) be written out when
   region_model::add_constraint fails.  */

class rejected_constraint
{
public:
  virtual ~rejected_constraint () {}
  virtual void dump_to_pp (pretty_printer *pp) const = 0;

  const region_model &get_model () const { return m_model; }

protected:
  rejected_constraint (const region_model &model)
  : m_model (model)
  {}

  region_model m_model;
};

class rejected_op_constraint : public rejected_constraint
{
public:
  rejected_op_constraint (const region_model &model,
			  tree lhs, enum tree_code op, tree rhs)
  : rejected_constraint (model),
    m_lhs (lhs), m_op (op), m_rhs (rhs)
  {}

  void dump_to_pp (pretty_printer *pp) const final override;

  tree m_lhs;
  enum tree_code m_op;
  tree m_rhs;
};

class rejected_default_case : public rejected_constraint
{
public:
  rejected_default_case (const region_model &model)
  : rejected_constraint (model)
  {}

  void dump_to_pp (pretty_printer *pp) const final override;
};

class rejected_ranges_constraint : public rejected_constraint
{
public:
  rejected_ranges_constraint (const region_model &model,
			      tree expr, const bounded_ranges *ranges)
  : rejected_constraint (model),
    m_expr (expr), m_ranges (ranges)
  {}

  void dump_to_pp (pretty_printer *pp) const final override;

private:
  tree m_expr;
  const bounded_ranges *m_ranges;
};

/* A bundle of state.  */

class engine
{
public:
  engine (const supergraph *sg = NULL, logger *logger = NULL);
  const supergraph *get_supergraph () { return m_sg; }
  region_model_manager *get_model_manager () { return &m_mgr; }
  known_function_manager *get_known_function_manager ()
  {
    return m_mgr.get_known_function_manager ();
  }

  void log_stats (logger *logger) const;

private:
  const supergraph *m_sg;
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
  bool warn (std::unique_ptr<pending_diagnostic> d,
	     const stmt_finder *) final override
  {
    m_diagnostics.safe_push (d.release ());
    return true;
  }

  unsigned get_num_diagnostics () const { return m_diagnostics.length (); }

  void on_unexpected_tree_code (tree t, const dump_location_t &)
    final override
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
