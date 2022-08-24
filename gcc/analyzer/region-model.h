/* Classes for modeling the state of memory.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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

} // namespace ana

namespace ana {

/* A class responsible for owning and consolidating region and svalue
   instances.
   region and svalue instances are immutable as far as clients are
   concerned, so they are provided as "const" ptrs.  */

class region_model_manager
{
public:
  region_model_manager (logger *logger = NULL);
  ~region_model_manager ();

  /* call_string consolidation.  */
  const call_string &get_empty_call_string () const
  {
    return m_empty_call_string;
  }

  /* svalue consolidation.  */
  const svalue *get_or_create_constant_svalue (tree cst_expr);
  const svalue *get_or_create_int_cst (tree type, poly_int64);
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
  const svalue *get_or_create_repeated_svalue (tree type,
					       const svalue *outer_size,
					       const svalue *inner_svalue);
  const svalue *get_or_create_bits_within (tree type,
					   const bit_range &bits,
					   const svalue *inner_svalue);
  const svalue *get_or_create_unmergeable (const svalue *arg);
  const svalue *get_or_create_widening_svalue (tree type,
					       const program_point &point,
					       const svalue *base_svalue,
					       const svalue *iter_svalue);
  const svalue *get_or_create_compound_svalue (tree type,
					       const binding_map &map);
  const svalue *get_or_create_conjured_svalue (tree type, const gimple *stmt,
					       const region *id_reg,
					       const conjured_purge &p);
  const svalue *
  get_or_create_asm_output_svalue (tree type,
				   const gasm *asm_stmt,
				   unsigned output_idx,
				   const vec<const svalue *> &inputs);
  const svalue *
  get_or_create_const_fn_result_svalue (tree type,
					tree fndecl,
					const vec<const svalue *> &inputs);

  const svalue *maybe_get_char_from_string_cst (tree string_cst,
						tree byte_offset_cst);

  /* Dynamically-allocated svalue instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  const svalue *create_unique_svalue (tree type);

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
  const region *get_sized_region (const region *parent,
				  tree type,
				  const svalue *byte_size_sval);
  const region *get_cast_region (const region *original_region,
				 tree type);
  const frame_region *get_frame_region (const frame_region *calling_frame,
					function *fun);
  const region *get_symbolic_region (const svalue *sval);
  const string_region *get_region_for_string (tree string_cst);
  const region *get_bit_range (const region *parent, tree type,
			       const bit_range &bits);
  const var_arg_region *get_var_arg_region (const frame_region *parent,
					    unsigned idx);

  const region *get_unknown_symbolic_region (tree region_type);

  const region *
  get_region_for_unexpected_tree_code (region_model_context *ctxt,
				       tree t,
				       const dump_location_t &loc);

  unsigned alloc_region_id () { return m_next_region_id++; }

  store_manager *get_store_manager () { return &m_store_mgr; }
  bounded_ranges_manager *get_range_manager () const { return m_range_mgr; }

  /* Dynamically-allocated region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  const region *create_region_for_heap_alloc ();
  const region *create_region_for_alloca (const frame_region *frame);

  void log_stats (logger *logger, bool show_objs) const;

  void begin_checking_feasibility (void) { m_checking_feasibility = true; }
  void end_checking_feasibility (void) { m_checking_feasibility = false; }

  logger *get_logger () const { return m_logger; }

  void dump_untracked_regions () const;

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
  const svalue *maybe_fold_repeated_svalue (tree type,
					    const svalue *outer_size,
					    const svalue *inner_svalue);
  const svalue *maybe_fold_bits_within_svalue (tree type,
					       const bit_range &bits,
					       const svalue *inner_svalue);
  const svalue *maybe_undo_optimize_bit_field_compare (tree type,
						       const compound_svalue *compound_sval,
						       tree cst, const svalue *arg1);
  const svalue *maybe_fold_asm_output_svalue (tree type,
					      const vec<const svalue *> &inputs);

  logger *m_logger;

  const call_string m_empty_call_string;

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

  typedef hash_map<repeated_svalue::key_t,
		   repeated_svalue *> repeated_values_map_t;
  repeated_values_map_t m_repeated_values_map;

  typedef hash_map<bits_within_svalue::key_t,
		   bits_within_svalue *> bits_within_values_map_t;
  bits_within_values_map_t m_bits_within_values_map;

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

  typedef hash_map<asm_output_svalue::key_t,
		   asm_output_svalue *> asm_output_values_map_t;
  asm_output_values_map_t m_asm_output_values_map;

  typedef hash_map<const_fn_result_svalue::key_t,
		   const_fn_result_svalue *> const_fn_result_values_map_t;
  const_fn_result_values_map_t m_const_fn_result_values_map;

  bool m_checking_feasibility;

  /* "Dynamically-allocated" svalue instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  auto_delete_vec<svalue> m_managed_dynamic_svalues;

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
  consolidation_map<sized_region> m_sized_regions;
  consolidation_map<cast_region> m_cast_regions;
  consolidation_map<frame_region> m_frame_regions;
  consolidation_map<symbolic_region> m_symbolic_regions;

  typedef hash_map<tree, string_region *> string_map_t;
  string_map_t m_string_map;

  consolidation_map<bit_range_region> m_bit_range_regions;
  consolidation_map<var_arg_region> m_var_arg_regions;

  store_manager m_store_mgr;

  bounded_ranges_manager *m_range_mgr;

  /* "Dynamically-allocated" region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  auto_delete_vec<region> m_managed_dynamic_regions;
};

struct append_regions_cb_data;

/* Helper class for handling calls to functions with known behavior.
   Implemented in region-model-impl-calls.c.  */

class call_details
{
public:
  call_details (const gcall *call, region_model *model,
		region_model_context *ctxt);

  region_model_manager *get_manager () const;
  region_model_context *get_ctxt () const { return m_ctxt; }
  uncertainty_t *get_uncertainty () const;
  tree get_lhs_type () const { return m_lhs_type; }
  const region *get_lhs_region () const { return m_lhs_region; }

  bool maybe_set_lhs (const svalue *result) const;

  unsigned num_args () const;

  const gcall *get_call_stmt () const { return m_call; }

  tree get_arg_tree (unsigned idx) const;
  tree get_arg_type (unsigned idx) const;
  const svalue *get_arg_svalue (unsigned idx) const;
  const char *get_arg_string_literal (unsigned idx) const;

  tree get_fndecl_for_call () const;

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (bool simple) const;

  const svalue *get_or_create_conjured_svalue (const region *) const;

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

  void debug () const;

  void validate () const;

  void canonicalize ();
  bool canonicalized_p () const;

  void
  on_stmt_pre (const gimple *stmt,
	       bool *out_terminate_path,
	       bool *out_unknown_side_effects,
	       region_model_context *ctxt);

  void on_assignment (const gassign *stmt, region_model_context *ctxt);
  const svalue *get_gassign_result (const gassign *assign,
				    region_model_context *ctxt);
  void on_asm_stmt (const gasm *asm_stmt, region_model_context *ctxt);
  bool on_call_pre (const gcall *stmt, region_model_context *ctxt,
		    bool *out_terminate_path);
  void on_call_post (const gcall *stmt,
		     bool unknown_side_effects,
		     region_model_context *ctxt);

  void purge_state_involving (const svalue *sval, region_model_context *ctxt);

  /* Specific handling for on_call_pre.  */
  void impl_call_alloca (const call_details &cd);
  void impl_call_analyzer_describe (const gcall *call,
				    region_model_context *ctxt);
  void impl_call_analyzer_dump_capacity (const gcall *call,
					 region_model_context *ctxt);
  void impl_call_analyzer_dump_escaped (const gcall *call);
  void impl_call_analyzer_eval (const gcall *call,
				region_model_context *ctxt);
  void impl_call_builtin_expect (const call_details &cd);
  void impl_call_calloc (const call_details &cd);
  bool impl_call_error (const call_details &cd, unsigned min_args,
			bool *out_terminate_path);
  void impl_call_fgets (const call_details &cd);
  void impl_call_fread (const call_details &cd);
  void impl_call_free (const call_details &cd);
  void impl_call_malloc (const call_details &cd);
  void impl_call_memcpy (const call_details &cd);
  void impl_call_memset (const call_details &cd);
  void impl_call_putenv (const call_details &cd);
  void impl_call_realloc (const call_details &cd);
  void impl_call_strchr (const call_details &cd);
  void impl_call_strcpy (const call_details &cd);
  void impl_call_strlen (const call_details &cd);
  void impl_call_operator_new (const call_details &cd);
  void impl_call_operator_delete (const call_details &cd);
  void impl_deallocation_call (const call_details &cd);

  /* Implemented in varargs.cc.  */
  void impl_call_va_start (const call_details &cd);
  void impl_call_va_copy (const call_details &cd);
  void impl_call_va_arg (const call_details &cd);
  void impl_call_va_end (const call_details &cd);

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
		   region_model_context *ctxt);

  bool maybe_update_for_edge (const superedge &edge,
			      const gimple *last_stmt,
			      region_model_context *ctxt,
			      rejected_constraint **out);

  void update_for_gcall (const gcall *call_stmt,
                         region_model_context *ctxt,
                         function *callee = NULL);
  
  void update_for_return_gcall (const gcall *call_stmt,
                                region_model_context *ctxt);

  const region *push_frame (function *fun, const vec<const svalue *> *arg_sids,
			    region_model_context *ctxt);
  const frame_region *get_current_frame () const { return m_current_frame; }
  function * get_current_function () const;
  void pop_frame (tree result_lvalue,
		  const svalue **out_result,
		  region_model_context *ctxt);
  int get_stack_depth () const;
  const frame_region *get_frame_at_index (int index) const;

  const region *get_lvalue (path_var pv, region_model_context *ctxt) const;
  const region *get_lvalue (tree expr, region_model_context *ctxt) const;
  const svalue *get_rvalue (path_var pv, region_model_context *ctxt) const;
  const svalue *get_rvalue (tree expr, region_model_context *ctxt) const;

  const region *deref_rvalue (const svalue *ptr_sval, tree ptr_tree,
			       region_model_context *ctxt) const;

  const svalue *get_rvalue_for_bits (tree type,
				     const region *reg,
				     const bit_range &bits,
				     region_model_context *ctxt) const;

  void set_value (const region *lhs_reg, const svalue *rhs_sval,
		  region_model_context *ctxt);
  void set_value (tree lhs, tree rhs, region_model_context *ctxt);
  void clobber_region (const region *reg);
  void purge_region (const region *reg);
  void fill_region (const region *reg, const svalue *sval);
  void zero_fill_region (const region *reg);
  void mark_region_as_unknown (const region *reg, uncertainty_t *uncertainty);

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

  const region *create_region_for_heap_alloc (const svalue *size_in_bytes,
					      region_model_context *ctxt);
  const region *create_region_for_alloca (const svalue *size_in_bytes,
					  region_model_context *ctxt);

  tree get_representative_tree (const svalue *sval) const;
  tree get_representative_tree (const region *reg) const;
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

  bool region_exists_p (const region *reg) const;

  void loop_replay_fixup (const region_model *dst_state);

  const svalue *get_capacity (const region *reg) const;

  /* Implemented in sm-malloc.cc  */
  void on_realloc_with_move (const call_details &cd,
			     const svalue *old_ptr_sval,
			     const svalue *new_ptr_sval);

 private:
  const region *get_lvalue_1 (path_var pv, region_model_context *ctxt) const;
  const svalue *get_rvalue_1 (path_var pv, region_model_context *ctxt) const;

  path_var
  get_representative_path_var_1 (const svalue *sval,
				 svalue_set *visited) const;
  path_var
  get_representative_path_var_1 (const region *reg,
				 svalue_set *visited) const;

  bool add_constraint (const svalue *lhs,
		       enum tree_code op,
		       const svalue *rhs,
		       region_model_context *ctxt);
  bool add_constraints_from_binop (const svalue *outer_lhs,
				   enum tree_code outer_op,
				   const svalue *outer_rhs,
				   bool *out,
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

  bool called_from_main_p () const;
  const svalue *get_initial_value_for_global (const region *reg) const;

  const svalue *check_for_poison (const svalue *sval,
				  tree expr,
				  region_model_context *ctxt) const;
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
  void check_region_access (const region *reg,
			    enum access_direction dir,
			    region_model_context *ctxt) const;
  void check_region_for_write (const region *dest_reg,
			       region_model_context *ctxt) const;
  void check_region_for_read (const region *src_reg,
			      region_model_context *ctxt) const;
  void check_region_size (const region *lhs_reg, const svalue *rhs_sval,
			  region_model_context *ctxt) const;
  void check_region_bounds (const region *reg, enum access_direction dir,
			    region_model_context *ctxt) const;

  void check_call_args (const call_details &cd) const;
  void check_external_function_for_access_attr (const gcall *call,
						tree callee_fndecl,
						region_model_context *ctxt) const;

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
     Return true if the diagnostic was stored, or false if it was deleted.  */
  virtual bool warn (pending_diagnostic *d) = 0;

  /* Hook for clients to add a note to the last previously stored pending diagnostic.
     Takes ownership of the pending_node (or deletes it).  */
  virtual void add_note (pending_note *pn) = 0;

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

  /* Hook for clients to split state with a non-standard path.
     Take ownership of INFO.  */
  virtual void bifurcate (custom_edge_info *info) = 0;

  /* Hook for clients to terminate the standard path.  */
  virtual void terminate_path () = 0;

  virtual const extrinsic_state *get_ext_state () const = 0;

  /* Hook for clients to access the "malloc" state machine in
     any underlying program_state.  */
  virtual bool get_malloc_map (sm_state_map **out_smap,
			       const state_machine **out_sm,
			       unsigned *out_sm_idx) = 0;
  /* Likewise for the "taint" state machine.  */
  virtual bool get_taint_map (sm_state_map **out_smap,
			      const state_machine **out_sm,
			      unsigned *out_sm_idx) = 0;

  /* Get the current statement, if any.  */
  virtual const gimple *get_stmt () const = 0;
};

/* A "do nothing" subclass of region_model_context.  */

class noop_region_model_context : public region_model_context
{
public:
  bool warn (pending_diagnostic *) override { return false; }
  void add_note (pending_note *pn) override;
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

  void bifurcate (custom_edge_info *info) override;
  void terminate_path () override;

  const extrinsic_state *get_ext_state () const override { return NULL; }

  bool get_malloc_map (sm_state_map **,
		       const state_machine **,
		       unsigned *) override
  {
    return false;
  }
  bool get_taint_map (sm_state_map **,
		      const state_machine **,
		      unsigned *) override
  {
    return false;
  }

  const gimple *get_stmt () const override { return NULL; }
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
  bool warn (pending_diagnostic *d) override
  {
    return m_inner->warn (d);
  }

  void add_note (pending_note *pn) override
  {
    m_inner->add_note (pn);
  }

  void on_svalue_leak (const svalue *sval) override
  {
    m_inner->on_svalue_leak (sval);
  }

  void on_liveness_change (const svalue_set &live_svalues,
			   const region_model *model) override
  {
    m_inner->on_liveness_change (live_svalues, model);
  }

  logger *get_logger () override
  {
    return m_inner->get_logger ();
  }

  void on_condition (const svalue *lhs,
		     enum tree_code op,
		     const svalue *rhs) override
  {
    m_inner->on_condition (lhs, op, rhs);
  }

  void on_bounded_ranges (const svalue &sval,
			  const bounded_ranges &ranges) override
  {
    m_inner->on_bounded_ranges (sval, ranges);
  }

  void on_unknown_change (const svalue *sval, bool is_mutable) override
  {
    m_inner->on_unknown_change (sval, is_mutable);
  }

  void on_phi (const gphi *phi, tree rhs) override
  {
    m_inner->on_phi (phi, rhs);
  }

  void on_unexpected_tree_code (tree t,
				const dump_location_t &loc) override
  {
    m_inner->on_unexpected_tree_code (t, loc);
  }

  void on_escaped_function (tree fndecl) override
  {
    m_inner->on_escaped_function (fndecl);
  }

  uncertainty_t *get_uncertainty () override
  {
    return m_inner->get_uncertainty ();
  }

  void purge_state_involving (const svalue *sval) override
  {
    m_inner->purge_state_involving (sval);
  }

  void bifurcate (custom_edge_info *info) override
  {
    m_inner->bifurcate (info);
  }

  void terminate_path () override
  {
    m_inner->terminate_path ();
  }

  const extrinsic_state *get_ext_state () const override
  {
    return m_inner->get_ext_state ();
  }

  bool get_malloc_map (sm_state_map **out_smap,
		       const state_machine **out_sm,
		       unsigned *out_sm_idx) override
  {
    return m_inner->get_malloc_map (out_smap, out_sm, out_sm_idx);
  }

  bool get_taint_map (sm_state_map **out_smap,
		      const state_machine **out_sm,
		      unsigned *out_sm_idx) override
  {
    return m_inner->get_taint_map (out_smap, out_sm, out_sm_idx);
  }

  const gimple *get_stmt () const override
  {
    return m_inner->get_stmt ();
  }

protected:
  region_model_context_decorator (region_model_context *inner)
  : m_inner (inner)
  {
    gcc_assert (m_inner);
  }

  region_model_context *m_inner;
};

/* Subclass of region_model_context_decorator that adds a note
   when saving diagnostics.  */

class note_adding_context : public region_model_context_decorator
{
public:
  bool warn (pending_diagnostic *d) override
  {
    if (m_inner->warn (d))
      {
	add_note (make_note ());
	return true;
      }
    else
      return false;
  }

  /* Hook to make the new note.  */
  virtual pending_note *make_note () = 0;

protected:
  note_adding_context (region_model_context *inner)
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

  const region_model *m_model_a;
  const region_model *m_model_b;
  const program_point &m_point;
  region_model *m_merged_model;

  const extrinsic_state *m_ext_state;
  const program_state *m_state_a;
  const program_state *m_state_b;
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
  bool warn (pending_diagnostic *d) final override
  {
    m_diagnostics.safe_push (d);
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
