/* Consolidation of svalues and regions.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_REGION_MODEL_MANAGER_H
#define GCC_ANALYZER_REGION_MODEL_MANAGER_H

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

  unsigned get_num_symbols () const { return m_next_symbol_id; }
  unsigned alloc_symbol_id () { return m_next_symbol_id++; }

  /* call_string consolidation.  */
  const call_string &get_empty_call_string () const
  {
    return m_empty_call_string;
  }

  /* svalue consolidation.  */
  const svalue *get_or_create_constant_svalue (tree type, tree cst_expr);
  const svalue *get_or_create_constant_svalue (tree cst_expr);
  const svalue *get_or_create_int_cst (tree type, const poly_wide_int_ref &cst);
  const svalue *get_or_create_null_ptr (tree pointer_type);
  const svalue *get_or_create_unknown_svalue (tree type);
  const svalue *get_or_create_setjmp_svalue (const setjmp_record &r,
					     tree type);
  const svalue *get_or_create_poisoned_svalue (enum poison_kind kind,
					       tree type);
  const svalue *get_or_create_initial_value (const region *reg,
					     bool check_poisoned = true);
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
					       const function_point &point,
					       const svalue *base_svalue,
					       const svalue *iter_svalue);
  const svalue *get_or_create_compound_svalue (tree type,
					       const binding_map &map);
  const svalue *get_or_create_conjured_svalue (tree type, const gimple *stmt,
					       const region *id_reg,
					       const conjured_purge &p,
					       unsigned idx = 0);
  const svalue *
  get_or_create_asm_output_svalue (tree type,
				   const gasm *asm_stmt,
				   unsigned output_idx,
				   const vec<const svalue *> &inputs);
  const svalue *
  get_or_create_asm_output_svalue (tree type,
				   const char *asm_string,
				   unsigned output_idx,
				   unsigned num_outputs,
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
  const root_region *get_root_region () const { return &m_root_region; }
  const stack_region * get_stack_region () const { return &m_stack_region; }
  const heap_region *get_heap_region () const { return &m_heap_region; }
  const code_region *get_code_region () const { return &m_code_region; }
  const globals_region *get_globals_region () const
  {
    return &m_globals_region;
  }
  const errno_region *get_errno_region () const { return &m_errno_region; }
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
					const function &fun);
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

  store_manager *get_store_manager () { return &m_store_mgr; }
  bounded_ranges_manager *get_range_manager () const { return m_range_mgr; }

  known_function_manager *get_known_function_manager ()
  {
    return &m_known_fn_mgr;
  }

  /* Dynamically-allocated region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  const region *
  get_or_create_region_for_heap_alloc (const bitmap &base_regs_in_use);
  const region *create_region_for_alloca (const frame_region *frame);

  void log_stats (logger *logger, bool show_objs) const;

  void begin_checking_feasibility (void) { m_checking_feasibility = true; }
  void end_checking_feasibility (void) { m_checking_feasibility = false; }

  logger *get_logger () const { return m_logger; }

  void dump_untracked_regions () const;

  const svalue *maybe_fold_binop (tree type, enum tree_code op,
				  const svalue *arg0, const svalue *arg1);
private:
  bool too_complex_p (const complexity &c) const;
  bool reject_if_too_complex (svalue *sval);

  const svalue *maybe_fold_unaryop (tree type, enum tree_code op,
				    const svalue *arg);
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

  unsigned m_next_symbol_id;

  const call_string m_empty_call_string;

  root_region m_root_region;
  stack_region m_stack_region;
  heap_region m_heap_region;

  /* svalue consolidation.  */
  typedef hash_map<constant_svalue::key_t, constant_svalue *> constants_map_t;
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

  thread_local_region m_thread_local_region;
  errno_region m_errno_region;

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

  known_function_manager m_known_fn_mgr;

  /* "Dynamically-allocated" region instances.
     The number of these within the analysis can grow arbitrarily.
     They are still owned by the manager.  */
  auto_delete_vec<region> m_managed_dynamic_regions;
};

} // namespace ana

#endif /* GCC_ANALYZER_REGION_MODEL_MANAGER_H */
