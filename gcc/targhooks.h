/* Default target hook functions.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

extern bool default_legitimate_address_p (enum machine_mode, rtx, bool);

extern void default_external_libcall (rtx);
extern rtx default_legitimize_address (rtx, rtx, enum machine_mode);

extern int default_unspec_may_trap_p (const_rtx, unsigned);
extern enum machine_mode default_promote_function_mode (const_tree, enum machine_mode,
							int *, const_tree, int);
extern enum machine_mode default_promote_function_mode_always_promote
			(const_tree, enum machine_mode, int *, const_tree, int);

extern enum machine_mode default_cc_modes_compatible (enum machine_mode,
						      enum machine_mode);

extern bool default_return_in_memory (const_tree, const_tree);

extern rtx default_expand_builtin_saveregs (void);
extern void default_setup_incoming_varargs (cumulative_args_t, enum machine_mode, tree, int *, int);
extern rtx default_builtin_setjmp_frame_value (void);
extern bool default_pretend_outgoing_varargs_named (cumulative_args_t);

extern enum machine_mode default_eh_return_filter_mode (void);
extern enum machine_mode default_libgcc_cmp_return_mode (void);
extern enum machine_mode default_libgcc_shift_count_mode (void);
extern enum machine_mode default_unwind_word_mode (void);
extern unsigned HOST_WIDE_INT default_shift_truncation_mask
  (enum machine_mode);
extern unsigned int default_min_divisions_for_recip_mul (enum machine_mode);
extern int default_mode_rep_extended (enum machine_mode, enum machine_mode);

extern tree default_stack_protect_guard (void);
extern tree default_external_stack_protect_fail (void);
extern tree default_hidden_stack_protect_fail (void);

extern enum machine_mode default_mode_for_suffix (char);

extern tree default_cxx_guard_type (void);
extern tree default_cxx_get_cookie_size (tree);

extern bool hook_pass_by_reference_must_pass_in_stack
  (cumulative_args_t, enum machine_mode mode, const_tree, bool);
extern bool hook_callee_copies_named
  (cumulative_args_t ca, enum machine_mode, const_tree, bool);

extern void default_print_operand (FILE *, rtx, int);
extern void default_print_operand_address (FILE *, rtx);
extern bool default_print_operand_punct_valid_p (unsigned char);
extern tree default_mangle_assembler_name (const char *);

extern bool default_scalar_mode_supported_p (enum machine_mode);
extern bool targhook_words_big_endian (void);
extern bool targhook_float_words_big_endian (void);
extern bool default_decimal_float_supported_p (void);
extern bool default_fixed_point_supported_p (void);

extern const char * default_invalid_within_doloop (const_rtx);

extern tree default_builtin_vectorized_function (tree, tree, tree);

extern tree default_builtin_vectorized_conversion (unsigned int, tree, tree);

extern int default_builtin_vectorization_cost (enum vect_cost_for_stmt, tree, int);

extern tree default_builtin_reciprocal (unsigned int, bool, bool);

extern HOST_WIDE_INT default_vector_alignment (const_tree);

extern bool default_builtin_vector_alignment_reachable (const_tree, bool);
extern bool
default_builtin_support_vector_misalignment (enum machine_mode mode,
					     const_tree,
					     int, bool);
extern enum machine_mode default_preferred_simd_mode (enum machine_mode mode);
extern unsigned int default_autovectorize_vector_sizes (void);

/* These are here, and not in hooks.[ch], because not all users of
   hooks.h include tm.h, and thus we don't have CUMULATIVE_ARGS.  */

extern bool hook_bool_CUMULATIVE_ARGS_false (cumulative_args_t);
extern bool hook_bool_CUMULATIVE_ARGS_true (cumulative_args_t);

extern bool hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false
  (cumulative_args_t, enum machine_mode, const_tree, bool);
extern bool hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true
  (cumulative_args_t, enum machine_mode, const_tree, bool);
extern int hook_int_CUMULATIVE_ARGS_mode_tree_bool_0
  (cumulative_args_t, enum machine_mode, tree, bool);
extern const char *hook_invalid_arg_for_unprototyped_fn
  (const_tree, const_tree, const_tree);
extern void default_function_arg_advance
  (cumulative_args_t, enum machine_mode, const_tree, bool);
extern rtx default_function_arg
  (cumulative_args_t, enum machine_mode, const_tree, bool);
extern rtx default_function_incoming_arg
  (cumulative_args_t, enum machine_mode, const_tree, bool);
extern unsigned int default_function_arg_boundary (enum machine_mode,
						   const_tree);
extern unsigned int default_function_arg_round_boundary (enum machine_mode,
							 const_tree);
extern bool hook_bool_const_rtx_commutative_p (const_rtx, int);
extern rtx default_function_value (const_tree, const_tree, bool);
extern rtx default_libcall_value (enum machine_mode, const_rtx);
extern bool default_function_value_regno_p (const unsigned int);
extern rtx default_internal_arg_pointer (void);
extern rtx default_static_chain (const_tree, bool);
extern void default_trampoline_init (rtx, tree, rtx);
extern int default_return_pops_args (tree, tree, int);
extern reg_class_t default_branch_target_register_class (void);
extern reg_class_t default_secondary_reload (bool, rtx, reg_class_t,
					     enum machine_mode,
					     secondary_reload_info *);
extern void default_target_option_override (void);
extern void hook_void_bitmap (bitmap);
extern int default_reloc_rw_mask (void);
extern tree default_mangle_decl_assembler_name (tree, tree);
extern tree default_emutls_var_fields (tree, tree *);
extern tree default_emutls_var_init (tree, tree, tree);
extern bool default_hard_regno_scratch_ok (unsigned int);
extern bool default_mode_dependent_address_p (const_rtx addr);
extern bool default_target_option_valid_attribute_p (tree, tree, tree, int);
extern bool default_target_option_pragma_parse (tree, tree);
extern bool default_target_can_inline_p (tree, tree);
extern bool default_valid_pointer_mode (enum machine_mode);
extern bool default_ref_may_alias_errno (struct ao_ref_s *);
extern enum machine_mode default_addr_space_pointer_mode (addr_space_t);
extern enum machine_mode default_addr_space_address_mode (addr_space_t);
extern bool default_addr_space_valid_pointer_mode (enum machine_mode,
						   addr_space_t);
extern bool default_addr_space_legitimate_address_p (enum machine_mode, rtx,
						     bool, addr_space_t);
extern rtx default_addr_space_legitimize_address (rtx, rtx, enum machine_mode,
						  addr_space_t);
extern bool default_addr_space_subset_p (addr_space_t, addr_space_t);
extern rtx default_addr_space_convert (rtx, tree, tree);
extern unsigned int default_case_values_threshold (void);
extern bool default_have_conditional_execution (void);

extern tree default_builtin_tm_load_store (tree);

extern int default_memory_move_cost (enum machine_mode, reg_class_t, bool);
extern int default_register_move_cost (enum machine_mode, reg_class_t,
				       reg_class_t);

extern bool default_profile_before_prologue (void);
extern reg_class_t default_preferred_reload_class (rtx, reg_class_t);
extern reg_class_t default_preferred_output_reload_class (rtx, reg_class_t);
extern reg_class_t default_preferred_rename_class (reg_class_t rclass);
extern bool default_class_likely_spilled_p (reg_class_t);
extern unsigned char default_class_max_nregs (reg_class_t, enum machine_mode);

extern enum unwind_info_type default_debug_unwind_info (void);

extern int default_label_align_after_barrier_max_skip (rtx);
extern int default_loop_align_max_skip (rtx);
extern int default_label_align_max_skip (rtx);
extern int default_jump_align_max_skip (rtx);
extern section * default_function_section(tree decl, enum node_frequency freq,
					  bool startup, bool exit);
extern enum machine_mode default_get_reg_raw_mode(int);

extern void *default_get_pch_validity (size_t *);
extern const char *default_pch_valid_p (const void *, size_t);
