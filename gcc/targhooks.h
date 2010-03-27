/* Default target hook functions.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010
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
extern void default_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int);
extern rtx default_builtin_setjmp_frame_value (void);
extern bool default_pretend_outgoing_varargs_named (CUMULATIVE_ARGS *);

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
  (CUMULATIVE_ARGS *, enum machine_mode mode, const_tree, bool);
extern bool hook_callee_copies_named
  (CUMULATIVE_ARGS *ca, enum machine_mode, const_tree, bool);

extern void default_unwind_emit (FILE *, rtx);

extern bool default_scalar_mode_supported_p (enum machine_mode);
extern bool default_decimal_float_supported_p (void);
extern bool default_fixed_point_supported_p (void);

extern const char * default_invalid_within_doloop (const_rtx);

extern tree default_builtin_vectorized_function (tree, tree, tree);

extern tree default_builtin_vectorized_conversion (unsigned int, tree);

extern tree default_builtin_reciprocal (unsigned int, bool, bool);

extern bool default_builtin_vector_alignment_reachable (const_tree, bool);
extern bool
default_builtin_support_vector_misalignment (enum machine_mode mode,
					     const_tree,
					     int, bool);

/* These are here, and not in hooks.[ch], because not all users of
   hooks.h include tm.h, and thus we don't have CUMULATIVE_ARGS.  */

extern bool hook_bool_CUMULATIVE_ARGS_false (CUMULATIVE_ARGS *);
extern bool hook_bool_CUMULATIVE_ARGS_true (CUMULATIVE_ARGS *);

extern bool hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false
  (CUMULATIVE_ARGS *, enum machine_mode, const_tree, bool);
extern bool hook_bool_CUMULATIVE_ARGS_mode_tree_bool_true
  (CUMULATIVE_ARGS *, enum machine_mode, const_tree, bool);
extern int hook_int_CUMULATIVE_ARGS_mode_tree_bool_0
  (CUMULATIVE_ARGS *, enum machine_mode, tree, bool);
extern const char *hook_invalid_arg_for_unprototyped_fn
  (const_tree, const_tree, const_tree);
extern bool hook_bool_const_rtx_commutative_p (const_rtx, int);
extern rtx default_function_value (const_tree, const_tree, bool);
extern rtx default_libcall_value (enum machine_mode, const_rtx);
extern rtx default_internal_arg_pointer (void);
extern rtx default_static_chain (const_tree, bool);
extern void default_trampoline_init (rtx, tree, rtx);
extern enum reg_class default_branch_target_register_class (void);
#ifdef IRA_COVER_CLASSES
extern const enum reg_class *default_ira_cover_classes (void);
#endif
extern enum reg_class default_secondary_reload (bool, rtx, enum reg_class,
						enum machine_mode,
						secondary_reload_info *);
extern void hook_void_bitmap (bitmap);
extern bool default_handle_c_option (size_t, const char *, int);
extern int default_reloc_rw_mask (void);
extern tree default_mangle_decl_assembler_name (tree, tree);
extern tree default_emutls_var_fields (tree, tree *);
extern tree default_emutls_var_init (tree, tree, tree);
extern bool default_hard_regno_scratch_ok (unsigned int);
extern bool default_target_option_valid_attribute_p (tree, tree, tree, int);
extern bool default_target_option_pragma_parse (tree, tree);
extern bool default_target_can_inline_p (tree, tree);
extern bool default_valid_pointer_mode (enum machine_mode);
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
