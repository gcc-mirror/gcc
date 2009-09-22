/* Definitions of target machine for GNU compiler for IA-64.
   Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005, 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Functions defined in ia64.c */

extern int bundling_p;
#ifdef RTX_CODE
extern int ia64_st_address_bypass_p (rtx, rtx);
extern int ia64_ld_address_bypass_p (rtx, rtx);
extern int ia64_produce_address_p (rtx);

extern bool ia64_legitimate_constant_p (rtx);

extern rtx ia64_expand_move (rtx, rtx);
extern int ia64_move_ok (rtx, rtx);
extern int ia64_load_pair_ok (rtx, rtx);
extern int addp4_optimize_ok (rtx, rtx);
extern void ia64_emit_cond_move (rtx, rtx, rtx);
extern int ia64_depz_field_mask (rtx, rtx);
extern void ia64_split_tmode_move (rtx[]);
extern bool ia64_expand_movxf_movrf (enum machine_mode, rtx[]);
extern void ia64_expand_compare (rtx *, rtx *, rtx *);
extern void ia64_expand_vecint_cmov (rtx[]);
extern bool ia64_expand_vecint_minmax (enum rtx_code, enum machine_mode, rtx[]);
extern void ia64_expand_widen_sum (rtx[], bool);
extern void ia64_expand_dot_prod_v8qi (rtx[], bool);
extern void ia64_expand_call (rtx, rtx, rtx, int);
extern void ia64_split_call (rtx, rtx, rtx, rtx, rtx, int, int);
extern void ia64_reload_gp (void);
extern void ia64_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx);

extern HOST_WIDE_INT ia64_initial_elimination_offset (int, int);
extern void ia64_expand_prologue (void);
extern void ia64_expand_epilogue (int);

extern int ia64_direct_return (void);
extern bool ia64_expand_load_address (rtx, rtx);
extern int ia64_hard_regno_rename_ok (int, int);

extern void ia64_print_operand_address (FILE *, rtx);
extern void ia64_print_operand (FILE *, rtx, int);
extern enum reg_class ia64_preferred_reload_class (rtx, enum reg_class);
extern enum reg_class ia64_secondary_reload_class (enum reg_class,
						   enum machine_mode, rtx);
extern void process_for_unwind_directive (FILE *, rtx);
extern const char *get_bundle_name (int);
#endif /* RTX_CODE */

#ifdef TREE_CODE
#ifdef RTX_CODE
extern rtx ia64_function_arg (CUMULATIVE_ARGS *, enum machine_mode,
			      tree, int, int);
extern rtx ia64_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
extern rtx ia64_va_arg (tree, tree);
extern rtx ia64_function_value (const_tree, const_tree);
#endif /* RTX_CODE */

extern void ia64_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
				       tree, int);
extern int ia64_function_arg_boundary (enum machine_mode, tree);
extern void ia64_asm_output_external (FILE *, tree, const char *);
extern void ia64_vms_output_aligned_decl_common (FILE *, tree, const char *,
						 unsigned HOST_WIDE_INT,
						 unsigned int);
extern void ia64_vms_elf_asm_named_section (const char *, unsigned int, tree);
#endif /* TREE_CODE */

extern int ia64_register_move_cost (enum machine_mode, enum reg_class,
				    enum reg_class);
extern int ia64_epilogue_uses (int);
extern int ia64_eh_uses (int);
extern void emit_safe_across_calls (void);
extern void ia64_init_builtins (void);
extern void ia64_override_options (void);
extern int ia64_dbx_register_number (int);

extern rtx ia64_return_addr_rtx (HOST_WIDE_INT, rtx);
extern void ia64_split_return_addr_rtx (rtx);

#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction'.  */
extern enum direction ia64_hpux_function_arg_padding (enum machine_mode, const_tree);
#endif /* ARGS_SIZE_RTX */

extern void ia64_hpux_handle_builtin_pragma (struct cpp_reader *);
extern void ia64_output_function_profiler (FILE *, int);
extern void ia64_profile_hook (int);

extern void ia64_optimization_options (int, int);
extern void ia64_init_expanders (void);

extern rtx ia64_dconst_0_5 (void);
extern rtx ia64_dconst_0_375 (void);
