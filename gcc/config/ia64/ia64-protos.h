/* Definitions of target machine for GNU compiler for IA-64.
   Copyright (C) 1999, 2000, 2002, 2003, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Variables defined in ia64.c.  */

#ifdef RTX_CODE
extern GTY(()) rtx ia64_compare_op0;
extern GTY(()) rtx ia64_compare_op1;
#endif

/* Functions defined in ia64.c */

extern int bundling_p;
#ifdef RTX_CODE
extern int ia64_st_address_bypass_p (rtx, rtx);
extern int ia64_ld_address_bypass_p (rtx, rtx);
extern int ia64_produce_address_p (rtx);
extern int call_operand (rtx, enum machine_mode);
extern int sdata_symbolic_operand (rtx, enum machine_mode);
extern int got_symbolic_operand (rtx, enum machine_mode);
extern int symbolic_operand (rtx, enum machine_mode);
extern int tls_symbolic_operand (rtx, enum machine_mode);
extern int function_operand (rtx, enum machine_mode);
extern int setjmp_operand (rtx, enum machine_mode);
extern int move_operand (rtx, enum machine_mode);
extern int gr_register_operand (rtx, enum machine_mode);
extern int fr_register_operand (rtx, enum machine_mode);
extern int grfr_register_operand (rtx, enum machine_mode);
extern int gr_nonimmediate_operand (rtx, enum machine_mode);
extern int fr_nonimmediate_operand (rtx, enum machine_mode);
extern int grfr_nonimmediate_operand (rtx, enum machine_mode);
extern int gr_reg_or_0_operand (rtx, enum machine_mode);
extern int gr_reg_or_5bit_operand (rtx, enum machine_mode);
extern int gr_reg_or_6bit_operand (rtx, enum machine_mode);
extern int gr_reg_or_8bit_operand (rtx, enum machine_mode);
extern int grfr_reg_or_8bit_operand (rtx, enum machine_mode);
extern int gr_reg_or_8bit_adjusted_operand (rtx, enum machine_mode);
extern int gr_reg_or_8bit_and_adjusted_operand (rtx, enum machine_mode);
extern int gr_reg_or_14bit_operand (rtx, enum machine_mode);
extern int gr_reg_or_22bit_operand (rtx, enum machine_mode);
extern int shift_count_operand (rtx, enum machine_mode);
extern int shift_32bit_count_operand (rtx, enum machine_mode);
extern int shladd_operand (rtx, enum machine_mode);
extern int fetchadd_operand (rtx, enum machine_mode);
extern int fr_reg_or_fp01_operand (rtx, enum machine_mode);
extern int normal_comparison_operator (rtx, enum machine_mode);
extern int adjusted_comparison_operator (rtx, enum machine_mode);
extern int signed_inequality_operator (rtx, enum machine_mode);
extern int destination_operand (rtx, enum machine_mode);
extern int not_postinc_memory_operand (rtx, enum machine_mode);
extern int predicate_operator (rtx, enum machine_mode);
extern int ar_lc_reg_operand (rtx, enum machine_mode);
extern int ar_ccv_reg_operand (rtx, enum machine_mode);
extern int ar_pfs_reg_operand (rtx, enum machine_mode);
extern int general_xfmode_operand (rtx, enum machine_mode);
extern int destination_xfmode_operand (rtx, enum machine_mode);
extern int xfreg_or_fp01_operand (rtx, enum machine_mode);
extern int basereg_operand (rtx, enum machine_mode);

extern rtx ia64_expand_move (rtx, rtx);
extern int ia64_move_ok (rtx, rtx);
extern int addp4_optimize_ok (rtx, rtx);
extern void ia64_emit_cond_move (rtx, rtx, rtx);
extern int ia64_depz_field_mask (rtx, rtx);
extern void ia64_split_tmode_move (rtx[]);
extern rtx spill_xfmode_operand (rtx, int);
extern rtx ia64_expand_compare (enum rtx_code, enum machine_mode);
extern void ia64_expand_call (rtx, rtx, rtx, int);
extern void ia64_split_call (rtx, rtx, rtx, rtx, rtx, int, int);
extern void ia64_reload_gp (void);

extern HOST_WIDE_INT ia64_initial_elimination_offset (int, int);
extern void ia64_expand_prologue (void);
extern void ia64_expand_epilogue (int);

extern int ia64_direct_return (void);
extern void ia64_expand_load_address (rtx, rtx);
extern int ia64_hard_regno_rename_ok (int, int);

extern void ia64_initialize_trampoline (rtx, rtx, rtx);
extern void ia64_print_operand_address (FILE *, rtx);
extern void ia64_print_operand (FILE *, rtx, int);
extern enum reg_class ia64_secondary_reload_class (enum reg_class,
						   enum machine_mode, rtx);
extern void ia64_output_dwarf_dtprel (FILE*, int, rtx);
extern void process_for_unwind_directive (FILE *, rtx);
extern const char *get_bundle_name (int);
#endif /* RTX_CODE */

#ifdef TREE_CODE
#ifdef RTX_CODE
extern rtx ia64_function_arg (CUMULATIVE_ARGS *, enum machine_mode,
			      tree, int, int);
extern rtx ia64_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
extern rtx ia64_va_arg (tree, tree);
extern rtx ia64_function_value (tree, tree);
#endif /* RTX_CODE */

extern void ia64_setup_incoming_varargs (CUMULATIVE_ARGS, int, tree,
					 int *, int);
extern int ia64_function_arg_partial_nregs (CUMULATIVE_ARGS *,
					    enum machine_mode, tree, int);
extern void ia64_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
				       tree, int);
extern int ia64_function_arg_pass_by_reference (CUMULATIVE_ARGS *,
						enum machine_mode, tree, int);
extern int ia64_return_in_memory (tree);
extern void ia64_asm_output_external (FILE *, tree, const char *);
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

#ifdef SDATA_SECTION_ASM_OP
extern void sdata_section (void);
#endif

#ifdef SBSS_SECTION_ASM_OP
extern void sbss_section (void);
#endif

#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction'.  */
extern enum direction ia64_hpux_function_arg_padding (enum machine_mode, tree);
#endif /* ARGS_SIZE_RTX */

extern void ia64_hpux_handle_builtin_pragma (struct cpp_reader *);
