/* Definitions of target machine for GNU compiler for IA-64.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Variables defined in ia64.c.  */

#ifdef RTX_CODE
extern rtx ia64_compare_op0, ia64_compare_op1;
#endif

/* Functions defined in ia64.c */

#ifdef RTX_CODE
extern int call_operand PARAMS((rtx, enum machine_mode));
extern int sdata_symbolic_operand PARAMS((rtx, enum machine_mode));
extern int got_symbolic_operand PARAMS((rtx, enum machine_mode));
extern int symbolic_operand PARAMS((rtx, enum machine_mode));
extern int function_operand PARAMS((rtx, enum machine_mode));
extern int setjmp_operand PARAMS((rtx, enum machine_mode));
extern int move_operand PARAMS((rtx, enum machine_mode));
extern int gr_register_operand PARAMS((rtx, enum machine_mode));
extern int fr_register_operand PARAMS((rtx, enum machine_mode));
extern int grfr_register_operand PARAMS((rtx, enum machine_mode));
extern int gr_nonimmediate_operand PARAMS((rtx, enum machine_mode));
extern int fr_nonimmediate_operand PARAMS((rtx, enum machine_mode));
extern int grfr_nonimmediate_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_0_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_5bit_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_6bit_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_8bit_operand PARAMS((rtx, enum machine_mode));
extern int grfr_reg_or_8bit_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_8bit_adjusted_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_8bit_and_adjusted_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_14bit_operand PARAMS((rtx, enum machine_mode));
extern int gr_reg_or_22bit_operand PARAMS((rtx, enum machine_mode));
extern int shift_count_operand PARAMS((rtx, enum machine_mode));
extern int shift_32bit_count_operand PARAMS((rtx, enum machine_mode));
extern int shladd_operand PARAMS((rtx, enum machine_mode));
extern int fetchadd_operand PARAMS((rtx, enum machine_mode));
extern int fr_reg_or_fp01_operand PARAMS((rtx, enum machine_mode));
extern int normal_comparison_operator PARAMS((rtx, enum machine_mode));
extern int adjusted_comparison_operator PARAMS((rtx, enum machine_mode));
extern int signed_inequality_operator PARAMS((rtx, enum machine_mode));
extern int destination_operand PARAMS((rtx, enum machine_mode));
extern int not_postinc_memory_operand PARAMS((rtx, enum machine_mode));
extern int predicate_operator PARAMS((rtx, enum machine_mode));
extern int ar_lc_reg_operand PARAMS((rtx, enum machine_mode));
extern int ar_ccv_reg_operand PARAMS((rtx, enum machine_mode));
extern int ar_pfs_reg_operand PARAMS((rtx, enum machine_mode));
extern int general_tfmode_operand PARAMS((rtx, enum machine_mode));
extern int destination_tfmode_operand PARAMS((rtx, enum machine_mode));
extern int tfreg_or_fp01_operand PARAMS((rtx, enum machine_mode));

extern int ia64_move_ok PARAMS((rtx, rtx));
extern int ia64_depz_field_mask PARAMS((rtx, rtx));
extern rtx ia64_gp_save_reg PARAMS((int));
extern rtx ia64_split_timode PARAMS((rtx[], rtx, rtx));
extern rtx spill_tfmode_operand PARAMS((rtx, int));
extern rtx ia64_expand_compare PARAMS((enum rtx_code, enum machine_mode));
extern void ia64_expand_call PARAMS((rtx, rtx, rtx, int));

extern HOST_WIDE_INT ia64_initial_elimination_offset PARAMS((int, int));
extern void ia64_expand_prologue PARAMS((void));
extern void ia64_expand_epilogue PARAMS((int));

extern int ia64_direct_return PARAMS((void));
extern void ia64_expand_load_address PARAMS((rtx, rtx, rtx));
extern int ia64_hard_regno_rename_ok PARAMS((int, int));

extern void ia64_initialize_trampoline PARAMS((rtx, rtx, rtx));
extern void ia64_print_operand_address PARAMS((FILE *, rtx));
extern void ia64_print_operand PARAMS((FILE *, rtx, int));
extern enum reg_class ia64_secondary_reload_class PARAMS((enum reg_class,
							  enum machine_mode,
							  rtx));
extern void ia64_reorg PARAMS((rtx));
extern void process_for_unwind_directive PARAMS ((FILE *, rtx));
extern const char *get_bundle_name PARAMS ((int));
#endif /* RTX_CODE */

#ifdef TREE_CODE
#ifdef RTX_CODE
extern rtx ia64_function_arg PARAMS((CUMULATIVE_ARGS *, enum machine_mode,
				     tree, int, int));
extern rtx ia64_expand_builtin PARAMS((tree, rtx, rtx,
				       enum machine_mode, int));
extern void ia64_va_start PARAMS((int, tree, rtx));
extern rtx ia64_va_arg PARAMS((tree, tree));
extern rtx ia64_function_value PARAMS((tree, tree));
#endif /* RTX_CODE */

extern void ia64_setup_incoming_varargs PARAMS((CUMULATIVE_ARGS, int, tree,
						int *, int));
extern int ia64_function_arg_partial_nregs PARAMS((CUMULATIVE_ARGS *,
						   enum machine_mode,
						   tree, int));
extern void ia64_function_arg_advance PARAMS((CUMULATIVE_ARGS *,
					      enum machine_mode,
					      tree, int));
extern int ia64_return_in_memory PARAMS((tree));
extern void ia64_asm_output_external PARAMS((FILE *, tree, const char *));

extern void ia64_encode_section_info PARAMS((tree));
#endif /* TREE_CODE */

extern int ia64_register_move_cost PARAMS((enum machine_mode, enum reg_class,
					   enum reg_class));
extern int ia64_epilogue_uses PARAMS((int));
extern void emit_safe_across_calls PARAMS((FILE *));
extern void ia64_init_builtins PARAMS((void));
extern void ia64_override_options PARAMS((void));
extern int ia64_dbx_register_number PARAMS((int));

#ifdef SDATA_SECTION_ASM_OP
extern void sdata_section PARAMS ((void));
#endif

#ifdef SBSS_SECTION_ASM_OP
extern void sbss_section PARAMS ((void));
#endif

#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction'.  */
extern enum direction ia64_hpux_function_arg_padding PARAMS ((enum machine_mode, tree));
#endif /* ARGS_SIZE_RTX */
