/* Prototypes for alpha.c functions used in the md file & elsewhere.
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

extern void literal_section PARAMS ((void));
extern void override_options PARAMS ((void));
extern int zap_mask PARAMS ((HOST_WIDE_INT));
extern int direct_return PARAMS ((void));

extern int alpha_sa_size PARAMS ((void));
extern int alpha_pv_save_size PARAMS ((void));
extern int alpha_using_fp PARAMS ((void));
extern void alpha_write_verstamp PARAMS ((FILE *));
extern void alpha_expand_prologue PARAMS ((void));
extern void output_end_prologue PARAMS ((FILE *));
extern void alpha_expand_epilogue PARAMS ((void));
extern void alpha_output_filename PARAMS ((FILE *, const char *));
extern void alpha_output_lineno PARAMS ((FILE *, int));

#ifdef RTX_CODE
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_6bit_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_8bit_operand PARAMS ((rtx, enum machine_mode));
extern int cint8_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int sext_add_operand PARAMS ((rtx, enum machine_mode));
extern int const48_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int or_operand PARAMS ((rtx, enum machine_mode));
extern int mode_width_operand PARAMS ((rtx, enum machine_mode));
extern int mode_mask_operand PARAMS ((rtx, enum machine_mode));
extern int mul8_operand PARAMS ((rtx, enum machine_mode));
extern int fp0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_fp0_operand PARAMS ((rtx, enum machine_mode));
extern int hard_fp_register_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_operand PARAMS ((rtx, enum machine_mode));
extern int some_operand PARAMS ((rtx, enum machine_mode));
extern int some_ni_operand PARAMS ((rtx, enum machine_mode));
extern int input_operand PARAMS ((rtx, enum machine_mode));
extern int current_file_function_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int alpha_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int alpha_swapped_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int signed_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int alpha_fp_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int divmod_operator PARAMS ((rtx, enum machine_mode));
extern int aligned_memory_operand PARAMS ((rtx, enum machine_mode));
extern int unaligned_memory_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_unaligned_mem_operand PARAMS ((rtx, enum machine_mode));
extern int any_memory_operand PARAMS ((rtx, enum machine_mode));
extern int reg_not_elim_operand PARAMS ((rtx, enum machine_mode));
extern int normal_memory_operand PARAMS ((rtx, enum machine_mode));
extern int reg_no_subreg_operand PARAMS ((rtx, enum machine_mode));
extern int addition_operation PARAMS ((rtx, enum machine_mode));

extern void get_aligned_mem PARAMS ((rtx, rtx *, rtx *));
extern rtx get_unaligned_address PARAMS ((rtx, int));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						     enum machine_mode, 
						     rtx, int));
extern void alpha_set_memflags PARAMS ((rtx, rtx));
extern rtx alpha_emit_set_const PARAMS ((rtx, enum machine_mode,
					HOST_WIDE_INT, int));
extern rtx alpha_emit_set_long_const PARAMS ((rtx, HOST_WIDE_INT,
					     HOST_WIDE_INT));
extern rtx alpha_emit_conditional_branch PARAMS ((enum rtx_code));
extern rtx alpha_emit_conditional_move PARAMS ((rtx, enum machine_mode));
extern void alpha_emit_xfloating_arith PARAMS ((enum rtx_code, rtx[]));
extern void alpha_emit_xfloating_cvt PARAMS ((enum rtx_code, rtx[]));
extern void alpha_expand_unaligned_load PARAMS ((rtx, rtx, HOST_WIDE_INT,
						HOST_WIDE_INT, int));
extern void alpha_expand_unaligned_store PARAMS ((rtx, rtx, HOST_WIDE_INT,
						 HOST_WIDE_INT));
extern int alpha_expand_block_move PARAMS ((rtx []));
extern int alpha_expand_block_clear PARAMS ((rtx []));
extern int alpha_adjust_cost PARAMS ((rtx, rtx, rtx, int));
extern rtx alpha_return_addr PARAMS ((int, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void alpha_initialize_trampoline PARAMS ((rtx, rtx, rtx, int, int, int));
extern void alpha_reorg PARAMS ((rtx));
#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value PARAMS ((enum machine_mode,
				     REAL_VALUE_TYPE *, int));
#endif

#if OPEN_VMS
#ifdef HAVE_MACHINE_MODES
extern enum avms_arg_type alpha_arg_type PARAMS ((enum machine_mode));
#endif
extern rtx alpha_arg_info_reg_val PARAMS ((CUMULATIVE_ARGS));
extern void alpha_write_linkage PARAMS ((FILE *));
#endif /* OPEN_VMS */

extern void alpha_need_linkage PARAMS ((const char *, int));

#ifdef TREE_CODE
extern tree alpha_build_va_list PARAMS ((void));
#ifdef RTX_CODE
extern void alpha_va_start PARAMS ((int, tree, rtx));
extern rtx alpha_va_arg PARAMS ((tree, tree));
extern rtx function_arg PARAMS ((CUMULATIVE_ARGS, enum machine_mode,
				 tree, int));
#endif
extern int vms_valid_decl_attribute_p PARAMS ((tree, tree, tree, tree));
extern void alpha_start_function PARAMS ((FILE *, const char *, tree));
extern void alpha_end_function PARAMS ((FILE *, const char *, tree));
#endif /* TREE CODE */
