/* Prototypes for alpha.c functions used in the md file & elsewhere.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

extern void literal_section PROTO ((void));
extern void override_options PROTO ((void));
extern int zap_mask PROTO ((HOST_WIDE_INT));
extern int direct_return PROTO ((void));

extern int alpha_sa_size PROTO ((void));
extern int alpha_pv_save_size PROTO ((void));
extern int alpha_using_fp PROTO ((void));
extern void alpha_write_verstamp PROTO ((FILE *));
extern void alpha_expand_prologue PROTO ((void));
extern void output_end_prologue PROTO ((FILE *));
extern void alpha_expand_epilogue PROTO ((void));
extern void alpha_output_filename PROTO ((FILE *, const char *));
extern void alpha_output_lineno PROTO ((FILE *, int));

#ifdef RTX_CODE
extern int reg_or_0_operand PROTO ((rtx, enum machine_mode));
extern int reg_or_6bit_operand PROTO ((rtx, enum machine_mode));
extern int reg_or_8bit_operand PROTO ((rtx, enum machine_mode));
extern int cint8_operand PROTO ((rtx, enum machine_mode));
extern int add_operand PROTO ((rtx, enum machine_mode));
extern int sext_add_operand PROTO ((rtx, enum machine_mode));
extern int const48_operand PROTO ((rtx, enum machine_mode));
extern int and_operand PROTO ((rtx, enum machine_mode));
extern int or_operand PROTO ((rtx, enum machine_mode));
extern int mode_width_operand PROTO ((rtx, enum machine_mode));
extern int mode_mask_operand PROTO ((rtx, enum machine_mode));
extern int mul8_operand PROTO ((rtx, enum machine_mode));
extern int fp0_operand PROTO ((rtx, enum machine_mode));
extern int reg_or_fp0_operand PROTO ((rtx, enum machine_mode));
extern int hard_fp_register_operand PROTO ((rtx, enum machine_mode));
extern int reg_or_cint_operand PROTO ((rtx, enum machine_mode));
extern int some_operand PROTO ((rtx, enum machine_mode));
extern int some_ni_operand PROTO ((rtx, enum machine_mode));
extern int input_operand PROTO ((rtx, enum machine_mode));
extern int current_file_function_operand PROTO ((rtx, enum machine_mode));
extern int call_operand PROTO ((rtx, enum machine_mode));
extern int alpha_comparison_operator PROTO ((rtx, enum machine_mode));
extern int alpha_swapped_comparison_operator PROTO ((rtx, enum machine_mode));
extern int signed_comparison_operator PROTO ((rtx, enum machine_mode));
extern int divmod_operator PROTO ((rtx, enum machine_mode));
extern int aligned_memory_operand PROTO ((rtx, enum machine_mode));
extern int unaligned_memory_operand PROTO ((rtx, enum machine_mode));
extern int reg_or_unaligned_mem_operand PROTO ((rtx, enum machine_mode));
extern int any_memory_operand PROTO ((rtx, enum machine_mode));
extern int reg_not_elim_operand PROTO ((rtx, enum machine_mode));
extern int normal_memory_operand PROTO ((rtx, enum machine_mode));
extern int reg_no_subreg_operand PROTO ((rtx, enum machine_mode));
extern int addition_operation PROTO ((rtx, enum machine_mode));

extern void get_aligned_mem PROTO ((rtx, rtx *, rtx *));
extern rtx get_unaligned_address PROTO ((rtx, int));
extern enum reg_class secondary_reload_class PROTO ((enum reg_class,
						     enum machine_mode, 
						     rtx, int));
extern void alpha_set_memflags PROTO ((rtx, rtx));
extern rtx alpha_emit_set_const PROTO ((rtx, enum machine_mode,
					HOST_WIDE_INT, int));
extern rtx alpha_emit_set_long_const PROTO ((rtx, HOST_WIDE_INT,
					     HOST_WIDE_INT));
extern rtx alpha_emit_conditional_branch PROTO ((enum rtx_code));
extern rtx alpha_emit_conditional_move PROTO ((rtx, enum machine_mode));
extern void alpha_expand_unaligned_load PROTO ((rtx, rtx, HOST_WIDE_INT,
						HOST_WIDE_INT, int));
extern void alpha_expand_unaligned_store PROTO ((rtx, rtx, HOST_WIDE_INT,
						 HOST_WIDE_INT));
extern int alpha_expand_block_move PROTO ((rtx []));
extern int alpha_expand_block_clear PROTO ((rtx []));
extern int alpha_adjust_cost PROTO ((rtx, rtx, rtx, int));
extern rtx alpha_return_addr PROTO ((int, rtx));
extern void print_operand PROTO ((FILE *, rtx, int));
extern void print_operand_address PROTO ((FILE *, rtx));
extern void alpha_initialize_trampoline PROTO ((rtx, rtx, rtx, int, int, int));
extern void alpha_reorg PROTO ((rtx));
#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern int check_float_value PROTO ((enum machine_mode,
				     REAL_VALUE_TYPE *, int));
#endif

#if OPEN_VMS
#ifdef HAVE_MACHINE_MODES
extern enum avms_arg_type alpha_arg_type PROTO ((enum machine_mode));
#endif
extern rtx alpha_arg_info_reg_val PROTO ((CUMULATIVE_ARGS));
extern void alpha_write_linkage PROTO ((FILE *));
#endif /* OPEN_VMS */

extern void alpha_need_linkage PROTO ((const char *, int));

#ifdef TREE_CODE
extern tree alpha_build_va_list PROTO ((void));
#ifdef RTX_CODE
extern void alpha_va_start PROTO ((int, tree, rtx));
extern rtx alpha_va_arg PROTO ((tree, tree));
#endif
extern int vms_valid_decl_attribute_p PROTO ((tree, tree, tree, tree));
extern void alpha_start_function PROTO ((FILE *, const char *, tree));
extern void alpha_end_function PROTO ((FILE *, const char *, tree));
#endif /* TREE CODE */
