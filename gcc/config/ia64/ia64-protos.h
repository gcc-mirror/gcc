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
Boston, MA 02111-1307, USA. */

/* Variables defined in ia64.c.  */

#ifdef RTX_CODE
extern rtx ia64_compare_op0, ia64_compare_op1;
#endif

/* Functions defined in ia64.c */

#ifdef RTX_CODE
extern int call_operand PARAMS((rtx, enum machine_mode));
extern int sdata_symbolic_operand PARAMS((rtx, enum machine_mode));
extern int symbolic_operand PARAMS((rtx, enum machine_mode));
extern int function_operand PARAMS((rtx, enum machine_mode));
extern int setjmp_operand PARAMS((rtx, enum machine_mode));
extern int move_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_6bit_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_8bit_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_8bit_adjusted_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_8bit_and_adjusted_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_14bit_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_22bit_operand PARAMS((rtx, enum machine_mode));
extern int shift_count_operand PARAMS((rtx, enum machine_mode));
extern int shift_32bit_count_operand PARAMS((rtx, enum machine_mode));
extern int shladd_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_fp01_operand PARAMS((rtx, enum machine_mode));
extern int normal_comparison_operator PARAMS((rtx, enum machine_mode));
extern int adjusted_comparison_operator PARAMS((rtx, enum machine_mode));
extern int call_multiple_values_operation PARAMS((rtx, enum machine_mode));
#endif
extern int ia64_rap_fp_offset PARAMS((void));
extern unsigned int ia64_compute_frame_size PARAMS((int));
extern void save_restore_insns PARAMS((int));
extern void ia64_expand_prologue PARAMS((void));
extern void ia64_expand_epilogue PARAMS((void));
extern void ia64_function_prologue PARAMS((FILE *, int));
extern void ia64_funtion_epilogue PARAMS((FILE *, int));
extern int ia64_direct_return PARAMS((void));
#ifdef TREE_CODE
extern void ia64_setup_incoming_varargs PARAMS((CUMULATIVE_ARGS, int, tree,
						int *, int));
#ifdef RTX_CODE
extern rtx ia64_function_arg PARAMS((CUMULATIVE_ARGS *, enum machine_mode,
				     tree, int, int));
extern void ia64_init_builtins PARAMS((void));
extern rtx ia64_expand_builtin PARAMS((tree, rtx, rtx, enum machine_mode, int));
#endif
extern int ia64_function_arg_partial_nregs PARAMS((CUMULATIVE_ARGS *,
						   enum machine_mode,
						   tree, int));
extern void ia64_function_arg_advance PARAMS((CUMULATIVE_ARGS *,
					      enum machine_mode,
					      tree, int));
#ifdef RTX_CODE
extern void ia64_va_start PARAMS((int, tree, rtx));
extern rtx ia64_va_arg PARAMS((tree, tree));
#endif
extern int ia64_return_in_memory PARAMS((tree));
#ifdef RTX_CODE
extern rtx ia64_function_value PARAMS((tree, tree));
#endif
#endif
#ifdef RTX_CODE
extern void ia64_print_operand_address PARAMS((FILE *, rtx));
extern void ia64_print_operand PARAMS((FILE *, rtx, int));
extern enum reg_class ia64_secondary_reload_class PARAMS((enum reg_class,
							  enum machine_mode,
							  rtx));
#endif
#ifdef TREE_CODE
extern void ia64_asm_output_external PARAMS((FILE *, tree, char *));
#endif
extern void ia64_override_options PARAMS((void));
#ifdef RTX_CODE
extern void ia64_reorg PARAMS((rtx));
#endif
extern int ia64_epilogue_uses PARAMS((int));
#ifdef TREE_CODE
extern int ia64_valid_type_attribute PARAMS((tree, tree, tree, tree));
extern void ia64_encode_section_info PARAMS((tree));
#endif
