/* Definitions of target machine for GNU compiler, for Intel 80960
   Copyright (C) 2000
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

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

#ifdef RTX_CODE
extern struct rtx_def *legitimize_address PARAMS ((rtx, rtx, enum machine_mode));
/* Define the function that build the compare insn for scc and bcc.  */

extern struct rtx_def *gen_compare_reg PARAMS ((enum rtx_code, rtx, rtx));

/* Define functions in i960.c and used in insn-output.c.  */

extern const char *i960_output_ldconst PARAMS ((rtx, rtx));
extern const char *i960_output_call_insn PARAMS ((rtx, rtx, rtx, rtx));
extern const char *i960_output_ret_insn PARAMS ((rtx));
extern const char *i960_output_move_double PARAMS ((rtx, rtx));
extern const char *i960_output_move_double_zero PARAMS ((rtx));
extern const char *i960_output_move_quad PARAMS ((rtx, rtx));
extern const char *i960_output_move_quad_zero PARAMS ((rtx));

extern int literal PARAMS ((rtx, enum machine_mode));
extern int hard_regno_mode_ok PARAMS ((int, enum machine_mode));
extern int fp_literal PARAMS ((rtx, enum machine_mode));
extern int signed_literal PARAMS ((rtx, enum machine_mode));
extern int legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern void i960_print_operand PARAMS ((FILE *, rtx, int));
extern int fpmove_src_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int logic_operand PARAMS ((rtx, enum machine_mode));
extern int fp_arith_operand PARAMS ((rtx, enum machine_mode));
extern int signed_arith_operand PARAMS ((rtx, enum machine_mode));
extern int fp_literal_one PARAMS ((rtx, enum machine_mode));
extern int fp_literal_zero PARAMS ((rtx, enum machine_mode));
extern int symbolic_memory_operand PARAMS ((rtx, enum machine_mode));
extern int eq_or_neq PARAMS ((rtx, enum machine_mode));
extern int arith32_operand PARAMS ((rtx, enum machine_mode));
extern int power2_operand PARAMS ((rtx, enum machine_mode));
extern int cmplpower2_operand PARAMS ((rtx, enum machine_mode));
extern enum machine_mode select_cc_mode PARAMS ((RTX_CODE, rtx));
extern int i960_address_cost PARAMS ((rtx));
extern int emit_move_sequence PARAMS ((rtx *, enum machine_mode));
extern int i960_bypass PARAMS ((rtx, rtx, rtx, int));
extern void i960_print_operand_addr PARAMS ((FILE *, rtx));
extern int i960_expr_alignment PARAMS ((rtx, int));
extern int i960_improve_align PARAMS ((rtx, rtx, int));
extern int i960_si_ti PARAMS ((rtx, rtx));
extern int i960_si_di PARAMS ((rtx, rtx));
#ifdef TREE_CODE
extern struct rtx_def *i960_function_arg PARAMS ((CUMULATIVE_ARGS *,
						  enum machine_mode,
						  tree, int));
extern rtx i960_va_arg PARAMS ((tree, tree));
extern void i960_va_start PARAMS ((int, tree, rtx));
#endif /* TREE_CODE */
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class, enum machine_mode, rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void i960_function_name_declare PARAMS ((FILE *, const char *, tree));
extern void i960_function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern int i960_round_align PARAMS ((int, tree));
extern void i960_setup_incoming_varargs PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int));
extern tree i960_build_va_list PARAMS ((void));
extern int i960_final_reg_parm_stack_space PARAMS ((int, tree));
extern int i960_reg_parm_stack_space PARAMS ((tree));
#endif /* TREE_CODE */

#ifdef REAL_VALUE_TYPE
extern void i960_output_long_double PARAMS ((FILE *, REAL_VALUE_TYPE));
extern void i960_output_double PARAMS ((FILE *, REAL_VALUE_TYPE));
extern void i960_output_float PARAMS ((FILE *, REAL_VALUE_TYPE));
#endif /* REAL_VALUE_TYPE */

extern int process_pragma PARAMS ((int(*)(void), void(*)(int), const char *));
extern int i960_object_bytes_bitalign PARAMS ((int));
extern void i960_initialize PARAMS ((void));
extern int bitpos PARAMS ((unsigned int));
extern int is_mask PARAMS ((unsigned int));
extern int bitstr PARAMS ((unsigned int, int *, int *));
extern int compute_frame_size PARAMS ((int));
extern void i960_function_prologue PARAMS ((FILE *, unsigned int));
extern void output_function_profiler PARAMS ((FILE *, int));
extern void i960_function_epilogue PARAMS ((FILE *, unsigned int));
extern void i960_scan_opcode PARAMS ((const char *));
