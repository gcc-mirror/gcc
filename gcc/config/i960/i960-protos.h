/* Definitions of target machine for GNU compiler, for Intel 80960
   Copyright (C) 2000
   Free Software Foundation, Inc.
   Contributed by Steven McGeady, Intel Corp.
   Additional Work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Jim Wilson and Michael Tiemann, Cygnus Support.

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

#ifndef GCC_I960_PROTOS_H
#define GCC_I960_PROTOS_H

#ifdef RTX_CODE
extern struct rtx_def *legitimize_address (rtx, rtx, enum machine_mode);
/* Define the function that build the compare insn for scc and bcc.  */

extern struct rtx_def *gen_compare_reg (enum rtx_code, rtx, rtx);

/* Define functions in i960.c and used in insn-output.c.  */

extern const char *i960_output_ldconst (rtx, rtx);
extern const char *i960_output_call_insn (rtx, rtx, rtx, rtx);
extern const char *i960_output_ret_insn (rtx);
extern const char *i960_output_move_double (rtx, rtx);
extern const char *i960_output_move_double_zero (rtx);
extern const char *i960_output_move_quad (rtx, rtx);
extern const char *i960_output_move_quad_zero (rtx);

extern int literal (rtx, enum machine_mode);
extern int hard_regno_mode_ok (int, enum machine_mode);
extern int fp_literal (rtx, enum machine_mode);
extern int signed_literal (rtx, enum machine_mode);
extern int legitimate_address_p (enum machine_mode, rtx, int);
extern void i960_print_operand (FILE *, rtx, int);
extern int fpmove_src_operand (rtx, enum machine_mode);
extern int arith_operand (rtx, enum machine_mode);
extern int logic_operand (rtx, enum machine_mode);
extern int fp_arith_operand (rtx, enum machine_mode);
extern int signed_arith_operand (rtx, enum machine_mode);
extern int fp_literal_one (rtx, enum machine_mode);
extern int fp_literal_zero (rtx, enum machine_mode);
extern int symbolic_memory_operand (rtx, enum machine_mode);
extern int eq_or_neq (rtx, enum machine_mode);
extern int arith32_operand (rtx, enum machine_mode);
extern int power2_operand (rtx, enum machine_mode);
extern int cmplpower2_operand (rtx, enum machine_mode);
extern enum machine_mode select_cc_mode (RTX_CODE, rtx);
extern int emit_move_sequence (rtx *, enum machine_mode);
extern int i960_bypass (rtx, rtx, rtx, int);
extern void i960_print_operand_addr (FILE *, rtx);
extern int i960_expr_alignment (rtx, int);
extern int i960_improve_align (rtx, rtx, int);
extern int i960_si_ti (rtx, rtx);
extern int i960_si_di (rtx, rtx);
#ifdef TREE_CODE
extern struct rtx_def *i960_function_arg (CUMULATIVE_ARGS *,
					  enum machine_mode,
					  tree, int);
extern rtx i960_va_arg (tree, tree);
extern void i960_va_start (tree, rtx);
#endif /* TREE_CODE */
extern enum reg_class secondary_reload_class (enum reg_class, enum machine_mode, rtx);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void i960_function_name_declare (FILE *, const char *, tree);
extern void i960_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern int i960_round_align (int, tree);
extern void i960_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int);
extern int i960_final_reg_parm_stack_space (int, tree);
extern int i960_reg_parm_stack_space (tree);
#endif /* TREE_CODE */

extern int process_pragma (int(*)(void), void(*)(int), const char *);
extern int i960_object_bytes_bitalign (int);
extern void i960_initialize (void);
extern int bitpos (unsigned int);
extern int is_mask (unsigned int);
extern int bitstr (unsigned int, int *, int *);
extern int compute_frame_size (int);
extern void output_function_profiler (FILE *, int);
extern void i960_scan_opcode (const char *);

extern void i960_pr_align (struct cpp_reader *);
extern void i960_pr_noalign (struct cpp_reader *);

#endif /* ! GCC_I960_PROTOS_H  */
