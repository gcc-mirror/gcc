/* Definitions of target machine for GNU compiler for iq2000.
   Copyright (C) 2003 Free Software Foundation, Inc.

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

#ifndef GCC_IQ2000_PROTOS_H
#define GCC_IQ2000_PROTOS_H

extern int iq2000_check_split PARAMS ((rtx, enum machine_mode));
extern int iq2000_reg_mode_ok_for_base_p PARAMS ((rtx, enum machine_mode, int));
extern int iq2000_legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern const char* iq2000_fill_delay_slot PARAMS ((const char*, enum delay_type, rtx*, rtx));
extern const char *iq2000_move_1word PARAMS ((rtx *, rtx, int));
extern int iq2000_address_cost PARAMS ((rtx));
extern void override_options PARAMS ((void));
extern HOST_WIDE_INT iq2000_debugger_offset PARAMS ((rtx, HOST_WIDE_INT));
extern void final_prescan_insn PARAMS ((rtx, rtx*, int));
extern HOST_WIDE_INT compute_frame_size PARAMS ((HOST_WIDE_INT));
extern int iq2000_initial_elimination_offset (int, int);
extern void iq2000_expand_prologue PARAMS ((void));
extern void iq2000_expand_epilogue PARAMS ((void));
extern void iq2000_expand_eh_return PARAMS ((rtx));
extern int iq2000_can_use_return_insn PARAMS ((void));
int function_arg_pass_by_reference PARAMS ((CUMULATIVE_ARGS*, enum machine_mode, tree, int));
int iq2000_adjust_insn_length PARAMS ((rtx, int));
char *iq2000_output_conditional_branch PARAMS ((rtx, rtx*, int, int, int, int));
extern void iq2000_init_builtins PARAMS ((void));
extern void iq2000_setup_incoming_varargs PARAMS ((CUMULATIVE_ARGS, int, tree, int*, int));
extern void print_operand_address PARAMS ((FILE*, rtx));
extern void print_operand PARAMS ((FILE*, rtx, int));

#ifdef RTX_CODE
extern rtx gen_int_relational PARAMS ((enum rtx_code, rtx, rtx, rtx, int*));
extern void gen_conditional_branch PARAMS ((rtx *, enum rtx_code));
#endif

#ifdef TREE_CODE
extern void init_cumulative_args PARAMS ((CUMULATIVE_ARGS*, tree, rtx));
extern void function_arg_advance PARAMS ((CUMULATIVE_ARGS*, enum machine_mode, tree, int));
extern struct rtx_def* function_arg PARAMS ((CUMULATIVE_ARGS*, enum machine_mode, tree, int));
extern int function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS*, enum machine_mode, tree, int));
extern void iq2000_va_start PARAMS ((tree, rtx));
extern rtx iq2000_va_arg PARAMS ((tree, tree));
extern rtx iq2000_function_value PARAMS ((tree, tree));
extern rtx iq2000_expand_builtin PARAMS ((tree, rtx, rtx,
					  enum machine_mode, int));
#endif

#endif /* ! GCC_IQ2000_PROTOS_H */
