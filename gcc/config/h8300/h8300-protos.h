/* Definitions of target machine for GNU compiler.
   Hitachi H8/300 version generating coff
   Copyright (C) 2000, 2002, 2003 Free SoftwareFoundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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

#ifndef GCC_H8300_PROTOS_H
#define GCC_H8300_PROTOS_H

/* Declarations for functions used in insn-output.c.  */
#ifdef RTX_CODE
extern const char *output_a_shift PARAMS ((rtx *));
extern unsigned int compute_a_shift_length PARAMS ((rtx, rtx *));
extern const char *emit_a_rotate PARAMS ((enum rtx_code, rtx *));
extern const char *output_simode_bld PARAMS ((int, rtx[]));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern int const_costs PARAMS ((rtx, enum rtx_code, enum rtx_code));
extern int h8300_and_costs PARAMS ((rtx));
extern int h8300_shift_costs PARAMS ((rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void final_prescan_insn PARAMS ((rtx, rtx *, int));
extern int do_movsi PARAMS ((rtx[]));
extern void notice_update_cc PARAMS ((rtx, rtx));
extern const char *output_logical_op PARAMS ((enum machine_mode, rtx *));
extern unsigned int compute_logical_op_length PARAMS ((enum machine_mode,
						       rtx *));
extern int compute_logical_op_cc PARAMS ((enum machine_mode, rtx *));
extern int expand_a_shift PARAMS ((enum machine_mode, int, rtx[]));
extern int h8300_shift_needs_scratch_p PARAMS ((int, enum machine_mode));
extern int expand_a_rotate PARAMS ((enum rtx_code, rtx[]));
extern int fix_bit_operand PARAMS ((rtx *, int, enum rtx_code));
extern int h8300_adjust_insn_length PARAMS ((rtx, int));
extern void split_adds_subs PARAMS ((enum machine_mode, rtx[]));

extern int general_operand_src PARAMS ((rtx, enum machine_mode));
extern int general_operand_dst PARAMS ((rtx, enum machine_mode));
extern int single_one_operand PARAMS ((rtx, enum machine_mode));
extern int single_zero_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int two_insn_adds_subs_operand PARAMS ((rtx, enum machine_mode));
extern int small_call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int jump_address_operand PARAMS ((rtx, enum machine_mode));
extern int bit_operand PARAMS ((rtx, enum machine_mode));
extern int bit_memory_operand PARAMS ((rtx, enum machine_mode));
extern int stack_pointer_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_gt_2_operand PARAMS ((rtx, enum machine_mode));
extern int const_int_ge_8_operand PARAMS ((rtx, enum machine_mode));
extern int bit_operator PARAMS ((rtx, enum machine_mode));
extern int nshift_operator PARAMS ((rtx, enum machine_mode));

extern int h8300_eightbit_constant_address_p PARAMS ((rtx));
extern int h8300_tiny_constant_address_p PARAMS ((rtx));

/* Used in builtins.c */
extern rtx h8300_return_addr_rtx PARAMS ((int, rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg PARAMS ((CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int));
extern int h8300_funcvec_function_p PARAMS ((tree));
extern int h8300_eightbit_data_p PARAMS ((tree));
extern int h8300_tiny_data_p PARAMS ((tree));
#endif /* TREE_CODE */

extern void h8300_init_once PARAMS ((void));
extern void asm_file_start PARAMS ((FILE *));
extern void asm_file_end PARAMS ((FILE *));
extern int h8300_initial_elimination_offset PARAMS ((int, int));

#ifdef GCC_C_PRAGMA_H
extern void h8300_pr_interrupt PARAMS ((cpp_reader *));
extern void h8300_pr_saveall PARAMS ((cpp_reader *));
#endif

#endif /* ! GCC_H8300_PROTOS_H */
