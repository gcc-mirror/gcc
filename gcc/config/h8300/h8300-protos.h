/* Definitions of target machine for GNU compiler.
   Renesas H8/300 version
   Copyright (C) 2000, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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

#ifndef GCC_H8300_PROTOS_H
#define GCC_H8300_PROTOS_H

/* Declarations for functions used in insn-output.c.  */
#ifdef RTX_CODE
extern unsigned int compute_mov_length (rtx *);
extern const char *output_plussi (rtx *);
extern unsigned int compute_plussi_length (rtx *);
extern int compute_plussi_cc (rtx *);
extern const char *output_a_shift (rtx *);
extern unsigned int compute_a_shift_length (rtx, rtx *);
extern int compute_a_shift_cc (rtx, rtx *);
extern const char *output_a_rotate (enum rtx_code, rtx *);
extern unsigned int compute_a_rotate_length (rtx *);
extern const char *output_simode_bld (int, rtx[]);
extern void print_operand_address (FILE *, rtx);
extern void print_operand (FILE *, rtx, int);
extern void final_prescan_insn (rtx, rtx *, int);
extern int h8300_expand_movsi (rtx[]);
extern void notice_update_cc (rtx, rtx);
extern const char *output_logical_op (enum machine_mode, rtx *);
extern unsigned int compute_logical_op_length (enum machine_mode,
					       rtx *);
extern int compute_logical_op_cc (enum machine_mode, rtx *);
extern void expand_a_shift (enum machine_mode, int, rtx[]);
extern int h8300_shift_needs_scratch_p (int, enum machine_mode);
extern int expand_a_rotate (enum rtx_code, rtx[]);
extern int fix_bit_operand (rtx *, int, enum rtx_code);
extern int h8300_adjust_insn_length (rtx, int);
extern void split_adds_subs (enum machine_mode, rtx[]);

extern int general_operand_src (rtx, enum machine_mode);
extern int general_operand_dst (rtx, enum machine_mode);
extern int single_one_operand (rtx, enum machine_mode);
extern int single_zero_operand (rtx, enum machine_mode);
extern int call_insn_operand (rtx, enum machine_mode);
extern int two_insn_adds_subs_operand (rtx, enum machine_mode);
extern int small_call_insn_operand (rtx, enum machine_mode);
extern int jump_address_operand (rtx, enum machine_mode);
extern int bit_operand (rtx, enum machine_mode);
extern int bit_memory_operand (rtx, enum machine_mode);
extern int stack_pointer_operand (rtx, enum machine_mode);
extern int const_int_gt_2_operand (rtx, enum machine_mode);
extern int const_int_ge_8_operand (rtx, enum machine_mode);
extern int const_int_qi_operand (rtx, enum machine_mode);
extern int const_int_hi_operand (rtx, enum machine_mode);
extern int incdec_operand (rtx, enum machine_mode);
extern int bit_operator (rtx, enum machine_mode);
extern int nshift_operator (rtx, enum machine_mode);
extern int eqne_operator (rtx, enum machine_mode);
extern int gtle_operator (rtx, enum machine_mode);
extern int gtuleu_operator (rtx, enum machine_mode);
extern int iorxor_operator (rtx, enum machine_mode);

extern int h8300_eightbit_constant_address_p (rtx);
extern int h8300_tiny_constant_address_p (rtx);
extern int byte_accesses_mergeable_p (rtx, rtx);
extern int same_cmp_preceding_p (rtx);
extern int same_cmp_following_p (rtx);

/* Used in builtins.c */
extern rtx h8300_return_addr_rtx (int, rtx);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern struct rtx_def *function_arg (CUMULATIVE_ARGS *,
				     enum machine_mode, tree, int);
extern int h8300_funcvec_function_p (tree);
extern int h8300_eightbit_data_p (tree);
extern int h8300_tiny_data_p (tree);
#endif /* TREE_CODE */

extern void h8300_init_once (void);
extern int h8300_can_use_return_insn_p (void);
extern void h8300_expand_prologue (void);
extern void h8300_expand_epilogue (void);
extern int h8300_current_function_interrupt_function_p (void);
extern int h8300_initial_elimination_offset (int, int);
extern int h8300_hard_regno_rename_ok (unsigned int, unsigned int);

struct cpp_reader;
extern void h8300_pr_interrupt (struct cpp_reader *);
extern void h8300_pr_saveall (struct cpp_reader *);

#endif /* ! GCC_H8300_PROTOS_H */
