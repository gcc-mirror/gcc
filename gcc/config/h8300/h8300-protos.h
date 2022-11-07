/* Definitions of target machine for GNU compiler.
   Renesas H8/300 version
   Copyright (C) 2000-2022 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_H8300_PROTOS_H
#define GCC_H8300_PROTOS_H

/* Declarations for functions used in insn-output.cc.  */
#ifdef RTX_CODE
extern unsigned int compute_mov_length (rtx *);
extern const char *output_plussi (rtx *, bool);
extern unsigned int compute_plussi_length (rtx *, bool);
extern const char *output_a_shift (rtx[4], rtx_code);
extern unsigned int compute_a_shift_length (rtx[4], rtx_code);
extern const char *output_a_rotate (enum rtx_code, rtx *);
extern unsigned int compute_a_rotate_length (rtx *);
extern const char *output_simode_bld (int, rtx[]);
extern void final_prescan_insn (rtx_insn *, rtx *, int);
extern int h8300_expand_movsi (rtx[]);
extern machine_mode  h8300_select_cc_mode (RTX_CODE, rtx, rtx);
extern const char *output_logical_op (machine_mode, rtx_code code,
				      rtx *, rtx_insn *);
extern unsigned int compute_logical_op_length (machine_mode, rtx_code,
					      rtx *, rtx_insn *);

extern int compute_a_shift_cc (rtx *, rtx_code);
#ifdef HAVE_ATTR_cc
extern enum attr_cc compute_plussi_cc (rtx *);
#endif
extern void h8300_expand_store (rtx[]);
extern bool expand_a_shift (machine_mode, enum rtx_code, rtx[]);
extern int h8300_shift_needs_scratch_p (int, machine_mode, rtx_code);
extern int expand_a_rotate (rtx[]);
extern int fix_bit_operand (rtx *, enum rtx_code);
extern int h8300_adjust_insn_length (rtx, int);
extern void split_adds_subs (machine_mode, rtx[]);

extern int h8300_eightbit_constant_address_p (rtx);
extern int h8300_tiny_constant_address_p (rtx);
extern int byte_accesses_mergeable_p (rtx, rtx);
extern int same_cmp_preceding_p (rtx_insn *);
extern int same_cmp_following_p (rtx_insn *);

/* Used in builtins.cc */
extern rtx h8300_return_addr_rtx (int, rtx);

/* Classifies an h8sx shift operation.

   H8SX_SHIFT_NONE
	The shift cannot be done in a single instruction.

   H8SX_SHIFT_UNARY
	The shift is effectively a unary operation.  The instruction will
	allow any sort of destination operand and have a format similar
	to neg and not.  This is true of certain power-of-2 shifts.

   H8SX_SHIFT_BINARY
	The shift is a binary operation.  The destination must be a
	register and the source can be a register or a constant.  */
enum h8sx_shift_type {
  H8SX_SHIFT_NONE,
  H8SX_SHIFT_UNARY,
  H8SX_SHIFT_BINARY
};

extern enum h8sx_shift_type h8sx_classify_shift (machine_mode, enum rtx_code, rtx);
extern int h8300_ldm_stm_parallel (rtvec, int, int);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int h8300_funcvec_function_p (tree);
extern int h8300_eightbit_data_p (tree);
extern int h8300_tiny_data_p (tree);
#endif /* TREE_CODE */

extern int h8300_can_use_return_insn_p (void);
extern void h8300_expand_prologue (void);
extern void h8300_expand_epilogue (bool);
extern int h8300_current_function_interrupt_function_p (void);
extern int h8300_current_function_monitor_function_p (void);
extern int h8300_initial_elimination_offset (int, int);
extern int h8300_regs_ok_for_stm (int, rtx[]);
extern int h8300_hard_regno_rename_ok (unsigned int, unsigned int);
extern bool h8300_move_ok (rtx, rtx);
extern bool pre_incdec_with_reg (rtx, unsigned int);

struct cpp_reader;
extern void h8300_pr_interrupt (struct cpp_reader *);
extern void h8300_pr_saveall (struct cpp_reader *);
extern enum reg_class  h8300_reg_class_from_letter (int);
extern unsigned int    h8300_insn_length_from_table (rtx_insn *, rtx *);
extern const char *    output_h8sx_shift (rtx *, int, int);
extern bool            h8300_operands_match_p (rtx *);
extern bool            h8sx_mergeable_memrefs_p (rtx, rtx);
extern poly_int64      h8300_push_rounding (poly_int64);

#endif /* ! GCC_H8300_PROTOS_H */
