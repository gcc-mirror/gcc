/* Prototypes of target machine for GNU compiler for Xtensa.
   Copyright 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef __XTENSA_PROTOS_H__
#define __XTENSA_PROTOS_H__

/* Functions to test whether an immediate fits in a given field.  */
extern int xtensa_simm7 (int);
extern int xtensa_simm8 (int);
extern int xtensa_simm8x256 (int);
extern int xtensa_simm12b (int);
extern int xtensa_uimm8 (int);
extern int xtensa_uimm8x2 (int);
extern int xtensa_uimm8x4 (int);
extern int xtensa_ai4const (int);
extern int xtensa_lsi4x4 (int);
extern int xtensa_b4const (int);
extern int xtensa_b4constu (int);
extern int xtensa_tp7 (int);

/* Functions within xtensa.c that we reference.  */
#ifdef RTX_CODE
extern int xt_true_regnum (rtx);
extern int add_operand (rtx, enum machine_mode);
extern int arith_operand (rtx, enum machine_mode);
extern int nonimmed_operand (rtx, enum machine_mode);
extern int mem_operand (rtx, enum machine_mode);
extern int xtensa_valid_move (enum machine_mode, rtx *);
extern int mask_operand (rtx, enum machine_mode);
extern int extui_fldsz_operand (rtx, enum machine_mode);
extern int sext_operand (rtx, enum machine_mode);
extern int sext_fldsz_operand (rtx, enum machine_mode);
extern int lsbitnum_operand (rtx, enum machine_mode);
extern int branch_operand (rtx, enum machine_mode);
extern int ubranch_operand (rtx, enum machine_mode);
extern int call_insn_operand (rtx, enum machine_mode);
extern int move_operand (rtx, enum machine_mode);
extern int smalloffset_mem_p (rtx);
extern int constantpool_address_p (rtx);
extern int constantpool_mem_p (rtx);
extern int const_float_1_operand (rtx, enum machine_mode);
extern int fpmem_offset_operand (rtx, enum machine_mode);
extern void xtensa_extend_reg (rtx, rtx);
extern int branch_operator (rtx, enum machine_mode);
extern int ubranch_operator (rtx, enum machine_mode);
extern int boolean_operator (rtx, enum machine_mode);
extern void xtensa_expand_conditional_branch (rtx *, enum rtx_code);
extern int xtensa_expand_conditional_move (rtx *, int);
extern int xtensa_expand_scc (rtx *);
extern int xtensa_expand_block_move (rtx *);
extern void xtensa_split_operand_pair (rtx *, enum machine_mode);
extern int xtensa_emit_move_sequence (rtx *, enum machine_mode);
extern rtx xtensa_copy_incoming_a7 (rtx);
extern void xtensa_emit_block_move (rtx *, rtx *, int);
extern void xtensa_expand_nonlocal_goto (rtx *);
extern void xtensa_emit_loop_end (rtx, rtx *);
extern char *xtensa_emit_call (int, rtx *);

#ifdef TREE_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, int);
extern void xtensa_va_start (tree, rtx);
extern rtx xtensa_va_arg (tree, tree);
#endif /* TREE_CODE */

extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern void xtensa_output_literal (FILE *, rtx, enum machine_mode, int);
extern rtx xtensa_return_addr (int, rtx);
extern rtx xtensa_builtin_saveregs (void);
extern enum reg_class xtensa_preferred_reload_class (rtx, enum reg_class, int);
extern enum reg_class xtensa_secondary_reload_class (enum reg_class,
						     enum machine_mode, rtx,
						     int);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree);
extern struct rtx_def *function_arg (CUMULATIVE_ARGS *, enum machine_mode,
				     tree, int);
#endif /* TREE_CODE */

extern int xtensa_mask_immediate (int);
extern int xtensa_mem_offset (unsigned, enum machine_mode);
extern void xtensa_setup_frame_addresses (void);
extern int xtensa_dbx_register_number (int);
extern void override_options (void);
extern long compute_frame_size (int);
extern int xtensa_frame_pointer_required (void);
extern void xtensa_expand_prologue (void);
extern void order_regs_for_local_alloc (void);

#endif /* !__XTENSA_PROTOS_H__ */
