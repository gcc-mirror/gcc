/* Definitions of target machine for GNU compiler for Hitachi / SuperH SH.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2004
   Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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

#ifndef GCC_SH_PROTOS_H
#define GCC_SH_PROTOS_H

#ifdef RTX_CODE
extern struct rtx_def *sh_builtin_saveregs PARAMS ((void));
extern struct rtx_def *prepare_scc_operands PARAMS ((enum rtx_code));

/* Declare functions defined in sh.c and used in templates.  */

extern const char *output_branch PARAMS ((int, rtx, rtx *));
extern const char *output_ieee_ccmpeq PARAMS ((rtx, rtx *));
extern const char *output_branchy_insn PARAMS ((enum rtx_code, const char *, rtx, rtx *));
extern const char *output_movedouble PARAMS ((rtx, rtx[], enum machine_mode));
extern const char *output_movepcrel PARAMS ((rtx, rtx[], enum machine_mode));
extern const char *output_far_jump PARAMS ((rtx, rtx));

extern void machine_dependent_reorg PARAMS ((rtx));
extern struct rtx_def *sfunc_uses_reg PARAMS ((rtx));
extern int barrier_align PARAMS ((rtx));
extern int sh_loop_align PARAMS ((rtx));
extern int fp_zero_operand PARAMS ((rtx));
extern int fp_one_operand PARAMS ((rtx));
extern int fp_int_operand PARAMS ((rtx));
extern rtx get_fpscr_rtx PARAMS ((void));
extern rtx legitimize_pic_address PARAMS ((rtx, enum machine_mode, rtx));
extern int nonpic_symbol_mentioned_p PARAMS ((rtx));
extern void emit_sf_insn PARAMS ((rtx));
extern void emit_df_insn PARAMS ((rtx));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void output_pic_addr_const PARAMS ((FILE *, rtx));
extern int expand_block_move PARAMS ((rtx *));
extern int prepare_move_operands PARAMS ((rtx[], enum machine_mode mode));
extern void from_compare PARAMS ((rtx *, int));
extern int shift_insns_rtx PARAMS ((rtx));
extern int shiftcosts PARAMS ((rtx));
extern int addsubcosts PARAMS ((rtx));
extern int andcosts PARAMS ((rtx));
extern int multcosts PARAMS ((rtx));
extern void gen_ashift PARAMS ((int, int, rtx));
extern void gen_ashift_hi PARAMS ((int, int, rtx));
extern void gen_shifty_op PARAMS ((int, rtx *));
extern void gen_shifty_hi_op PARAMS ((int, rtx *));
extern int expand_ashiftrt PARAMS ((rtx *));
extern int sh_dynamicalize_shift_p PARAMS ((rtx));
extern int shl_and_kind PARAMS ((rtx, rtx, int *));
extern int shl_and_length PARAMS ((rtx));
extern int shl_and_scr_length PARAMS ((rtx));
extern int gen_shl_and PARAMS ((rtx, rtx, rtx, rtx));
extern int shl_sext_kind PARAMS ((rtx, rtx, int *));
extern int shl_sext_length PARAMS ((rtx));
extern int gen_shl_sext PARAMS ((rtx, rtx, rtx, rtx));
extern rtx gen_datalabel_ref PARAMS ((rtx));
extern int regs_used PARAMS ((rtx, int));
extern void fixup_addr_diff_vecs PARAMS ((rtx));
extern int get_dest_uid PARAMS ((rtx, int));
extern void final_prescan_insn PARAMS ((rtx, rtx *, int));
extern int symbol_ref_operand PARAMS ((rtx, enum machine_mode));
extern int system_reg_operand PARAMS ((rtx, enum machine_mode));
extern int general_movsrc_operand PARAMS ((rtx, enum machine_mode));
extern int general_movdst_operand PARAMS ((rtx, enum machine_mode));
extern int arith_reg_operand PARAMS ((rtx, enum machine_mode));
extern int fp_arith_reg_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int arith_reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int logical_operand PARAMS ((rtx, enum machine_mode));
extern int tertiary_reload_operand PARAMS ((rtx, enum machine_mode));
extern int fpscr_operand PARAMS ((rtx, enum machine_mode));
extern int fpul_operand PARAMS ((rtx, enum machine_mode));
extern int commutative_float_operator PARAMS ((rtx, enum machine_mode));
extern int noncommutative_float_operator PARAMS ((rtx, enum machine_mode));
extern int reg_unused_after PARAMS ((rtx, rtx));
extern void expand_sf_unop PARAMS ((rtx (*)(rtx, rtx, rtx), rtx *));
extern void expand_sf_binop PARAMS ((rtx (*)(rtx, rtx, rtx, rtx), rtx *));
extern void expand_df_unop PARAMS ((rtx (*)(rtx, rtx, rtx), rtx *));
extern void expand_df_binop PARAMS ((rtx (*)(rtx, rtx, rtx, rtx), rtx *));
extern void expand_fp_branch PARAMS ((rtx (*)(void), rtx (*)(void)));
extern int sh_insn_length_adjustment PARAMS ((rtx));
extern int sh_can_redirect_branch PARAMS ((rtx, rtx));
extern void sh_expand_unop_v2sf PARAMS ((enum rtx_code, rtx, rtx));
extern void sh_expand_binop_v2sf PARAMS ((enum rtx_code, rtx, rtx, rtx));
#ifdef TREE_CODE
extern void sh_va_start PARAMS ((tree, rtx));
extern rtx sh_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern tree sh_build_va_list PARAMS ((void));
#endif /* TREE_CODE */

extern const char *output_jump_label_table PARAMS ((void));
extern int sh_handle_pragma PARAMS ((int (*)(void), void (*)(int), const char *));
extern struct rtx_def *get_fpscr_rtx PARAMS ((void));
extern void output_file_start PARAMS ((FILE *));
extern int sh_media_register_for_return PARAMS ((void));
extern void sh_expand_prologue PARAMS ((void));
extern void sh_expand_epilogue PARAMS ((bool));
extern int sh_need_epilogue PARAMS ((void));
extern int initial_elimination_offset PARAMS ((int, int));
extern int fldi_ok PARAMS ((void));
extern int sh_pr_n_sets PARAMS ((void));
extern int sh_hard_regno_rename_ok PARAMS ((unsigned int, unsigned int));
extern int sh_cfun_interrupt_handler_p PARAMS ((void));
extern void sh_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern bool sh_cannot_change_mode_class
	      PARAMS ((enum machine_mode, enum machine_mode, enum reg_class));
extern void sh_mark_label PARAMS ((rtx, int));
extern int sh_register_move_cost
  PARAMS ((enum machine_mode mode, enum reg_class, enum reg_class));
extern int check_use_sfunc_addr (rtx, rtx);

#ifdef HARD_CONST
extern void fpscr_set_from_mem PARAMS ((int, HARD_REG_SET));
#endif

#ifdef GCC_C_PRAGMA_H
extern void sh_pr_interrupt PARAMS ((cpp_reader *));
extern void sh_pr_trapa PARAMS ((cpp_reader *));
extern void sh_pr_nosave_low_regs PARAMS ((cpp_reader *));
#endif

#endif /* ! GCC_SH_PROTOS_H */
