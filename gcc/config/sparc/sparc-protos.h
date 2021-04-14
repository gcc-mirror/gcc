/* Prototypes of target machine for SPARC.
   Copyright (C) 1999-2021 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   64-bit SPARC-V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

#ifndef __SPARC_PROTOS_H__
#define __SPARC_PROTOS_H__

#ifdef TREE_CODE
#ifdef RTX_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
#endif
extern unsigned long sparc_type_code (tree);
#endif /* TREE_CODE */

extern void order_regs_for_local_alloc (void);
extern int sparc_initial_elimination_offset (int);
extern void sparc_expand_prologue (void);
extern void sparc_flat_expand_prologue (void);
extern void sparc_expand_epilogue (bool);
extern void sparc_flat_expand_epilogue (bool);
extern bool sparc_can_use_return_insn_p (void);
extern int check_pic (int);
extern void sparc_profile_hook (int);
extern void sparc_override_options (void);
extern void sparc_output_scratch_registers (FILE *);
extern void sparc_target_macros (void);
extern void sparc_emit_membar_for_model (enum memmodel, int, int);
extern int sparc_branch_cost (bool, bool);

#ifdef RTX_CODE
extern machine_mode select_cc_mode (enum rtx_code, rtx, rtx);
/* Define the function that build the compare insn for scc and bcc.  */
extern rtx gen_compare_reg (rtx cmp);
extern rtx sparc_emit_float_lib_cmp (rtx, rtx, enum rtx_code);
extern void sparc_emit_floatunsdi (rtx [2], machine_mode);
extern void sparc_emit_fixunsdi (rtx [2], machine_mode);
extern void emit_tfmode_binop (enum rtx_code, rtx *);
extern void emit_tfmode_unop (enum rtx_code, rtx *);
extern void emit_tfmode_cvt (enum rtx_code, rtx *);
extern bool constant_address_p (rtx);
extern bool legitimate_pic_operand_p (rtx);
extern rtx sparc_legitimize_reload_address (rtx, machine_mode, int, int,
					    int, int *win);
extern void load_got_register (void);
extern void sparc_emit_call_insn (rtx, rtx);
extern void sparc_defer_case_vector (rtx, rtx, int);
extern bool sparc_expand_move (machine_mode, rtx *);
extern void sparc_emit_set_symbolic_const64 (rtx, rtx, rtx);
extern int sparc_split_reg_mem_legitimate (rtx, rtx);
extern void sparc_split_reg_mem (rtx, rtx, machine_mode);
extern void sparc_split_mem_reg (rtx, rtx, machine_mode);
extern int sparc_split_reg_reg_legitimate (rtx, rtx);
extern void sparc_split_reg_reg (rtx, rtx, machine_mode);
extern const char *output_load_pcrel_sym (rtx *);
extern const char *output_ubranch (rtx, rtx_insn *);
extern const char *output_cbranch (rtx, rtx, int, int, int, rtx_insn *);
extern const char *output_return (rtx_insn *);
extern const char *output_sibcall (rtx_insn *, rtx);
extern const char *output_v8plus_shift (rtx_insn *, rtx *, const char *);
extern const char *output_v8plus_mult (rtx_insn *, rtx *, const char *);
extern const char *output_v9branch (rtx, rtx, int, int, int, int, rtx_insn *);
extern const char *output_probe_stack_range (rtx, rtx);
extern const char *output_cbcond (rtx, rtx, rtx_insn *);
extern bool emit_scc_insn (rtx []);
extern void emit_conditional_branch_insn (rtx []);
extern int registers_ok_for_ldd_peep (rtx, rtx);
extern int mems_ok_for_ldd_peep (rtx, rtx, rtx);
extern rtx widen_mem_for_ldd_peep (rtx, rtx, machine_mode);
extern int empty_delay_slot (rtx_insn *);
extern int emit_cbcond_nop (rtx_insn *);
extern int eligible_for_return_delay (rtx_insn *);
extern int eligible_for_sibcall_delay (rtx_insn *);
extern int emit_move_sequence (rtx, machine_mode);
extern int fp_sethi_p (rtx);
extern int fp_mov_p (rtx);
extern int fp_high_losum_p (rtx);
extern int mem_min_alignment (rtx, int);
extern int pic_address_needs_scratch (rtx);
extern int register_ok_for_ldd (rtx);
extern int memory_ok_for_ldd (rtx);
extern int v9_regcmp_p (enum rtx_code);
/* Function used for V8+ code generation.  Returns 1 if the high
   32 bits of REG are 0 before INSN.  */   
extern int sparc_check_64 (rtx, rtx_insn *);
extern rtx gen_df_reg (rtx, int);
extern void sparc_expand_compare_and_swap (rtx op[]);
extern void sparc_expand_vector_init (rtx, rtx);
extern void sparc_expand_vec_perm_bmask(machine_mode, rtx);
extern bool sparc_expand_conditional_move (machine_mode, rtx *);
extern void sparc_expand_vcond (machine_mode, rtx *, int, int);
unsigned int sparc_regmode_natural_size (machine_mode);
#endif /* RTX_CODE */

extern rtl_opt_pass *make_pass_work_around_errata (gcc::context *);

/* Routines implemented in sparc-d.c  */
extern void sparc_d_target_versions (void);
extern void sparc_d_register_target_info (void);

#endif /* __SPARC_PROTOS_H__ */
