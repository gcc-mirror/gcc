/* Definitions of target machine for GNU compiler, Synopsys DesignWare ARC cpu.
   Copyright (C) 2000-2021 Free Software Foundation, Inc.

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

#ifdef RTX_CODE

extern machine_mode arc_select_cc_mode (enum rtx_code, rtx, rtx);

/* Define the function that build the compare insn for scc, bcc and mov*cc.  */
extern struct rtx_def *gen_compare_reg (rtx, machine_mode);

/* Declarations for various fns used in the .md file.  */
extern const char *output_shift (rtx *);
extern bool compact_sda_memory_operand (rtx, machine_mode, bool);
extern bool arc_double_limm_p (rtx);
extern void arc_print_operand (FILE *, rtx, int);
extern void arc_print_operand_address (FILE *, rtx);
extern void arc_final_prescan_insn (rtx_insn *, rtx *, int);
extern const char *arc_output_libcall (const char *);
extern int arc_output_addsi (rtx *operands, bool, bool);
extern int arc_output_commutative_cond_exec (rtx *operands, bool);
extern bool arc_expand_cpymem (rtx *operands);
extern bool prepare_move_operands (rtx *operands, machine_mode mode);
extern void emit_shift (enum rtx_code, rtx, rtx, rtx);
extern void arc_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx);
extern void arc_split_compare_and_swap (rtx *);
extern void arc_expand_compare_and_swap (rtx *);
extern bool compact_memory_operand_p (rtx, machine_mode, bool, bool);
extern bool arc_is_uncached_mem_p (rtx);
extern bool gen_operands_ldd_std (rtx *operands, bool load, bool commute);
extern bool arc_check_multi (rtx, bool);
extern void arc_adjust_reg_alloc_order (void);
extern bool arc_check_ior_const (HOST_WIDE_INT );
extern void arc_split_ior (rtx *);
extern bool arc_check_mov_const (HOST_WIDE_INT );
extern bool arc_split_mov_const (rtx *);
extern bool arc_can_use_return_insn (void);
extern bool arc_split_move_p (rtx *);
#endif /* RTX_CODE */

extern bool arc_ccfsm_branch_deleted_p (void);
extern void arc_ccfsm_record_branch_deleted (void);

void arc_asm_output_aligned_decl_local (FILE *, tree, const char *,
					unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT);
extern rtx arc_return_addr_rtx (int , rtx);
extern bool check_if_valid_regno_const (rtx *, int);
extern bool arc_legitimate_constant_p (machine_mode, rtx);
extern bool arc_legitimate_pic_addr_p (rtx);
extern bool arc_raw_symbolic_reference_mentioned_p (rtx, bool);
extern bool arc_is_longcall_p (rtx);
extern bool arc_is_shortcall_p (rtx);
extern bool valid_brcc_with_delay_p (rtx *);
extern bool arc_ccfsm_cond_exec_p (void);
extern rtx disi_highpart (rtx);
extern int arc_adjust_insn_length (rtx_insn *, int, bool);
extern int arc_corereg_hazard (rtx, rtx);
extern int arc_hazard (rtx_insn *, rtx_insn *);
extern int arc_write_ext_corereg (rtx);
extern rtx gen_acc1 (void);
extern rtx gen_acc2 (void);
extern bool arc_branch_size_unknown_p (void);
struct arc_ccfsm;
extern void arc_ccfsm_record_condition (rtx, bool, rtx_insn *,
					struct arc_ccfsm *);
extern void arc_expand_prologue (void);
extern void arc_expand_epilogue (int);
extern void arc_init_expanders (void);
extern int arc_check_millicode (rtx op, int offset, int load_p);
extern void arc_clear_unalign (void);
extern void arc_toggle_unalign (void);
extern void split_addsi (rtx *);
extern void split_subsi (rtx *);
extern void arc_split_move (rtx *);
extern const char *arc_short_long (rtx_insn *insn, const char *, const char *);
extern rtx arc_regno_use_in (unsigned int, rtx);
extern int arc_label_align (rtx_insn *label);
extern bool arc_text_label (rtx_insn *insn);

extern bool arc_short_comparison_p (rtx, int);
extern bool arc_epilogue_uses (int regno);
extern bool arc_eh_uses (int regno);
/* insn-attrtab.c doesn't include reload.h, which declares regno_clobbered_p. */
extern int regno_clobbered_p (unsigned int, rtx_insn *, machine_mode, int);
extern bool arc_legitimize_reload_address (rtx *, machine_mode, int, int);
extern void arc_secondary_reload_conv (rtx, rtx, rtx, bool);
extern void arc_cpu_cpp_builtins (cpp_reader *);
extern bool arc_store_addr_hazard_p (rtx_insn *, rtx_insn *);
extern void arc_eh_return_address_location (rtx);
extern bool arc_is_jli_call_p (rtx);
extern void arc_file_end (void);
extern bool arc_is_secure_call_p (rtx);

rtl_opt_pass * make_pass_arc_ifcvt (gcc::context *ctxt);
rtl_opt_pass * make_pass_arc_predicate_delay_insns (gcc::context *ctxt);
