/* Definitions of target machine for GNU compiler, Synopsys DesignWare ARC cpu.
   Copyright (C) 2000-2016 Free Software Foundation, Inc.

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
extern void arc_output_function_epilogue (FILE *, HOST_WIDE_INT, int);
extern const char *output_shift (rtx *);
extern bool compact_sda_memory_operand (rtx op,machine_mode  mode);
extern bool arc_double_limm_p (rtx);
extern void arc_print_operand (FILE *, rtx, int);
extern void arc_print_operand_address (FILE *, rtx);
extern void arc_final_prescan_insn (rtx_insn *, rtx *, int);
extern void arc_set_default_type_attributes(tree type);
extern const char *arc_output_libcall (const char *);
extern bool prepare_extend_operands (rtx *operands, enum rtx_code code,
				     machine_mode omode);
extern int arc_output_addsi (rtx *operands, bool, bool);
extern int arc_output_commutative_cond_exec (rtx *operands, bool);
extern bool arc_expand_movmem (rtx *operands);
extern bool prepare_move_operands (rtx *operands, machine_mode mode);
extern void emit_shift (enum rtx_code, rtx, rtx, rtx);
extern void arc_expand_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx);
extern void arc_split_compare_and_swap (rtx *);
extern void arc_expand_compare_and_swap (rtx *);
extern bool compact_memory_operand_p (rtx, machine_mode, bool, bool);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern enum arc_function_type arc_compute_function_type (struct function *);
#endif /* TREE_CODE */


extern void arc_init (void);
extern unsigned int arc_compute_frame_size (int);
extern bool arc_ccfsm_branch_deleted_p (void);
extern void arc_ccfsm_record_branch_deleted (void);

void arc_asm_output_aligned_decl_local (FILE *, tree, const char *,
					unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT,
					unsigned HOST_WIDE_INT);
extern rtx arc_return_addr_rtx (int , rtx);
extern bool check_if_valid_regno_const (rtx *, int);
extern bool check_if_valid_sleep_operand (rtx *, int);
extern bool arc_legitimate_constant_p (machine_mode, rtx);
extern bool arc_legitimate_pc_offset_p (rtx);
extern bool arc_legitimate_pic_addr_p (rtx);
extern bool arc_raw_symbolic_reference_mentioned_p (rtx, bool);
extern bool arc_legitimate_pic_operand_p (rtx);
extern bool arc_is_longcall_p (rtx);
extern bool arc_is_shortcall_p (rtx);
extern bool arc_profile_call (rtx callee);
extern bool valid_brcc_with_delay_p (rtx *);
extern bool small_data_pattern (rtx , machine_mode);
extern rtx arc_rewrite_small_data (rtx);
extern bool arc_ccfsm_cond_exec_p (void);
struct secondary_reload_info;
extern int arc_register_move_cost (machine_mode, enum reg_class,
				   enum reg_class);
extern rtx disi_highpart (rtx);
extern int arc_adjust_insn_length (rtx_insn *, int, bool);
extern int arc_corereg_hazard (rtx, rtx);
extern int arc_hazard (rtx_insn *, rtx_insn *);
extern int arc_write_ext_corereg (rtx);
extern rtx gen_acc1 (void);
extern rtx gen_acc2 (void);
extern rtx gen_mlo (void);
extern rtx gen_mhi (void);
extern bool arc_branch_size_unknown_p (void);
struct arc_ccfsm;
extern void arc_ccfsm_record_condition (rtx, bool, rtx_insn *,
					struct arc_ccfsm *);
extern void arc_expand_prologue (void);
extern void arc_expand_epilogue (int);
extern void arc_init_expanders (void);
extern int arc_check_millicode (rtx op, int offset, int load_p);
extern int arc_get_unalign (void);
extern void arc_clear_unalign (void);
extern void arc_toggle_unalign (void);
extern void split_addsi (rtx *);
extern void split_subsi (rtx *);
extern void arc_pad_return (void);
extern void arc_split_move (rtx *);
extern int arc_verify_short (rtx_insn *insn, int unalign, int);
extern const char *arc_short_long (rtx_insn *insn, const char *, const char *);
extern rtx arc_regno_use_in (unsigned int, rtx);
extern int arc_attr_type (rtx_insn *);
extern bool arc_scheduling_not_expected (void);
extern bool arc_sets_cc_p (rtx_insn *insn);
extern int arc_label_align (rtx_insn *label);
extern bool arc_need_delay (rtx_insn *insn);
extern bool arc_text_label (rtx_insn *insn);

extern int arc_decl_pretend_args (tree decl);
extern bool arc_short_comparison_p (rtx, int);
extern bool arc_epilogue_uses (int regno);
extern bool arc_eh_uses (int regno);
/* insn-attrtab.c doesn't include reload.h, which declares regno_clobbered_p. */
extern int regno_clobbered_p (unsigned int, rtx_insn *, machine_mode, int);
extern int arc_return_slot_offset (void);
extern bool arc_legitimize_reload_address (rtx *, machine_mode, int, int);
extern void arc_secondary_reload_conv (rtx, rtx, rtx, bool);
extern bool insn_is_tls_gd_dispatch (rtx_insn *);
