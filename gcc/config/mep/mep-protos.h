/* Prototypes for exported functions defined in mep.c
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Red Hat Inc (dj@redhat.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

extern int mep_regno_reg_class (int);
extern rtx mep_mulr_source (rtx, rtx, rtx, rtx);
extern bool mep_reuse_lo_p (rtx, rtx, rtx, bool);
extern bool mep_use_post_modify_p (rtx, rtx, rtx);
extern bool mep_allow_clip (rtx, rtx, int);
extern bool mep_bit_position_p (rtx, bool);
extern bool mep_split_mov (rtx *, int);
extern bool mep_vliw_mode_match (rtx);
extern bool mep_vliw_jmp_match (rtx);
extern bool mep_multi_slot (rtx);
extern bool mep_legitimate_address (enum machine_mode, rtx, int);
extern int mep_legitimize_address (rtx *, rtx, enum machine_mode);
extern int mep_legitimize_reload_address (rtx *, enum machine_mode, int, /*enum reload_type*/ int, int);
extern int mep_core_address_length (rtx, int);
extern int mep_cop_address_length (rtx, int);
extern bool mep_expand_mov (rtx *, enum machine_mode);
extern bool mep_mov_ok (rtx *, enum machine_mode);
extern void mep_split_wide_move (rtx *, enum machine_mode);
#ifdef RTX_CODE
extern bool mep_expand_setcc (rtx *);
extern rtx mep_expand_cbranch (rtx *);
#endif
extern const char *mep_emit_cbranch (rtx *, int);
extern void mep_expand_call (rtx *, int);
extern rtx mep_find_base_term (rtx);
extern enum reg_class mep_secondary_input_reload_class (enum reg_class, enum machine_mode, rtx);
extern enum reg_class mep_secondary_output_reload_class (enum reg_class, enum machine_mode, rtx);
extern bool mep_secondary_memory_needed (enum reg_class, enum reg_class,
					 enum machine_mode);
extern void mep_expand_reload (rtx *, enum machine_mode);
extern enum reg_class mep_preferred_reload_class (rtx, enum reg_class);
extern int mep_register_move_cost (enum machine_mode, enum reg_class, enum reg_class);
extern void mep_init_expanders (void);
extern rtx mep_return_addr_rtx (int);
extern bool mep_epilogue_uses (int);
extern int mep_elimination_offset (int, int);
extern void mep_expand_prologue (void);
extern void mep_expand_epilogue (void);
extern void mep_expand_eh_return (rtx *);
extern void mep_emit_eh_epilogue (rtx *);
extern void mep_expand_sibcall_epilogue (void);
extern rtx mep_return_stackadj_rtx (void);
extern rtx mep_return_handler_rtx (void);
extern void mep_function_profiler (FILE *);
extern const char *mep_emit_bb_trace_ret (void);
extern void mep_print_operand_address (FILE *, rtx);
extern void mep_print_operand (FILE *, rtx, int);
extern void mep_final_prescan_insn (rtx, rtx *, int);
extern void mep_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
extern bool mep_return_in_memory (const_tree, const_tree);
extern rtx mep_function_value (const_tree, const_tree);
extern rtx mep_libcall_value (enum machine_mode);
extern void mep_asm_output_opcode (FILE *, const char *);
extern void mep_note_pragma_disinterrupt (const char *);
extern void mep_note_pragma_call (const char *);
extern void mep_file_cleanups (void);
extern const char *mep_strip_name_encoding (const char *);
extern void mep_output_aligned_common (FILE *, tree, const char *,
				       int, int, int);
extern void mep_emit_doloop (rtx *, int);
extern bool mep_vliw_function_p (tree);
extern bool mep_store_data_bypass_p (rtx, rtx);
extern bool mep_mul_hilo_bypass_p (rtx, rtx);
extern bool mep_ipipe_ldc_p (rtx);
extern bool mep_emit_intrinsic (int, const rtx *);
extern bool mep_expand_unary_intrinsic (int, rtx *);
extern bool mep_expand_binary_intrinsic (int, int, int, int, rtx *);
extern int mep_intrinsic_length (int);

extern void mep_register_pragmas (void);
extern int mep_section_tag (rtx);
extern bool mep_lookup_pragma_call (const char *);
extern bool mep_have_core_copro_moves_p;
extern bool mep_have_copro_copro_moves_p;

extern bool mep_cannot_change_mode_class (enum machine_mode, enum machine_mode,
					  enum reg_class);

/* These are called from mep-pragmas (front end) and then call into
   the RTL layer to re-initialize the register tables once we're done
   changing them via pragmas.  */
extern void mep_save_register_info (void);
extern void mep_reinit_regs (void);
extern void mep_init_regs (void);


extern int cgen_h_uint_6a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_7a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_8a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_6a2_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_22a4_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_2a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_24a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_6a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_5a4_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_2a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_16a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_3a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_5a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_16a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_8a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_7a2_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_6a4_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_5a8_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_4a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_10a1_immediate (rtx, enum machine_mode);
extern int cgen_h_sint_12a1_immediate (rtx, enum machine_mode);
extern int cgen_h_uint_20a1_immediate (rtx, enum machine_mode);
