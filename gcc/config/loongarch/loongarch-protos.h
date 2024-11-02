/* Prototypes of target machine for GNU compiler.  LoongArch version.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.
   Based on MIPS target for GNU compiler.

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

#ifndef GCC_LOONGARCH_PROTOS_H
#define GCC_LOONGARCH_PROTOS_H

/* Classifies a SYMBOL_REF, LABEL_REF or UNSPEC address.

   SYMBOL_GOT_DISP
       The symbol's value will be loaded directly from the GOT.

   SYMBOL_PCREL
       The symbol's value will be loaded directly from data section within
       +/- 2GiB range.

   SYMBOL_PCREL64
       The symbol's value will be loaded directly from data section within
       +/- 8EiB range.

   SYMBOL_TLS
       A thread-local symbol.

   SYMBOL_TLS_IE
   SYMBOL_TLSGD
   SYMBOL_TLSLDM
       UNSPEC wrappers around SYMBOL_TLS, corresponding to the
       thread-local storage relocation operators.
   */
enum loongarch_symbol_type {
  SYMBOL_GOT_DISP,
  SYMBOL_PCREL,
  SYMBOL_PCREL64,
  SYMBOL_TLS,
  SYMBOL_TLS_IE,
  SYMBOL_TLS_LE,
  SYMBOL_TLSGD,
  SYMBOL_TLSLDM,
};
#define NUM_SYMBOL_TYPES (SYMBOL_TLSLDM + 1)

/* Routines implemented in loongarch.cc.  */
extern rtx loongarch_emit_move (rtx, rtx);
extern HOST_WIDE_INT loongarch_initial_elimination_offset (int, int);
extern void loongarch_expand_prologue (void);
extern void loongarch_expand_epilogue (int);
extern bool loongarch_can_use_return_insn (void);

extern bool loongarch_symbolic_constant_p (rtx, enum loongarch_symbol_type *);
extern int loongarch_regno_mode_ok_for_base_p (int, machine_mode, bool);
extern int loongarch_address_insns (rtx, machine_mode, bool);
extern int loongarch_const_insns (rtx);
extern int loongarch_split_const_insns (rtx);
extern int loongarch_split_128bit_const_insns (rtx);
extern int loongarch_load_store_insns (rtx, rtx_insn *);
extern int loongarch_idiv_insns (machine_mode);
#ifdef RTX_CODE
extern void loongarch_emit_binary (enum rtx_code, rtx, rtx, rtx);
#endif
extern rtx loongarch_unspec_address (rtx, enum loongarch_symbol_type);
extern rtx loongarch_strip_unspec_address (rtx);
extern void loongarch_move_integer (rtx, rtx, unsigned HOST_WIDE_INT);
extern bool loongarch_legitimize_move (machine_mode, rtx, rtx);
extern rtx loongarch_legitimize_call_address (rtx);

extern rtx loongarch_subword (rtx, bool);
extern bool loongarch_split_move_p (rtx, rtx);
extern void loongarch_split_move (rtx, rtx);
extern bool loongarch_addu16i_imm12_operand_p (HOST_WIDE_INT, machine_mode);
extern void loongarch_split_plus_constant (rtx *, machine_mode);
extern void loongarch_split_vector_move (rtx, rtx);
extern const char *loongarch_output_move (rtx, rtx);
#ifdef RTX_CODE
extern void loongarch_expand_scc (rtx *);
extern void loongarch_expand_vec_cmp (rtx *);
extern void loongarch_expand_conditional_branch (rtx *);
extern void loongarch_expand_conditional_move (rtx *);
extern void loongarch_expand_conditional_trap (rtx);
#endif
extern void loongarch_set_return_address (rtx, rtx);
extern bool loongarch_move_by_pieces_p (unsigned HOST_WIDE_INT, unsigned int);
extern bool loongarch_expand_block_move (rtx, rtx, rtx, rtx);
extern bool loongarch_do_optimize_block_move_p (void);

extern bool loongarch_expand_ext_as_unaligned_load (rtx, rtx, HOST_WIDE_INT,
						    HOST_WIDE_INT, bool);
extern bool loongarch_expand_ins_as_unaligned_store (rtx, rtx, HOST_WIDE_INT,
						     HOST_WIDE_INT);
extern HOST_WIDE_INT loongarch_debugger_offset (rtx, HOST_WIDE_INT);

extern void loongarch_output_external (FILE *, tree, const char *);
extern void loongarch_output_ascii (FILE *, const char *, size_t);
extern bool loongarch_small_data_pattern_p (rtx);
extern rtx loongarch_rewrite_small_data (rtx);
extern rtx loongarch_return_addr (int, rtx);

extern bool loongarch_const_vector_same_val_p (rtx, machine_mode);
extern bool loongarch_const_vector_same_bytes_p (rtx, machine_mode);
extern bool loongarch_const_vector_same_int_p (rtx, machine_mode, HOST_WIDE_INT,
					  HOST_WIDE_INT);
extern bool loongarch_const_vector_shuffle_set_p (rtx, machine_mode);
extern bool loongarch_const_vector_bitimm_set_p (rtx, machine_mode);
extern bool loongarch_const_vector_bitimm_clr_p (rtx, machine_mode);
extern rtx loongarch_lsx_vec_parallel_const_half (machine_mode, bool);
extern rtx loongarch_gen_const_int_vector (machine_mode, HOST_WIDE_INT);
extern enum reg_class loongarch_secondary_reload_class (enum reg_class,
							machine_mode,
							rtx, bool);
extern int loongarch_class_max_nregs (enum reg_class, machine_mode);

extern machine_mode loongarch_hard_regno_caller_save_mode (unsigned int,
							   unsigned int,
							   machine_mode);
extern const char *loongarch_output_conditional_branch (rtx_insn *, rtx *,
							const char *,
							const char *);
extern const char *loongarch_output_order_conditional_branch (rtx_insn *,
							      rtx *,
							      bool);
extern const char *loongarch_output_equal_conditional_branch (rtx_insn *,
							      rtx *,
							      bool);
extern const char *loongarch_output_division (const char *, rtx *);
extern const char *loongarch_lsx_output_division (const char *, rtx *);
extern const char *loongarch_output_probe_stack_range (rtx, rtx, rtx);
extern bool loongarch_hard_regno_rename_ok (unsigned int, unsigned int);
extern int loongarch_dspalu_bypass_p (rtx, rtx);
extern rtx loongarch_prefetch_cookie (rtx, rtx);

extern bool loongarch_global_symbol_p (const_rtx);
extern bool loongarch_global_symbol_noweak_p (const_rtx);
extern bool loongarch_weak_symbol_p (const_rtx);
extern bool loongarch_symbol_binds_local_p (const_rtx);

extern unsigned int current_section_flags (void);
extern bool loongarch_use_ins_ext_p (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
extern bool loongarch_check_zero_div_p (void);
extern bool loongarch_pre_reload_split (void);
extern int loongarch_use_bstrins_for_ior_with_mask (machine_mode, rtx *);
extern rtx loongarch_rewrite_mem_for_simple_ldst (rtx);

union loongarch_gen_fn_ptrs
{
  rtx (*fn_8) (rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx);
  rtx (*fn_7) (rtx, rtx, rtx, rtx, rtx, rtx, rtx);
  rtx (*fn_6) (rtx, rtx, rtx, rtx, rtx, rtx);
  rtx (*fn_5) (rtx, rtx, rtx, rtx, rtx);
  rtx (*fn_4) (rtx, rtx, rtx, rtx);
};

extern void loongarch_expand_atomic_qihi (union loongarch_gen_fn_ptrs,
					  rtx, rtx, rtx, rtx, rtx);

extern void loongarch_expand_vector_group_init (rtx, rtx);
extern void loongarch_expand_vector_init (rtx, rtx);
extern void loongarch_expand_vec_unpack (rtx op[2], bool, bool);
extern void loongarch_expand_vec_perm (rtx, rtx, rtx, rtx);
extern void loongarch_expand_vec_perm_1 (rtx[]);
extern void loongarch_expand_vector_extract (rtx, rtx, int);
extern void loongarch_expand_vector_reduc (rtx (*)(rtx, rtx, rtx), rtx, rtx);

extern int loongarch_ldst_scaled_shift (machine_mode);
extern bool loongarch_signed_immediate_p (unsigned HOST_WIDE_INT, int, int);
extern bool loongarch_unsigned_immediate_p (unsigned HOST_WIDE_INT, int, int);
extern bool loongarch_12bit_offset_address_p (rtx, machine_mode);
extern bool loongarch_14bit_shifted_offset_address_p (rtx, machine_mode);
extern bool loongarch_base_index_address_p (rtx, machine_mode);
extern rtx loongarch_expand_thread_pointer (rtx);

extern bool loongarch_eh_uses (unsigned int);
extern bool loongarch_epilogue_uses (unsigned int);
extern bool loongarch_load_store_bonding_p (rtx *, machine_mode, bool);
extern bool loongarch_split_symbol_type (enum loongarch_symbol_type);

extern void loongarch_register_frame_header_opt (void);
extern void loongarch_expand_vec_cond_expr (machine_mode, machine_mode, rtx *);
extern void loongarch_expand_vec_cond_mask_expr (machine_mode, machine_mode,
						 rtx *);
extern void loongarch_expand_vec_widen_hilo (rtx, rtx, rtx, bool, bool, const char *);

/* Routines implemented in loongarch-c.c.  */
void loongarch_cpu_cpp_builtins (cpp_reader *);

extern void loongarch_init_builtins (void);
extern void loongarch_atomic_assign_expand_fenv (tree *, tree *, tree *);
extern tree loongarch_builtin_decl (unsigned int, bool);
extern rtx loongarch_expand_builtin (tree, rtx, rtx subtarget ATTRIBUTE_UNUSED,
				     machine_mode, int);
extern rtx loongarch_gen_const_int_vector_shuffle (machine_mode, int);
extern tree loongarch_build_builtin_va_list (void);

extern rtx loongarch_build_signbit_mask (machine_mode, bool, bool);
extern void loongarch_emit_swrsqrtsf (rtx, rtx, machine_mode, bool);
extern void loongarch_emit_swdivsf (rtx, rtx, rtx, machine_mode);
extern bool loongarch_explicit_relocs_p (enum loongarch_symbol_type);
extern bool loongarch_symbol_extreme_p (enum loongarch_symbol_type);
#endif /* ! GCC_LOONGARCH_PROTOS_H */
